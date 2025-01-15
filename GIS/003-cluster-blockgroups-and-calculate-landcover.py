import networkx as nx
from qgis.core import QgsVectorLayer, QgsFeature, QgsField, QgsSpatialIndex, QgsProject, QgsVectorFileWriter
from qgis.PyQt.QtCore import QVariant
from collections import defaultdict

from qgis.core import (
    QgsProject, QgsVectorLayer, QgsRasterLayer, QgsSpatialIndex, QgsProcessingFeatureSourceDefinition, 
    QgsCoordinateReferenceSystem, QgsCoordinateTransform, QgsField, QgsVectorFileWriter,
    QgsFeatureRequest
)
from qgis import processing
import os

os.environ['GDAL_NUM_THREADS'] = 'ALL_CPUS'

# Function to ensure CRS compatibility
def ensure_crs(layer, target_crs):
    if layer.crs() != target_crs:
        return processing.run("qgis:reprojectlayer", {'INPUT': layer, 'TARGET_CRS': target_crs, 'OUTPUT': 'memory:'})['OUTPUT']
    return layer

# Main Function
def landcover_stat(block_layer, landcover_raster,  fractional_raster,  descriptor_raster, output_csv_path):
    """
    Calculates zonal histogram of a raster layer within vector polygons and saves the result as a CSV.

    Parameters:
    - vector_layer_path (str): Path to the vector layer file (e.g., shapefile).
    - raster_layer_path (str): Path to the raster layer file (e.g., TIFF).
    - output_csv_path (str): Path to save the output CSV file.
    """


    nlcd_crs = landcover_raster.crs()


    # Ensure CRS Compatibility
    block_layer = ensure_crs(block_layer, nlcd_crs)

    # Get the extent of the vector layer
    state_extent = block_layer.extent()

    # Temporary file for clipped raster
    temp_clipped_raster1 = '/Users/amir/Downloads/Data/blocks/temp1.tif'  # Faster in-memory file
    temp_clipped_raster2 = '/Users/amir/Downloads/Data/blocks/temp2.tif'  # Faster in-memory file
    temp_clipped_raster3 = '/Users/amir/Downloads/Data/blocks/temp3.tif'  # Faster in-memory file
    temp_clipped_raster4 = '/Users/amir/Downloads/Data/blocks/temp4.tif'  # Faster in-memory file

    if not fractional_raster is None:

        fractional_raster_clipped = processing.run("gdal:cliprasterbyextent", {'INPUT':fractional_raster,'PROJWIN':f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",'OUTPUT':temp_clipped_raster1})['OUTPUT']

        descriptor_raster_clipped = processing.run("gdal:cliprasterbyextent", {'INPUT':descriptor_raster,'PROJWIN':f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",'OUTPUT':temp_clipped_raster2})['OUTPUT']
        
        # Create inhabited raster: pixel is 1 if fractional > 1 and descriptor != 1 (non-road), otherwise 0
        calculated_descriptor_fractional_raster = processing.run("gdal:rastercalculator", {
            'INPUT_A': fractional_raster_clipped,
            'BAND_A': 1,
            'INPUT_B': descriptor_raster_clipped,
            'BAND_B': 1,
            'FORMULA': '(A > 1) * (A <= 100) * (B != 1) * (B != 250) * A',
            'OUTPUT':  temp_clipped_raster3 # f'/Users/amir/Downloads/Data/StateExtensionInhabited/inhabited-temp.tif'
        })['OUTPUT']


        with_imperv = processing.run("native:zonalstatisticsfb", {
            'INPUT': block_layer,  # The vector layer defining zones
            'INPUT_RASTER': calculated_descriptor_fractional_raster,   # The raster layer containing values to sum
            'RASTER_BAND': 1,                   # Specify the raster band (usually 1 if single-band)
            'STATISTICS': [1],                  # 1 represents 'Sum' in QGIS
            'OUTPUT': 'memory:',
            'COLUMN_PREFIX': 'habitability_'       # Prefix for the new field in vector layer
        })['OUTPUT']
    
    else: 
        with_imperv = block_layer



    # Clip the raster to the vector layer's extent
    landcover_raster_clipped = processing.run(
        "gdal:cliprasterbyextent",
        {
            'INPUT': landcover_raster,
            'PROJWIN': f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",
            'OUTPUT': temp_clipped_raster4
        }
    )['OUTPUT']

    # Run Zonal Histogram
    zonal_histogram_result = processing.run(
        "native:zonalhistogram",
        {
            'INPUT_RASTER': landcover_raster_clipped,
            'INPUT_VECTOR': with_imperv,
            'COLUMN_PREFIX': 'lc_',
            'OUTPUT': 'memory:'  # Result stored in memory
        }
    )['OUTPUT']

    # Write the result to a CSV file
    QgsVectorFileWriter.writeAsVectorFormat(
        zonal_histogram_result,
        output_csv_path,
        "utf-8",
        zonal_histogram_result.crs(),
        "CSV"
    )
    print(f"Successfully saved to: {output_csv_path}")




def get_tract_id(geoid):
    """Extract tract ID from block group GEOID by removing the last digit."""
    try:
        return str(geoid)[:-1]  # Convert to string and remove last character
    except:
        print(f"Warning: Unable to process GEOID: {geoid}")
        return None

def analyze_tract_block_groups(bg_layer, tract_layer):
    """Analyze how many block groups each tract contains."""
    tract_counts = defaultdict(int)
    tract_bg_mapping = defaultdict(list)
    
    # Create a set of valid tract GEOIDs
    valid_tracts = {f['GEOID'] for f in tract_layer.getFeatures()}
    
    # Process block groups
    for feature in bg_layer.getFeatures():
        try:
            bg_geoid = str(feature['GEOID'])  # Convert to string
            tract_id = get_tract_id(bg_geoid)
            
            if tract_id and tract_id in valid_tracts:
                tract_counts[tract_id] += 1
                tract_bg_mapping[tract_id].append(bg_geoid)  # Store GEOID instead of feature.id()
            else:
                print(f"Warning: No matching tract found for block group {bg_geoid}")
        except Exception as e:
            print(f"Error processing block group: {e}")
            continue
    
    return dict(tract_counts), dict(tract_bg_mapping)

def build_contiguity_graph(tract_layer, tract_counts):
    """Build a graph of tract contiguity using tract geometries."""
    index = QgsSpatialIndex()
    features = {}
    
    # Populate spatial index and cache features
    for f in tract_layer.getFeatures():
        if not f.hasGeometry():
            continue
        index.insertFeature(f)
        features[f.id()] = f
    
    G = nx.Graph()
    processed = set()
    
    # Add nodes with sizes (number of block groups)
    for f in tract_layer.getFeatures():
        tract_id = str(f['GEOID'])  # Convert to string
        G.add_node(tract_id, size=tract_counts.get(tract_id, 0))
    
    # Build edges based on tract adjacency
    for fid, feature in features.items():
        tract_id = str(feature['GEOID'])
        candidates = index.intersects(feature.geometry().boundingBox())
        
        for candidate_id in candidates:
            if candidate_id == fid:
                continue
            
            pair = tuple(sorted([fid, candidate_id]))
            if pair in processed:
                continue
            
            candidate_feature = features[candidate_id]
            candidate_tract_id = str(candidate_feature['GEOID'])
            
            if feature.geometry().touches(candidate_feature.geometry()):
                G.add_edge(tract_id, candidate_tract_id)
            processed.add(pair)
    
    return G

def merge_clusters(source_id, target_id, G, clusters, tract_bg_mapping):
    """Merge clusters while maintaining block group mappings."""
    if source_id in clusters and target_id in clusters:
        clusters[target_id].extend(clusters[source_id])
        if source_id in tract_bg_mapping and target_id in tract_bg_mapping:
            tract_bg_mapping[target_id].extend(tract_bg_mapping[source_id])
            G.nodes[target_id]['size'] = len(tract_bg_mapping[target_id])
        
        new_edges = [(target_id, n) for n in G.neighbors(source_id) if n != target_id]
        G.add_edges_from(new_edges)
        G.remove_node(source_id)
        clusters.pop(source_id, None)
        tract_bg_mapping.pop(source_id, None)



def get_cluster_centroid(tract_layer, tract_ids):
    """Calculate the centroid of a cluster of tracts."""
    combined_geom = None
    for feature in tract_layer.getFeatures():
        if str(feature['GEOID']) in tract_ids:
            if combined_geom is None:
                combined_geom = feature.geometry()
            else:
                combined_geom = combined_geom.combine(feature.geometry())
    
    if combined_geom:
        return combined_geom.centroid()
    return None

def find_closest_cluster(tract_layer, source_cluster_ids, all_clusters, excluded_ids=None):
    """Find the closest cluster based on centroid distance."""
    if excluded_ids is None:
        excluded_ids = set()
    
    source_centroid = get_cluster_centroid(tract_layer, source_cluster_ids)
    if not source_centroid:
        return None
    
    min_distance = float('inf')
    closest_cluster = None
    
    for cluster_id, tract_ids in all_clusters.items():
        if cluster_id in excluded_ids or any(tid in source_cluster_ids for tid in tract_ids):
            continue
            
        target_centroid = get_cluster_centroid(tract_layer, tract_ids)
        if target_centroid:
            distance = source_centroid.distance(target_centroid)
            if distance < min_distance:
                min_distance = distance
                closest_cluster = cluster_id
    
    return closest_cluster


def merge_small_clusters(G, clusters, tract_bg_mapping, min_size=12, tract_layer=None):
    """
    Merge small clusters into larger ones until all clusters have size >= min_size.
    """
    while True:
        # Get all small clusters sorted by size
        small_clusters = sorted(
            [(n, G.nodes[n]['size']) for n in G.nodes if G.nodes[n]['size'] < min_size],
            key=lambda x: x[1]
        )
        
        if not small_clusters:
            break
        
        for cluster_id, cluster_size in small_clusters:
            if cluster_id not in G.nodes:  # Ensure node still exists
                continue
            
            neighbors_to_check = [cluster_id]
            visited = set()
            total_size = G.nodes[cluster_id]['size']
            
            # Accumulate neighbors to merge
            while neighbors_to_check and total_size < min_size:
                current_cluster = neighbors_to_check.pop(0)
                visited.add(current_cluster)
                
                # Get neighbors of the current cluster, sorted by size
                neighbors = [
                    (n, G.nodes[n]['size']) for n in G.neighbors(current_cluster)
                    if n in G.nodes and n not in visited
                ]
                neighbors.sort(key=lambda x: x[1])
                
                for neighbor_id, neighbor_size in neighbors:
                    if neighbor_id in G.nodes:  # Ensure neighbor still exists
                        merge_clusters(current_cluster, neighbor_id, G, clusters, tract_bg_mapping)
                        visited.add(neighbor_id)
                        
                        # Update the total size
                        total_size = G.nodes[current_cluster]['size'] if current_cluster in G.nodes else 0
                        
                        if total_size >= min_size:
                            break
                    
                if current_cluster not in G.nodes:  # If current node is removed, stop processing it
                    break
            
            # If no neighbors are left and still below the threshold, mark the cluster as 'unprocessible'
            if cluster_id in G.nodes and G.nodes[cluster_id]['size'] < min_size:
                G.nodes[cluster_id]['size'] = 999

    # Final pass to handle remaining small clusters

    remaining_small_clusters = [
        cluster_id for cluster_id in G.nodes if G.nodes[cluster_id]['size'] == 999
    ]
    
    # Create a spatial index for efficiency
    spatial_index = QgsSpatialIndex()
    tract_to_cluster = {}  # Map tract IDs to their cluster IDs
    geoid_to_fid = {}

    for feature in tract_layer.getFeatures():
        tract_id = str(feature['GEOID'])
        geoid_to_fid[str(feature['GEOID'])] = feature.id()

        for cluster_id, tracts in clusters.items():
            if tract_id in tracts:
                tract_to_cluster[tract_id] = cluster_id
                if cluster_id not in remaining_small_clusters:
                    spatial_index.insertFeature(feature)
                break
    
    for small_cluster_id in remaining_small_clusters:
        if small_cluster_id not in clusters:
            continue
        
        small_cluster_tracts = clusters[small_cluster_id]
        small_cluster_geoms = [
            tract_layer.getFeature(geoid_to_fid[tract]).geometry() for tract in small_cluster_tracts
        ]
        if not small_cluster_geoms:
            continue
        
        if len(small_cluster_geoms) == 1:
            small_cluster_centroid = small_cluster_geoms[0].centroid()
        else:
            small_cluster_centroid = small_cluster_geoms[0].combine(
                *small_cluster_geoms[1:]
            ).centroid()

        
        # Find the closest tract to the small cluster
        closest_tract_id = spatial_index.nearestNeighbor(small_cluster_centroid.asPoint(), 1)
        if not closest_tract_id:
            continue
        
        closest_tract_id = str(tract_layer.getFeature(closest_tract_id[0])['GEOID'])
        if closest_tract_id not in tract_to_cluster:
            continue
        
        # Find the cluster that the closest tract belongs to
        target_cluster_id = tract_to_cluster[closest_tract_id]

        
        # Merge small cluster into the target cluster
        if target_cluster_id != small_cluster_id:
            merge_clusters(small_cluster_id, target_cluster_id, G, clusters, tract_bg_mapping)
    

    return clusters, tract_bg_mapping



def create_output_layer(bg_layer, clusters, tract_bg_mapping):
    """Create output layer with cluster information for block groups."""
    output_layer = QgsVectorLayer(f"Polygon?crs={bg_layer.crs().authid()}", "clustered_blocks", "memory")
    provider = output_layer.dataProvider()

    fields = QgsFields()
    for field in bg_layer.fields():
        fields.append(field)
    fields.append(QgsField("cluster_id", QVariant.String))
    fields.append(QgsField("cluster_size", QVariant.Int))
    
    provider.addAttributes(fields)
    output_layer.updateFields()

    # Create reverse mapping from block group GEOID to cluster info
    bg_cluster_info = {}
    for cluster_id, tract_ids in clusters.items():
        bg_ids = set()  # Use set to avoid duplicates
        for tract_id in tract_ids:
            if tract_id in tract_bg_mapping:
                bg_ids.update(tract_bg_mapping[tract_id])
        cluster_size = len(bg_ids)
        for bg_id in bg_ids:
            bg_cluster_info[bg_id] = (cluster_id, cluster_size)

    new_features = []
    for feature in bg_layer.getFeatures():
        try:
            bg_geoid = str(feature['GEOID'])
            if bg_geoid in bg_cluster_info:
                cluster_id, size = bg_cluster_info[bg_geoid]
                new_feature = QgsFeature(output_layer.fields())
                new_feature.setGeometry(feature.geometry())
                new_feature.setAttributes(feature.attributes() + [cluster_id, size])
                new_features.append(new_feature)
            else:
                print(f"Warning: No cluster found for block group {bg_geoid}")
                # Add feature with default cluster info
                new_feature = QgsFeature(output_layer.fields())
                new_feature.setGeometry(feature.geometry())
                new_feature.setAttributes(feature.attributes() + ["unassigned", 1])
                new_features.append(new_feature)
        except Exception as e:
            print(f"Error processing feature: {e}")
            continue
    
    provider.addFeatures(new_features)
    # Show on QGIS Project
    #QgsProject.instance().addMapLayer(output_layer) 
    return output_layer

def cluster_block_groups(bg_layer, tract_layer):
    """Main function handling both block groups and tracts."""
    # Analyze tract-block group relationships
    tract_counts, tract_bg_mapping = analyze_tract_block_groups(bg_layer, tract_layer)
    
    if not tract_counts:
        raise ValueError("No valid tract-block group relationships found")
    
    # Build graph using tract geometries
    G = build_contiguity_graph(tract_layer, tract_counts)
    
    if not G.nodes:
        raise ValueError("No valid tract relationships found")
    
    # Initialize clusters with tract IDs
    clusters = {tract_id: [tract_id] for tract_id in tract_counts.keys()}
    
    # Merge clusters
    clusters, tract_bg_mapping = merge_small_clusters(G, clusters, tract_bg_mapping, 12, tract_layer)
    
    # Create output layer
    return create_output_layer(bg_layer, clusters, tract_bg_mapping)






year = 2016

directory = '/Users/amir/Downloads/Data/'

landcover_path = directory + f'NLCD/Annual_NLCD_LndCov_{year}_CU_C1V0.tif'
imperv_frc_path = directory + f'NLCD/Annual_NLCD_FctImp_{year}_CU_C1V0.tif'
imperv_dsc_path = directory + f'NLCD/Annual_NLCD_ImpDsc_{year}_CU_C1V0.tif'

landcover_raster = QgsRasterLayer(landcover_path, "Land Cover Raster")
fractional_raster = QgsRasterLayer(imperv_frc_path, "Fractional Raster")
descriptor_raster = QgsRasterLayer(imperv_dsc_path, "Descriptor Raster")


states = [
    "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "ia", "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy", "dc"
]

states = ["tx"]

for state in states:
    print(state)
    bg_path = os.path.join(directory, f'Block Groups {year}/tl_{year}_{state}_bg.zip')
    tract_path = os.path.join(directory, f'tracts {year}/{state}_{year}_tract.zip')
    output_path = os.path.join(directory, f'clustered-tracts/{state}-cluster-{year}.csv')

    if os.path.exists( output_path ):
        print(f"Output file {output_path} already exists. Skipping...")
        continue

    bg_vector = QgsVectorLayer(bg_path, "Block Groups", "ogr")

    tract_vector = QgsVectorLayer(tract_path, "Census Tracts", "ogr")

    clustered_bg = cluster_block_groups(bg_vector, tract_vector)

    landcover_stat(clustered_bg, landcover_raster, None, None, output_path)
