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

    assert block_layer.isValid(), "Block layer is not valid."
    assert landcover_raster.isValid(), "Landcover raster is not valid."
    assert fractional_raster.isValid(), "Fractional raster is not valid."
    assert descriptor_raster.isValid(), "Descriptor raster is not valid."



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





directory = '/Users/amir/Downloads/Data/'

landcover_path = directory + 'NLCD/Annual_NLCD_LndCov_2020_CU_C1V0.tif'
imperv_frc_path = directory + 'NLCD/Annual_NLCD_FctImp_2020_CU_C1V0.tif'
imperv_dsc_path = directory + 'NLCD/Annual_NLCD_ImpDsc_2020_CU_C1V0.tif'

landcover_raster = QgsRasterLayer(landcover_path, "Land Cover Raster")
fractional_raster = QgsRasterLayer(imperv_frc_path, "Fractional Raster")
descriptor_raster = QgsRasterLayer(imperv_dsc_path, "Descriptor Raster")


states = [
    "al", "ar", "az", "ca", "co", "ct", "de", "fl", "ga", "ia", "id", "il", "in", "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or", "pa", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vt", "wa", "wi", "wv", "wy", "dc"
]

for state in states:
    blocks_path = os.path.join(directory, f'blocks/{state}-blocks.zip')
    output_path = os.path.join(directory, f'blocks/processed/{state}-blocks.csv')

    if os.path.exists( output_path ):
        print(f"Output file {output_path} already exists. Skipping...")
        continue
    #if state == 'ia' or state == 'id':
    #    continue

    blocks_vector = QgsVectorLayer(blocks_path, "Blocks", "ogr")

    landcover_stat(blocks_vector, landcover_raster, fractional_raster, descriptor_raster, output_path)
