from qgis.core import (
    QgsProject, QgsVectorLayer, QgsRasterLayer, QgsSpatialIndex, QgsProcessingFeatureSourceDefinition, 
    QgsCoordinateReferenceSystem, QgsCoordinateTransform, QgsRasterLayer, QgsField, QgsVectorFileWriter,
    QgsFeatureRequest
)
from qgis import processing
from PyQt5.QtCore import QVariant
import csv
import os

os.environ['GDAL_NUM_THREADS'] = 'ALL_CPUS'

year = 2016

# Function to ensure CRS compatibility
def ensure_crs(layer, target_crs):
    if layer.crs() != target_crs:
        return processing.run("qgis:reprojectlayer", {'INPUT': layer, 'TARGET_CRS': target_crs, 'OUTPUT': 'memory:'})['OUTPUT']
    return layer

# Paths to your input rasters
fractional_raster_path = f"/Users/amir/Downloads/Data/NLCD/Annual_NLCD_FctImp_{year}_CU_C1V0.tif"
descriptor_raster_path = f"/Users/amir/Downloads/Data/NLCD/Annual_NLCD_ImpDsc_{year}_CU_C1V0.tif"
landcover_raster_path = f"/Users/amir/Downloads/Data/NLCD/Annual_NLCD_LndCov_{year}_CU_C1V0.tif"

fractional_raster = QgsRasterLayer(fractional_raster_path, "Fractional Raster")
descriptor_raster = QgsRasterLayer(descriptor_raster_path, "Descriptor Raster")
landcover_raster = QgsRasterLayer(landcover_raster_path, "Land Cover Raster")
nlcd_crs = landcover_raster.crs()

bg_directory = f'/Users/amir/Downloads/Data/Block Groups {year}/'
precincts_directory = f'/Users/amir/Downloads/Data/precincts/Elections {year}/'

zcta_shapefile = f'/Users/amir/Downloads/Data/zcta-shapefiles/tl_{year}_us_zcta.zip'
zcta_layer  = QgsVectorLayer(zcta_shapefile, 'ZCTAs', 'ogr')
if not zcta_layer.isValid():
    print("Invalid ZCTA layer")
zcta_layer = ensure_crs(zcta_layer, nlcd_crs)


output_directory = '/Users/amir/Downloads/Data/intersections-zcta/'

temp_1 = f'{output_directory}temp1.tif'
temp_2 = f'{output_directory}temp2.tif'
temp_3 = f'{output_directory}temp3.tif'
temp_4 = f'{output_directory}temp4.tif'

bg_files = [f for f in os.listdir(bg_directory) if f.endswith('.zip')]  # Assuming files are .zip shapefiles


for bg_file in bg_files:

    state = bg_file.split('_')[2]
    
    #if state != 'ri':
    #    continue

    print(state)

    
    csv_file_path = f'{output_directory}intersections-zcta-{state}-{year}.csv'

    if os.path.exists( csv_file_path ):
        print(f"Output file {csv_file_path} already exists. Skipping...")
        continue


    if state == 'ak' or state == 'hi':
        continue


    precincts_shapefile = os.path.join(precincts_directory, f'{state}_{year}.zip')
    bg_shapefile = os.path.join(bg_directory, bg_file)


    precincts_layer = QgsVectorLayer(precincts_shapefile, 'precincts', 'ogr')
    bg_layer = QgsVectorLayer(bg_shapefile, 'block_groups', 'ogr')

    if not precincts_layer.isValid() or not bg_layer.isValid():
        print(f"Error loading layers for {state}")
        continue


    precincts_layer = ensure_crs(precincts_layer, nlcd_crs)
    bg_layer = ensure_crs(bg_layer, nlcd_crs)

    precincts_layer = processing.run("qgis:fixgeometries", {
        'INPUT': precincts_layer,
        'OUTPUT': 'memory:'  # Output the fixed geometries to memory
    })['OUTPUT']

    bg_layer = processing.run("qgis:fixgeometries", {
        'INPUT': bg_layer,
        'OUTPUT': 'memory:'  # Output the fixed geometries to memory
    })['OUTPUT']



    precinct_area_field = QgsField("precinct_area", QVariant.Double)
    precinct_unique_id_field = QgsField("precinct_unique_id", QVariant.Int)
    
    precincts_layer.startEditing()  # Start editing the layer
    precincts_layer.dataProvider().addAttributes([precinct_area_field, precinct_unique_id_field])
    precincts_layer.updateFields()

    attribute_updates = {}

    for feature in precincts_layer.getFeatures():
        geom = feature.geometry()
        area = geom.area()  # Calculate area in layer's CRS units
        fid = feature.id()
        attribute_updates[fid] = {
            precincts_layer.fields().indexFromName("precinct_area"): area,
            precincts_layer.fields().indexFromName("precinct_unique_id"): fid
            }

    precincts_layer.dataProvider().changeAttributeValues(attribute_updates)



    params = {
        'INPUT': precincts_layer,
        'OVERLAYS': [zcta_layer, bg_layer],
        'OUTPUT': 'memory:',
        'OVERLAY_FIELDS_PREFIX': 'zg_'
    }

    intersect_layer = processing.run("qgis:multiintersection", params)['OUTPUT']


    # Add a new field for area
    area_field = QgsField("fraction_area", QVariant.Double)
    
    intersect_layer.startEditing()  # Start editing the layer
    intersect_layer.dataProvider().addAttributes([area_field])
    intersect_layer.updateFields()

    attribute_updates = {}

    # Populate the dictionary with feature IDs and new area values
    for feature in intersect_layer.getFeatures():
        geom = feature.geometry()
        area = geom.area()  # Calculate area in layer's CRS units
        attribute_updates[feature.id()] = {intersect_layer.fields().indexFromName("fraction_area"): area}

    # Apply the batch update
    intersect_layer.dataProvider().changeAttributeValues(attribute_updates)


    intersect_layer = intersect_layer.materialize(
        QgsFeatureRequest().setFilterExpression(
            ' ("fraction_area" >= 0.001 * "precinct_area") OR '
            ' ("fraction_area" >= 0.001 * ("zg_ALAND20" + "zg_AWATER20")) OR '
            ' ("fraction_area" >= 0.001 * ("zg_ALAND" + "zg_AWATER"))'
        )
    )

    print(intersect_layer.featureCount())


    intersect_layer = processing.run("qgis:fixgeometries", {
        'INPUT': intersect_layer,
        'OUTPUT': 'memory:'  # Output the fixed geometries to memory
        })['OUTPUT']

    print("Step 4")

    #QgsVectorFileWriter.writeAsVectorFormat(intersect2_layer, f'{output_directory}{state}-{year}2.shp', "utf-8", intersect2_layer.crs(), "ESRI Shapefile")
    #break

    state_extent = bg_layer.extent()

    fractional_raster_clipped = processing.run("gdal:cliprasterbyextent", {'INPUT':fractional_raster,'PROJWIN':f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",'OUTPUT':temp_1})['OUTPUT']

    descriptor_raster_clipped = processing.run("gdal:cliprasterbyextent", {'INPUT':descriptor_raster,'PROJWIN':f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",'OUTPUT':temp_2})['OUTPUT']
    
    # Create inhabited raster: pixel is 1 if fractional > 1 and descriptor != 1 (non-road), otherwise 0
    calculated_descriptor_fractional_raster = processing.run("gdal:rastercalculator", {
        'INPUT_A': fractional_raster_clipped,
        'BAND_A': 1,
        'INPUT_B': descriptor_raster_clipped,
        'BAND_B': 1,
        'FORMULA': '(A > 1) * (A <= 100) * (B != 1) * (B != 250) * A', #250 means no data
        'OUTPUT':  temp_3 # f'/Users/amir/Downloads/Data/StateExtensionInhabited/inhabited-temp.tif'
    })['OUTPUT']

    print("Step 5")


    intersect_imperv = processing.run("native:zonalstatisticsfb", {
        'INPUT': intersect_layer,  # The vector layer defining zones
        'INPUT_RASTER': calculated_descriptor_fractional_raster,   # The raster layer containing values to sum
        'RASTER_BAND': 1,                   # Specify the raster band (usually 1 if single-band)
        'STATISTICS': [1],                  # 1 represents 'Sum' in QGIS
        'OUTPUT': 'memory:',
        'COLUMN_PREFIX': 'habitability_'       # Prefix for the new field in vector layer
    })['OUTPUT']

    print("Step 6")


    landcover_raster_clipped = processing.run("gdal:cliprasterbyextent", {'INPUT':landcover_raster,'PROJWIN':f"{state_extent.xMinimum()},{state_extent.xMaximum()},{state_extent.yMinimum()},{state_extent.yMaximum()}",'OUTPUT':temp_4})['OUTPUT']

    intersect_imperv_landcover = processing.run("native:zonalhistogram", {
        'INPUT_RASTER': landcover_raster_clipped,
        'INPUT_VECTOR': intersect_imperv,
        'COLUMN_PREFIX': 'lc_',
        'OUTPUT': 'memory:'
    })['OUTPUT']

    print("Step 7")

    
    QgsVectorFileWriter.writeAsVectorFormat(
        intersect_imperv_landcover,
        csv_file_path,
        "utf-8",
        intersect_imperv_landcover.crs(),
        "CSV"
    )


