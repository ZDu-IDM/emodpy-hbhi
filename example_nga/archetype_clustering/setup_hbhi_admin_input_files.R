# setup_hbhi_admin_input_files.R

# creates the csv input files describing
#  1) population size in each admin (aggregating from facebook raster)
#  2) insecticide resistance in each admin (mean mortality from MAP mortality rasters)
#  3) relative vector abundance in each admin (admin means from MAP vector rasters)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# -                               Setup                                 - #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
country_name = 'Burundi'  # options: 'Burkina', 'Nigeria', 'SierraLeone','Burundi'

# filepaths
if(country_name =='BurkinaFaso'){
  box_filepath = 'C:/Users/mambrose/Box'
  hbhi_dir = paste0(box_filepath, '/hbhi_burkina')
  population_filepath = paste0(box_filepath, '/burkina_rasterfiles')
  base_filepath_raster =  paste0(box_filepath, '/hbhi_burkina/SpatialClustering_BF')
  
  # input filenames
  ds_shapefile_filepath =  paste0(box_filepath, '/hbhi_burkina/SpatialClustering_BF/reference_rasters_shapefiles/BF_DS_clusteringProjection.shp')
  pop_raster_filepath = paste(population_filepath, '/BF_pop_raster_Facebook_20181001.tif', sep='')
  insect_resistance_filepath = paste(box_filepath, '/burkina_rasterfiles/insecticide_resistance', sep='')
  
  # output filenames
  ento_output_filepath = paste(box_filepath, '/hbhi_burkina/ento', sep='')
  
} else if(country_name == 'Nigeria'){
  box_filepath = 'C:/Users/mambrose/Box'
  hbhi_dir = paste0(box_filepath, '/hbhi_nigeria')
  base_filepath_raster = paste(box_filepath, '/hbhi_nigeria/SpatialClustering', sep='')
  insect_resistance_filepath = paste(box_filepath, '/nigeria_rasterfiles/insecticide_resistance', sep='')
  
  # input filenames
  ds_shapefile_filepath = paste(box_filepath, '/hbhi_nigeria/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp', sep='')
  
  # output filenames
  ento_output_filepath = paste(box_filepath, '/hbhi_nigeria/ento', sep='')
  
} else if (country_name == 'SierraLeone'){
  box_filepath = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects'
  hbhi_dir = paste0(box_filepath, '/SierraLeone_hbhi')
  population_filepath = paste0(box_filepath, '/SierraLeone_hbhi/shapefiles_rasterfiles')
  base_filepath_raster =  paste0(box_filepath, '/SierraLeone_hbhi/SpatialClustering')
  
  # input filenames
  ds_shapefile_filepath =  paste0(box_filepath, '/SierraLeone_hbhi/SpatialClustering/reference_rasters_shapefiles/gadm36_SLE_3.shp')
  pop_raster_filepath = paste(population_filepath, '/population_sle_2018-10-01/population_sle_2018-10-01.tif', sep='')
  insect_resistance_filepath = paste(box_filepath, '/SierraLeone_hbhi/shapefiles_rasterfiles/insecticide_resistance', sep='')
  
  # output filenames
  ento_output_filepath = paste(box_filepath, '/SierraLeone_hbhi/ento', sep='')
  
}else if (country_name == 'Burundi'){
  box_filepath = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects'
  hbhi_dir = paste0(box_filepath, '/burundi_hbhi')
  population_filepath = paste0(box_filepath, '/burundi_hbhi/shapefiles_rasterfiles')
  base_filepath_raster =  paste0(box_filepath, '/burundi_hbhi/SpatialClustering')
  var_weight_string = 'vector1_rain3_tsi2_centroid2_pre2010itn2'
  num_clusters = 1  # 4
  
  # input filenames
  ds_shapefile_filepath =  paste0(box_filepath, '/burundi_hbhi/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
  pop_raster_filepath = paste(population_filepath, '/population_bdi_2019-07-01_geotiff/population_bdi_2019-07-01.tif', sep='')
  insect_resistance_filepath = paste(box_filepath, '/burundi_hbhi/shapefiles_rasterfiles/insecticide_resistance', sep='')
  
  # output filenames
  ento_output_filepath = paste(box_filepath, '/burundi_hbhi/ento', sep='')
  
} else{
  warning('that country name is not currently supported')
}
vector_abundance_input_filepath = paste0(base_filepath_raster, '/unscaled_layers') # location of rasters containing relative vector abundances
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')


base_filepath_scripts = 'C:/Users/mambrose/Documents/hbhi-spatial-clustering'
source(paste0(base_filepath_scripts, '/prepare_clustering_inputs/calculate_mean_FB_pop_dens_each_DS.R'))
source(paste0(base_filepath_scripts, '/prepare_clustering_inputs/save_admin_centroid_coords.R'))
source(paste0(base_filepath_scripts, '/prepare_clustering_inputs/cluster_by_DS_mean.R'))
source(paste0(base_filepath_scripts, '/prepare_clustering_inputs/get_mean_insecticide_resistance_each_DS.R'))
source(paste0(base_filepath_scripts, '/prepare_clustering_inputs/get_mean_vector_rel_abundance_each_DS.R'))
source(paste0(base_filepath_scripts, '/explore_clustering_results/get_DS_archetype_repDS_associations.R'))


############        population sizes in each admin        ############
# check whether population csv already exists in the core simulation directory. if not, create it
create_admin_pop_csv_from_raster(ds_shapefile_filepath=ds_shapefile_filepath, pop_raster_filepath=pop_raster_filepath, ds_pop_df_filename=ds_pop_df_filename)
############        add centroid latitude/longitude coordinages to the pop file       ############
add_coordinates_to_pop_file(base_filepath_raster, ds_pop_df_filename)
############        add representative admin for seasonality archetypes to the pop file        ############
save_repDS_in_pop_file(base_filepath_raster, ds_pop_df_filename, var_weight_string, num_clusters)
############        add unique node_id for climate file generation        ############
# add_node_id(ds_pop_df_filename)  #  <-- only needed for some versions of climate-file generation

############        insecticide resistance in each admin        ############
# set the names of each of the insecticide bands (make sure these correspond in order with the .grd files)
var_weight_string_vector = c('insecticide_DDT_mortality_DS_means', 'insecticide_Deltamethrin_mortality_DS_means', 'insecticide_Permethrin_mortality_DS_means')
if(!file.exists(paste0(ento_output_filepath,'/insecticide_resistance_DS_means/',var_weight_string_vector[1],'.csv'))){
  resistance_years = 2005:2017
  create_admin_resistance_csv_from_raster(insect_resistance_filepath=insect_resistance_filepath, ento_output_filepath=ento_output_filepath, ds_shapefile_filepath=ds_shapefile_filepath, resistance_years=resistance_years, var_weight_string_vector=var_weight_string_vector)
}


############        relative vector abundance        ############
if(!file.exists(paste0(ento_output_filepath, '/DS_vector_rel_abundance.csv'))){
  create_admin_relVectAbundance_csv_from_raster(vector_abundance_input_filepath=vector_abundance_input_filepath, vector_abundance_output_filepath=ento_output_filepath, ds_shapefile_filepath=ds_shapefile_filepath)
}

  




