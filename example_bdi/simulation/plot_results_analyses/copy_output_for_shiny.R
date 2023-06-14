# subset_sim_output.R

# objective: copy over files that are needed to run the shiny app into the repo



##############################################################
# setup

library(data.table)

# copy simulation output
hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
shiny_dir = 'C:/Users/mambrose/Documents/malaria-sle-hbhi/bdi_shiny/data'
future_projection_flag = TRUE
if (future_projection_flag){
  # simulation output directory
  sim_output_filepath = paste0(hbhi_dir, '/simulation_output/simulations_future')
  shiny_output_filepath = paste0(shiny_dir, '/simulation_output/simulations_future')
  # experiment information csv (same as intervention input info file)
  intervention_csv_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')
} else{
  # simulation output directory
  sim_output_filepath = paste0(hbhi_dir, '/simulation_output/simulations_to_present')
  shiny_output_filepath = paste0(shiny_dir, '/simulation_output/simulations_to_present')
  # experiment information csv (same as intervention input info file)
  intervention_csv_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_2010_2020.csv')
}
# create new output directories for new simulation subsets
if(!dir.exists(paste0(shiny_dir, '/simulation_output'))) dir.create(paste0(shiny_dir, '/simulation_output'))
if(!dir.exists(paste0(shiny_output_filepath))) dir.create(paste0(shiny_output_filepath))
if(!dir.exists(paste0(shiny_output_filepath, '/simulation_subsets'))) dir.create(paste0(shiny_output_filepath, '/simulation_subsets'))
scenario_adjustment_info = read.csv(intervention_csv_filename, as.is=TRUE)
experiment_names = scenario_adjustment_info$ScenarioName
# set which experiment output files need to be split (many aren't used in downstream analyses, so only include the ones that are)
# output_files_needed = c('malariaBurden_withAdjustments.csv', 'monthly_Event_Count.csv', 'MonthlyUsageLLIN.csv')
output_file_strings_needed = c('cumulativeBurden', 'totalBurden', 'totalU1Burden')
num_seeds = 51
combine_BF_functions = TRUE

sim_seed_group_names = c('/', paste0('/simulation_subsets/', c('mortLogLog_blockOurs', 'mortLog_blockOurs', 'mortLogLog_blockNash', 'mortLog_blockNash')))
if(combine_BF_functions){
  # combine the BF versions
  sim_seed_group_names = c('/', paste0('/simulation_subsets/', c('mortLogLog', 'mortLog')))

}
for(ss in 1:length(sim_seed_group_names)){
  if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss]))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss]))
  if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots'))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots'))
  if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs'))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs'))
}


# iterate through experiments and output files within experiments, separating out the seed groups and saving subsets in appropriate directory
for(ee in 1:length(experiment_names)){
  for(file_name_string in output_file_strings_needed){
    output_files_needed =list.files(paste0(sim_output_filepath, '/', experiment_names[ee]), pattern=file_name_string)
    
    for(i_file in 1:length(output_files_needed)){
      if(file.exists(paste0(sim_output_filepath, '/', experiment_names[ee], '/', output_files_needed[i_file]))){
        for(ss in 1:length(sim_seed_group_names)){
          if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss]))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss]))
          if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss] ,'/', experiment_names[ee]))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss],'/', experiment_names[ee]))
          # copy over file to new location
          file.copy(from=paste0(sim_output_filepath, sim_seed_group_names[ss], '/', experiment_names[ee], '/', output_files_needed[i_file]), 
                    to=paste0(shiny_output_filepath, sim_seed_group_names[ss], '/', experiment_names[ee], '/', output_files_needed[i_file]))
        }
      }
    }
  }
}

# copy timeseries folders
for(ss in 1:length(sim_seed_group_names)){
  all_timeseries_files = list.files(paste0(sim_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs'))
  # copy over file to new location
  file.copy(from=paste0(sim_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs/', all_timeseries_files), 
            to=paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs/'))
}



# copy other needed hbhi files
# population sizes
file.copy(from=paste0(hbhi_dir, '/admin_pop_archetype.csv'), 
          to=paste0(shiny_dir, '/admin_pop_archetype.csv'))
# shapefile
if(!dir.exists(paste0(shiny_dir, '/SpatialClustering'))) dir.create(paste0(shiny_dir, '/SpatialClustering'))
if(!dir.exists(paste0(shiny_dir, '/SpatialClustering/reference_rasters_shapefiles'))) dir.create(paste0(shiny_dir, '/SpatialClustering/reference_rasters_shapefiles'))
files_to_copy = list.files(paste0(hbhi_dir,  '/SpatialClustering/reference_rasters_shapefiles'), 'bdi_adm2')
file.copy(from=paste0(hbhi_dir,  '/SpatialClustering/reference_rasters_shapefiles/', files_to_copy), 
          to=paste0(shiny_dir,  '/SpatialClustering/reference_rasters_shapefiles'))
# intervention file references
if(!dir.exists(paste0(shiny_dir, '/simulation_inputs'))) dir.create(paste0(shiny_dir, '/simulation_inputs'))
if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/_intervention_file_references'))) dir.create(paste0(shiny_dir, '/simulation_inputs/_intervention_file_references'))
files_to_copy = c('Interventions_for_2010_2020.csv', 'Interventions_for_projections.csv')
file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/_intervention_file_references/', files_to_copy), 
          to=paste0(shiny_dir,  '/simulation_inputs/_intervention_file_references'))
# intervention csvs
if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/interventions_2010_2020'))) dir.create(paste0(shiny_dir, '/simulation_inputs/interventions_2010_2020'))
files_to_copy = list.files(paste0(hbhi_dir,  '/simulation_inputs/interventions_2010_2020'))
file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/interventions_2010_2020/', files_to_copy), 
          to=paste0(shiny_dir,  '/simulation_inputs/interventions_2010_2020'))
if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/interventions_projections'))) dir.create(paste0(shiny_dir, '/simulation_inputs/interventions_projections'))
files_to_copy = list.files(paste0(hbhi_dir,  '/simulation_inputs/interventions_projections'))
file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/interventions_projections/', files_to_copy), 
          to=paste0(shiny_dir,  '/simulation_inputs/interventions_projections'))
