# subset_sim_output.R

# objective: create datasets that only include the simulation results for specified seeds (e.g., the seeds that use a particular functional form to get parameter inputs)
# steps:
#    - determine experiment names to be split (from the input file reference csv)
#    - determine which seeds go in which group and specify names for groups
#    - set which experiment output files need to be split (many aren't used in downstream analyses, so only include the ones that are)
#    - iterate through experiments and output files within experiments, separating out the seed groups and saving subsets in appropriate directory



##############################################################
# setup

library(data.table)


hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
future_projection_flag = TRUE
if (future_projection_flag){
  # simulation output directory
  sim_output_filepath = paste0(hbhi_dir, '/simulation_output/simulations_future')
  # experiment information csv (same as intervention input info file)
  intervention_csv_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')
} else{
  # simulation output directory
  sim_output_filepath = paste0(hbhi_dir, '/simulation_output/simulations_to_present')
  # experiment information csv (same as intervention input info file)
  intervention_csv_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_2010_2020.csv')
}
# create new output directories for new simulation subsets
if(!dir.exists(paste0(sim_output_filepath, '/simulation_subsets'))) dir.create(paste0(sim_output_filepath, '/simulation_subsets'))
scenario_adjustment_info = read.csv(intervention_csv_filename, as.is=TRUE)
experiment_names = scenario_adjustment_info$ScenarioName
# set which experiment output files need to be split (many aren't used in downstream analyses, so only include the ones that are)
output_files_needed = c('malariaBurden_withAdjustments.csv', 'monthly_Event_Count.csv', 'MonthlyUsageLLIN.csv')
num_seeds = 51
combine_BF_functions = TRUE


# determine which seeds go in which group and specify names for groups
# keep in the same order as used to create input files:
      # mortality_function_names = c('loglogistic_Nash','logistic_Nash','loglogistic_Nash','logistic_Nash')
      # blocking_function_names = c('ITN_extraction','ITN_extraction','Nash_2021','Nash_2021')
sim_seed_group_names = c('mortLogLog_blockOurs', 'mortLog_blockOurs', 'mortLogLog_blockNash', 'mortLog_blockNash')
seeds_each_group = list()
for(ss in 1:length(sim_seed_group_names)){
  ff_seed_mod = ss %% length(sim_seed_group_names)
  seeds_each_group[[ss]] = which(((1:num_seeds) %% length(sim_seed_group_names)) == ff_seed_mod)
  seeds_each_group[[ss]] = seeds_each_group[[ss]] - 1  # subtract one from sample index to get seed index in simulation output
}

if(combine_BF_functions){
  # combine the BF versions
  sim_seed_group_names = c('mortLogLog', 'mortLog')
  seeds_each_group = list(c(seeds_each_group[[1]], seeds_each_group[[3]]),
                          c(seeds_each_group[[2]], seeds_each_group[[4]]))
}

# iterate through experiments and output files within experiments, separating out the seed groups and saving subsets in appropriate directory
for(ee in c(37:44)){#} 1:length(experiment_names)){
  for(i_file in 1:length(output_files_needed)){
    if(file.exists(paste0(sim_output_filepath, '/', experiment_names[ee], '/', output_files_needed[i_file]))){
      cur_file = fread(paste0(sim_output_filepath, '/', experiment_names[ee], '/', output_files_needed[i_file]))
      for(ss in 1:length(sim_seed_group_names)){
        if(!dir.exists(paste0(sim_output_filepath, '/simulation_subsets/', sim_seed_group_names[ss]))) dir.create(paste0(sim_output_filepath, '/simulation_subsets/', sim_seed_group_names[ss]))
        if(!dir.exists(paste0(sim_output_filepath, '/simulation_subsets/', sim_seed_group_names[ss] ,'/', experiment_names[ee]))) dir.create(paste0(sim_output_filepath, '/simulation_subsets/', sim_seed_group_names[ss],'/', experiment_names[ee]))
        # subset to relevant seed and save new dataframe
        cur_file_ss = cur_file[cur_file$Run_Number %in% seeds_each_group[[ss]], ]
        write.csv(cur_file_ss, paste0(sim_output_filepath, '/simulation_subsets/', sim_seed_group_names[ss], '/', experiment_names[ee], '/', output_files_needed[i_file]))
      }
    }
  }
}
