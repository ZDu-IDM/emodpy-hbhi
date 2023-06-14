# precalculate_and_copy_output_for_shiny.R

# objective: pre-calculate the outputs needed for plotting and the Shiny app and save and/or copy over all files that are needed to run the shiny app into the shiny repo



##############################################################
# setup

library(data.table)

# copy simulation output
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022')
shiny_base_dir = file.path(user_path, 'Documents/malaria-nga-snt22/nga_shiny')
shiny_dir = file.path(shiny_base_dir, 'data')

source(paste0(shiny_base_dir, '/scripts/process_sim_output_functions.R'))
source(paste0(shiny_base_dir, '/scripts/plot_sim_output_functions.R'))

# simulation output directory and simulation information
num_seeds = 5
indoor_protection_fraction = 0.75
sim_past_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present')
shiny_output_filepath_past = paste0(shiny_dir, '/simulation_output/simulations_to_present')
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future')
shiny_output_filepath_future = paste0(shiny_dir, '/simulation_output/simulations_future')
# experiment information csv (same as intervention input info file)
intervention_coordinator_past_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_to_present.csv')
intervention_coordinator_future_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')

# set whether old files should be deleted
delete_old_outputs = FALSE

# create new output directories for processed outputs and plots
if(!dir.exists(paste0(shiny_dir, '/simulation_output'))) dir.create(paste0(shiny_dir, '/simulation_output'))
for(base_filepath in c(sim_past_output_dir, shiny_output_filepath_past, sim_future_output_dir,shiny_output_filepath_future)){
  if(!dir.exists(paste0(base_filepath))) dir.create(paste0(base_filepath))
  if(!dir.exists(paste0(base_filepath, '/_plots'))) dir.create(paste0(base_filepath, '/_plots'))
  if(!dir.exists(paste0(base_filepath, '/_plots/timeseries_dfs'))) dir.create(paste0(base_filepath, '/_plots/timeseries_dfs'))
}

# scenario file describing which LGAs are in each plan
scenario_nmcp_df = read.csv(paste0(user_path, '/Dropbox (IDM)/NU_collaboration/nigeria_who/NGA_2022_SNT/future_projection_scenarios_20230225.csv'))
# LGA-specific file giving population sizes, archetypes, etc.
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')



###############################################
# from simulation output, create new dataframes with all the needed plotting inputs

### set the scenarios and admin subsets to be analyzed and plotted
intervention_coordinator_past = read.csv(intervention_coordinator_past_filepath)
intervention_coordinator_future = read.csv(intervention_coordinator_future_filepath)
# check that only one to-present scenario is indicated
if (sum(intervention_coordinator_past$shiny_include) != 1) warning('PROBLEM DETEcTED: There needs to be a single to-present simulation scenario.')

# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_past_filepath, sum(intervention_coordinator_past$shiny_include)),
                              rep(intervention_coordinator_future_filepath, sum(intervention_coordinator_future$shiny_include)))
experiment_names = c(intervention_coordinator_past$ScenarioName[intervention_coordinator_past$shiny_include],
                     intervention_coordinator_future$ScenarioName[intervention_coordinator_future$shiny_include])
scenario_names = c('to-present', experiment_names[-1])
experiment_base_directories = c(rep(sim_past_output_dir,sum(intervention_coordinator_past$shiny_include)),
                            rep(sim_future_output_dir,sum(intervention_coordinator_future$shiny_include)))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = c(rep(2021, sum(intervention_coordinator_past$shiny_include)),
                  rep(2029, sum(intervention_coordinator_future$shiny_include)))

# set all possible subsets of admins to be plotted
district_subset_name_options = c('all LGAs', 'only LGAs with prioritiy vaccine', 'only LGAs with PMC')
# set the possible timeframes and burden indicators that could be plotted
time_res_options = c('annual average','monthly average')
burden_metric_options = c('PfPR', 'incidence', 'mortality')
age_plotted_options = c('U5', 'all')
LLIN2y_flag=FALSE
overwrite_files=delete_old_outputs
pyr='c2022'
chw_cov='c2022'

# set minimum and maximum years
min_year = 2012
max_year = 2029
barplot_start_year=2022
barplot_end_year=2024
start_year=2022
end_year=2024

# set colors for each scenario
scenario_palette = c(rgb(0,0,0),rgb(0/255,120/255,0/255), 
                     rgb(55/255,80/255,220/255), rgb(90/255,180/255,238/255), 
                     rgb(170/255,51/255,119/255), rgb(255/255,140/255,175/255))[1:length(scenario_names)]
scenario_palette = c('#000000', '#007700', 
  '#0000CC', '#0080FF', '#33FFFF', '#CCFFE5',
  '#660033', '#FF007F', '#FF99CC')
names(scenario_palette) = scenario_names
plot(1:length(scenario_palette), col=scenario_palette, cex=3, pch=20)

### delete old cumulativeBurden and totalBurden and timeseries outputs, (if delete_old_outputs is TRUE)
if(delete_old_outputs){
  relevant_dirs = c(sim_past_output_dir, shiny_output_filepath_past, sim_future_output_dir,shiny_output_filepath_future)
  for(cur_dir in relevant_dirs){
    # delete timeseries files
    delfiles = dir(path=paste0(cur_dir, '/_plots/timeseries_dfs'), pattern="df_.*Timeseries_.*.csv")
    file.remove(file.path(paste0(cur_dir, '/_plots/timeseries_dfs'), delfiles))
    print(paste0('Files deleted:', delfiles))
    
    # delete cumulative and total burden dfs in each experiment dir
    expt_dirs = dir(path=cur_dir, pattern="NGA_")
    for(cur_exp in expt_dirs){
      # delete cumulative burden files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="cumulativeBurden_*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      print(paste0('Files deleted:', delfiles))
      
      # delete total burden files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="totalBurden_*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      print(paste0('Files deleted:', delfiles))
      
    }
  }
}
# for debugging
if(FALSE){
  i_dist=1
  i_time=1
  burden_metric=burden_metric_options[1]
  age_plotted=age_plotted_options[1] 
  pyr='c2022'
  chw_cov='c2022'
}


## =========================================== ##
# timeseries of interventions and burden
## =========================================== ##
# iterate through admin subsets
for(i_dist in 1:length(district_subset_name_options)){
  district_subset_name = district_subset_name_options[i_dist]
  if(district_subset_name == 'all LGAs'){
    cur_admins = 'all'
    district_subset = 'districtsAll'
  } else if(district_subset_name == 'only LGAs with prioritiy vaccine'){
    cur_admins = scenario_nmcp_df$adm2[(scenario_nmcp_df$vaccine_cat=='Vac1p')]
    district_subset = 'districtsVaccPri'
  } else if(district_subset_name == 'only LGAs with PMC'){
    cur_admins = scenario_nmcp_df$adm2[scenario_nmcp_df$ipti_nsp=='IPTi']
    district_subset = 'districtsPMC'
  } else{
    warning('Name for the subset of districts to plot not recognized; results for all LGAs will be shown')
    cur_admins = 'all'
    district_subset = 'districtsAll'
  }
  
  # # calculate cumulative burdens for U5 and all-ages
  # for(i_exp in 2:length(experiment_names)){
  #   cc=get_cumulative_burden(sim_output_filepath=scenario_base_dirs[i_exp], experiment_name=experiment_names[i_exp], start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  #   tt=get_total_burden(sim_output_filepath=scenario_base_dirs[i_exp], experiment_name=experiment_names[i_exp], admin_pop=admin_pop, comparison_start_year=start_year, comparison_end_year=end_year, district_subset=district_subset, cur_admins=cur_admins)
  # }
  
  # create timeseries dfs
  for(i_time in 1:length(time_res_options)){
    time_res = time_res_options[i_time]
    if(time_res == 'annual average'){
      plot_by_month = FALSE
    } else plot_by_month = TRUE
    # interventions note: don't need to iterate through age and burden metrics since they will all be calculated in plot_simulation_output_burden_all. Here, we just need to generate intervention files.
    plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                        burden_metric=burden_metric_options[1], age_plotted=age_plotted_options[1], 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                        indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
    
    # malaria burden
    plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                                      plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years,
                                      pyr=pyr, chw_cov=chw_cov,
                                      scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  }
}
## =========================================== ##
 # timeseries of burden
## =========================================== ##







#################################################
# copy all needed files (including simulation results and other input files) to shiny directory

# set which experiment output files need to be split (many aren't used in downstream analyses, so only include the ones that are)
# output_files_needed = c('malariaBurden_withAdjustments.csv', 'monthly_Event_Count.csv', 'MonthlyUsageLLIN.csv')
output_file_strings_needed = c('cumulativeBurden', 'totalBurden', 'totalU1Burden')

# iterate through experiments and output files within experiments, saving subsets in appropriate directory
scenario_adjustment_info = read.csv(intervention_csv_filename, as.is=TRUE)
experiment_names = scenario_adjustment_info$ScenarioName
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
