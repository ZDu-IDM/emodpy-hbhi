# precalculate_and_copy_output_for_shiny.R

# objective: pre-calculate the outputs needed for plotting and the Shiny app and save and/or copy over all files that are needed to run the shiny app into the shiny repo



##############################################################
# setup

library(data.table)
library(ggplot2)
library(scales)

# copy simulation output
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022')
shiny_base_dir = file.path(user_path, 'Documents/malaria-nga-snt22/nga_shiny')
shiny_dir = file.path(shiny_base_dir, 'data')
script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'

source(paste0(shiny_base_dir, '/scripts/process_sim_output_functions.R'))
source(paste0(shiny_base_dir, '/scripts/plot_sim_output_functions.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# simulation output directory and simulation information
indoor_protection_fraction = 0.75
sim_past_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present')
shiny_output_filepath_past = paste0(shiny_dir, '/simulation_output/simulations_to_present')
end_year_to_present = 2021
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future/v5_GF_PAAR')
shiny_output_filepath_future = paste0(shiny_dir, '/simulation_output/simulations_future')
end_year_future = 2029
# experiment information csv (same as intervention input info file)
intervention_coordinator_past_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_to_present.csv')
intervention_coordinator_future_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections_GF_PAAR.csv')

# set whether old files should be deleted
delete_old_outputs = FALSE

# create new output directories for processed outputs and plots
if(!dir.exists(paste0(shiny_dir, '/simulation_output'))) dir.create(paste0(shiny_dir, '/simulation_output'))
# for(base_filepath in c(sim_past_output_dir, shiny_output_filepath_past, sim_future_output_dir,shiny_output_filepath_future)){
for(base_filepath in c(sim_future_output_dir,shiny_output_filepath_future)){
  if(!dir.exists(paste0(base_filepath))) dir.create(paste0(base_filepath))
  if(!dir.exists(paste0(base_filepath, '/_plots'))) dir.create(paste0(base_filepath, '/_plots'))
  if(!dir.exists(paste0(base_filepath, '/_plots/timeseries_dfs'))) dir.create(paste0(base_filepath, '/_plots/timeseries_dfs'))
}
# LGA-specific file giving population sizes, archetypes, etc.
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
archetype_info = read.csv(pop_filepath)
admin_shapefile_filepath = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp')
shapefile_admin_colname = 'NOMDEP'


# scenario file describing which LGAs are in each plan
scenario_nmcp_df = read.csv(paste0(user_path, '/Dropbox (IDM)/NU_collaboration/nigeria_who/NGA_2022_SNT/future_projection_scenarios_20230225.csv'))
# standardize names
scenario_nmcp_df$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$LGA, origin_names=scenario_nmcp_df$adm2)


###############################################
# parameters, functions, and cleaning
                                 
# set the possible timeframes and burden indicators that could be plotted
time_res_options = c('annual average','monthly average')
burden_metric_options = c('PfPR', 'incidence', 'directMortality', 'allMortality')
age_plotted_options = c('U5', 'all')
LLIN2y_flag=FALSE
overwrite_files=delete_old_outputs
pyr='c2022'
chw_cov='c2022'
plot_by_month=FALSE
align_seeds = FALSE  # plot only averages across all runs in an experiment, not seedwise comparisons



# set minimum and maximum years
min_year = 2012
max_year = 2028
barplot_start_year=2024
barplot_end_year=2026
start_year=2023
end_year=2026
extend_past_timeseries_year=2022  # in timeseries plot, the to-present line should extend to this year, even if it is part of 'future-projection' simulations (as long as all future simulations have same value)

# read in coordinator csvs
intervention_coordinator_past = read.csv(intervention_coordinator_past_filepath)
intervention_coordinator_future = read.csv(intervention_coordinator_future_filepath)
intervention_coordinator_future$shiny_include = intervention_coordinator_future$shiny_include %in% c(TRUE, 'True', 'TRUE')


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
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="cumulativeBurden_.*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      print(paste0('Files deleted:', delfiles))
      
      # delete burden timeseries files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="timeseries_.*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      print(paste0('Files deleted:', delfiles))
      
      # delete total burden files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="totalBurden_.*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      print(paste0('Files deleted:', delfiles))
    }
  }
}



### function to get the subset of admins that should be plotted
get_admin_set_given_group_name = function(scenario_nmcp_df, district_subset_name = 'Global Fund'){
  cur_admins_gf = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$funder == 'Global Fund'])
  if(district_subset_name == 'all LGAs'){
    district_subset = 'all_LGAs'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  }  else if (district_subset_name == 'funder_GF'){
    district_subset = 'GF_LGAs'
    cur_admins = cur_admins_gf
  } else if (district_subset_name == 'GF LGAs with PBO in prioritized plan'){
    district_subset = 'PBO_IG2_GF_LGAs'
    cur_admins_pbo = unique(scenario_nmcp_df$admin_name[grepl('PBO', scenario_nmcp_df$mass_llins_fund)])
    cur_admins = intersect(cur_admins_gf, cur_admins_pbo)
  } else if (district_subset_name == 'GF LGAs with PMC'){
    district_subset = 'PMC_GF_LGAs'
    cur_admins_pmc = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$ipti_nsp=='IPTi'])
    cur_admins = intersect(cur_admins_gf, cur_admins_pmc)
  } else if (district_subset_name == 'only LGAs with GF SMC'){
    district_subset = 'SMC_GF_LGAs'
    cur_admins_smc = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$smc_fund == 'SMC'])
    cur_admins = intersect(cur_admins_gf, cur_admins_smc)
  } else {
    warning(paste0('Name for district set not recognized: ', district_subset_name,'. Using all GF admins.'))
    district_subset = 'GF_LGAs'
    cur_admins = cur_admins_gf
  }
  return(list(district_subset, cur_admins))
}





##########################################################################################
# plot timeseries and burden reduction barplots for all scenarios across all GF LGAs
##########################################################################################

### set the scenarios and admin subsets to be analyzed and plotted
# check that only one to-present scenario is indicated
if (sum(intervention_coordinator_past$shiny_include) != 1) warning('PROBLEM DETECTED: There needs to be a single to-present simulation scenario.')

# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_past_filepath, sum(intervention_coordinator_past$shiny_include)),
                              rep(intervention_coordinator_future_filepath, sum(intervention_coordinator_future$shiny_include)))
experiment_names = c(intervention_coordinator_past$ScenarioName[intervention_coordinator_past$shiny_include],
                     intervention_coordinator_future$ScenarioName[intervention_coordinator_future$shiny_include])
scenario_names = c('to-present', experiment_names[-1])
experiment_base_directories = c(rep(sim_past_output_dir,sum(intervention_coordinator_past$shiny_include)),
                                rep(sim_future_output_dir,sum(intervention_coordinator_future$shiny_include)))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = c(rep(end_year_to_present, sum(intervention_coordinator_past$shiny_include)),
                  rep(end_year_future, sum(intervention_coordinator_future$shiny_include)))

# set colors for each scenario
scenario_palette = c('#000000', # to-present
                     'grey', # continue current coverage
                     '#D17407', # Within allocation
                     '#FFC000', # within allocation plus 2026 CM
                     '#008492', '#00C9DE', '#5800B0', '#A953FF',  # GF PAAR scenarios
                     '#93CF4F')  # prioritized plan
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names

barplot_start_year=2024
barplot_end_year=2026

# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'incidence', 'directMortality', 'mLBW_deaths', 'MiP_stillbirths')

# set all possible subsets of admins to be plotted
district_subset_name_options = c('funder_GF')#, 'LGAs with different net types',  'LGAs with same net types')  #, 'funder_WB', 'funder_GF', 'funder_PMI')


for (i_admin_subset in 1:length(district_subset_name_options)){
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = district_subset_name_options[i_admin_subset])
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  if(district_subset == 'all_LGAs'){
    separate_plots_flag = TRUE
  } else separate_plots_flag = FALSE

  # plot annual timeseries
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                         plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                         pyr=pyr, chw_cov=chw_cov,
                                         scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                         separate_plots_flag=separate_plots_flag, extend_past_timeseries_year=extend_past_timeseries_year, plot_CI=FALSE,
                                         burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_',district_subset,'.png'), gg1, dpi=600, width=12, height=8, units='in')
  
  # plot burden relative to 'continue current implementation'
  gg2 = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                     barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                     pyr=pyr, chw_cov=chw_cov,
                                     scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, overwrite_files=overwrite_files, 
                                     separate_plots_flag=separate_plots_flag, show_error_bar=FALSE, align_seeds=align_seeds,
                                     burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_percent_reduction_burden_',district_subset,'_relativeToContCur.png'), gg2, dpi=600, width=14, height=8, units='in')
  
  # plot burden relative to 'continue current implementation'; excluding the 'within allocation' scenario
  exclude_indices = which(scenario_names == 'NGA_GF_PAAR_sc1')
  gg2 = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                      barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                      pyr=pyr, chw_cov=chw_cov,
                                      scenario_names=scenario_names[-exclude_indices], experiment_names=experiment_names[-exclude_indices], scenario_palette=scenario_palette[-exclude_indices], overwrite_files=overwrite_files, 
                                      separate_plots_flag=separate_plots_flag, show_error_bar=FALSE, align_seeds=align_seeds,
                                      burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_percent_reduction_burden_',district_subset,'_relativeToContCur_excludeWA.png'), gg2, dpi=600, width=14, height=8, units='in')
  
  # plot burden relative to the 'within allocation' scenario
  exclude_indices = which(scenario_names == 'NGA_GF_PAAR_imp')
  gg2 = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                      barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                      pyr=pyr, chw_cov=chw_cov,
                                      scenario_names=scenario_names[-exclude_indices], experiment_names=experiment_names[-exclude_indices], scenario_palette=scenario_palette[-exclude_indices], overwrite_files=overwrite_files, 
                                      separate_plots_flag=separate_plots_flag, show_error_bar=FALSE, align_seeds=align_seeds,
                                      burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_percent_reduction_burden_',district_subset,'_relativeToWA.png'), gg2, dpi=600, width=14, height=8, units='in')
  
  
  
  
  # plot timeseries of interventions
  gg3 = plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                           plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                           burden_metric=burden_metric_options[1], age_plotted=age_plotted_options[1],
                                           pyr=pyr, chw_cov=chw_cov,
                                           scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                           indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_interventions_',district_subset,'.png'), gg3, dpi=600, width=5, height=15, units='in')
  
  
  # # plot maps of burden
  # plot_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
  #                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
  #                  pyr=pyr, chw_cov=chw_cov,
  #                  scenario_names=scenario_names[-1], experiment_names=experiment_names[-1], admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname, LLIN2y_flag=LLIN2y_flag,
  #                  overwrite_files=overwrite_files)
}












##########################################################################################
# plot timeseries and burden reduction barplots for current imp, WA+CM, and WA-2024 continued
##########################################################################################

### set the scenarios and admin subsets to be analyzed and plotted
intervention_name = 'scaledown_after_2024'
experiment_names_without = c("NGA_GF_PAAR_sc1b")
experiment_names_with = c("NGA_GF_PAAR_sc2")
future_expts  = c(experiment_names_without, experiment_names_with)

# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_past_filepath, sum(intervention_coordinator_past$shiny_include)),
                              rep(intervention_coordinator_future_filepath, length(future_expts)))
experiment_names = c(intervention_coordinator_past$ScenarioName[intervention_coordinator_past$shiny_include],
                     future_expts)
scenario_names = c('to-present', experiment_names[-1])
experiment_base_directories = c(rep(sim_past_output_dir,sum(intervention_coordinator_past$shiny_include)),
                                rep(sim_future_output_dir,length(future_expts)))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = c(rep(end_year_to_present, sum(intervention_coordinator_past$shiny_include)),
                  rep(end_year_future, length(future_expts)))

# set colors for each scenario
scenario_palette = c('#000000', # to-present
                     # 'grey', # continue current coverage
                     '#FFC000', # within allocation plus 2026 CM
                     '#008492')  # prioritized plan
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names

barplot_start_year=2024
barplot_end_year=2026
age_plotted_options = c('U5','all')  

# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'incidence', 'directMortality')

# set subset of admins to be plotted
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]

if(district_subset == 'all_LGAs'){
  separate_plots_flag = TRUE
} else separate_plots_flag = FALSE

# plot annual timeseries
gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                        separate_plots_flag=separate_plots_flag, extend_past_timeseries_year=extend_past_timeseries_year, plot_CI=FALSE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_sc23_',district_subset,'.png'), gg1, dpi=600, width=12, height=8, units='in')

# plot burden relative to 'continue current implementation'
gg2 = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                    pyr=pyr, chw_cov=chw_cov,
                                    scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, overwrite_files=overwrite_files, 
                                    separate_plots_flag=separate_plots_flag, show_error_bar=FALSE, align_seeds=align_seeds,
                                    burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_percent_reduction_burden_sc23_',district_subset,'_relativeToContCur.png'), gg2, dpi=600, width=14, height=8, units='in')
  

### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                  burden_metric_subset=burden_metric_subset)
  # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg2
}
# ----- combine all bar plots
gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
# remove legend from main plots
for(bb in 2:length(gg_list)){
  gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
    ylab('Percent reduction')
}
nrow_plot = length(age_plotted_options)
gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_sc23_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=7, units='in')






##########################################################################################
# plot impact of specific interventions
##########################################################################################
include_all_relevant_sims=FALSE




###  =====================  IPTp  =====================   ###
if(include_all_relevant_sims){
  experiment_names_without = c('NGA_GF_PAAR_sc2', 'NGA_GF_PAAR_sc4')
  experiment_names_with = c('NGA_GF_PAAR_sc3', 'NGA_GF_PAAR_sc5')
  intervention_name='IPTp'
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  age_plotted_options = c('all')  #'U5',
  barplot_start_year = 2024
  barplot_end_year = 2026
  # set which burden metrics to plot
  burden_metric_subset = c('directMortality', 'mLBW_deaths', 'MiP_stillbirths')
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c('#008492', '#5800B0', '#00C9DE', '#A953FF')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=2)
  names(scenario_linetypes) = scenario_names
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=FALSE, 
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutIPTp_', district_subset,'.png'), gg1, dpi=600, width=6, height=10*3/4, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = length(age_plotted_options)
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=5, units='in')
  
  
} else{
  ###   IPTp - only 2 scenarios  
  experiment_names_without = c('NGA_GF_PAAR_sc2')
  experiment_names_with = c('NGA_GF_PAAR_sc3')
  intervention_name='IPTp'
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  age_plotted_options = c('all')  #'U5',
  barplot_start_year = 2024
  barplot_end_year = 2026
  # set which burden metrics to plot
  burden_metric_subset = c('directMortality', 'mLBW_deaths', 'MiP_stillbirths')
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c('#008492', '#00C9DE')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=1)
  names(scenario_linetypes) = scenario_names
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=FALSE, 
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutIPTp_origITNs_', district_subset,'.png'), gg1, dpi=600, width=6, height=10*3/4, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = length(age_plotted_options)
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_origITNs_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=5, units='in')
  
}

# plot maps of which LGAs are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                        admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)











###  =====================  Case management in and after 2026  =====================   ###
experiment_names_without = c('NGA_GF_PAAR_sc1')
experiment_names_with = c('NGA_GF_PAAR_sc1b')
intervention_name='CM'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U5', 'all')
barplot_start_year = 2024
barplot_end_year = 2026
# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'directMortality')


### set the scenarios and admin subsets to be analyzed and plotted
# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = c('#D17407', '#FFC000')  # note: need to match colors from earlier section for the 'with intervention' scenarios
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=1)
names(scenario_linetypes) = scenario_names


### plot timeseries comparison
gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=FALSE, 
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutCM_', district_subset,'.png'), gg1, dpi=600, width=6, height=10*3/4, units='in')


### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                  burden_metric_subset=burden_metric_subset)
  # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg2
}
# ----- combine all bar plots
gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
# remove legend from main plots
for(bb in 2:length(gg_list)){
  gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
    ylab('Percent reduction')
}
nrow_plot = length(age_plotted_options)
gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=8, units='in')








if(include_all_relevant_sims){
  ###  =====================  Net type - compare pyrethroid-only against the prioritized net types (PBO and IG2) and against all-IG2 nets  =====================   ###
  experiment_names_without = c('NGA_GF_PAAR_sc2_pyr', 'NGA_GF_PAAR_sc3_pyr', 'NGA_GF_PAAR_sc2_pyr', 'NGA_GF_PAAR_sc3_pyr')
  experiment_names_with = c('NGA_GF_PAAR_sc2', 'NGA_GF_PAAR_sc3',  'NGA_GF_PAAR_sc4', 'NGA_GF_PAAR_sc5')
  intervention_name='netType'
  age_plotted_options = c('U1', 'U5', 'all')
  barplot_start_year = 2024
  barplot_end_year = 2026
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c(rep(c('#663300', '#C2906A'), 2), '#008492', '#00C9DE', '#5800B0', '#A953FF')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=4)
  names(scenario_linetypes) = scenario_names
  
  # set which burden metrics to plot
  burden_metric_subset = c('PfPR', 'incidence', 'directMortality')
  
  
  
  # - - - - show for all admins - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
  # admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_',district_subset,'.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset, default_ylim_max=0.06)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
  
  
  
  
  # - - - - show for admins that are originally PBO - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_',district_subset,'.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset, default_ylim_max=0.06)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
  
  # # plot maps of which LGAs are included
  # plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
  #                         admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)
  
  
  
  ###  =====================  Net type - compare all-IG2 nets against prioritized plan with PBO and IG2  =====================   ###
  experiment_names_without = c('NGA_GF_PAAR_sc2', 'NGA_GF_PAAR_sc3')
  experiment_names_with = c('NGA_GF_PAAR_sc4', 'NGA_GF_PAAR_sc5')
  intervention_name='netType'
  age_plotted_options = c('U1', 'U5', 'all')
  barplot_start_year = 2024
  barplot_end_year = 2026
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c('#008492', '#00C9DE', '#5800B0', '#A953FF')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=2)
  names(scenario_linetypes) = scenario_names
  
  # set which burden metrics to plot
  burden_metric_subset = c('PfPR', 'incidence', 'directMortality')
  
  
  
  # - - - - show for all admins - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_',district_subset,'_relativePriPlan.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '_relativePriPlan.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
  
  
  
  
  
} else{
  ### only include simulations with IPTp, compare net type
  
  ###  =====================  Net type - compare pyrethroid-only against the prioritized net types (PBO and IG2) and against all-IG2 nets  =====================   ###
  experiment_names_without = c('NGA_GF_PAAR_sc3_pyr', 'NGA_GF_PAAR_sc3_pyr')
  experiment_names_with = c('NGA_GF_PAAR_sc3', 'NGA_GF_PAAR_sc5')
  intervention_name='netType'
  age_plotted_options = c('U1', 'U5', 'all')
  barplot_start_year = 2024
  barplot_end_year = 2026
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c(rep(c( '#C2906A'), 2),'#00C9DE', '#A953FF')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=2)
  names(scenario_linetypes) = scenario_names
  
  # set which burden metrics to plot
  burden_metric_subset = c('PfPR', 'incidence', 'directMortality')
  
  
  
  # - - - - show for all admins - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'funder_GF')
  # admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_onlyWithIPTp_',district_subset,'.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset, default_ylim_max=0.06)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_onlyWithIPTp_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
  
  
  
  
  # - - - - show for admins that are originally PBO - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_onlyWithIPTp_',district_subset,'.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset, default_ylim_max=0.06)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_onlyWithIPTp_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
  
  
  ###  =====================  Net type - compare all-IG2 nets against prioritized plan with PBO and IG2  =====================   ###
  experiment_names_without = c('NGA_GF_PAAR_sc3')
  experiment_names_with = c('NGA_GF_PAAR_sc5')
  intervention_name='netType'
  age_plotted_options = c('U1', 'U5', 'all')
  barplot_start_year = 2024
  barplot_end_year = 2026
  
  
  ### set the scenarios and admin subsets to be analyzed and plotted
  # get the names and locations of experiments to be included
  scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
  experiment_names = c(experiment_names_without, experiment_names_with)
  scenario_names = experiment_names
  experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
  scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
  sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))
  
  # set colors for each scenario
  scenario_palette = c( '#00C9DE', '#A953FF')  # note: need to match colors from earlier section for the 'with intervention' scenarios
  names(scenario_palette) = scenario_names
  scenario_linetypes = rep(c(2,1), each=1)
  names(scenario_linetypes) = scenario_names
  
  # set which burden metrics to plot
  burden_metric_subset = c('PfPR', 'incidence', 'directMortality')
  
  
  
  # - - - - show for all admins - - - - #
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'GF LGAs with PBO in prioritized plan')
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  
  ### plot timeseries comparison
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=unique(scenario_filepaths), scenario_names=unique(scenario_names), experiment_names=unique(experiment_names), scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                          burden_metric_subset=burden_metric_subset)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_netType_onlyWithIPTp_',district_subset,'_relativePriPlan.png'), gg1, dpi=600, width=9, height=10, units='in')
  
  
  ### plot reduction in burden relative to version without intervention
  gg_list = list()
  for (aa in 1:length(age_plotted_options)){
    age_group = age_plotted_options[aa]
    gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                    burden_metric_subset=burden_metric_subset)
    # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
    # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
    gg_list[[aa]] = gg2
  }
  # ----- combine all bar plots
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:length(gg_list)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + 
      theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 20, vjust = 1, hjust=0.8), plot.title=element_blank())+
      ylab('Percent reduction')
  }
  nrow_plot = 3
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(1:length(gg_list), ncol=1))
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_onlyWithIPTp_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '_relativePriPlan.png'), gg, dpi=600, width=3.5, height=10, units='in')
  
}






















###  =====================  partial SMC  =====================   ###
experiment_names_without = c('NGA_fund_endSMC2025')
experiment_names_with = c('NGA_fund')
intervention_name='SMC'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'only LGAs with GF SMC')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U5', 'all')
barplot_start_year_smc = 2024
barplot_end_year_smc = 2026


### set the scenarios and admin subsets to be analyzed and plotted
# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c('#0040C0'), 2)  # note: need to match colors from earlier section for the 'with intervention' scenarios
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=1)
names(scenario_linetypes) = scenario_names


### plot timeseries comparison
gg6 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        colname_mort_U1_1=colname_mort_U1_1, colname_mort_U5_1=colname_mort_U5_1, colname_mort_all_1=colname_mort_all_1)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutSMCend2025_',district_subset,'.png'), gg6, dpi=600, width=9, height=10, units='in')


### plot reduction in burden relative to version without intervention
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg7 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_smc, barplot_end_year=barplot_end_year_smc, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=FALSE,
                                                  colname_mort_U1_1=colname_mort_U1_1, colname_mort_U1_2=colname_mort_U1_2, colname_mort_U5_1=colname_mort_U5_1, colname_mort_U5_2=colname_mort_U5_2, colname_mort_all_1=colname_mort_all_1, colname_mort_all_2=colname_mort_all_2)
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, 'end2025_percent_reduction_burden_', age_group, '_', barplot_start_year_smc, '_', barplot_end_year_smc, '_', district_subset, '.png'), gg7, dpi=600, width=4.8, height=4.8, units='in')
}

# 
# # look at error bars on estimates from stochastic variability between simulations
# plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
#                                           district_subset=district_subset, cur_admins=cur_admins, 
#                                           barplot_start_year=barplot_start_year_smc, barplot_end_year=barplot_end_year_smc, 
#                                           pyr=pyr, chw_cov=chw_cov,
#                                           experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
#                                           intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=TRUE, align_seeds=TRUE,
#                                           colname_mort_U1_1=colname_mort_U1_1, colname_mort_U1_2=colname_mort_U1_2, colname_mort_U5_1=colname_mort_U5_1, colname_mort_U5_2=colname_mort_U5_2, colname_mort_all_1=colname_mort_all_1, colname_mort_all_2=colname_mort_all_2)

# plot maps of which LGAs are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                        admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)









# # iterate through admin subsets
# for(i_dist in 1:length(district_subset_name_options)){
#   district_subset_name = district_subset_name_options[i_dist]
#   if(district_subset_name == 'all LGAs'){
#     cur_admins = 'all'
#     district_subset = 'districtsAll'
#   } else if(district_subset_name == 'only LGAs with prioritiy vaccine'){
#     cur_admins = scenario_nmcp_df$admin_name[(scenario_nmcp_df$vaccine_cat=='Vac1p')]
#     district_subset = 'districtsVaccPri'
#   } else if(district_subset_name == 'only LGAs with PMC'){
#     cur_admins = scenario_nmcp_df$admin_name[scenario_nmcp_df$ipti_nsp=='IPTi']
#     district_subset = 'districtsPMC'
#   } else{
#     warning('Name for the subset of districts to plot not recognized; results for all LGAs will be shown')
#     cur_admins = 'all'
#     district_subset = 'districtsAll'
#   }
#   
#   # # calculate cumulative burdens for U5 and all-ages
#   # for(i_exp in 2:length(experiment_names)){
#   #   cc=get_cumulative_burden(sim_output_filepath=scenario_base_dirs[i_exp], experiment_name=experiment_names[i_exp], start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
#   #   tt=get_total_burden(sim_output_filepath=scenario_base_dirs[i_exp], experiment_name=experiment_names[i_exp], admin_pop=admin_pop, comparison_start_year=start_year, comparison_end_year=end_year, district_subset=district_subset, cur_admins=cur_admins)
#   # }
#   
#   # create timeseries dfs
#   for(i_time in 1:length(time_res_options)){
#     time_res = time_res_options[i_time]
#     if(time_res == 'annual average'){
#       plot_by_month = FALSE
#     } else plot_by_month = TRUE
#     # interventions note: don't need to iterate through age and burden metrics since they will all be calculated in plot_simulation_output_burden_all. Here, we just need to generate intervention files.
#     plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
#                                         plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
#                                         burden_metric=burden_metric_options[1], age_plotted=age_plotted_options[1], 
#                                         pyr=pyr, chw_cov=chw_cov,
#                                         scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
#                                         indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
#     
#     # malaria burden
#     plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
#                                       plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years,
#                                       pyr=pyr, chw_cov=chw_cov,
#                                       scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
#   }
# }
# ## =========================================== ##
#  # timeseries of burden
# ## =========================================== ##
# 
# 
# 
# 
# 
# 
# 
# #################################################
# # copy all needed files (including simulation results and other input files) to shiny directory
# 
# # set which experiment output files need to be split (many aren't used in downstream analyses, so only include the ones that are)
# # output_files_needed = c('malariaBurden_withAdjustments.csv', 'monthly_Event_Count.csv', 'MonthlyUsageLLIN.csv')
# output_file_strings_needed = c('cumulativeBurden', 'totalBurden', 'totalU1Burden')
# 
# # iterate through experiments and output files within experiments, saving subsets in appropriate directory
# scenario_adjustment_info = read.csv(intervention_csv_filename, as.is=TRUE)
# experiment_names = scenario_adjustment_info$ScenarioName
# for(ee in 1:length(experiment_names)){
#   for(file_name_string in output_file_strings_needed){
#     output_files_needed =list.files(paste0(sim_output_filepath, '/', experiment_names[ee]), pattern=file_name_string)
#     
#     for(i_file in 1:length(output_files_needed)){
#       if(file.exists(paste0(sim_output_filepath, '/', experiment_names[ee], '/', output_files_needed[i_file]))){
#         for(ss in 1:length(sim_seed_group_names)){
#           if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss]))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss]))
#           if(!dir.exists(paste0(shiny_output_filepath, sim_seed_group_names[ss] ,'/', experiment_names[ee]))) dir.create(paste0(shiny_output_filepath, sim_seed_group_names[ss],'/', experiment_names[ee]))
#           # copy over file to new location
#           file.copy(from=paste0(sim_output_filepath, sim_seed_group_names[ss], '/', experiment_names[ee], '/', output_files_needed[i_file]), 
#                     to=paste0(shiny_output_filepath, sim_seed_group_names[ss], '/', experiment_names[ee], '/', output_files_needed[i_file]))
#         }
#       }
#     }
#   }
# }
# 
# # copy timeseries folders
# for(ss in 1:length(sim_seed_group_names)){
#   all_timeseries_files = list.files(paste0(sim_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs'))
#   # copy over file to new location
#   file.copy(from=paste0(sim_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs/', all_timeseries_files), 
#             to=paste0(shiny_output_filepath, sim_seed_group_names[ss], '/_plots/timeseries_dfs/'))
# }
# 
# 
# 
# # copy other needed hbhi files
# # population sizes
# file.copy(from=paste0(hbhi_dir, '/admin_pop_archetype.csv'), 
#           to=paste0(shiny_dir, '/admin_pop_archetype.csv'))
# # shapefile
# if(!dir.exists(paste0(shiny_dir, '/SpatialClustering'))) dir.create(paste0(shiny_dir, '/SpatialClustering'))
# if(!dir.exists(paste0(shiny_dir, '/SpatialClustering/reference_rasters_shapefiles'))) dir.create(paste0(shiny_dir, '/SpatialClustering/reference_rasters_shapefiles'))
# files_to_copy = list.files(paste0(hbhi_dir,  '/SpatialClustering/reference_rasters_shapefiles'), 'bdi_adm2')
# file.copy(from=paste0(hbhi_dir,  '/SpatialClustering/reference_rasters_shapefiles/', files_to_copy), 
#           to=paste0(shiny_dir,  '/SpatialClustering/reference_rasters_shapefiles'))
# # intervention file references
# if(!dir.exists(paste0(shiny_dir, '/simulation_inputs'))) dir.create(paste0(shiny_dir, '/simulation_inputs'))
# if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/_intervention_file_references'))) dir.create(paste0(shiny_dir, '/simulation_inputs/_intervention_file_references'))
# files_to_copy = c('Interventions_for_2010_2020.csv', 'Interventions_for_projections.csv')
# file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/_intervention_file_references/', files_to_copy), 
#           to=paste0(shiny_dir,  '/simulation_inputs/_intervention_file_references'))
# # intervention csvs
# if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/interventions_2010_2020'))) dir.create(paste0(shiny_dir, '/simulation_inputs/interventions_2010_2020'))
# files_to_copy = list.files(paste0(hbhi_dir,  '/simulation_inputs/interventions_2010_2020'))
# file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/interventions_2010_2020/', files_to_copy), 
#           to=paste0(shiny_dir,  '/simulation_inputs/interventions_2010_2020'))
# if(!dir.exists(paste0(shiny_dir, '/simulation_inputs/interventions_projections'))) dir.create(paste0(shiny_dir, '/simulation_inputs/interventions_projections'))
# files_to_copy = list.files(paste0(hbhi_dir,  '/simulation_inputs/interventions_projections'))
# file.copy(from=paste0(hbhi_dir,  '/simulation_inputs/interventions_projections/', files_to_copy), 
#           to=paste0(shiny_dir,  '/simulation_inputs/interventions_projections'))
