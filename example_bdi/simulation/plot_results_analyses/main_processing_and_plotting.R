# precalculate_and_copy_output_for_shiny.R

# objective: pre-calculate the outputs needed for plotting and the Shiny app and save and/or copy over all files that are needed to run the shiny app into the shiny repo



##############################################################
# setup

library(data.table)
library(ggplot2)
library(scales)
library(cowplot)

# copy simulation output
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023')
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'

source(paste0(script_dir, '/simulation/plots_results_analyses/process_sim_output_functions.R'))
source(paste0(script_dir, '/simulation/plots_results_analyses/plot_sim_output_functions.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# simulation output directory and simulation information
indoor_protection_fraction = 0.75
sim_past_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present')
end_year_to_present = 2021
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future/v3')
end_year_future = 2029
# experiment information csv (same as intervention input info file)
intervention_coordinator_past_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_to_present.csv')
intervention_coordinator_future_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')

# set whether old files should be deleted
delete_old_outputs = FALSE

# for(base_filepath in c(sim_past_output_dir, shiny_output_filepath_past, sim_future_output_dir,shiny_output_filepath_future)){
for(base_filepath in c(sim_future_output_dir)){
  if(!dir.exists(paste0(base_filepath))) dir.create(paste0(base_filepath))
  if(!dir.exists(paste0(base_filepath, '/_plots'))) dir.create(paste0(base_filepath, '/_plots'))
  if(!dir.exists(paste0(base_filepath, '/_plots/timeseries_dfs'))) dir.create(paste0(base_filepath, '/_plots/timeseries_dfs'))
}
# LGA-specific file giving population sizes, archetypes, etc.
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
archetype_info = read.csv(pop_filepath)
admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
shapefile_admin_colname = 'NOMDEP'


# scenario file describing which admins are in each plan
scenario_nmcp_df = read.csv(paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/snt_2022/BDI_NSP_PRI_mixes_reviewed.csv'))
# standardize names
scenario_nmcp_df$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=scenario_nmcp_df$adm2)




## ========================================================================= ##
# setup and functions
## ========================================================================= ##


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
}


get_admin_set_given_group_name = function(scenario_nmcp_df, district_subset_name = 'all_admins'){
  if(district_subset_name == 'all_admins'){
    district_subset = 'all_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  } else if (district_subset_name == 'PriVacc_admins'){
    district_subset = 'PriVacc_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$vac_pri == 1])
  } else if (district_subset_name == 'PriPMC_admins'){
    district_subset = 'PriPMC_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$cp_pri == 1])
  } else if (district_subset_name == 'PriPyr_admins'){
    district_subset = 'PriPyr_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name[grepl('Pyr', scenario_nmcp_df$llins_mass_pri_type_original)])
  } else if (district_subset_name == 'PriIRS_admins'){
    district_subset = 'PriIRS_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$irs_pri == 1])
  } else {
    warning(paste0('Name for district set not recognized: ', district_subset_name,'. Using all districts.'))
    district_subset = 'all_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  }
  return(list(district_subset, cur_admins))
}





## ========================================================================= ##
# plot timeseries of interventions and burden; plot burden relative to BAU
#    create plot panels for each subset of admins
## ========================================================================= ##

###############################################
# parameters, functions, and cleaning

# set the possible timeframes and burden indicators that could be plotted
time_res_options = c('annual average','monthly average')
burden_metric_options = c('PfPR', 'incidence', 'mortality')
age_plotted_options = c('U5', 'all')
LLIN2y_flag=FALSE
overwrite_files=delete_old_outputs
pyr=''
chw_cov=''
plot_by_month=FALSE
align_seeds = TRUE  # plot only averages across all runs in an experiment, not seedwise comparisons

# set minimum and maximum years
min_year = 2012
max_year = 2028
barplot_start_year=2024
barplot_end_year=2026
start_year=2024
end_year=2026
extend_past_timeseries_year=2022  # in timeseries plot, the to-present line should extend to this year, even if it is part of 'future-projection' simulations (as long as all future simulations have same value)

# read in coordinator csvs
intervention_coordinator_past = read.csv(intervention_coordinator_past_filepath)
intervention_coordinator_future = read.csv(intervention_coordinator_future_filepath)
intervention_coordinator_future$shiny_include = intervention_coordinator_future$shiny_include %in% c(TRUE, 'True', 'TRUE')



##########################################################################################
# plot national-level (or funder-level) timeseries and burden reduction barplots
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
scenario_palette = c('#000000', '#006633', 
                     '#C00020',# '#E67386', 'red',
                     '#0040C0')#, '#739AE6')
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names

# set all possible subsets of admins to be plotted
district_subset_name_options = c('all_admins', 'PriPyr_admins')#, 'LGAs with different net types',  'LGAs with same net types')  #, 'funder_WB', 'funder_GF', 'funder_PMI')


for (i_admin_subset in 1:length(district_subset_name_options)){
  admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = district_subset_name_options[i_admin_subset])
  district_subset = admin_subset_info[[1]]
  cur_admins = admin_subset_info[[2]]
  
  if(district_subset == 'all_admins'){
    separate_plots_flag = TRUE
  } else separate_plots_flag = FALSE

  # plot annual timeseries
  gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                         plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                         pyr=pyr, chw_cov=chw_cov,
                                         scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                         separate_plots_flag=separate_plots_flag, extend_past_timeseries_year=extend_past_timeseries_year, plot_CI=FALSE)
  ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_',district_subset,'.png'), gg1, dpi=600, width=10, height=8, units='in')
  
  # plot burden relative to 'continue current implementation'
  gg2 = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                     barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                     pyr=pyr, chw_cov=chw_cov,
                                     scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, overwrite_files=overwrite_files, 
                                     separate_plots_flag=separate_plots_flag, show_error_bar=FALSE, align_seeds=align_seeds)
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_percent_reduction_burden_',district_subset,'.png'), gg2, dpi=600, width=10, height=8, units='in')
  

  
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

# plot intervention coverage for IRS districts only
intervention_name='IRS'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'only LGAs with NSP IRS')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
# plot timeseries of interventions
gg9 = plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                          burden_metric=burden_metric_options[1], age_plotted='U5',
                                          pyr=pyr, chw_cov=chw_cov,
                                          scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                          indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                          colname_mort_U5_1=colname_mort_U5_1, colname_mort_all_1=colname_mort_all_1)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_interventions_',district_subset,'.png'), gg9, dpi=600, width=5, height=15, units='in')





##########################################################################################
# plot impact of vaccine, PMC, and IRS in prioritized plan or NSP
##########################################################################################

plot_by_month = FALSE
LLIN2y_flag = FALSE
min_year = 2012
max_year = 2028
overwrite_files = FALSE

###  =====================  vaccine - prioritized plan  =====================   ###
experiment_names_without = c('BDI_pri1_noVacc')#, 'BDI_pri2_noVacc')
experiment_names_with = c('BDI_pri1')#, 'BDI_pri2')
intervention_name='vaccine'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'PriVacc_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U1', 'U5', 'all')
barplot_start_year_vacc = 2024
barplot_end_year_vacc = 2026
# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'incidence', 'directMortality')


### set the scenarios and admin subsets to be analyzed and plotted
# get the names and locations of experiments to be included
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c('#0040C0'), 2) #, '#739AE6' # note: need to match colors from earlier section for the 'with intervention' scenarios
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=1)
names(scenario_linetypes) = scenario_names


### plot timeseries comparison
gg1 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutVacc_', district_subset,'.png'), gg1, dpi=600, width=9, height=10, units='in')


### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg2 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_vacc, barplot_end_year=barplot_end_year_vacc, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset)
  # gg2 = gg2 + coord_cartesian(ylim=c(0,0.17))
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year_vacc, '_', barplot_end_year_vacc, '_', district_subset, '.png'), gg2, dpi=600, width=4.8, height=4.8, units='in')
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
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_vacc, '_', barplot_end_year_vacc, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')


# plot maps of which DS are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                        admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)





###  =====================  PMC - prioritized plan   =====================   ###
experiment_names_without = c('BDI_pri1_noPMC')
experiment_names_with = c('BDI_pri1')
intervention_name='PMC'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'PriPMC_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U1', 'U5', 'all')
barplot_start_year_pmc = 2024
barplot_end_year_pmc = 2026
# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'incidence', 'directMortality')


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
gg3 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_withWithoutPMC_',district_subset,'.png'), gg3, dpi=600, width=9, height=10, units='in')


### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_pmc, barplot_end_year=barplot_end_year_pmc, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset)
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year_pmc, '_', barplot_end_year_pmc, '_', district_subset, '.png'), gg4, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg4
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
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_pmc, '_', barplot_end_year_pmc, '_', district_subset, '.png'), gg, dpi=600, width=3.5, height=10, units='in')



# look at error bars on estimates from stochastic variability between simulations
age_group='U1'
plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                          district_subset=district_subset, cur_admins=cur_admins, 
                                          barplot_start_year=barplot_start_year_pmc, barplot_end_year=barplot_end_year_pmc, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                          intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=TRUE, align_seeds=TRUE,
                                          burden_metric_subset=burden_metric_subset)


# plot maps of which DS are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                       admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)







###  =====================  impact of IRS versus mass LLIN campaigns with pyrethroid or IG2 nets in prioritized plan   =====================  ###
intervention_name='IRS'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'PriIRS_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U5', 'all')
barplot_start_year_irs = 2024
barplot_end_year_irs = 2026
# set which burden metrics to plot
burden_metric_subset = c('PfPR', 'incidence', 'directMortality')

# - - - - - impact of adding IRS relative to using mass distributions of LLINs - current resistance, include counterfactual without either - - - - - - - #
# get the names and locations of experiments to be included
experiment_names_with = c('BDI_pri1_replaceIRSwithPyr', 'BDI_pri1_replaceIRSwithIG2', 'BDI_pri1_replaceIRSwithNothing')
experiment_names_without = c('BDI_pri1', 'BDI_pri1', 'BDI_pri1')
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c(rep( rgb(0.7,0.3,0.7), 3), '#0040C0', '#739AE6', rgb(0,0,0)), 1)
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names
scenario_linetypes = c(2,2,2,1,1,3)
names(scenario_linetypes) = scenario_names

# plot annual timeseries
gg7 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=TRUE,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_',district_subset,'_relativeMassLLIN_curIR.png'), gg7, dpi=600, width=9, height=10, units='in')


# - - - - - impact of adding IRS relative to using mass distributions of LLINs - current resistance - - - - - - - #
# get the names and locations of experiments to be included
experiment_names_with = c('BDI_pri1_replaceIRSwithPyr', 'BDI_pri1_replaceIRSwithIG2')
experiment_names_without = c('BDI_pri1', 'BDI_pri1')
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c('#0040C0', '#0040C0', '#0040C0', '#739AE6'), 1)
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=2)
names(scenario_linetypes) = scenario_names

### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_irs, barplot_end_year=barplot_end_year_irs, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset)
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN.png'), gg4, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg4
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
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN_curIR.png'), gg, dpi=600, width=3.5, height=10, units='in')

# all on single plot (contains all seeds)
gg5=plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                          district_subset=district_subset, cur_admins=cur_admins, 
                                          barplot_start_year=barplot_start_year_irs, barplot_end_year=barplot_end_year_irs, 
                                          pyr=pyr, chw_cov=chw_cov,
                                          experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                          intervention_name=intervention_name, age_group=age_plotted_options, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                          burden_metric_subset=burden_metric_subset)
gg5 = gg5 + 
  theme(legend.position = "none")  + 
  theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 35, vjust = 1, hjust=1), plot.title=element_blank())+
  ylab('Percent reduction')
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN_curIR.png'), gg5, dpi=600, width=4, height=3, units='in')


# plot subsets of seeds based on IRS kill multiplier
# set which multipliers were used for different seeds
irs_kill_seed_multipliers = c(0.3, 0.15, 0.45)
num_samples = 50
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii
  gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_irs, barplot_end_year=barplot_end_year_irs, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_plotted_options, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset,
                                                  seed_subset=seed_subset, seed_subset_name=paste0('_seedSubset', round(100*irs_kill_seed_multipliers[ii])),
                                                  default_ylim_min=-0.25, default_ylim_max=0.1)
  gg4 = gg4 + 
    theme(legend.position = "none")  + 
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 35, vjust = 1, hjust=1), plot.title=element_blank())+
    ylab('Percent reduction')
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_seedSubset', round(irs_kill_seed_multipliers[ii]*100), '_relativeMassLLIN_curIR.png'), gg4, dpi=600, width=4, height=3, units='in')
}


# - - - - - impact of adding IRS relative to using mass distributions of LLINs - higher resistance - - - - - - - #
# get the names and locations of experiments to be included
experiment_names_with = c( 'BDI_pri1_replaceIRSwithPyr_50mort', 'BDI_pri1_replaceIRSwithIG2_50mort')
experiment_names_without = c( 'BDI_pri1_50mort', 'BDI_pri1_50mort')
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c( '#548235', '#548235', '#548235',  '#A9D18E'), 1)
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=2)
names(scenario_linetypes) = scenario_names

# plot annual timeseries
gg7 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=TRUE,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_',district_subset,'_relativeMassLLIN_higherIR.png'), gg7, dpi=600, width=9, height=10, units='in')



### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year_irs, barplot_end_year=barplot_end_year_irs, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset)
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN.png'), gg4, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg4
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
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN_higherIR.png'), gg, dpi=600, width=3.5, height=10, units='in')


# plot subsets of seeds based on IRS kill multiplier
# set which multipliers were used for different seeds
irs_kill_seed_multipliers = c(0.3, 0.15, 0.45)
num_samples = 50
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii

    gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                    district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year_irs, barplot_end_year=barplot_end_year_irs, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                    intervention_name=intervention_name, age_group=age_plotted_options, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                    burden_metric_subset=burden_metric_subset,
                                                    seed_subset=seed_subset, seed_subset_name=paste0('_seedSubset', round(100*irs_kill_seed_multipliers[ii])),
                                                    default_ylim_min=-0.25, default_ylim_max=0.1)
  gg4 = gg4 + 
    theme(legend.position = "none")  + 
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 35, vjust = 1, hjust=1), plot.title=element_blank())+
    ylab('Percent reduction')
  ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_seedSubset', round(irs_kill_seed_multipliers[ii]*100), '_relativeMassLLIN_higherIR.png'), gg4, dpi=600, width=4, height=3, units='in')
}


# plot maps of which DS are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                        admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)










###  =====================  impact of upgrading pyrethroid nets to IG2 in prioritized-pyrethroid districts   =====================  ###
intervention_name='UpgradeIG2'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'PriPyr_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U5', 'all')
barplot_start_year = 2024
barplot_end_year = 2026

# get the names and locations of experiments to be included
experiment_names_without = c('BDI_pri1', 'BDI_pri1_50mort')
experiment_names_with = c('BDI_pri2', 'BDI_pri2_50mort')
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(c(experiment_names_without, experiment_names_with))))
experiment_names = c(experiment_names_without, experiment_names_with)
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(c(experiment_names_without, experiment_names_with))))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
sim_end_years = rep(end_year_future, length(c(experiment_names_without, experiment_names_with)))

# set colors for each scenario
scenario_palette = rep(c('#0040C0', '#548235',  '#739AE6', '#A9D18E'), 1)
# plot(1:length(scenario_palette), col=scenario_palette, cex=8, pch=20)
names(scenario_palette) = scenario_names
scenario_linetypes = rep(c(2,1), each=2)
names(scenario_linetypes) = scenario_names

# plot annual timeseries
gg7 = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                        plot_by_month=plot_by_month, min_year=2022, max_year=max_year, sim_end_years=sim_end_years, 
                                        pyr=pyr, chw_cov=chw_cov,
                                        scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=TRUE,
                                        separate_plots_flag=FALSE, extend_past_timeseries_year=FALSE, scenario_linetypes=scenario_linetypes, plot_CI=FALSE, include_U1=TRUE,
                                        burden_metric_subset=burden_metric_subset)
ggsave(paste0(sim_future_output_dir, '/_plots/timeseries_annual_burden_',intervention_name,'_',district_subset,'_relativeMassLLIN.png'), gg7, dpi=600, width=9, height=10, units='in')



### plot reduction in burden relative to version without intervention
gg_list = list()
for (aa in 1:length(age_plotted_options)){
  age_group = age_plotted_options[aa]
  gg4 = plot_barplot_impact_specific_intervention(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, 
                                                  district_subset=district_subset, cur_admins=cur_admins, 
                                                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                  pyr=pyr, chw_cov=chw_cov,
                                                  experiment_names_without=experiment_names_without, experiment_names_with=experiment_names_with, scenario_palette=scenario_palette, 
                                                  intervention_name=intervention_name, age_group=age_group, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, show_error_bar=FALSE, align_seeds=TRUE,
                                                  burden_metric_subset=burden_metric_subset)
  # ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_', age_group, '_', barplot_start_year_irs, '_', barplot_end_year_irs, '_', district_subset, '_relativeMassLLIN.png'), gg4, dpi=600, width=4.8, height=4.8, units='in')
  gg_list[[aa]] = gg4
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
ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', intervention_name, '_percent_reduction_burden_allAgeGroups_', barplot_start_year, '_', barplot_end_year, '_', district_subset, '_relativeMassLLIN.png'), gg, dpi=600, width=3.5, height=8, units='in')



# plot maps of which DS are included
plot_included_admin_map(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins,
                        admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)





