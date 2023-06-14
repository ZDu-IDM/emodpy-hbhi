# compare_IPTi_est_withBoE.R


# compare Manuela's estimate with what we get when we do a rough calculation of burden averted in U1



library(dplyr)
library(readr)
library(ggplot2)



################################################################
########## core information on simulation set #############
################################################################

# source functions needed for gathering data and creating plots
# script_base_filepath = paste0('C:/Users/mambrose/Documents/malaria-sle-hbhi/simulation/plot_results_analyses')
# source(paste0(script_base_filepath, '/compare_simOutput_interventionCoverage_functions.R'))
# source(paste0(script_base_filepath, '/compare_seedwise_scenario_burden_functions.R'))
# source(paste0(script_base_filepath, '/plot_IPTi_impact.R'))
# source(paste0(script_base_filepath, '/plot_PBO_impact.R'))
# source(paste0(script_base_filepath, '/optimize_LLIN_IRS/optimization_functions.R'))

script_base_filepath = paste0('C:/Users/mambrose/Documents/malaria-sle-hbhi/bdi_shiny/scripts')
source(paste0(script_base_filepath, '/process_sim_output_functions.R'))
source(paste0(script_base_filepath, '/plot_sim_output_functions.R'))


# set whether old files should be deleted
delete_old_outputs = FALSE

# directory where scenario output is located
hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
# test that all files were appropriatedly transfered to shiny directory
# hbhi_dir = 'C:/Users/mambrose/Documents/malaria-sle-hbhi/bdi_shiny/data'


sim_output_dir = paste0(hbhi_dir, '/simulation_output')
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
admin_pop = read.csv(pop_filepath)
admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
shapefile_admin_colname = 'NOMDEP'

# which experiments/scenarios should be plotted?
scenario_names = c('to-present', 'BAU', 'GF', 'GF with PBO', 'GF at 80%', 'GF at 80% with PBO')
scenario_input_references = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/', c('Interventions_for_2010_2020', rep('Interventions_for_projections',5)), '.csv')
sim_end_years = c(2020, rep(2030, 5))
# set colors for each scenario
scenario_palette = c('to-present'=rgb(0,0,0), 'BAU'=rgb(0/255,120/255,0/255), 
                     'GF'=rgb(55/255,80/255,220/255),  'GF with PBO'=rgb(90/255,180/255,238/255), 
                     'GF at 80%'=rgb(170/255,51/255,119/255), 'GF at 80% with PBO'=rgb(255/255,140/255,175/255))

# set minimum and maximum years
min_year = 2012
max_year = 2030
barplot_start_year=2022
barplot_end_year=2024
start_year=2022
end_year=2024

# which admin will be plotted?
irs_districts = c('Buye', 'Gashoho', 'Kiremba','Muyinga')
comm_llin_districts = c('Giteranyi','Ngozi')
llin_gf_districts = c("Bubanza", "Bugarama", "Buhiga", "Bururi", "Busoni", "Butezi",   "Cankuzo", "Cibitoke", "Fota", "Gahombo","Gihofi",  "Gitega", "Giteranyi", "Isale" ,  "Kabezi",  "Kayanza",     
                      "Kibumbu",  "Kibuye" , "Kiganda", "Kinyinya", "Kirundo",  "Mabayi",  "Makamba",  "Matana", "Mpanda",  "Mukenke",  "Muramvya", "Murore" ,  "Musema" ,  "Mutaho", "Ngozi",  "Nyabikere" ,  
                      "Nyanza-Lac", "Rumonge", "Rutana", "Ruyigi", "Rwibaga", "Ryansoro", "Vumbi", "Zone Centre", "Zone Nord", "Zone Sud")

pbo_gf_districts = c("Bubanza", "Bugarama", "Buhiga", "Busoni", "Butezi",   "Cankuzo", "Cibitoke",  "Gahombo","Gihofi",  "Gitega",   "Kayanza",     
                     "Kibuye" ,"Kinyinya", "Kirundo",  "Mabayi",  "Makamba", "Mpanda",  "Mukenke", "Murore" ,  "Musema" ,  "Mutaho",  "Nyabikere" ,  
                     "Nyanza-Lac", "Rumonge", "Rutana", "Ruyigi", "Ryansoro", "Vumbi")
other_districts = 'Bubanza'
ipti_districts = c('Busoni', 'Buye', 'Gashoho', 'Gihofi', 'Kinyinya', 'Kiremba', 'Kirundo', 'Mukenke', 'Murore', 'Muyinga', 'Vumbi')

# fraction of indoor time protected under net - use same value as in simulation. Use this value to back-calculate the use rate from effective use rate.
indoor_protection_fraction = 0.75
overwrite_files = FALSE




seed_subset_options = c('all', 'more sensitive', 'less sensitive')
pyr_options = c('64','40')
chw_cov_options = c('higher','lower')
itn_halflife_options = c('1.31','2')
district_subset_name_options = c('all health districts', 'only IRS districts', 'only districts receiving PBO/IG2 in GF plan','only districts that receive community LLIN distribution')
time_res_options = c('annual average','monthly average')
burden_metric_options = c('PfPR', 'incidence', 'mortality')
age_plotted_options = c('U5', 'all')


seed_subset = seed_subset_options[2]
pyr = pyr_options[1]
chw_cov = chw_cov_options[1]
itn_halflife = itn_halflife_options[1]
district_subset_name = district_subset_name_options[1]
time_res = time_res_options[1]




if(seed_subset == 'all'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present')
} else if(seed_subset == 'more sensitive'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLog')
} else if(seed_subset == 'less sensitive'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
  sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLogLog')
}

# IPTi

if(itn_halflife == '2'){
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN'),  # GF without IPTi
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_IPTi'))  # GF with IPTi
} else{
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF without IPTi
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_IPTi'))  # GF with IPTi
}
# burden metrics
burden_colnames = c('average_PfPR_U1', 'incidence_U1', 'death_rate_mean_U1')
burden_metric_names = c('PfPR (U1)', 'incidence (U1)', 'mortality (U1)')
cur_admins = ipti_districts
district_subset = 'districtsIPTi'
save_plots = TRUE

# first experiment is without IPTi, second experiment is IPTi
reference_experiment_name = experiment_names[1]
# calculating the burden reduction of all metrics relative to no IPTi (seedwise comparisons, so one output for each run). 
comparison_experiment_name = experiment_names[2]
relative_burden_df = get_relative_U1_burden(sim_output_filepath=sim_future_output_dir, reference_experiment_name=reference_experiment_name, comparison_experiment_name=comparison_experiment_name, start_year=barplot_start_year, end_year=barplot_end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
# only save relevant columns for plotting
relative_burden_df = relative_burden_df[,which(colnames(relative_burden_df) %in% c('scenario', 'Run_Number', burden_colnames))]

for(bb in 1:length(burden_colnames)){
  current_burden_name = burden_colnames[bb]
  burden_metric_name = burden_metric_names[bb]
  select_col_names = c(current_burden_name, 'scenario')
  # get mean, min, and max among all runs for this burden metric
  rel_burden_agg_bb = as.data.frame(relative_burden_df) %>% dplyr::select(match(select_col_names, names(.))) %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(mean_rel = mean(get(current_burden_name)),
                     max_rel = max(get(current_burden_name)),
                     min_rel = min(get(current_burden_name)))
  rel_burden_agg_bb$burden_metric = burden_metric_name
  if(bb == 1){
    rel_burden_agg = rel_burden_agg_bb
  } else{
    rel_burden_agg = rbind(rel_burden_agg, rel_burden_agg_bb)
  }
}
# plot_relative_burden_IPTi_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, district_subset=district_subset, 
#                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
#                                    pyr=pyr, chw_cov=chw_cov,
#                                    experiment_names=experiment_names, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
# plot_IPTi_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, 
#                       barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
#                       experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)

burden_change_Manuela = rel_burden_agg




######################################################################################
#  use 'back of envelope' approach to calculate impact of IPTi
######################################################################################

# rate ratio parameters
clinical <- cbind("mean" = 1-0.30, "low_ci" = 1-0.39, "up_ci" = 1-0.20)
severe <- cbind("mean" = 1-.38, "low_ci" = 1-0.56, "up_ci" = 1-0.13)
allDeaths <- severe
pfpr <- cbind("mean" = 0.66, "low_ci" = 0.56, "up_ci" = 0.79)

# get coverage in each IPTi admin
ipti_coverage = read.csv(paste0(hbhi_dir, '/simulation_inputs/IPTi/assumedIPTicov.csv'))
# get original burden values in each admin
orig_burden = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_names[1], admin_pop=admin_pop, comparison_start_year=start_year, comparison_end_year=end_year, district_subset=district_subset, cur_admins=cur_admins)
# calculate new burden values in each admin
boe_burden = orig_burden[,which(colnames(orig_burden) %in% c('admin_name', 'pop_size_u1', 'pfpr_u1', 'deaths_u1', 'clinical_cases_u1'))]
boe_burden$pfpr_u1 = orig_burden$pfpr_u1 * pfpr[1]
boe_burden$deaths_u1 = orig_burden$deaths_u1 * severe[1]
boe_burden$clinical_cases_u1 = orig_burden$clinical_cases_u1 * clinical[1]
boe_burden$incidence_u1 = boe_burden$clinical_cases_u1 / boe_burden$pop_size_u1 / (end_year - start_year + 1) * 1000
boe_burden$mortality_rate_u1 = boe_burden$deaths_u1 / boe_burden$pop_size_u1 / (end_year - start_year + 1) * 1000

# compare against the burden as adjusted in post-processing
adjusted_burden = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_names[2], admin_pop=admin_pop, comparison_start_year=start_year, comparison_end_year=end_year, district_subset=district_subset, cur_admins=cur_admins)
plot(boe_burden$pfpr_u1, adjusted_burden$pfpr_u1, type='p')
lines(c(0,1), c(0,1))
plot(boe_burden$incidence_u1, adjusted_burden$incidence_u1, type='p')
lines(c(0,5000), c(0,5000))
plot(boe_burden$mortality_rate_u1, adjusted_burden$mortality_rate_u1, type='p')
lines(c(0,50), c(0,50))

# get country-level original and new burdens
# calculate relative change


