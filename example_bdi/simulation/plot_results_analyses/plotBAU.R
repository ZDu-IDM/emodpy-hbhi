# plotBAU.R

library(shiny)
library(shinythemes)
if("plyr" %in% search()) detach("plyr", unload=TRUE) 
library(dplyr)
library(readr)
library(ggplot2)



################################################################
########## core information on simulation set #############
################################################################

# source functions needed for gathering data and creating plots
script_base_filepath = paste0('C:/Users/mambrose/Documents/malaria-sle-hbhi/simulation/plot_results_analyses')
source(paste0(script_base_filepath, '/compare_simOutput_interventionCoverage_functions.R'))
source(paste0(script_base_filepath, '/compare_seedwise_scenario_burden_functions.R'))


# directory where scenario output is located
hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
sim_output_dir = paste0(hbhi_dir, '/simulation_output')
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
shapefile_admin_colname = 'NOMDEP'

# which experiments/scenarios should be plotted?
scenario_names = c( 'BAU')
scenario_input_references = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/', c(rep('Interventions_for_projections',1)), '.csv')
sim_end_years = c(rep(2030, 1))
# set colors for each scenario
scenario_palette = c('BAU'=rgb(0/255,120/255,0/255))

# set minimum and maximum years
min_year = 2021
max_year = 2026
barplot_start_year=2022
barplot_end_year=2024

# which admin will be plotted?
irs_districts = c('Buye', 'Gashoho', 'Kiremba','Muyinga')
comm_llin_districts = c('Giteranyi','Ngozi')
pbo_gf_districts = c("Bubanza", "Bugarama", "Buhiga", "Bururi", "Busoni", "Butezi",   "Cankuzo", "Cibitoke", "Fota", "Gahombo","Gihofi",  "Gitega", "Giteranyi", "Isale" ,  "Kabezi",  "Kayanza",     
                     "Kibumbu",  "Kibuye" , "Kiganda", "Kinyinya", "Kirundo",  "Mabayi",  "Makamba",  "Matana", "Mpanda",  "Mukenke",  "Muramvya", "Murore" ,  "Musema" ,  "Mutaho", "Ngozi",  "Nyabikere" ,  
                     "Nyanza-Lac", "Rumonge", "Rutana", "Ruyigi", "Rwibaga", "Ryansoro", "Vumbi", "Zone Centre", "Zone Nord", "Zone Sud")
other_districts = 'Bubanza'
ipti_districts = c('Busoni', 'Buye', 'Gashoho', 'Gihofi', 'Kinyinya', 'Kiremba', 'Kirundo', 'Mukenke', 'Murore', 'Muyinga', 'Vumbi')

# fraction of indoor time protected under net - use same value as in simulation. Use this value to back-calculate the use rate from effective use rate.
indoor_protection_fraction = 0.75


# which experiments/scenarios should be plotted?
pyr = 64
chw_cov = 'higher'
seed_subset = 'more sensitive'

cur_admins = 'all'
district_subset = 'districtsAll'



if(seed_subset == 'all'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
} else if(seed_subset == 'more sensitive'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
} else if(seed_subset == 'less sensitive'){
  sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
}


experiment_names = c(paste0('BDI_projection_', pyr,'pyr'))

plot_by_month = FALSE


gg = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                       plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                       pyr=pyr, chw_cov=chw_cov,
                                       scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag)

ggsave(paste0(sim_future_output_dir, '/_plots/timeseriesBAU_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'.png'), gg, width=7, height=8, dpi=900, units='in')



