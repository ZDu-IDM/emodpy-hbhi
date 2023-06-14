# precalculate_shiny_outputs.R

# iterate through all user-input possible combinations and pre-calculate the outputs so that Shiny app runs faster



if("plyr" %in% search()) detach("plyr", unload=TRUE) 
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

script_hbhi_dir = 'C:/Users/moniqueam/Documents/malaria-sle-hbhi'
script_base_filepath = paste0(script_hbhi_dir, '/bdi_shiny/scripts')
source(paste0(script_base_filepath, '/process_sim_output_functions.R'))
source(paste0(script_base_filepath, '/plot_sim_output_functions.R'))


# set whether old files should be deleted
delete_old_outputs = FALSE

# directory where scenario output is located
hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
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



# delete old cumulativeBurden and totalBurden and timeseries outputs, (if delete_old_outputs is TRUE)
if(delete_old_outputs){
  relevant_dirs = c(paste0(sim_output_dir, '/simulations_future'),
                    paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog'), 
                    paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog'))
  for(cur_dir in relevant_dirs){
    # delete timeseries files
    delfiles = dir(path=paste0(cur_dir, '/_plots/timeseries_dfs'), pattern="df_.*Timeseries_.*.csv")
    file.remove(file.path(paste0(cur_dir, '/_plots/timeseries_dfs'), delfiles))
    
    # delete cumulative and total burden dfs in each experiment dir
    expt_dirs = dir(path=cur_dir, pattern="BDI_projection")
    for(cur_exp in expt_dirs){
      # delete cumulative burden files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="cumulativeBurden_*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
      
      # delete total burden files
      delfiles = dir(path=paste0(cur_dir, '/', cur_exp), pattern="totalBurden_*.csv")
      file.remove(file.path(paste0(cur_dir, '/', cur_exp), delfiles))
    }
  }
}



# iterate through scenarios and calculate intermediate outputs
# ss=1
# i_pyr=1
# i_chw=1
# i_itn=1
# i_dist=1
# i_exp=2

for(ss in 1:length(seed_subset_options)){
  seed_subset = seed_subset_options[ss]
  for(i_pyr in 1:length(pyr_options)){
    pyr = pyr_options[i_pyr]
    for(i_chw in 1:length(chw_cov_options)){
      chw_cov = chw_cov_options[i_chw]
      for(i_itn in 1:length(itn_halflife_options)){
        itn_halflife = itn_halflife_options[i_itn]
        
        
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
        scenario_base_dirs = c(sim_past_output_dir, rep(sim_future_output_dir,5))
        
        
        experiment_names = c(paste0('BDI_2010_2020_allInter'),
                             paste0('BDI_projection_', pyr,'pyr'),  # BAU
                             paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF
                             paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_PBO', pyr,'pyr'),  # GF with PBO nets
                             paste0('BDI_projection_GF_80Coverage_', pyr,'pyr'),  # GF at 80% coverage
                             paste0('BDI_projection_GF_80CoveragePBO_', pyr,'pyr')
        )
        if(itn_halflife == '2'){
          experiment_names = paste0(experiment_names, '_2yLLIN')
          LLIN2y_flag = TRUE
        } else LLIN2y_flag = FALSE
        scenario_filepaths = paste0(scenario_base_dirs, '/', experiment_names)
        
        
        
        
        # iterate through district subsets
        for(i_dist in 1:length(district_subset_name_options)){
          district_subset_name = district_subset_name_options[i_dist]
          
          if(district_subset_name == 'all health districts'){
            cur_admins = 'all'
            district_subset = 'districtsAll'
          } else if(district_subset_name == 'only IRS districts'){
            cur_admins = irs_districts
            district_subset = 'districtsIRS'
          } else if(district_subset_name == 'only districts that receive community LLIN distribution'){
            cur_admins = comm_llin_districts
            district_subset = 'districtsCommLLIN'
          } else if(district_subset_name == 'only districts receiving PBO/IG2 in GF plan'){
            cur_admins = pbo_gf_districts
            district_subset = 'districtsPBO'
          } else if(district_subset_name == 'DistrictsOther'){
            cur_admins = other_districts
            district_subset = 'DistrictsOther'
          } else{
            warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
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
            # plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
            #                                   plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
            #                                   pyr=pyr, chw_cov=chw_cov,
            #                                   scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
            # don't need to iterate through age and burden metrics since they will all be calculated in plot_simulation_output_burden_all. Here, we just need to generate intervention files.
            plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                                plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                                burden_metric=burden_metric_options[1], age_plotted=age_plotted_options[1], 
                                                pyr=pyr, chw_cov=chw_cov,
                                                scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                                indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
          }
        }
      }
    }
  }
}



# calculate IPTi-specific results

for(ss in 1:length(seed_subset_options)){
  seed_subset = seed_subset_options[ss]
  for(i_pyr in 1:length(pyr_options)){
    pyr = pyr_options[i_pyr]
    for(i_chw in 1:length(chw_cov_options)){
      chw_cov = chw_cov_options[i_chw]
      for(i_itn in 1:length(itn_halflife_options)){
        itn_halflife = itn_halflife_options[i_itn]
        
        
        if(seed_subset == 'all'){
          sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
        } else if(seed_subset == 'more sensitive'){
          sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
        } else if(seed_subset == 'less sensitive'){
          sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
        }
        scenario_base_dirs = sim_future_output_dir
        
        
        ipti_experiment_names = paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr')  # GF
        if(itn_halflife == '2'){
          ipti_experiment_names = paste0(ipti_experiment_names, '_2yLLIN')
          LLIN2y_flag = TRUE
        } else LLIN2y_flag = FALSE
        ipti_experiment_names = paste0(ipti_experiment_names, c('', '_IPTi'))
        # scenario_filepaths = paste0(scenario_base_dirs, '/', ipti_experiment_names)
        
        
        # calculate cumulative burdens for U1
        district_subset = 'districtsIPTi'
        cur_admins = ipti_districts
        for(i_exp in 1:length(ipti_experiment_names)){
          get_cumulative_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=ipti_experiment_names[i_exp], start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
          get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=ipti_experiment_names[i_exp], admin_pop=admin_pop, comparison_start_year=start_year, comparison_end_year=end_year, district_subset=district_subset, cur_admins=cur_admins)
        }
        
        plot_IPTi_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset='districtsIPTi', cur_admins=cur_admins, 
                              barplot_start_year=start_year, barplot_end_year=end_year, 
                              experiment_names=ipti_experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)
        
      }
    }
  }
}





# additional, specific plots for presentations - e.g., plots not used in Shiny app orspecific Shiny plot for presentation


    


seed_subset = seed_subset_options[2]
pyr = pyr_options[1]
chw_cov = chw_cov_options[1]
itn_halflife = itn_halflife_options[1]
district_subset_name = district_subset_name_options[1]
time_res = time_res_options[1]



# burden metrics
burden_colnames = c('average_PfPR_U5', 'average_PfPR_all', 'incidence_U5', 'incidence_all', 'death_rate_mean_U5', 'death_rate_mean_all')
burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)', 'mortality (U5)', 'mortality (all ages)')

# first comparison name is to-present (skip it), second is BAU (use as reference), comparison scenarios start at the third index
reference_experiment_name = experiment_names[2]
# iterate through comparison scenarios, calculating the burden reduction of all metrics relative to BAU (seedwise comparisons, so one output for each run). Combine all scenario reductions into a dataframe (each scenario set in separate rows)
relative_burden_all_df = data.frame()
for(ss in 3:length(scenario_names)){
  comparison_experiment_name = experiment_names[ss]
  comparison_scenario_name = scenario_names[ss]
  relative_burden_df = get_relative_burden(sim_output_filepath=sim_future_output_dir, reference_experiment_name=reference_experiment_name, comparison_experiment_name=comparison_experiment_name, comparison_scenario_name=comparison_scenario_name, 
                                           start_year=barplot_start_year, end_year=barplot_end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # only save relevant columns for plotting
  relative_burden_df = relative_burden_df[,which(colnames(relative_burden_df) %in% c('scenario', 'Run_Number', burden_colnames))]
  if(nrow(relative_burden_all_df) == 0){
    relative_burden_all_df = relative_burden_df
  }else{
    relative_burden_all_df = rbind(relative_burden_all_df, relative_burden_df)
  }
}
    
# get factors in the correct order (rather than alphabetical)
relative_burden_all_df$scenario = factor(relative_burden_all_df$scenario, levels=scenario_names[3:length(scenario_names)])

rel_burden_agg_list = list()
for(bb in 1:length(burden_colnames)){
  current_burden_name = burden_colnames[bb]
  burden_metric_name = burden_metric_names[bb]
  select_col_names = c(current_burden_name, 'scenario')
  # get mean, min, and max among all runs for this burden metric
  rel_burden_agg = as.data.frame(relative_burden_all_df) %>% dplyr::select(match(select_col_names, names(.))) %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(mean_rel = mean(get(current_burden_name)),
                     max_rel = max(get(current_burden_name)),
                     min_rel = min(get(current_burden_name)))
  rel_burden_agg_list[[bb]] = rel_burden_agg
}
  
gg = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                   barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                   pyr=pyr, chw_cov=chw_cov,
                                   scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, overwrite_files=overwrite_files)


save_plots=TRUE
plot_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                 barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                 pyr=pyr, chw_cov=chw_cov,
                 scenario_names=scenario_names[-1], experiment_names=experiment_names[-1], admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', LLIN2y_flag=LLIN2y_flag)
  




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
plot_relative_burden_IPTi_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, district_subset=district_subset, 
                                   barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                   pyr=pyr, chw_cov=chw_cov,
                                   experiment_names=experiment_names, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
plot_IPTi_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, 
                      barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                      experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)





# PBO
source(paste0(script_hbhi_dir, '/simulation/plot_results_analyses/plot_PBO_impact.R'))


seed_subset = seed_subset_options[2]
pyr = pyr_options[1]
chw_cov = chw_cov_options[1]
itn_halflife = itn_halflife_options[1]
district_subset_name = district_subset_name_options[1]
time_res = time_res_options[1]
cur_admins = pbo_gf_districts
district_subset = 'districtsPBO'
save_plots = TRUE

if(itn_halflife == '2'){
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_noPBO'), 
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN')) 
} else{
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_noPBO'), 
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr')) 
}

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

plot_relative_burden_intervention_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                  pyr=pyr, chw_cov=chw_cov,
                                  experiment_names=experiment_names, inter_name='PBO', overwrite_files=overwrite_files)
plot_relative_burden_intervention_barplots_reversed(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                                    barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                                    pyr=pyr, chw_cov=chw_cov,
                                                    experiment_names=experiment_names, inter_name='PBO', overwrite_files=overwrite_files)
plot_burden_maps_with_without_inter(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                     barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                     experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname, inter_name='PBO')








# IRS

if(itn_halflife == '2'){
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_noIRS'), 
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN')) 
} else{
  experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_noIRS'), 
                       paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr')) 
}

cur_admins = irs_districts
district_subset = 'districtsIRS'
save_plots = TRUE
plot_relative_burden_intervention_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                  barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                  pyr=pyr, chw_cov=chw_cov,
                                  experiment_names=experiment_names, inter_name='IRS', overwrite_files=overwrite_files)
plot_relative_burden_intervention_barplots_reversed(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                           barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                           pyr=pyr, chw_cov=chw_cov,
                                           experiment_names=experiment_names, inter_name='IRS', overwrite_files=overwrite_files)

plot_burden_maps_with_without_inter(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                     barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                     experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname, inter_name='IRS')



























    # need to iterate through burdens and age groups
    gg = plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                             plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                             burden_metric=input$burden_metric, age_plotted=input$age_plotted, 
                                             pyr=pyr, chw_cov=chw_cov,
                                             scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                             indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
    
 
  
  # Create plot with burden and intervention timeseries
  output$timeseriesplot_burdenIntervention <- renderPlot({
    print(get_plot_burden_intervention())
  })
  
  
  
  
  #  =======  barplot of IPTi impact   =======  #
  
  # Plot burden reduction with IPTi
  get_relative_IPTi_barplot = reactive({
    seed_subset = input$seed_subset3
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
    }
    
    pyr = input$pyr3
    chw_cov = input$chw_cov3
    
    if(input$itn_halflife3 == '2'){
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_IPTi'))  # GF with IPTi
    } else{
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_IPTi'))  # GF with IPTi
    }
    
    cur_admins = ipti_districts
    gg = plot_relative_burden_IPTi_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, 
                                            barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                            pyr=pyr, chw_cov=chw_cov,
                                            experiment_names=experiment_names ,overwrite_files=overwrite_files)
    
  })
  
  # Create relative burden barplots with all burden metrics
  output$barplot_IPTi_allBurdenMetrics <- renderPlot({
    print(get_relative_IPTi_barplot())
  })
  
  
  
  
  
  #  =======  maps of IPTi impact   =======  #
  
  
  # Plot burden reduction with IPTi
  get_IPTi_maps = reactive({
    seed_subset = input$seed_subset3
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
    }
    
    pyr = input$pyr3
    chw_cov = input$chw_cov3
    
    if(input$itn_halflife3 == '2'){
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_IPTi'))  # GF with IPTi
    } else{
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_IPTi'))  # GF with IPTi
    }
    
    cur_admins = ipti_districts
    plot_IPTi_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, cur_admins=cur_admins, 
                          barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                          experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)
    
  })
  

# Create Shiny object
shinyApp(ui = ui, server = server)


