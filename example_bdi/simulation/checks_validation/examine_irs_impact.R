# examine_irs_impact.R



# install.packages('haven')
library(haven)
library(dplyr)
library(ggplot2)

library(data.table)
library(ggplot2)
library(scales)
library(cowplot)
library(reshape2)


# copy simulation output
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023')
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'

source(paste0(script_dir, '/simulation/plots_results_analyses/process_sim_output_functions.R'))
source(paste0(script_dir, '/simulation/plots_results_analyses/plot_sim_output_functions.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

surveillance_base_filepath = 'C:/Users/moniqueam/OneDrive - Bill & Melinda Gates Foundation/projects/HBHI_Burundi/data_for_IRS_analysis/Case_surveillance_data'
surveillance_data_filename = paste0(surveillance_base_filepath, '/incid_month.dta')
older_surveillance_data_filename = paste0(surveillance_base_filepath, '/surveillance_suspect_2015_2019.csv')


##############################################################
# setup

# simulation output directory and simulation information
indoor_protection_fraction = 0.75
sim_past_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present')

# experiment information csv (same as intervention input info file)
intervention_coordinator_past_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_to_present.csv')
# set which multipliers were used for different seeds
irs_kill_seed_multipliers = c(0.3, 0.15, 0.45)
num_samples = 50

# set whether old files should be deleted
delete_old_outputs = FALSE

# LGA-specific file giving population sizes, archetypes, etc.
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
archetype_info = read.csv(pop_filepath)
admin_shapefile_filepath = paste0(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/SpatialClustering/reference_rasters_shapefiles/NGA_DS_clusteringProjection.shp')
shapefile_admin_colname = 'NOMDEP'

# scenario file describing which admins are in each plan
scenario_nmcp_df = read.csv(paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/snt_2022/BDI_NSP_PRI_mixes_reviewed.csv'))
# standardize names
scenario_nmcp_df$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=scenario_nmcp_df$adm2)


#############################
# functions

get_admin_set_given_group_name = function(scenario_nmcp_df, district_subset_name = 'all_admins'){
  if(district_subset_name == 'all_admins'){
    district_subset = 'all_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  } else if (district_subset_name == 'priIRS_admins'){
    district_subset = 'priIRS_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name[scenario_nmcp_df$irs_pri == 1])
  } else if (district_subset_name == 'pastIRS_admins'){
    district_subset = 'pastIRS_admins'
    cur_admins = c('Buhiga',
                   'Busoni',
                   'Buye',
                   'Cankuzo',
                   'Gashoho',
                   'Gihofi',
                   'Kiganda',
                   'Kinyinya',
                   'Kiremba',
                   'Kirundo',
                   'Murore',
                   'Muyinga',
                   'Nyabikere',
                   'Rutana',
                   'Ryansoro')
  }else if (district_subset_name == 'pastOnceIRS_admins'){
    district_subset = 'pastOnceIRS_admins'
    cur_admins = c('Buhiga',
                   'Busoni',
                   'Cankuzo',
                   'Gihofi',
                   'Kiganda',
                   'Kirundo',
                   'Murore',
                   'Nyabikere',
                   'Rutana',
                   'Ryansoro')
  } else if (district_subset_name == 'pastOnceIRS_admins_smaller'){
    district_subset = 'pastOnceIRS_admins_smaller'
    cur_admins = c('Buhiga',
                   'Cankuzo',
                   'Gihofi',
                   'Kiganda',
                   'Rutana',
                   'Ryansoro')
  }  else if (district_subset_name == 'noIRS_admins'){
    district_subset = 'noIRS_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
    irs_admins = c('Buhiga',
                   'Busoni',
                   'Buye',
                   'Cankuzo',
                   'Gashoho',
                   'Gihofi',
                   'Kiganda',
                   'Kinyinya',
                   'Kiremba',
                   'Kirundo',
                   'Murore',
                   'Muyinga',
                   'Nyabikere',
                   'Rutana',
                   'Ryansoro')
    cur_admins = cur_admins[!(cur_admins %in% irs_admins)]
  } else {
    warning(paste0('Name for district set not recognized: ', district_subset_name,'. Using all districts.'))
    district_subset = 'all_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  }
  return(list(district_subset, cur_admins))
}

get_individual_admin_timeseries = function(archetype_info, district_subset, cur_admins, 
                                           plot_by_month, min_year, max_year, sim_end_years, 
                                           burden_metric, age_plotted,
                                           exp_filepath, rescale=FALSE, rescale_years=NA, seed_subset=NA){
  burden_colname = NA
  if(burden_metric == 'PfPR'){
    if(age_plotted == 'U5'){
      burden_colname = 'PfPR_U5'
    } else if(age_plotted == 'all'){
      burden_colname = 'PfPR_MiP_adjusted'
    }
  } else if(burden_metric == 'incidence'){
    if(age_plotted == 'U5'){
      burden_colname = 'New_clinical_cases_U5'
    } else if(age_plotted == 'all'){
      burden_colname = 'New_Clinical_Cases'
    }
  } else if(burden_metric == 'directMortality'){
    if(age_plotted == 'U5'){
      burden_colname = 'direct_mortality_nonMiP_U5_mean'
    } else if(age_plotted == 'all'){
      burden_colname = 'direct_mortality_nonMiP_mean'
    }
  } else if(burden_metric == 'allMortality'){
    if(age_plotted == 'U5'){
      burden_colname = 'total_mortality_U5_mean'
    } else if(age_plotted == 'all'){
      burden_colname = 'total_mortality_mean'
    }
  } 
  if(is.na(burden_colname)){
    warning('PROBLEM DETECTED: name of burden metric or age group not currently supported')
  }
  
  
  ############ IRS coverage #############
  interv_dist_all = fread(paste0(exp_filepath, '/monthly_Event_Count.csv'))
  # subset to appropriate seeds, if relevant
  if(all(!is.na(seed_subset)) & is.numeric(seed_subset) & ('Run_Number' %in% colnames(interv_dist_all))){
    interv_dist_all = interv_dist_all[(interv_dist_all$Run_Number+1) %in% seed_subset,]
  }
  intervention_distribution_columns = c('Received_IRS')
  intervention_columns=intervention_distribution_columns
  intervention_distribution_columns_new_name = c('irs_per_cap')
  interv_dist_all = interv_dist_all %>% dplyr::select(one_of(c('admin_name', 'date', 'Run_Number', intervention_distribution_columns)))
  interv_all = interv_dist_all
  interv_all$date = as.Date(interv_all$date)
  interv_all$year = lubridate::year(interv_all$date)
  interv_all = interv_all[intersect(which(interv_all$year >= min_year), which(interv_all$year <= max_year)),]
  
  
  # subset to appropriate admins
  interv_all = interv_all[interv_all$admin_name %in% cur_admins,]  
  colnames(interv_all) = gsub(' ','.',colnames(interv_all))
  
  # get average coverage across months in a year for the annual report
  if(!plot_by_month){
    interv_all = interv_all %>% dplyr::select(one_of('year', 'Run_Number', intervention_distribution_columns, 'admin_name')) %>% dplyr::group_by(year, Run_Number, admin_name) %>% # take sum across months for annual total
      dplyr::summarise_all(sum) %>% ungroup()
  } 
  
  # take average across simulation seeds
  if(plot_by_month){
    interv_agg = interv_all %>% dplyr::select(one_of('year', 'date', intervention_columns, 'admin_name')) %>% 
      dplyr::group_by(year, date, admin_name) %>%
      dplyr::summarise_all(mean)
  } else{
    interv_agg = interv_all %>% dplyr::select(one_of('year', intervention_columns,'admin_name')) %>%  # , new_net_per_cap
      dplyr::group_by(year, admin_name) %>%
      dplyr::summarise_all(mean)
  }
  
  # update column names for plotting
  for(cc in intervention_columns){
    colnames(interv_agg)[colnames(interv_agg)==cc] = intervention_distribution_columns_new_name[which(intervention_distribution_columns==cc)]
  }
  
  
  ############ simulation burden ###############
  # read in simulation information, subset to appropriate years
  cur_sim_output = fread(paste0(exp_filepath, '/malariaBurden_withAdjustments.csv'))
  cur_sim_output = cur_sim_output[intersect(which(cur_sim_output$year >= min_year), which(cur_sim_output$year <= max_year)),]
  # subset to appropriate admins
  cur_sim_output = cur_sim_output[cur_sim_output$admin_name %in% cur_admins,]  
  # subset to appropriate seeds, if relevant
  if(all(!is.na(seed_subset)) & is.numeric(seed_subset) & ('Run_Number' %in% colnames(cur_sim_output))){
    cur_sim_output = cur_sim_output[(cur_sim_output$Run_Number+1) %in% seed_subset,]
  }
  cur_sim_output$burden = cur_sim_output[[burden_colname]]
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    cur_sim_output_agg = as.data.frame(cur_sim_output) %>% dplyr::select(month, year, date, burden, admin_name) %>%
      dplyr::group_by(month, year, date, admin_name) %>%
      dplyr::summarise(mean_burden = mean(burden),
                       max_burden = max(burden),
                       min_burden = min(burden))
  } else{
    cur_sim_output_agg = as.data.frame(cur_sim_output) %>% dplyr::select(year, burden, admin_name) %>%
      dplyr::group_by(year, admin_name) %>%
      dplyr::summarise(mean_burden = mean(burden),
                       max_burden = max(burden),
                       min_burden = min(burden))
  }
  mean(cur_sim_output_agg$mean_burden)
  
  
  
  ############ surveillance burden ###############
  surveillance_data = read_dta(surveillance_data_filename)
  surveillance_data$date = as.Date(paste0(surveillance_data$year,'-',surveillance_data$month,'-01'))
  surveillance_data$admin_name = surveillance_data$adm2
  surveillance_data = standardize_admin_names_in_df(target_names_df=archetype_info, origin_names_df=surveillance_data, target_names_col='admin_name', origin_names_col='admin_name')
  
  surveillance_data = surveillance_data[surveillance_data$admin_name %in% cur_admins,]
  
  
  ############ surveillance burden (older dataset) #############
  older_surve_df = read.csv(older_surveillance_data_filename)
  older_surve_df$admin_name = gsub('DS ', '', older_surve_df$Districts.sanitaires)
  older_surve_df$admin_name = trimws(older_surve_df$admin_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
  older_surve_df = older_surve_df[!is.na(older_surve_df$admin_name) & (older_surve_df$admin_name != ''),]
  older_surve_df = standardize_admin_names_in_df(target_names_df=archetype_info, origin_names_df=older_surve_df, target_names_col='admin_name', origin_names_col='admin_name')
  older_surve_df = older_surve_df[,which(colnames(older_surve_df) %in% c('admin_name', 'X2015', 'X2016', 'X2017', 'X2018', 'X2019'))]
  older_surve_df_long = reshape2::melt(older_surve_df, id.vars='admin_name', variable.name="year",  value.name="susp_cases")
  surv_data_old = older_surve_df_long %>% group_by(admin_name, year) %>%
    summarize_all(sum, na.rm=TRUE) %>% ungroup()
  surv_data_old$year = as.numeric(gsub('X', '', surv_data_old$year))
  
  
  ############ combine datasets and plot ###############
  if(plot_by_month){
    combined_df = merge(interv_agg[,c('admin_name','date','irs_per_cap')],
                        cur_sim_output_agg[,c('admin_name','date','mean_burden')], all=TRUE)
    combined_df = merge(combined_df,
                        surveillance_data[,c('admin_name','date','conf')], all=TRUE)
    
    # rescale all incidence values relative to start of 2017 if indicated
    if(rescale){
      combined_df_ref = combined_df[lubridate::year(combined_df$date) %in% rescale_years,]
      combined_df_ref = combined_df_ref %>% 
        rename(irs_remove = irs_per_cap,
               mean_burden_ref = mean_burden,
               conf_ref = conf) %>%
        dplyr::select(-date) %>% group_by(admin_name) %>%
        summarise_all(mean, na.rm=T) %>% ungroup()
      combined_df = merge(combined_df, combined_df_ref, all=TRUE)
      combined_df$conf = combined_df$conf / combined_df$conf_ref
      combined_df$mean_burden = combined_df$mean_burden / combined_df$mean_burden_ref
    }
    
  } else{
    surveillance_data = surveillance_data %>% 
      dplyr::group_by(year, admin_name) %>%
      dplyr::summarise(conf = sum(conf))
    
    combined_df = merge(interv_agg[,c('admin_name','year','irs_per_cap')],
                        cur_sim_output_agg[,c('admin_name','year','mean_burden')], all=TRUE)
    combined_df = merge(combined_df,
                        surveillance_data[,c('admin_name','year','conf')], all=TRUE)
    
    # rescale all incidence values relative to 2017 if indicated
    if(rescale){
      combined_df_ref = combined_df[combined_df$year %in% rescale_years,]
      combined_df_ref = combined_df_ref %>% 
        rename(irs_remove = irs_per_cap,
               mean_burden_ref = mean_burden,
               conf_ref = conf) %>%
        dplyr::select(-year) %>% group_by(admin_name) %>%
        summarise_all(mean, na.rm=T) %>% ungroup()
      combined_df = merge(combined_df, combined_df_ref, all=TRUE)
      combined_df$conf = combined_df$conf / combined_df$conf_ref
      combined_df$mean_burden = combined_df$mean_burden / combined_df$mean_burden_ref
    }
  }
  return(combined_df)
}


plot_individual_admin_timeseries = function(archetype_info, sim_future_output_dir, district_subset, cur_admins, 
                                            plot_by_month, min_year, max_year, sim_end_years, 
                                            burden_metric, age_plotted,
                                            scenario_filepaths, scenario_names, scenario_input_references, experiment_names, 
                                            LLIN2y_flag, overwrite_files, rescale=FALSE, rescale_years=NA, seed_subset=NA){
  

  combined_df = get_individual_admin_timeseries(archetype_info=archetype_info, district_subset=district_subset, cur_admins=cur_admins, 
                                                plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                                burden_metric=burden_metric, age_plotted=age_plotted,
                                                exp_filepath=exp_filepath, rescale=rescale, rescale_years=rescale_years, seed_subset=seed_subset)
  
  if(plot_by_month){
    # get mean incidence across all included districts
    ave_combined_df = combined_df %>% dplyr::select(date, conf, mean_burden) %>%
      group_by(date) %>%
      summarise_all(mean) %>%
      ungroup()
    
    g_irs = ggplot(combined_df, aes(x=as.Date(date), y=irs_per_cap, color=admin_name)) +
      geom_point(size=5) + 
      xlab('date') + 
      ylab(paste0('IRS given')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
    g_burden = ggplot() +
      geom_line(data=combined_df, aes(x=as.Date(date), y=mean_burden, color=admin_name), size=1) + 
      geom_line(data=ave_combined_df, aes(x=as.Date(date), y=mean_burden), color=rgb(0.5,0.5,0.5,0.8), size=4)+
      xlab('date') + 
      ylim(0,NA)+
      ylab(paste0(burden_metric, ' - ', age_plotted)) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
    g_surv = ggplot()+
      geom_line(data=combined_df, aes(x=as.Date(date), y=conf, color=admin_name), size=1) + 
      geom_line(data=ave_combined_df, aes(x=as.Date(date), y=conf), color=rgb(0.5,0.5,0.5,0.8), size=4)+
      xlab('date') + 
      ylab(paste0('confirmed cases, all ages')) + 
      ylim(0,NA)+
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size)) 
    
  } else{
    # get mean incidence across all included districts
    ave_combined_df = combined_df %>% dplyr::select(year, conf, mean_burden) %>%
      group_by(year) %>%
      summarise_all(mean) %>%
      ungroup()
    
    g_irs = ggplot(combined_df, aes(x=year, y=irs_per_cap, color=admin_name)) +
      geom_point(size=5) + 
      geom_line(alpha=0.2, size=2) +
      xlab('year') + 
      ylab(paste0('IRS given')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
    g_burden = ggplot() +
      geom_line(data=combined_df, aes(x=year, y=mean_burden, color=admin_name), size=1) + 
      geom_line(data=ave_combined_df, aes(x=year, y=mean_burden), color=rgb(0.5,0.5,0.5,0.8), size=4)+
      xlab('year') + 
      ylab(paste0(burden_metric, ' - ', age_plotted)) + 
      ylim(0,NA)+
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
    g_surv = ggplot()+
      geom_line(data=combined_df, aes(x=year, y=conf, color=admin_name), size=1) + 
      geom_line(data=ave_combined_df, aes(x=year, y=conf), color=rgb(0.5,0.5,0.5,0.8), size=4)+
      xlab('year') + 
      ylab(paste0('confirmed cases, all ages')) + 
      ylim(0,NA)+
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size)) 
  }
  gg = plot_grid(plotlist=list(g_irs, g_burden, g_surv), align='hv', nrow=3, ncol=1)
  return(gg)
}


plot_timeseries_separated_by_admin = function(archetype_info, sim_future_output_dir, district_subset, cur_admins, 
                                              plot_by_month, min_year, max_year, sim_end_years, 
                                              burden_metric, age_plotted,
                                              scenario_filepaths, scenario_names, scenario_input_references, experiment_names, 
                                              LLIN2y_flag, overwrite_files, rescale=TRUE, rescale_years=2017, seed_subset=NA){
  
  combined_df = get_individual_admin_timeseries(archetype_info=archetype_info, district_subset=district_subset, cur_admins=cur_admins, 
                                                plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                                burden_metric=burden_metric, age_plotted=age_plotted,
                                                exp_filepath=exp_filepath, rescale=rescale, rescale_years=rescale_years, seed_subset=seed_subset)
  
  if(plot_by_month){
    combined_df$irs_date = NA
    combined_df$irs_date[combined_df$irs_per_cap>0] = combined_df$date[combined_df$irs_per_cap>0]
    
    gg = ggplot() + 
      geom_line(data=combined_df, aes(x=as.Date(date), y=mean_burden), color='darkred', size=1)+
      geom_line(data=combined_df, aes(x=as.Date(date), y=conf), color='dodgerblue', size=1) + 
      geom_vline(data=combined_df, aes(xintercept=irs_date), color='black')+
      coord_cartesian(ylim=c(NA,3))+
      facet_wrap(~admin_name, nrow=7)
    
  } else{
    combined_df$irs_date = NA
    combined_df$irs_date[combined_df$irs_per_cap>0] = combined_df$year[combined_df$irs_per_cap>0]
    
    gg = ggplot() + 
      geom_line(data=combined_df, aes(x=year, y=mean_burden), color='darkred', size=1)+
      geom_line(data=combined_df, aes(x=year, y=conf), color='dodgerblue', size=1) + 
      geom_vline(data=combined_df, aes(xintercept=irs_date), color='black')+
      coord_cartesian(ylim=c(NA,2))+
      facet_wrap(~admin_name, nrow=7)
  }
  return(gg)
}


##############################################################



###############################################
# look at impact of past IRS
intervention_coordinator_past = read.csv(intervention_coordinator_past_filepath)

plot_by_month=FALSE
time_string=ifelse(plot_by_month, '_month', '')
rescale=TRUE
rescale_string=ifelse(rescale, '_rescaled', '')
rescale_years=2017:2021
min_year=2011
max_year=2021
sim_end_years=2021
burden_metric='incidence'
age_plotted='all'
LLIN2y_flag=FALSE
overwrite_files=delete_old_outputs
pyr='c2022'
chw_cov='c2022'
align_seeds = TRUE  # plot only averages across all runs in an experiment, not seedwise comparisons
experiment_names=intervention_coordinator_past$ScenarioName[1]
scenario_filepaths=paste0(sim_past_output_dir, '/', experiment_names)
scenario_names='to-present'
scenario_input_references=intervention_coordinator_past_filepath
scenario_palette = 'black'
exp_filepath=scenario_filepaths[1]
if(!dir.exists(paste0(exp_filepath, '/_plots'))) dir.create(paste0(exp_filepath, '/_plots'))
seed_subset = NA

# plot intervention coverage for certain IRS or non-IRS districts only
subset_names = c('pastIRS', 'priIRS', 'noIRS')
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii
  for(ss in 1:length(subset_names)){
    intervention_name=subset_names[ss]
    admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = paste0(subset_names[ss],'_admins'))
    district_subset = admin_subset_info[[1]]
    cur_admins = admin_subset_info[[2]]
    # plot timeseries of interventions
    gg9 = plot_individual_admin_timeseries(archetype_info=archetype_info, sim_future_output_dir=sim_past_output_dir, district_subset=district_subset, cur_admins=cur_admins, 
                                           plot_by_month=plot_by_month, min_year=2017, max_year=max_year, sim_end_years=sim_end_years, 
                                           burden_metric=burden_metric, age_plotted=age_plotted,
                                           scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, 
                                           LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, rescale=rescale, rescale_years=rescale_years, seed_subset=seed_subset)
    ggsave(paste0(exp_filepath, '/_plots/past_timeseries', time_string,'_interventions', rescale_string,'_', district_subset, '_', round(irs_kill_seed_multipliers[ii]*100), '.png'), gg9, dpi=600, width=10, height=15, units='in')
  }
}

admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'all_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
gg10 = plot_timeseries_separated_by_admin(archetype_info=archetype_info, sim_future_output_dir=sim_past_output_dir, district_subset=district_subset, cur_admins=cur_admins, 
                                          plot_by_month=plot_by_month, min_year=2011, max_year=max_year, sim_end_years=sim_end_years, 
                                          burden_metric=burden_metric, age_plotted=age_plotted,
                                          scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, 
                                          LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, rescale=TRUE, rescale_years=rescale_years, seed_subset=seed_subset)
gg10 = gg10 + theme_bw()
ggsave(paste0(exp_filepath, '/_plots/past_timeseries', time_string,'separateAdmins_IRS_', district_subset, '.png'), gg10, dpi=600, width=18, height=15, units='in')
# also plot results separated by irs_kill_seed_multipliers used in sim seed
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii
  gg11 = plot_timeseries_separated_by_admin(archetype_info=archetype_info, sim_future_output_dir=sim_past_output_dir, district_subset=district_subset, cur_admins=cur_admins, 
                                            plot_by_month=plot_by_month, min_year=2017, max_year=max_year, sim_end_years=sim_end_years, 
                                            burden_metric=burden_metric, age_plotted=age_plotted,
                                            scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, 
                                            LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, rescale=TRUE, rescale_years=rescale_years, seed_subset=seed_subset)
  gg11 = gg11 + theme_bw()
  ggsave(paste0(exp_filepath, '/_plots/past_timeseries', time_string,'separateAdmins_IRS_', district_subset, '_', round(irs_kill_seed_multipliers[ii]*100), '_zoomYear.png'), gg11, dpi=600, width=18, height=15, units='in')
}



# show results across IRS multipliers for subset of admins that only get IRS once or twice
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'pastOnceIRS_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii
  gg11 = plot_timeseries_separated_by_admin(archetype_info=archetype_info, sim_future_output_dir=sim_past_output_dir, district_subset=district_subset, cur_admins=cur_admins, 
                                            plot_by_month=plot_by_month, min_year=2017, max_year=max_year, sim_end_years=sim_end_years, 
                                            burden_metric=burden_metric, age_plotted=age_plotted,
                                            scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, 
                                            LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files, rescale=TRUE, rescale_years=rescale_years, seed_subset=seed_subset)
  gg11 = gg11 + theme_bw()
  ggsave(paste0(exp_filepath, '/_plots/past_timeseries', time_string,'separateAdmins_RareIRS_', district_subset, '_', round(irs_kill_seed_multipliers[ii]*100), '_zoomYear.png'), gg11, dpi=600, width=5, height=10, units='in')
}



### subset to only-once IRS admins; have separate colored lines for different seed subsets

admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'pastOnceIRS_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
combined_df = data.frame()
for(ii in 1:length(irs_kill_seed_multipliers)){
  seed_subset = (0:ceiling(num_samples/length(irs_kill_seed_multipliers))) * length(irs_kill_seed_multipliers) + ii
  combined_df_cur = get_individual_admin_timeseries(archetype_info=archetype_info, district_subset=district_subset, cur_admins=cur_admins, 
                                                    plot_by_month=plot_by_month, min_year=2017, max_year=max_year, sim_end_years=sim_end_years, 
                                                    burden_metric=burden_metric, age_plotted=age_plotted,
                                                    exp_filepath=exp_filepath, rescale=rescale, rescale_years=rescale_years, seed_subset=seed_subset)
  
  combined_df_cur$seed_subset_num=ii
  combined_df_cur$irs_date = NA
  combined_df_cur$irs_date[combined_df_cur$irs_per_cap>0] = combined_df_cur$year[combined_df_cur$irs_per_cap>0]
  if(nrow(combined_df)>0){
    combined_df = merge(combined_df, combined_df_cur, all=TRUE)
  } else{
    combined_df = combined_df_cur
    surv_df = combined_df_cur
  }
}
gg = ggplot() + 
  geom_line(data=combined_df, aes(x=year, y=mean_burden, color=as.factor(seed_subset_num), group=seed_subset_num), size=1)+
  geom_line(data=surv_df, aes(x=year, y=conf), color='black', size=1) + 
  geom_vline(data=surv_df, aes(xintercept=irs_date), color='red')+
  coord_cartesian(ylim=c(NA,2))+
  facet_wrap(~admin_name, nrow=2)


###########################################
# plot past surveillance timeseries with lines for IRS and LLINs

#  surveillance burden - newer dataset
surveillance_data = read_dta(surveillance_data_filename)
surveillance_data$date = as.Date(paste0(surveillance_data$year,'-',surveillance_data$month,'-01'))
surveillance_data$admin_name = surveillance_data$adm2
surveillance_data = standardize_admin_names_in_df(target_names_df=archetype_info, origin_names_df=surveillance_data, target_names_col='admin_name', origin_names_col='admin_name')
# surveillance_data = surveillance_data[surveillance_data$admin_name %in% cur_admins,]
surveillance_data = surveillance_data %>% 
  dplyr::group_by(year, admin_name) %>%
  dplyr::summarise(conf = sum(conf)) %>% ungroup()

#  surveillance burden (older dataset)
older_surve_df = read.csv(older_surveillance_data_filename)
older_surve_df$admin_name = gsub('DS ', '', older_surve_df$Districts.sanitaires)
older_surve_df$admin_name = trimws(older_surve_df$admin_name, which = c("both", "left", "right"), whitespace = "[ \t\r\n]")
older_surve_df = older_surve_df[!is.na(older_surve_df$admin_name) & (older_surve_df$admin_name != ''),]
older_surve_df = standardize_admin_names_in_df(target_names_df=archetype_info, origin_names_df=older_surve_df, target_names_col='admin_name', origin_names_col='admin_name')
older_surve_df = older_surve_df[,which(colnames(older_surve_df) %in% c('admin_name', 'X2015', 'X2016', 'X2017', 'X2018', 'X2019'))]
older_surve_df_long = reshape2::melt(older_surve_df, id.vars='admin_name', variable.name="year",  value.name="susp_cases")
surv_data_old = older_surve_df_long %>% group_by(admin_name, year) %>%
  summarize_all(sum, na.rm=TRUE) %>% ungroup()
surv_data_old$year = as.numeric(gsub('X', '', surv_data_old$year))

min_year = min(surv_data_old$year, na.rm=TRUE)
max_year = max(surveillance_data$year, na.rm=TRUE)

# simulation burden
# read in simulation information, subset to appropriate years
burden_colname = 'New_Clinical_Cases'
exp_filepath=paste0(sim_past_output_dir, '/', intervention_coordinator_past$ScenarioName[1])
cur_sim_output = fread(paste0(exp_filepath, '/malariaBurden_withAdjustments.csv'))
cur_sim_output = cur_sim_output[intersect(which(cur_sim_output$year >= min_year), which(cur_sim_output$year <= max_year)),]
# subset to appropriate seeds, if relevant
if(all(!is.na(seed_subset)) & is.numeric(seed_subset) & ('Run_Number' %in% colnames(cur_sim_output))){
  cur_sim_output = cur_sim_output[(cur_sim_output$Run_Number+1) %in% seed_subset,]
}
cur_sim_output$burden = cur_sim_output[[burden_colname]]
# take average across simulation seeds
cur_sim_output_agg = as.data.frame(cur_sim_output) %>% dplyr::select(year, burden, admin_name) %>%
  dplyr::group_by(year, admin_name) %>%
  dplyr::summarise(mean_burden = mean(burden)) %>% ungroup()


# rescale all incidence values relative to stand_year if indicated
if(rescale){
  stand_years = 2017:2019
  surveillance_data_ref = surveillance_data[surveillance_data$year %in% stand_years,]
  surveillance_data_ref = surveillance_data_ref %>% rename(conf_ref = conf) %>%
    dplyr::select(-year) %>% group_by(admin_name) %>%
    summarise_all(mean, na.rm=T) %>% ungroup()
  surveillance_data = merge(surveillance_data, surveillance_data_ref, all=TRUE)
  surveillance_data$conf = surveillance_data$conf / surveillance_data$conf_ref
  
  surv_data_old_ref = surv_data_old[surv_data_old$year %in% stand_years,]
  surv_data_old_ref = surv_data_old_ref %>% rename(susp_ref = susp_cases) %>%
    dplyr::select(-year)%>% group_by(admin_name) %>%
    summarise_all(mean, na.rm=T) %>% ungroup()
  surv_data_old = merge(surv_data_old, surv_data_old_ref, all=TRUE)
  surv_data_old$susp_cases = surv_data_old$susp_cases / surv_data_old$susp_ref
  
  cur_sim_output_ref = cur_sim_output_agg[cur_sim_output_agg$year %in% stand_years,]
  cur_sim_output_ref = cur_sim_output_ref %>% rename(burden_ref = mean_burden) %>%
    dplyr::select(-year)%>% group_by(admin_name) %>%
    summarise_all(mean, na.rm=T) %>% ungroup()
  cur_sim_output_agg = merge(cur_sim_output_agg, cur_sim_output_ref, all=TRUE)
  cur_sim_output_agg$mean_burden = cur_sim_output_agg$mean_burden / cur_sim_output_agg$burden_ref
}

# get years of interventions using simulation input files
irs_input_file = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent/IRS_2010_toPresent.csv'))
irs_input_file = irs_input_file %>% dplyr::select(admin_name, year)
irs_input_file = irs_input_file[irs_input_file$year>=min_year & irs_input_file$year<=max_year,]
irs_input_file = distinct(irs_input_file)
itn_input_file = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent/itn_mass_coverages_mort_2010_toPresent.csv'))
itn_input_file = itn_input_file %>% dplyr::select(admin_name, year)
itn_input_file = itn_input_file[itn_input_file$year>=min_year & itn_input_file$year<=max_year,]
itn_input_file = distinct(itn_input_file)

# create plot without simulation results
gg = ggplot()+
  geom_line(data=surveillance_data, aes(x=year, y=conf), color='blue')+
  geom_line(data=surv_data_old, aes(x=year, y=susp_cases), color='green')+
  # geom_line(data=cur_sim_output_agg, aes(x=year, y=mean_burden), color=rgb(1,0,1,0.5))+
  geom_vline(data=irs_input_file, aes(xintercept=year), color=rgb(0,0,0,0.2), size=2)+
  geom_vline(data=itn_input_file, aes(xintercept=year), color=rgb(1,0,0,0.8), size=0.8)+
  facet_wrap(~admin_name, nrow=7)+
  theme_bw()
ggsave(paste0(exp_filepath, '/_plots/past_surveillance_timeseries_rescaled_with_IRS_ITN_dates.png'), gg, dpi=600, width=10, height=10, units='in')
# create plot with simulation results
gg = ggplot()+
  geom_line(data=surveillance_data, aes(x=year, y=conf), color='blue')+
  geom_line(data=surv_data_old, aes(x=year, y=susp_cases), color='green')+
  geom_line(data=cur_sim_output_agg, aes(x=year, y=mean_burden), color=rgb(1,0,1,0.5))+
  geom_vline(data=irs_input_file, aes(xintercept=year), color=rgb(0,0,0,0.2), size=2)+
  geom_vline(data=itn_input_file, aes(xintercept=year), color=rgb(1,0,0,0.8), size=0.8)+
  facet_wrap(~admin_name, nrow=7)+
  theme_bw()
ggsave(paste0(exp_filepath, '/_plots/past_surveillance_and_sim_timeseries_rescaled_with_IRS_ITN_dates.png'), gg, dpi=600, width=10, height=10, units='in')








# plot newer surveillance, IRS dates, and simulation results broken out by multiplier for more recent years
# create plot with simulation results
gg = ggplot()+
  geom_line(data=surveillance_data, aes(x=year, y=conf), color='black')+
  geom_line(data=cur_sim_output_agg, aes(x=year, y=mean_burden), color=rgb(1,0,1,0.5))+
  geom_vline(data=irs_input_file, aes(xintercept=year), color=rgb(0,0,0,0.2), size=2)+
  coord_cartesian(ylim=c(2017,2021))+
  # geom_vline(data=itn_input_file, aes(xintercept=year), color=rgb(1,0,0,0.8), size=0.8)+
  facet_wrap(~admin_name, nrow=7)+
  theme_bw()
ggsave(paste0(exp_filepath, '/_plots/past_surveillance_and_sim_timeseries_rescaled_with_IRSdates_IRSmultipliers.png'), gg, dpi=600, width=10, height=10, units='in')



# #####################
# # irs impact looked too large, so generate new versions of the input file with initial mortality scaled down various degrees
# irs_multipliers = c(0.1, 0.3, 0.5, 0.7)
# original_irs = read.csv(paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent/IRS_2010_toPresent.csv'))
# for(ii in 1:length(irs_multipliers)){
#   new_irs = original_irs
#   new_irs$initial_kill = new_irs$initial_kill * irs_multipliers[ii]
#   write.csv(new_irs, paste0(hbhi_dir, '/simulation_inputs/interventions_2010_toPresent/IRS_2010_toPresent_mortMult', 100*irs_multipliers[ii], '.csv'), row.names=FALSE)
# }






### subset to only-once IRS admins; have separate colored lines for different seed subsets
### look across different experiments with different IRS multipliers
irs_multipliers_exp = c(0.1,0.3,0.5,0.7,1)
rescale_years = c(2017, 2018)

experiment_names=paste0('BDI_toPresent_allInter_v3_irs', round(100*irs_multipliers_exp))
scenario_filepaths=paste0(sim_past_output_dir, '/', experiment_names)

admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'pastOnceIRS_admins_smaller')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
combined_df = data.frame()
for(ii in 1:length(irs_multipliers_exp)){
  exp_filepath = scenario_filepaths[ii]
  combined_df_cur = get_individual_admin_timeseries(archetype_info=archetype_info, district_subset=district_subset, cur_admins=cur_admins, 
                                                    plot_by_month=plot_by_month, min_year=2017, max_year=max_year, sim_end_years=sim_end_years, 
                                                    burden_metric=burden_metric, age_plotted=age_plotted,
                                                    exp_filepath=exp_filepath, rescale=rescale, rescale_years=rescale_years, seed_subset=NA)
  
  
  combined_df_cur$irs_multiplier=irs_multipliers_exp[ii]
  combined_df_cur$irs_date = NA
  combined_df_cur$irs_date[combined_df_cur$irs_per_cap>0] = combined_df_cur$year[combined_df_cur$irs_per_cap>0]
  if(nrow(combined_df)>0){
    combined_df = merge(combined_df, combined_df_cur, all=TRUE)
  } else{
    combined_df = combined_df_cur
    surv_df = combined_df_cur
  }
}
# combined_df$irs_multiplier = factor(combined_df$irs_multiplier, levels=irs_multipliers_exp)
blue_colors = rev(brewer.pal(n = (4+length(irs_multipliers_exp)), "BuPu")[3:(length(irs_multipliers_exp)+2)])
gg = ggplot() + 
  geom_line(data=combined_df, aes(x=year, y=mean_burden, color=as.factor(irs_multiplier), group=irs_multiplier), size=0.7)+
  geom_line(data=surv_df, aes(x=year, y=conf), color='black', size=1) + 
  scale_colour_manual(values=blue_colors) +
  geom_vline(data=surv_df, aes(xintercept=irs_date+0.5), color=rgb(0.0,0.5,0,0.2), size=2)+
  geom_vline(xintercept=c(2017,2019)+0.5, color=rgb(0.7,0.2,0,0.2), size=1)+
  # geom_rect(data=surv_df, aes(xmin=irs_date-0.1, xmax=irs_date+1.2, ymin=0,ymax=Inf), alpha=0.2, fill=rgb(0.5,0,0)) +
  ylab('rescaled clinical cases')+
  coord_cartesian(ylim=c(NA,2))+
  facet_wrap(~admin_name, nrow=2)+
  theme_test()+
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=0.8))

ggsave(paste0(hbhi_dir, '/simulation_output/simulations_to_present/_plots/past_surveillance_and_sim_timeseries_rescaled1718_with_IRSdates_IRSmultipliers_sweep.png'), gg, dpi=600, width=7, height=4, units='in')

