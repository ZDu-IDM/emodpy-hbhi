# rank_districts_burden_averted.R
# May 2023

# original prompt: rank districts by the impact of IG2 instead of pyrethroid-only nets







##############################################################
# setup

library(data.table)
library(ggplot2)
library(scales)
library(cowplot)
library(sf)

# copy simulation output
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
hbhi_dir = paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023')
script_dir = 'C:/Users/moniqueam/Documents/malaria-snt-core'

source(paste0(script_dir, '/simulation/plots_results_analyses/process_sim_output_functions.R'))
source(paste0(script_dir,'/standardize_admin_names.R'))

# simulation output directory and simulation information
indoor_protection_fraction = 0.75
end_year_to_present = 2021
sim_future_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_future/v3')
end_year_future = 2029
# experiment information csv (same as intervention input info file)
intervention_coordinator_future_filepath = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')
admin_shapefile_filepath = (paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp'))

# set whether old files should be deleted
delete_old_outputs = FALSE

# for(base_filepath in c(sim_past_output_dir, shiny_output_filepath_past, sim_future_output_dir,shiny_output_filepath_future)){
for(base_filepath in c(sim_future_output_dir)){
  if(!dir.exists(paste0(base_filepath))) dir.create(paste0(base_filepath))
  if(!dir.exists(paste0(base_filepath, '/_plots'))) dir.create(paste0(base_filepath, '/_plots'))
}
# DS-specific file giving population sizes, archetypes, etc.
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
archetype_info = read.csv(pop_filepath)
admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
shapefile_admin_colname = 'NOMDEP'

# scenario file describing which admins are in each plan
scenario_nmcp_df = read.csv(paste0(user_path, '/Dropbox (IDM)/Malaria Team Folder/data/Burundi/WHO/snt_2022/BDI_NSP_PRI_mixes_reviewed.csv'))
# standardize names
scenario_nmcp_df$admin_name = standardize_admin_names_in_vector(target_names=archetype_info$admin_name, origin_names=scenario_nmcp_df$adm2)
# shapefile
admin_shapefile = st_read(admin_shapefile_filepath)



## ========================================================================= ##
# setup and functions
## ========================================================================= ##


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
  }else {
    warning(paste0('Name for district set not recognized: ', district_subset_name,'. Using all districts.'))
    district_subset = 'all_admins'
    cur_admins = unique(scenario_nmcp_df$admin_name)
  }
  return(list(district_subset, cur_admins))
}





#################################
# rank district where IG2 has largest impact (rescaled for burden averted per net distributed)
################################

# get the names and locations of experiments to be included
intervention_name='UpgradeIG2'
admin_subset_info = get_admin_set_given_group_name(scenario_nmcp_df, district_subset_name = 'PriPyr_admins')
district_subset = admin_subset_info[[1]]
cur_admins = admin_subset_info[[2]]
age_plotted_options = c('U5', 'all')
min_year = 2024
max_year = 2030
overwrite_files=FALSE
LLIN2y_flag=FALSE


experiment_names = c('BDI_pri1', 'BDI_pri1_50mort', 'BDI_pri2', 'BDI_pri2_50mort')
itn_types = c('standard', 'standard','IG2','IG2')
resistance = c('lower', 'higher', 'lower','higher')
scenario_input_references = c(rep(intervention_coordinator_future_filepath, length(experiment_names)))
scenario_names = experiment_names
experiment_base_directories = c(rep(sim_future_output_dir, length(experiment_names)))
scenario_filepaths = paste0(experiment_base_directories, '/', experiment_names)
scenario_palette = rep(c('#0040C0', '#548235',  '#739AE6', '#A9D18E'), 1)
names(scenario_palette) = scenario_names

# in each admin, record nets distributed per 100 people per year; percent reduction in PfPR, incidence, mortality
intervention_df = data.frame()
burden_df = data.frame()
for(ee in 1:length(experiment_names)){
  exp_filepath = scenario_filepaths[ee]
  exp_name = experiment_names[ee]
  intervention_cur = get_intervention_per_cap_each_admin(exp_filepath=exp_filepath, exp_name=exp_name, cur_admins=cur_admins, min_year=min_year, max_year=max_year)
  intervention_cur$scenario = scenario_names[ee]
  intervention_cur$itn_type = itn_types[ee]
  intervention_cur$resistance = resistance[ee]
  
  sim_output_filepath=experiment_base_directories[ee]
  burden_cur = get_cumulative_burden_each_admin(sim_output_filepath=sim_output_filepath, experiment_name=exp_name, start_year=min_year, end_year=max_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
  burden_cur$scenario = scenario_names[ee]
  burden_cur$itn_type = itn_types[ee]
  burden_cur$resistance = resistance[ee]

  if(nrow(intervention_df)<1){
    intervention_df = intervention_cur
    burden_df = burden_cur
  } else{
    intervention_df = merge(intervention_df, intervention_cur, all=TRUE)
    burden_df = merge(burden_df, burden_cur, all=TRUE)
  }
}
compare_all_df = merge(intervention_df, burden_df, all=TRUE)
# compare_all_df_agg = compare_all_df %>% group_by(admin_name, scenario, itn_type, resistance) %>%
#   summarise_all(.tbl = ., funs(min_95 = quantile(., 0.025, na.rm=TRUE), 
#                                max_95 = quantile(., 0.975, na.rm=TRUE), 
#                                median = median(., na.rm=TRUE), 
#                                mean = mean(., na.rm=TRUE))) %>%
#   ungroup()

compare_pyr = compare_all_df[compare_all_df$itn_type=='standard',]
compare_ig2 = compare_all_df[compare_all_df$itn_type=='IG2',]
# order rows so that both dataframes are exactly matched
compare_pyr = compare_pyr[with(compare_pyr, order(admin_name, Run_Number, resistance)),]
compare_ig2 = compare_ig2[with(compare_ig2, order(admin_name, Run_Number, resistance)),]


# if(all(compare_ig2$admin_name == compare_pyr$admin_name) & all(compare_ig2$Run_Number == compare_pyr$Run_Number) & all(compare_ig2$resistance == compare_pyr$resistance)){
#   num_cols = ncol(compare_ig2)
#   info_cols = compare_ig2[,c('admin_name', 'Run_Number', 'resistance')]
#   percent_reductions = (compare_pyr[,6:num_cols] - compare_ig2[,6:num_cols]) / compare_ig2[,6:num_cols] * 100
#   percent_averted = cbind(info_cols, percent_reductions)
# }
# 
# # get summary statistics between runs
# percent_averted_agg = percent_averted %>% group_by(admin_name, resistance) %>%
#   summarise_all(.tbl = ., funs(min_95 = quantile(., 0.025, na.rm=TRUE), 
#                                max_95 = quantile(., 0.975, na.rm=TRUE), 
#                                median = median(., na.rm=TRUE), 
#                                mean = mean(., na.rm=TRUE))) %>%
#   ungroup()
# 
# # order by background resistance and case burden averted
# percent_averted_agg = percent_averted_agg[with(percent_averted_agg, order(resistance, cases_pp_all_ages_mean)),]
# percent_averted_agg$admin_name = factor(percent_averted_agg$admin_name, levels=unique(percent_averted_agg$admin_name))
# ggplot(percent_averted_agg, aes(x=admin_name, y=cases_pp_all_ages_mean, fill=resistance)) + 
#   geom_bar(stat='identity', position=position_dodge())+
#   ylab('percent reduction in cases (all ages)')




# look at per-capita reduction in cases, not relative reduction

if(all(compare_ig2$admin_name == compare_pyr$admin_name) & all(compare_ig2$Run_Number == compare_pyr$Run_Number) & all(compare_ig2$resistance == compare_pyr$resistance)){
  num_cols = ncol(compare_ig2)
  info_cols = compare_ig2[,c('admin_name', 'Run_Number', 'resistance')]
  per_cap_reduction = (compare_pyr[,6:num_cols] - compare_ig2[,6:num_cols])
  per_cap_averted = cbind(info_cols, per_cap_reduction)
}

# get summary statistics between runs
per_cap_averted_agg = per_cap_averted %>% group_by(admin_name, resistance) %>%
  summarise_all(.tbl = ., funs(min_90 = quantile(., 0.05, na.rm=TRUE), 
                               max_90 = quantile(., 0.95, na.rm=TRUE), 
                               median = median(., na.rm=TRUE), 
                               mean = mean(., na.rm=TRUE))) %>%
  ungroup()


# plotting function
plot_comparsons_for_metric = function(compare_all_df, per_cap_averted_agg, burden_metric, burden_metric_name, resistance_cur, color_assignments, admin_shapefile, per_cap=FALSE){
  # switch from per-capita to per-1000 for appropriate columns if appropriate
  if(!per_cap) {
    compare_all_df[,grepl('pp', colnames(compare_all_df))] = 1000*compare_all_df[,grepl('pp', colnames(compare_all_df))]
    per_cap_averted_agg[,grepl('pp', colnames(per_cap_averted_agg))] = 1000*per_cap_averted_agg[,grepl('pp', colnames(per_cap_averted_agg))]
  }
  # order by background resistance and case burden averted
  per_cap_averted_agg_cur = per_cap_averted_agg[per_cap_averted_agg$resistance==resistance_cur,]
  per_cap_averted_agg_cur = per_cap_averted_agg_cur[order(per_cap_averted_agg_cur$resistance, per_cap_averted_agg_cur[[paste0(burden_metric, '_mean')]]),]
  ordered_admins = unique(per_cap_averted_agg_cur$admin_name)
  per_cap_averted_agg_cur$admin_name = factor(per_cap_averted_agg_cur$admin_name, levels=ordered_admins)
  compare_all_df_cur = compare_all_df[compare_all_df$resistance == resistance_cur,]
  compare_all_df_cur$admin_name = factor(compare_all_df_cur$admin_name, levels=ordered_admins)
  compare_all_df_cur$itn_type = factor(compare_all_df_cur$itn_type, levels=c('standard','IG2'))
  
  
  text_size = 15
  # plot violin of simulation output with standard and with IG2 nets
  gg1 = ggplot(compare_all_df_cur, aes(x=admin_name, y=!!sym(paste0(burden_metric)), fill=itn_type))+
    geom_violin(position=position_dodge(0.75), color=NA, width=1.5)+
    ylab(burden_metric_name)+
    xlab('')+
    ylim(min(compare_all_df[[burden_metric]], na.rm=T),max(compare_all_df[[burden_metric]], na.rm=T))+
    scale_fill_manual(values = color_assignments) + 
    theme_bw()+
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 35, vjust = 1, hjust=0.8), plot.title=element_blank(),legend.position="top")
  
  # plot bars showing mean difference between net types
  gg2 = ggplot(per_cap_averted_agg_cur, aes(x=admin_name, y=!!sym(paste0(burden_metric, '_mean')))) + #, fill=resistance
    geom_bar(stat='identity', position=position_dodge(), fill=toString(color_assignments['IG2']))+
    ylab(paste0('reduction in ',burden_metric_name))+
    xlab('')+
    ylim(min(0,min(per_cap_averted_agg[[paste0(burden_metric, '_mean')]], na.rm=T)),max(per_cap_averted_agg[[paste0(burden_metric, '_mean')]], na.rm=T))+
    # geom_errorbar(aes(x=admin_name, ymin=!!sym(paste0(burden_metric, '_min_90')), ymax=!!sym(paste0(burden_metric, '_max_90')), group=resistance), position='dodge',  colour="black", alpha=0.7, size=0.5)+
    theme_bw()+
    theme(text = element_text(size = text_size), axis.text.x = element_text(angle = 35, vjust = 1, hjust=0.8), plot.title=element_blank())
  
  
  # create map of impact and ranking
  # colors
  all_values = per_cap_averted_agg[[paste0(burden_metric, '_mean')]]
  num_colors=10
  max_value = max(all_values, na.rm=TRUE)
  min_value = min(all_values, na.rm=TRUE)
  colorscale = unname(pals::brewer.blues(num_colors+2))[-c(1:2)]

  per_cap_averted_agg_cur$output_value = per_cap_averted_agg_cur[[paste0(burden_metric, '_mean')]]
  admin_cur = admin_shapefile %>%
    left_join(per_cap_averted_agg_cur, by=c('NOMDEP' = 'admin_name')) %>%
    mutate(binned_values=cut(output_value,
                            # breaks=signif(seq((min_value), (max_value)*1.02, length.out = (num_colors+1)), digits=6)))
                            breaks=seq((min_value-abs(0.01*min_value)), (max_value+abs(0.01*max_value)), length.out = (num_colors+1))))
  gg3 = ggplot(admin_cur) +
      geom_sf(aes(fill=binned_values), size=0.1) +
      scale_fill_manual(values=setNames(colorscale, levels(admin_cur$binned_values)), drop=FALSE, name='averted', na.value='grey96') + 
      guides(fill = guide_legend(reverse=T)) +
      theme_void() 

  
  return(list(gg1, gg2, gg3))
}


# plot with different burden metrics
burden_metrics = c('pfpr_all_ages', 'cases_pp_all_ages', 'direct_deaths_1_pp_all_ages', 
                   'pfpr_U5', 'cases_pp_U5', 'direct_deaths_1_pp_U5')
burden_metric_names = c('PfPR (all ages)', 'cases (annual per capita, all ages)', 'mortality (annual per capita, all ages)',
                        'PfPR (U5)', 'cases (annual per capita, U5)', 'mortality (annual per capita, U5)')
for(bb in 1:length(burden_metrics)){
  burden_metric = burden_metrics[bb]
  burden_metric_name = burden_metric_names[bb]
  plot_list = list()
  for(resistance_cur in c('lower', 'higher')){
    color_assignments = scenario_palette[resistance == resistance_cur]
    names(color_assignments) = itn_types[resistance == resistance_cur]
    plot_outputs = plot_comparsons_for_metric(compare_all_df=compare_all_df, per_cap_averted_agg=per_cap_averted_agg, burden_metric=burden_metric, burden_metric_name=burden_metric_name, resistance_cur=resistance_cur, color_assignments=color_assignments, admin_shapefile=admin_shapefile)
    plot_list = c(plot_list, plot_outputs)
  }
  ggsave(filename=paste0(sim_future_output_dir, '/_plots/impact_of_ITN_upgrade_', burden_metric,'_',district_subset,'.png'), 
    plot=marrangeGrob(plot_list, nrow=3, ncol=2), 
    width=9, height=9)
}
