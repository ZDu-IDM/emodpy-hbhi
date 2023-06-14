# compare_seedwise_scenario_burden.R
library(data.table)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

source('C:/Users/moniqueam/Documents/malaria-sle-hbhi/simulation/plot_results_analyses/compare_seedwise_scenario_burden_functions.R')


hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
admin_pop = read.csv(ds_pop_df_filename)
admin_pop = admin_pop[,colnames(admin_pop) %in% c('admin_name','pop_size')]
# simulation output directory
sim_output_filepath = paste0(hbhi_dir, '/simulation_output/simulations_future')
# get names of experiments to calculate cumulative burden for
intervention_csv_filename = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/Interventions_for_projections.csv')
scenario_adjustment_info = read.csv(intervention_csv_filename, as.is=TRUE)
experiment_names = scenario_adjustment_info$ScenarioName
# set start and end years foc tallying cumulative burden
start_year = 2022
end_year = 2024



# create and save data tables with the cumulative burden of all simulation sets
for(experiment_name in experiment_names[c(1:2, 13:22)]){
  get_cumulative_burden(sim_output_filepath, experiment_name, start_year, end_year, admin_pop)
}


# plot ratios of values between different scenarios
# comparison scenarios
permMort = 64  # current options: 64, 40
# CHW assumption coverage level
chw_assumption = 'higherCHWCoverage_'  # current options: 'lowerCHWCoverage_', 'higherCHWCoverage_'
# BAU vs GF
comp1 = c(paste0("BDI_projection_", permMort, "pyr"), paste0("BDI_projection_GF_", chw_assumption, permMort, "pyr"))
# GF vs GF with PBO
comp2 = c(paste0("BDI_projection_GF_", chw_assumption, permMort, "pyr"), paste0("BDI_projection_GF_", chw_assumption, "PBO", permMort, "pyr"))
# GF vs GF at 80% coverage
comp3 = c(paste0("BDI_projection_GF_", chw_assumption, permMort, "pyr"), paste0("BDI_projection_GF_80Coverage_", permMort, "pyr"))

comparisons = list(comp1, comp2, comp3)
comparison_names = c('BAU vs GF', 'GF vs all-PBO', 'GF at current coverage vs GF with 80% coverage')
comparison_names_ylab = c('BAU vs GF', 'GF vs all-PBO', 'GF vs GF with 80% cov')
gg_list = list()
for(ii in 1:length(comparison_names)){
  comp_cur = comparisons[[ii]]
  burden_1 = read.csv(paste0(sim_output_filepath, '/', comp_cur[1], '/cumulativeBurden_', start_year, '_', end_year, '.csv'))
  burden_2 = read.csv(paste0(sim_output_filepath, '/', comp_cur[2], '/cumulativeBurden_', start_year, '_', end_year, '.csv'))
  burden_1 = melt(burden_1, id.vars='Run_Number')
  colnames(burden_1)[colnames(burden_1) == 'value'] = 'value_1'
  burden_2 = melt(burden_2, id.vars='Run_Number')
  colnames(burden_2)[colnames(burden_2) == 'value'] = 'value_2'
  burden_both = merge(burden_1, burden_2, by=c('Run_Number', 'variable'))
  burden_both$ratio = burden_both$value_2 / burden_both$value_1
  # subset to plotted comparisons
  burden_both = burden_both[burden_both$variable %in% c('Run_Number','incidence_all', 'incidence_U5', 'annual_num_mLBW', 'annual_num_mStill',
                                                        'death_rate_mean_all', 'death_rate_mean_U5', 'average_PfPR_all','average_PfPR_U5'),]
  # record which mortality/BF function set was used in each seed
  sim_seed_group_names = c('mortLogLog_blockOurs', 'mortLog_blockOurs', 'mortLogLog_blockNash', 'mortLog_blockNash')
  burden_both$mort_BF_func_index = as.factor(burden_both$Run_Number %% 4 + 1)
  burden_both$mort_BF_func = sim_seed_group_names[burden_both$mort_BF_func_index]

  # get mean ratio across all seeds for each burden metric
  burden_both_means = burden_both[,c('variable','ratio')]  %>% group_by(variable) %>%
    summarise_all(mean) %>%
    as.data.frame
  burden_both_means$ratio = round(burden_both_means$ratio, 2)

  # create ggplot
  base_string = strsplit(comparison_names_ylab[ii], ' vs ')[[1]][1]
  comp_string = strsplit(comparison_names_ylab[ii], ' vs ')[[1]][2]
  gg = ggplot(burden_both, aes(x=variable, y=ratio)) +
    geom_violin(trim=FALSE) +
    geom_jitter(shape=16, position=position_jitter(0.1), size=1, aes(color=mort_BF_func)) +  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, binwidth=0.001)
    ggtitle(comparison_names[ii]) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    ylab(paste0(comp_string, ' / ', base_string)) +
    geom_hline(yintercept=1, color = rgb(0.5,0.5,0.5,0.2), size=2) +
    # ylim(0.5, 1.25) +
    geom_text(data=burden_both_means, aes(x=variable, y=1.2, label=ratio)) +
    theme_classic() +
    theme(axis.title.x = element_blank())#, axis.text = element_text(size = 5))
  gg_list[[ii]] = gg
  # print(gg)
}
# grid.arrange(grobs=gg_list)
ggsave(paste0(sim_output_filepath, '/_plots/compareCumulativeBurdens_', start_year, '_', end_year, '_', chw_assumption, 'perm', permMort,'.png'), arrangeGrob(grobs=gg_list, nrow=3), units='in',width=9, height=6, dpi=320)



# subset to mortLog or mortLogLog
mort_groups = list(c('mortLogLog_blockOurs', 'mortLogLog_blockNash'), c('mortLog_blockOurs', 'mortLog_blockNash'))
mort_group_names = c('mortLogLog', 'mortLog')
for(mm in 1:length(mort_groups)){
  gg_list = list()
  for(ii in 1:length(comparison_names)){
    comp_cur = comparisons[[ii]]
    burden_1 = read.csv(paste0(sim_output_filepath, '/', comp_cur[1], '/cumulativeBurden_', start_year, '_', end_year, '.csv'))
    burden_2 = read.csv(paste0(sim_output_filepath, '/', comp_cur[2], '/cumulativeBurden_', start_year, '_', end_year, '.csv'))
    burden_1 = melt(burden_1, id.vars='Run_Number')
    colnames(burden_1)[colnames(burden_1) == 'value'] = 'value_1'
    burden_2 = melt(burden_2, id.vars='Run_Number')
    colnames(burden_2)[colnames(burden_2) == 'value'] = 'value_2'
    burden_both = merge(burden_1, burden_2, by=c('Run_Number', 'variable'))
    burden_both$ratio = burden_both$value_2 / burden_both$value_1
    # subset to plotted comparisons
    burden_both = burden_both[burden_both$variable %in% c('Run_Number','incidence_all', 'incidence_U5', 'annual_num_mLBW', 'annual_num_mStill', 
                                                          'death_rate_mean_all', 'death_rate_mean_U5', 'average_PfPR_all','average_PfPR_U5'),]
    # record which mortality/BF function set was used in each seed
    sim_seed_group_names = c('mortLogLog_blockOurs', 'mortLog_blockOurs', 'mortLogLog_blockNash', 'mortLog_blockNash')
    burden_both$mort_BF_func_index = as.factor(burden_both$Run_Number %% 4 + 1)
    burden_both$mort_BF_func = sim_seed_group_names[burden_both$mort_BF_func_index]
    
    # subset to seeds with the appropriate mortality function
    burden_both = burden_both[burden_both$mort_BF_func %in% mort_groups[[mm]],]
    
    # get mean ratio across all seeds for each burden metric
    burden_both_means = burden_both[,c('variable','ratio')]  %>% group_by(variable) %>%
      summarise_all(mean) %>%
      as.data.frame
    burden_both_means$ratio = round(burden_both_means$ratio, 2)
    
    # create ggplot
    base_string = strsplit(comparison_names_ylab[ii], ' vs ')[[1]][1]
    comp_string = strsplit(comparison_names_ylab[ii], ' vs ')[[1]][2]
    gg = ggplot(burden_both, aes(x=variable, y=ratio)) + 
      geom_violin(trim=FALSE) + 
      geom_jitter(shape=16, position=position_jitter(0.1), size=1, color=rgb(0.5,0.5,0.5,0.5)) +#, aes(color=mort_BF_func)) +  #geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1, binwidth=0.001)
      ggtitle(comparison_names[ii]) + 
      scale_x_discrete(guide = guide_axis(n.dodge = 2), 
                       limits=c('average_PfPR_U5',  'average_PfPR_all','incidence_U5', 'incidence_all', 'death_rate_mean_U5','death_rate_mean_all', 'annual_num_mStill', 'annual_num_mLBW')) +
      ylab(paste0(comp_string, ' / ', base_string)) + 
      geom_hline(yintercept=1, color = rgb(0.5,0.5,0.5,0.2), size=2) + 
      ylim(0.5, 1.15) +
      geom_text(data=burden_both_means, aes(x=variable, y=1.15, label=ratio)) +
      theme_classic() + 
      theme(axis.title.x = element_blank())#, axis.text = element_text(size = 5)) 
    gg_list[[ii]] = gg
    # print(gg)
  }
  # grid.arrange(grobs=gg_list)
  ggsave(paste0(sim_output_filepath, '/_plots/compareCumulativeBurdens_', start_year, '_', end_year, '_', mort_group_names[mm], '_', chw_assumption, 'perm', permMort,'.png'), arrangeGrob(grobs=gg_list, nrow=3), units='in',width=9, height=6, dpi=320)
}

 
