# compare_seedwise_scenario_burden_functions.R


text_size = 15
save_plots = FALSE

# get cumulative burden over specified years (for all malaria metrics, separate values for each seed)
get_cumulative_burden = function(sim_output_filepath, experiment_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  # create data frame cumulative_burden, where each row is a seed and each column is the total over all included years of different burden metrics:
  # sum of:
  #  - clinical cases (all ages)
  #  - clinical cases (U5)
  #  - deaths (all ages) - upper, lower, average parameter estimates
  #  - deaths (U5) - upper, lower, average parameter estimates
  #  - mLBW
  #  - malaria-attributable stillbirths
  # average of (population weighted):
  #  - PfPR (all ages)
  #  - PfPR (U5)
  if(LLIN2y_flag){
    llin2y_string = '_2yLLIN'
  } else{
    llin2y_string = ''
  }
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/cumulativeBurden_', start_year, '_', end_year, '_', district_subset, llin2y_string, '.csv')
  if(file.exists(output_filename) & !overwrite_files){
    df_aggregated = read.csv(output_filename)
  }else{
    # if we include all admins, get list of names from population size dataframe
    if(cur_admins[1] == 'all'){
      cur_admins = unique(admin_pop$admin_name)
    }
    
    cur_file = fread(paste0(sim_output_filepath, '/', experiment_name, '/malariaBurden_withAdjustments.csv'), check.names=TRUE)
    # filter to relevant years
    cur_file = cur_file[cur_file$year <= end_year,]
    cur_file = cur_file[cur_file$year >= start_year,]
    # merge population sizes in each admin
    df = merge(cur_file, admin_pop, by='admin_name')
    # subset to appropriate admins
    df = df[df$admin_name %in% cur_admins,]  
    
    # all age metrics - rescaled to full population
    df$positives_all_ages = df$PfPR_MiP_adjusted * df$pop_size
    df$cases_all_ages = df$New_Clinical_Cases * df$pop_size / df$Statistical_Population
    df$deaths_1_all_ages = df$total_mortality_1 * df$pop_size / df$Statistical_Population
    df$deaths_2_all_ages = df$total_mortality_2 * df$pop_size / df$Statistical_Population
    df$num_mLBW = df$mLBW_births * df$pop_size / df$Statistical_Population
    df$num_mStillbirths = df$MiP_stillbirths * df$pop_size / df$Statistical_Population
    # U5 metrics - rescaled to full population
    df$pop_size_U5 = df$pop_size * (df$Pop_U5 / df$Statistical_Population)  # assumes fraction of individual U5 in simulation is same as fraction in full population
    df$positives_U5 = df$PfPR_U5 * df$pop_size_U5
    df$cases_U5 = df$New_clinical_cases_U5 * df$pop_size_U5 / df$Pop_U5
    df$deaths_1_U5 = df$total_mortality_U5_1 * df$pop_size_U5 / df$Pop_U5
    df$deaths_2_U5 = df$total_mortality_U5_2 * df$pop_size_U5 / df$Pop_U5
    
    df_aggregated = df %>% group_by(Run_Number) %>%
      dplyr::summarize(pop_all_sum = sum(pop_size),
                pop_U5_sum = sum(pop_size_U5),
                cases_all_sum = sum(cases_all_ages),
                cases_U5_sum = sum(cases_U5),
                positives_all_sum = sum(positives_all_ages),
                positives_U5_sum = sum(positives_U5),
                deaths_1_all_sum = sum(deaths_1_all_ages),
                deaths_2_all_sum = sum(deaths_2_all_ages),
                deaths_1_U5_sum = sum(deaths_1_U5),
                deaths_2_U5_sum = sum(deaths_2_U5),
                mLBW_sum = sum(num_mLBW),
                mStill_sum = sum(num_mStillbirths),
                num_values_grouped = n())
    
    
    # clinical incidence (annual): sum of number of cases over all months / (pop size) / (number of years) * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months /  number of months) / (number of months / 12)  * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months / 12)  * 1000
    df_aggregated$incidence_all = (df_aggregated$cases_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$incidence_U5 = (df_aggregated$cases_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_1_all = (df_aggregated$deaths_1_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$death_rate_2_all = (df_aggregated$deaths_2_all_sum / (df_aggregated$pop_all_sum / 12) * 1000)
    df_aggregated$death_rate_mean_all = (df_aggregated$death_rate_1_all + df_aggregated$death_rate_2_all) / 2
    df_aggregated$death_rate_1_U5 = (df_aggregated$deaths_1_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_2_U5 = (df_aggregated$deaths_2_U5_sum / (df_aggregated$pop_U5_sum / 12) * 1000)
    df_aggregated$death_rate_mean_U5 = (df_aggregated$death_rate_1_U5 + df_aggregated$death_rate_2_U5) / 2
    df_aggregated$average_PfPR_all = (df_aggregated$positives_all_sum  / (df_aggregated$pop_all_sum))
    df_aggregated$average_PfPR_U5 = (df_aggregated$positives_U5_sum  / (df_aggregated$pop_U5_sum))
    df_aggregated$annual_num_mLBW = (df_aggregated$mLBW_sum / (end_year - start_year + 1))
    df_aggregated$annual_num_mStill = (df_aggregated$mStill_sum / (end_year - start_year + 1))
    
    
    df_aggregated = df_aggregated[,which(colnames(df_aggregated) %in% c('Run_Number','cases_all_sum','cases_U5_sum', 'deaths_1_all_sum', 'deaths_2_all_sum', 'deaths_1_U5_sum', 'deaths_2_U5_sum', 'mLBW_sum', 'mStill_sum',
                                                                        'incidence_all', 'incidence_U5', 'death_rate_1_all', 'death_rate_2_all', 'death_rate_1_U5', 'death_rate_2_U5', 'annual_num_mLBW', 'annual_num_mStill', 
                                                                        'death_rate_mean_all', 'death_rate_mean_U5', 'average_PfPR_all','average_PfPR_U5'))]
    write.csv(df_aggregated, output_filename, row.names=FALSE)
  }
  return(df_aggregated)
}




get_relative_burden = function(sim_output_filepath, reference_experiment_name, comparison_experiment_name, comparison_scenario_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  reference_df = get_cumulative_burden(sim_output_filepath=sim_output_filepath, experiment_name=reference_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  comparison_df = get_cumulative_burden(sim_output_filepath=sim_output_filepath, experiment_name=comparison_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # align seeds
  reference_df = reference_df[order(reference_df$Run_Number),]
  comparison_df = comparison_df[order(comparison_df$Run_Number),]
  
  relative_burden_df = data.frame('Run_Number' = reference_df$Run_Number)
  # iterate through burden indicators, calculating relative burden and adding to dataframe
  burden_indicators = colnames(reference_df)[-which(colnames(reference_df) == 'Run_Number')]
  for(bb in 1:length(burden_indicators)){
    relative_burden_cur = (comparison_df[[burden_indicators[bb]]] - reference_df[[burden_indicators[bb]]]) / reference_df[[burden_indicators[bb]]]
    relative_burden_df[[burden_indicators[bb]]] = relative_burden_cur
  }
  relative_burden_df$scenario = comparison_scenario_name
  return(relative_burden_df)
}








plot_relative_burden_barplots = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                         barplot_start_year, barplot_end_year, 
                                         pyr, chw_cov,
                                         scenario_names, experiment_names, scenario_palette, LLIN2y_flag=FALSE, overwrite_files=FALSE){
  admin_pop = read.csv(pop_filepath)
  
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
  
  # get minimum and maximum reductions - these will be used if they are smaller / greater than the current min/max
  standard_min_x = -0.5
  standard_max_x = 0.1
  cur_min = min(relative_burden_all_df[,2:(1+length(burden_colnames))])
  cur_max = max(relative_burden_all_df[,2:(1+length(burden_colnames))])
  if(cur_min < standard_min_x) standard_min_x = cur_min
  if(cur_max > standard_max_x) standard_max_x = cur_max
  
  gg_list = list()
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
    
    gg_list[[bb]] = ggplot(rel_burden_agg) + 
      geom_bar(aes(x=scenario, y=mean_rel, fill=scenario), stat='identity') +
      geom_errorbar(aes(x=scenario, ymin=min_rel, ymax=max_rel), width=0.4, colour="black", alpha=0.9, size=1) + 
      ylim(standard_min_x, standard_max_x) + 
      ylab('(GF - BAU) / BAU') + 
      geom_hline(yintercept=0, color='black') +
      ggtitle(burden_metric_name) +
      scale_fill_manual(values = scenario_palette) + 
      theme_classic()+ 
      theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size), 
            axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),axis.line.x=element_blank(),
            plot.margin=unit(c(0,1,1,0), 'cm'))
  }
  # for each burden type, 
  # get mean, min, and max among all runs for each burden metric, each saved as a separate column
  # create barplot for each burden type (using columns of dataframe, separate bar for each scenario)
  
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:(length(burden_colnames)+1)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + theme(text = element_text(size = text_size))   
  }

  if(save_plots){
    gg_saved = grid.arrange(grobs = gg_list[-1], layout_matrix = matrix(c(1:(length(burden_colnames))), nrow=2, byrow=FALSE))
    ggsave(paste0(sim_future_output_dir, '/_plots/barplot_relative_burden_', pyr, '_', chw_cov, 'CHW_',district_subset,'.png'), gg_saved, dpi=600, width=14, height=7, units='in')
  }

  # ----- combine all burden plots ----- #
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(c(1,1,2:(length(burden_colnames)+1)), ncol=2, byrow=TRUE))
  
  return(gg)
}













#####################################################################
# plot maps of burden with and without the intervention
#####################################################################
plot_burden_maps = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins,
                            barplot_start_year, barplot_end_year,
                            pyr, chw_cov,
                            scenario_names, experiment_names, admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', LLIN2y_flag=FALSE){

  
  admin_pop = read.csv(pop_filepath)
  if(!(cur_admins[1] == 'all')){
    admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),]
  }
  admin_shapefile = shapefile(admin_shapefile_filepath)
  
  years_included = barplot_end_year - barplot_start_year + 1
  
  # burden metrics
  # burden_colnames_for_map = c('average_PfPR_U5', 'average_PfPR_all', 'incidence_U5', 'incidence_all', 'death_rate_mean_U5', 'death_rate_mean_all')
  # burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)', 'mortality (U5)', 'mortality (all ages)')
  burden_colnames_for_map = c('pfpr_all', 'incidence_all', 'mortality_rate_all')
  burden_metric_names = c('PfPR (all ages)', 'incidence (all ages)', 'mortality (all)')
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #   iterate through scenarios, creating dataframe including all burden metrics
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  num_scenarios = length(experiment_names)
  burden_df_all = data.frame()
  for(ee in 1:num_scenarios){
    experiment_name = experiment_names[ee]
    cur_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop, comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, cur_admins=cur_admins)
    cur_burden_df$scenario_name = scenario_names[ee]
    if(nrow(burden_df_all) == 0){
      burden_df_all = cur_burden_df
    } else{
      burden_df_all = rbind(burden_df_all, cur_burden_df)
    }
  }

  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #      create maps showing each burden metric for all scenarios
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  if(LLIN2y_flag){
    llin2y_string = '_2yLLIN'
  } else{
    llin2y_string = ''
  }
  num_colors = 40
  colorscale = colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_colors)


  
  # iterate through burden metrics, creating plots for each
  for(cc in 1:length(burden_colnames_for_map)){
    
    if(save_plots) png(paste0(sim_future_output_dir, '/_plots/map_', burden_colnames_for_map[cc], '_', pyr, '_', chw_cov, 'CHW_', district_subset, llin2y_string, '.png'), res=600, width=(num_scenarios*3+2)*3/4, height=3, units='in')
    par(mar=c(0,1,2,0))
    # set layout for panel of maps
    layout_matrix = matrix(rep(c(rep(1:num_scenarios, each=3),rep((num_scenarios+1),2)),2), nrow=2, byrow=TRUE)
    layout(mat = layout_matrix)

    cur_colname = burden_colnames_for_map[cc]
    min_value = min(burden_df_all[[cur_colname]], na.rm=TRUE)
    max_value = max(burden_df_all[[cur_colname]], na.rm=TRUE)
    
    # iterate through scenarios
    for(ee in 1:num_scenarios){
      cur_burden_df = burden_df_all[burden_df_all$scenario_name == scenario_names[ee],]
      vals_ordered = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
      for (i_ds in 1:length(vals_ordered$ds_ordered)){
        cur_ds = vals_ordered$ds_ordered[i_ds]
        if(toupper(cur_ds) %in% toupper(cur_burden_df$admin_name)){
          vals_ordered$value[i_ds] = cur_burden_df[which(toupper(cur_burden_df$admin_name) == toupper(cur_ds)), cur_colname]
        }
      }
  
      col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered$value - min_value) / (max_value - min_value))+1, min, num_colors)]
      col_cur[is.na(col_cur)] = 'grey'
      plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=scenario_names[ee])
    }
    # legend
    legend_label_vals = seq(min_value, max_value, length.out=5)
    legend_image = as.raster(matrix(rev(colorscale[sapply(floor((num_colors)*(legend_label_vals - min_value) / (max_value - min_value))+1, min, num_colors)]), ncol=1))
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = burden_metric_names[cc])
    text(x=1.5, y = seq(0,1,length.out=5), labels = round(legend_label_vals,2))
    rasterImage(legend_image, 0, 0, 1,1)
    # fourth blank plot
    # plot(NA, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ylab='', xlab='')
    par(mfrow=c(1,1), mar=c(5,4,4,2))
    if(save_plots) dev.off()    
  }

  
   
  
}




