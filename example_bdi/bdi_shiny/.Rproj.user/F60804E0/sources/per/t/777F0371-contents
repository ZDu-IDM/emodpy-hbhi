# plot_sim_output_functions.R

library(rgdal)
library(raster)
library(ggplot2)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(ggpubr)
library(cowplot)
library(tidyverse)
library(sf)
library(reshape2)
library(data.table)
library(dplyr)



text_size = 15
save_plots = FALSE


####################################################################################
# barplots for burden relative to BAU
####################################################################################


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




##################################################################
# barplot of without IPTi versus with IPTi
##################################################################


plot_relative_burden_IPTi_barplots = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                              barplot_start_year, barplot_end_year, 
                                              pyr, chw_cov,
                                              experiment_names, LLIN2y_flag=FALSE, overwrite_files=FALSE){
  admin_pop = read.csv(pop_filepath)
  
  # burden metrics
  burden_colnames = c('average_PfPR_U1', 'incidence_U1', 'death_rate_mean_U1')
  burden_metric_names = c('PfPR (U1)', 'incidence (U1)', 'mortality (U1)')
  
  # first experiment is without IPTi, second experiment is IPTi
  reference_experiment_name = experiment_names[1]
  # calculating the burden reduction of all metrics relative to no IPTi (seedwise comparisons, so one output for each run). 
  comparison_experiment_name = experiment_names[2]
  relative_burden_df = get_relative_U1_burden(sim_output_filepath=sim_future_output_dir, reference_experiment_name=reference_experiment_name, comparison_experiment_name=comparison_experiment_name, start_year=barplot_start_year, end_year=barplot_end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # only save relevant columns for plotting
  relative_burden_df = relative_burden_df[,which(colnames(relative_burden_df) %in% c('scenario', 'Run_Number', burden_colnames))]
  
  # get minimum and maximum reductions - these will be used if they are smaller / greater than the current min/max
  standard_min_x = -0.4
  standard_max_x = 0.1
  cur_min = min(relative_burden_df[,2:(1+length(burden_colnames))])
  cur_max = max(relative_burden_df[,2:(1+length(burden_colnames))])
  if(cur_min < standard_min_x) standard_min_x = cur_min
  if(cur_max > standard_max_x) standard_max_x = cur_max
  
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
  
  rel_burden_agg$burden_metric = factor(rel_burden_agg$burden_metric, levels=burden_metric_names)
  gg = ggplot(rel_burden_agg) + 
    geom_bar(aes(x=burden_metric, y=mean_rel), fill=rgb(0,0.5,0.5), stat='identity') +
    # geom_errorbar(aes(x=burden_metric, ymin=min_rel, ymax=max_rel), width=0.4, colour="black", alpha=0.9, size=1) + 
    ylim(standard_min_x, standard_max_x) + 
    ylab('(with IPTi - without IPTi) / without IPTi') + 
    geom_hline(yintercept=0, color='black') +
    ggtitle('Comparison of burden in proposed IPTi districts') + 
    theme_classic()+ 
    theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size), 
          axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          plot.margin=unit(c(0,1,1,0), 'cm'))
  
  if(save_plots){
    ggsave(paste0(sim_future_output_dir, '/_plots/barplot_IPTi_relative_burden_', pyr, '_', chw_cov, 'CHW.png'), gg, dpi=600, width=4.8, height=4.8, units='in')
  }
  
  return(gg)
}











#####################################################################
# plot maps of burden with and without the intervention
#####################################################################
plot_burden_maps = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins,
                            barplot_start_year, barplot_end_year,
                            pyr, chw_cov,
                            scenario_names, experiment_names, admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', LLIN2y_flag=FALSE,
                            overwrite_files=FALSE){
  
  
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
    cur_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop, comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
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





#####################################################################
# plot maps of burden with and without IPTi
#####################################################################

line2user <- function(line, side) {
  lh <- par('cin')[2] * par('cex') * par('lheight')
  x_off <- diff(grconvertX(0:1, 'inches', 'user'))
  y_off <- diff(grconvertY(0:1, 'inches', 'user'))
  switch(side,
         `1` = par('usr')[3] - line * y_off * lh,
         `2` = par('usr')[1] - line * x_off * lh,
         `3` = par('usr')[4] + line * y_off * lh,
         `4` = par('usr')[2] + line * x_off * lh,
         stop("side must be 1, 2, 3, or 4", call.=FALSE))
}



plot_IPTi_burden_maps = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                 barplot_start_year, barplot_end_year, 
                                 experiment_names, admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', overwrite_files=FALSE){
  
  admin_pop = read.csv(pop_filepath)
  admin_shapefile = shapefile(admin_shapefile_filepath)
  
  years_included = barplot_end_year - barplot_start_year + 1 
  
  # burden metrics
  burden_metric_names = c('PfPR (U1)', 'incidence (U1)', 'mortality (U1)')
  burden_colnames_for_map = c('pfpr_u1', 'incidence_u1', 'mortality_rate_u1')
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #      read in and format malaria burden simulation output in IPTi admins
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  experiment_names_descriptions = c('noIPTi', 'IPTi')
  pfpr_u1_df = data.frame(admin_name=cur_admins)
  deaths_u1_df = data.frame(admin_name=cur_admins)
  clinical_cases_u1_df = data.frame(admin_name=cur_admins)
  pop_u1_df = data.frame(admin_name=admin_pop$admin_name)
  
  # no-IPTi burden df
  experiment_name = experiment_names[1]
  option_name = experiment_names_descriptions[1]
  noIPTi_burden_df = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
  # IPTi burden df
  experiment_name = experiment_names[2]
  option_name = experiment_names_descriptions[2]
  IPTi_burden_df = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
  
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #      create panel of maps showing all burden metrics
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  if(save_plots) png(paste0(sim_future_output_dir, '/_plots/map_IPTi_burden_', pyr, '_', chw_cov, 'CHW.png'), res=600, width=6, height=3*length(burden_metric_names), units='in')
  par(mar=c(0,1,2,0))
  
  num_colors = 40
  colorscale = colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_colors)
  
  # set layout for panel of maps
  base_matrix = matrix(c(1,1,1,2,2,2,3,3, 1,1,1,2,2,2,3,3), nrow=2, byrow=TRUE)
  # add rows for each burden metric
  layout_matrix = base_matrix
  for(cc in 2:length(burden_colnames_for_map)){
    layout_matrix = rbind(layout_matrix, base_matrix + 3*(cc-1))
  }
  # add row for title
  layout_matrix = layout_matrix + 1
  layout_matrix = rbind(rep(1, ncol(layout_matrix)), layout_matrix)
  layout(mat = layout_matrix)
  
  # title
  plot.new()
  text(0.5,0.5,"Malaria burden in each health district",cex=2.5,font=1)
  # text(line2user(line=mean(par('mar')[c(2, 4)]), side=2), 
  #      line2user(line=4, side=3), "Malaria burden in each health district", xpd=NA, cex=2, font=2)
  
  # iterate through burden metrics, creating plots of each
  for(cc in 1:length(burden_colnames_for_map)){
    cur_colname = burden_colnames_for_map[cc]
    vals_ordered_noipti = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
    vals_ordered_ipti = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
    for (i_ds in 1:length(vals_ordered_noipti$ds_ordered)){
      cur_ds = vals_ordered_noipti$ds_ordered[i_ds]
      if(toupper(cur_ds) %in% toupper(noIPTi_burden_df$admin_name)){
        vals_ordered_noipti$value[i_ds] = noIPTi_burden_df[which(toupper(noIPTi_burden_df$admin_name) == toupper(cur_ds)), cur_colname]
        vals_ordered_ipti$value[i_ds] = IPTi_burden_df[which(toupper(IPTi_burden_df$admin_name) == toupper(cur_ds)), cur_colname]
      }
    }
    min_value = min(c(vals_ordered_noipti$value, vals_ordered_ipti$value), na.rm=TRUE)
    max_value = max(c(vals_ordered_noipti$value, vals_ordered_ipti$value), na.rm=TRUE)
    # without IPTi
    col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered_noipti$value - min_value) / (max_value - min_value))+1, min, num_colors)]
    col_cur[is.na(col_cur)] = 'grey'
    plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=paste0(burden_metric_names[cc], ' - without IPTi'))
    # with IPTi
    col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered_ipti$value - min_value) / (max_value - min_value))+1, min, num_colors)]
    col_cur[is.na(col_cur)] = 'grey'
    plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=paste0(burden_metric_names[cc], ' - with IPTi'))
    # legend
    legend_label_vals = seq(min_value, max_value, length.out=5)
    legend_image = as.raster(matrix(rev(colorscale[sapply(floor((num_colors)*(legend_label_vals - min_value) / (max_value - min_value))+1, min, num_colors)]), ncol=1))
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = burden_metric_names[cc])
    text(x=1.5, y = seq(0,1,length.out=5), labels = round(legend_label_vals,2))
    rasterImage(legend_image, 0, 0, 1,1)
    # fourth blank plot
    # plot(NA, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ylab='', xlab='')
    
  }
  par(mfrow=c(1,1), mar=c(5,4,4,2))
  if(save_plots) dev.off()
  
  
}



#####################################################################
# plot maps of burden with and without the intervention
#####################################################################

plot_burden_maps_with_without_inter = function(sim_future_output_dir, pop_filepath, cur_admins, district_subset,
                                               barplot_start_year, barplot_end_year,
                                               experiment_names, admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', inter_name='PBO',
                                               overwrite_files=FALSE){
  
  admin_pop = read.csv(pop_filepath)
  admin_shapefile = shapefile(admin_shapefile_filepath)
  
  years_included = barplot_end_year - barplot_start_year + 1
  
  # burden metrics
  # burden_colnames_for_map = c('average_PfPR_U5', 'average_PfPR_all', 'incidence_U5', 'incidence_all', 'death_rate_mean_U5', 'death_rate_mean_all')
  # burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)', 'mortality (U5)', 'mortality (all ages)')
  burden_colnames_for_map = c('pfpr_all', 'incidence_all', 'mortality_rate_all')
  burden_metric_names = c('PfPR (all ages)', 'incidence (all ages)', 'mortality (all)')
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #      read in and format malaria burden simulation output in specified set of admins
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  experiment_names_descriptions = paste0(c('no', ''), inter_name)
  
  # no-intervention burden df
  experiment_name = experiment_names[1]
  option_name = experiment_names_descriptions[1]
  noPBO_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
  # with intervention burden df
  experiment_name = experiment_names[2]
  option_name = experiment_names_descriptions[2]
  PBO_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, district_subset=district_subset, cur_admins=cur_admins, overwrite_files=overwrite_files)
  
  # # increase in burden in each admin: (without intervention - with intervention)  /  with intervention
  # rel_burd_increase = (noPBO_burden_df$incidence_all - PBO_burden_df$incidence_all ) / PBO_burden_df$incidence_all
  # min(rel_burd_increase)
  # max(rel_burd_increase)
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  #      create panel of maps showing all burden metrics
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  if(save_plots) png(paste0(sim_future_output_dir, '/_plots/map_withWithout', inter_name,'_burden_', pyr, '_', chw_cov, 'CHW.png'), res=600, width=6, height=3*length(burden_metric_names), units='in')
  par(mar=c(0,1,2,0))
  
  num_colors = 40
  colorscale = colorRampPalette(brewer.pal(9, 'YlGnBu'))(num_colors)
  
  # set layout for panel of maps
  base_matrix = matrix(c(1,1,1,2,2,2,3,3, 1,1,1,2,2,2,3,3), nrow=2, byrow=TRUE)
  # add rows for each burden metric
  layout_matrix = base_matrix
  for(cc in 2:length(burden_colnames_for_map)){
    layout_matrix = rbind(layout_matrix, base_matrix + 3*(cc-1))
  }
  # add row for title
  layout_matrix = layout_matrix + 1
  layout_matrix = rbind(rep(1, ncol(layout_matrix)), layout_matrix)
  layout(mat = layout_matrix)
  
  # title
  plot.new()
  text(0.5,0.5,"Malaria burden in each health district",cex=2.5,font=1)
  # text(line2user(line=mean(par('mar')[c(2, 4)]), side=2),
  #      line2user(line=4, side=3), "Malaria burden in each health district", xpd=NA, cex=2, font=2)
  
  # iterate through burden metrics, creating plots of each
  for(cc in 1:length(burden_colnames_for_map)){
    cur_colname = burden_colnames_for_map[cc]
    vals_ordered_noPBO = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
    vals_ordered_PBO = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
    for (i_ds in 1:length(vals_ordered_noPBO$ds_ordered)){
      cur_ds = vals_ordered_noPBO$ds_ordered[i_ds]
      if(toupper(cur_ds) %in% toupper(noPBO_burden_df$admin_name)){
        vals_ordered_noPBO$value[i_ds] = noPBO_burden_df[which(toupper(noPBO_burden_df$admin_name) == toupper(cur_ds)), cur_colname]
        vals_ordered_PBO$value[i_ds] = PBO_burden_df[which(toupper(PBO_burden_df$admin_name) == toupper(cur_ds)), cur_colname]
      }
    }
    min_value = min(c(vals_ordered_noPBO$value, vals_ordered_PBO$value), na.rm=TRUE)
    max_value = max(c(vals_ordered_noPBO$value, vals_ordered_PBO$value), na.rm=TRUE)
    # without intervention
    col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered_noPBO$value - min_value) / (max_value - min_value))+1, min, num_colors)]
    col_cur[is.na(col_cur)] = 'grey'
    plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=paste0(burden_metric_names[cc], ' - without ', inter_name))
    # with intervention
    col_cur = colorscale[sapply(floor((num_colors)*(vals_ordered_PBO$value - min_value) / (max_value - min_value))+1, min, num_colors)]
    col_cur[is.na(col_cur)] = 'grey'
    plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3), main=paste0(burden_metric_names[cc], ' - with ', inter_name))
    # legend
    legend_label_vals = seq(min_value, max_value, length.out=5)
    legend_image = as.raster(matrix(rev(colorscale[sapply(floor((num_colors)*(legend_label_vals - min_value) / (max_value - min_value))+1, min, num_colors)]), ncol=1))
    plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = burden_metric_names[cc])
    text(x=1.5, y = seq(0,1,length.out=5), labels = round(legend_label_vals,2))
    rasterImage(legend_image, 0, 0, 1,1)
    # fourth blank plot
    # plot(NA, ylim=c(0,1), xlim=c(0,1), axes=FALSE, ylab='', xlab='')
    
  }
  par(mfrow=c(1,1), mar=c(5,4,4,2))
  if(save_plots) dev.off()
  
  
}








######################################################################
# create plot panel with all burden metrics, no intervention info
######################################################################

plot_simulation_output_burden_all = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                             plot_by_month, min_year, max_year, sim_end_years, 
                                             pyr, chw_cov,
                                             scenario_filepaths, scenario_names, experiment_names, scenario_palette, LLIN2y_flag=FALSE, overwrite_files=FALSE){
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  # combine simulation output from multiple scenarios
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  pop_sizes = read.csv(pop_filepath)
  pop_sizes = pop_sizes[,c('admin_name','pop_size')]
  # if we include all admins, get list of names from population size dataframe
  if(cur_admins[1] == 'all'){
    cur_admins = unique(pop_sizes$admin_name)
  }
  
  # create output directories
  if(!dir.exists(paste0(sim_future_output_dir, '/_plots'))) dir.create(paste0(sim_future_output_dir, '/_plots'))
  if(!dir.exists(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))) dir.create(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))
  if(plot_by_month){
    time_string = 'monthly'
  } else time_string = 'annual'
  
  
  # ----- malaria burden ----- #
  burden_metrics = c('PfPR', 'PfPR', 'incidence', 'incidence', 'mortality', 'mortality')
  burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)', 'mortality (U5)', 'mortality (all ages)')
  burden_colnames = c('PfPR_U5', 'PfPR_MiP_adjusted', 'New_clinical_cases_U5', 'New_Clinical_Cases', 'total_mortality_U5_1', 'total_mortality_1')
  
  gg_list = list()
  for(bb in 1:length(burden_colnames)){
    burden_metric_name = burden_metric_names[bb]
    burden_colname = burden_colnames[bb]
    burden_metric = burden_metrics[bb]
    
    if(grepl('U5', burden_metric_name)){
      age_plotted = 'U5'
    } else age_plotted = 'all'
    
    
    # check whether burden output already exists for this comparison
    if(LLIN2y_flag){
      llin2y_string = '_2yLLIN'
    } else{
      llin2y_string = ''
    }
    burden_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_burden_',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset, llin2y_string,'.csv')
    if(file.exists(burden_df_filepath) & !overwrite_files){
      burden_df = read.csv(burden_df_filepath)
    } else{
      # iterate through scenarios, storing relevant output
      burden_df = data.frame()
      for(ee in 1:length(scenario_filepaths)){
        cur_sim_output_agg = get_burden_timeseries_exp(exp_filepath = scenario_filepaths[ee],
                                                       exp_name = scenario_names[ee], 
                                                       cur_admins=cur_admins, pop_sizes = pop_sizes, min_year=min_year, max_year=max_year, burden_colname=burden_colname, age_plotted=age_plotted, plot_by_month=plot_by_month)
        if(nrow(burden_df)==0){
          burden_df = cur_sim_output_agg
        } else{
          burden_df = rbind(burden_df, cur_sim_output_agg)
        }
      }
      
      # add the final 'to-present' row to all future simulations for a continuous plot
      to_present_df = burden_df[burden_df$scenario == 'to-present',]
      if(plot_by_month){
        final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
        for(ss in 2:length(scenario_names)){
          final_to_present_row$scenario = scenario_names[ss]
          burden_df = rbind(burden_df, final_to_present_row)
        }
      } else{
        final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
        for(ss in 2:length(scenario_names)){
          final_to_present_row$scenario = scenario_names[ss]
          burden_df = rbind(burden_df, final_to_present_row)
        }
      }
      write.csv(burden_df, burden_df_filepath, row.names=FALSE)
    }
    
    
    
    
    ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
    # create scenario-comparison plots
    ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
    
    # get factors in the correct order (rather than alphabetical)
    burden_df$scenario = factor(burden_df$scenario, levels=scenario_names)
    
    # ----- malaria burden ----- #
    
    if(plot_by_month){
      gg_list[[bb]] = ggplot(burden_df, aes(x=as.Date(date), y=mean_burden, color=scenario)) +
        geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
        scale_fill_manual(values = scenario_palette) + 
        geom_line(size=1) + 
        scale_color_manual(values = scenario_palette) + 
        xlab('date') + 
        ylab(burden_metric_name) + 
        xlim(as.Date(paste0(min_year, '-01-01')), as.Date(paste0(max_year, '-01-01'))) +
        theme_classic()+ 
        theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = text_size))
    } else{
      gg_list[[bb]] = ggplot(burden_df, aes(x=year, y=mean_burden, color=scenario)) +
        geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
        scale_fill_manual(values = scenario_palette) + 
        geom_line(size=1) + 
        scale_color_manual(values = scenario_palette) + 
        xlab('year') + 
        ylab(burden_metric_name) + 
        xlim(min_year, max_year) +
        theme_classic()+ 
        theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = text_size))
    }
    
  }
  # gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]])), (ggplot() + theme_void())), gg_list)
  gg_list = append(list(ggpubr::as_ggplot(ggpubr::get_legend(gg_list[[1]]))), gg_list)
  # remove legend from main plots
  for(bb in 2:(length(burden_colnames)+1)){
    gg_list[[bb]] = gg_list[[bb]] + theme(legend.position = "none")  + theme(text = element_text(size = text_size))   
  }
  # ----- combine all burden plots ----- #
  # gg = grid.arrange(grobs = gg_list, ncol = 2)
  gg = grid.arrange(grobs = gg_list, layout_matrix = matrix(c(1,1,2:(length(burden_colnames)+1)), ncol=2, byrow=TRUE))
  return(gg)
}












######################################################################
# create plot panel with selected burden metric and intervention info
######################################################################
# note: plot of ITN use rates through time is for the entire population (always shows all-age, even when burden plot shows U5)

plot_simulation_intervention_output = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                               plot_by_month, min_year, max_year, sim_end_years, 
                                               burden_metric, age_plotted, 
                                               pyr, chw_cov,
                                               scenario_filepaths, scenario_names, scenario_input_references, experiment_names, scenario_palette, 
                                               indoor_protection_fraction=0.75, LLIN2y_flag=FALSE, overwrite_files=FALSE){
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  # combine simulation output from multiple scenarios
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  pop_sizes = read.csv(pop_filepath)
  pop_sizes = pop_sizes[,c('admin_name','pop_size')]
  # if we include all admins, get list of names from population size dataframe
  if(cur_admins[1] == 'all'){
    cur_admins = unique(pop_sizes$admin_name)
  }
  
  # create output directories
  if(!dir.exists(paste0(sim_future_output_dir, '/_plots'))) dir.create(paste0(sim_future_output_dir, '/_plots'))
  if(!dir.exists(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))) dir.create(paste0(sim_future_output_dir, '/_plots/timeseries_dfs'))
  if(plot_by_month){
    time_string = 'monthly'
  } else time_string = 'annual'
  
  
  # ----- malaria burden ----- #
  
  # Get output column name for specified burden metric
  # Note: need to divide by pop size and multiply by 1000 if not PfPR
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
  }  else if(burden_metric == 'mortality'){
    if(age_plotted == 'U5'){
      burden_colname = 'total_mortality_U5_1'
    } else if(age_plotted == 'all'){
      burden_colname = 'total_mortality_1'
    }
  } 
  if(is.na(burden_colname)){
    warning('PROBLEM DETECTED: name of burden metric or age group not currently supported')
  }
  
  
  # check whether burden output already exists for this comparison
  if(LLIN2y_flag){
    llin2y_string = '_2yLLIN'
  } else{
    llin2y_string = ''
  }
  burden_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_burden_',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset, llin2y_string,'.csv')
  if(file.exists(burden_df_filepath) & !overwrite_files){
    burden_df = read.csv(burden_df_filepath)
  } else{
    # iterate through scenarios, storing relevant output
    burden_df = data.frame()
    for(ee in 1:length(scenario_filepaths)){
      cur_sim_output_agg = get_burden_timeseries_exp(exp_filepath = scenario_filepaths[ee],
                                                     exp_name = scenario_names[ee], 
                                                     cur_admins=cur_admins, pop_sizes = pop_sizes, min_year=min_year, max_year=max_year, burden_colname=burden_colname, age_plotted=age_plotted, plot_by_month=plot_by_month)
      if(nrow(burden_df)==0){
        burden_df = cur_sim_output_agg
      } else{
        burden_df = rbind(burden_df, cur_sim_output_agg)
      }
    }
    
    # add the final 'to-present' row to all future simulations for a continuous plot
    to_present_df = burden_df[burden_df$scenario == 'to-present',]
    if(plot_by_month){
      final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        burden_df = rbind(burden_df, final_to_present_row)
      }
    } else{
      final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        burden_df = rbind(burden_df, final_to_present_row)
      }
    }
    write.csv(burden_df, burden_df_filepath, row.names=FALSE)
  }
  
  
  
  
  # ----- LLIN and IRS intervention coverage ----- #
  
  # check whether LLIN/IRS output already exists for this comparison
  llin_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_llin_irs_',time_string,'Timeseries', '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset, llin2y_string,'.csv')
  if(file.exists(llin_df_filepath) & !overwrite_files){
    net_use_df = read.csv(llin_df_filepath)
  } else{
    # iterate through scenarios, storing relevant output
    net_use_df = data.frame()
    for(ee in 1:length(scenario_filepaths)){
      cur_net_agg = get_net_use_timeseries_exp(exp_filepath = scenario_filepaths[ee],
                                               exp_name = scenario_names[ee], 
                                               cur_admins=cur_admins, pop_sizes=pop_sizes, min_year=min_year, max_year=max_year, indoor_protection_fraction=indoor_protection_fraction, plot_by_month=plot_by_month)
      if(nrow(net_use_df)==0){
        net_use_df = cur_net_agg
      } else{
        net_use_df = rbind(net_use_df, cur_net_agg)
      }
    }
    
    # first, remove the final 'to-present' month or year - it should was overwritten in the pick-up from burn-in
    # then, add the final 'to-present' row to all future simulations for a continuous plot
    if(plot_by_month){
      # remove excess month from to-present simulation
      max_to_present_date = max(net_use_df$date[net_use_df$scenario == 'to-present'])
      row_to_remove = intersect(which(net_use_df$scenario == 'to-present'), which(net_use_df$date == max_to_present_date))
      net_use_df = net_use_df[-row_to_remove,]
      
      # join past and future simulation trajectories
      to_present_df = net_use_df[net_use_df$scenario == 'to-present',]
      final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        net_use_df = rbind(net_use_df, final_to_present_row)
      }
    } else{
      # remove excess year from to-present simulation
      max_to_present_date = max(net_use_df$year[net_use_df$scenario == 'to-present'])
      row_to_remove = intersect(which(net_use_df$scenario == 'to-present'), which(net_use_df$year == max_to_present_date))
      net_use_df = net_use_df[-row_to_remove,]
      
      # join past and future simulation trajectories
      to_present_df = net_use_df[net_use_df$scenario == 'to-present',]
      final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        net_use_df = rbind(net_use_df, final_to_present_row)
      }
    }
    write.csv(net_use_df, llin_df_filepath, row.names=FALSE)
  }
  
  
  
  
  # ----- Case management ----- #
  
  # check whether LLIN/IRS output already exists for this comparison
  cm_df_filepath = paste0(sim_future_output_dir, '/_plots/timeseries_dfs/df_cm_',time_string,'Timeseries', '_pyr', pyr, '_', chw_cov, 'CHW_',district_subset, llin2y_string,'.csv')
  if(file.exists(cm_df_filepath)){
    cm_df = read.csv(cm_df_filepath)
  } else{
    # iterate through scenarios, storing input CM coverages
    cm_df = data.frame()
    for(ee in 1:length(scenario_filepaths)){
      intervention_csv_filepath = scenario_input_references[ee]
      intervention_file_info = read.csv(intervention_csv_filepath)
      experiment_intervention_name = experiment_names[ee]
      end_year = sim_end_years[ee]
      cur_int_row = which(intervention_file_info$ScenarioName == experiment_intervention_name)
      # read in intervention files
      cm_filepath = paste0(hbhi_dir, '/simulation_inputs/', intervention_file_info$CM_filename[cur_int_row], '.csv')
      
      cur_cm_agg = get_cm_timeseries_exp(cm_filepath=cm_filepath, pop_sizes=pop_sizes, end_year=end_year, exp_name = scenario_names[ee], 
                                         cur_admins=cur_admins, min_year=min_year, plot_by_month=plot_by_month)
      
      if(nrow(cm_df)==0){
        cm_df = cur_cm_agg
      } else{
        cm_df = rbind(cm_df, cur_cm_agg)
      }
    }
    
    # add the final 'to-present' row to all future simulations for a continuous plot
    if(plot_by_month){
      # join past and future simulation trajectories
      to_present_df = cm_df[cm_df$scenario == 'to-present',]
      final_to_present_row = to_present_df[as.Date(to_present_df$date) == max(as.Date(to_present_df$date)),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        cm_df = rbind(cm_df, final_to_present_row)
      }
    } else{
      # join past and future simulation trajectories
      to_present_df = cm_df[cm_df$scenario == 'to-present',]
      final_to_present_row = to_present_df[to_present_df$year == max(to_present_df$year),]
      for(ss in 2:length(scenario_names)){
        final_to_present_row$scenario = scenario_names[ss]
        cm_df = rbind(cm_df, final_to_present_row)
      }
    }
    write.csv(cm_df, cm_df_filepath, row.names=FALSE)
  }
  
  
  
  
  
  
  
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  # create scenario-comparison plots
  ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
  
  # get factors in the correct order (rather than alphabetical)
  burden_df$scenario = factor(burden_df$scenario, levels=scenario_names)
  
  # ----- malaria burden ----- #
  
  if(plot_by_month){
    g_burden = ggplot(burden_df, aes(x=as.Date(date), y=mean_burden, color=scenario)) +
      geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('date') + 
      ylab(paste0(burden_metric, ' - ', age_plotted)) + 
      theme_classic()+ 
      theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size))
  } else{
    g_burden = ggplot(burden_df, aes(x=year, y=mean_burden, color=scenario)) +
      geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('year') + 
      ylab(paste0(burden_metric, ' - ', age_plotted)) + 
      theme_classic()+ 
      theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size))
  }
  
  
  
  # ----- LLIN use and distribution ----- #
  
  # plot net use through time
  if(plot_by_month){
    g_net_use = ggplot(net_use_df, aes(x=as.Date(date), y=mean_coverage, color=scenario)) +
      geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('date') + 
      ylab(paste0('LLIN use (all ages)')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  } else{
    g_net_use = ggplot(net_use_df, aes(x=year, y=mean_coverage, color=scenario)) +
      geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      # geom_hline(yintercept=0.22, alpha=0.1)+
      # geom_hline(yintercept=0.39, alpha=0.1)+
      xlab('year') + 
      ylab(paste0('LLIN use (all ages)')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  }
  
  # # plot net distribution numbers through time (how many nets distributed in each month or year per person?)
  # if(plot_by_month){
  #   g_net_dist = ggplot(net_use_df, aes(x=as.Date(date), y=new_net_per_cap, color=scenario)) +
  #     geom_point(size=1) + 
  #     scale_color_manual(values = scenario_palette) + 
  #     xlab('date') + 
  #     ylab(paste0('LLINs distributed per person')) + 
  #     theme_classic()+ 
  #     theme(legend.position = "none", text = element_text(size = text_size))
  # } else{
  #   g_net_dist = ggplot(net_use_df, aes(x=year, y=new_net_per_cap, color=scenario)) +
  #     geom_point(size=2) + 
  #     geom_line(alpha=0.2, size=2) +
  #     scale_color_manual(values = scenario_palette) + 
  #     xlab('year') + 
  #     ylab(paste0('LLINs distributed per person')) + 
  #     theme_classic()+ 
  #     theme(legend.position = "none", text = element_text(size = text_size))
  # }
  
  
  
  # ----- IRS ----- #
  
  if(plot_by_month){
    g_irs = ggplot(net_use_df, aes(x=as.Date(date), y=irs_per_cap, color=scenario)) +
      geom_point(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('date') + 
      ylab(paste0('IRS rounds per person')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  } else{
    g_irs = ggplot(net_use_df, aes(x=year, y=irs_per_cap, color=scenario)) +
      geom_point(size=2) + 
      geom_line(alpha=0.2, size=2) +
      scale_color_manual(values = scenario_palette) + 
      xlab('year') + 
      ylab(paste0('IRS per person')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  }
  
  
  
  # ----- Case management ----- #
  
  if(plot_by_month){
    g_cm = ggplot(cm_df, aes(x=as.Date(date), y=mean_coverage, color=scenario)) +
      geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('date') + 
      ylab(paste0('Effective treatment rate (U5)')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  } else{
    g_cm = ggplot(cm_df, aes(x=year, y=mean_coverage, color=scenario)) +
      geom_ribbon(aes(ymin=min_coverage, ymax=max_coverage, fill=scenario), alpha=0.1, color=NA)+
      scale_fill_manual(values = scenario_palette) + 
      geom_line(size=1) + 
      scale_color_manual(values = scenario_palette) + 
      xlab('year') + 
      ylab(paste0('Effective treatment rate (U5)')) + 
      theme_classic()+ 
      theme(legend.position = "none", text = element_text(size = text_size))
  }
  
  
  
  
  # ----- combine burden and intervention plots ----- #
  
  gg_leg = ggpubr::as_ggplot(ggpubr::get_legend(g_burden))
  g_burden = g_burden + theme(legend.position = "none")
  
  
  # include plot with IRS rounds per person (when there are two rounds in the same buildinging, counts it twice)
  gg = plot_grid(gg_leg, g_burden, g_net_use, g_cm, g_irs, ncol=1, nrow=5, align='vh', axis='lrtb')
  
  if(save_plots){
    ggsave(paste0(sim_future_output_dir, '/_plots/',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_versusInterventions_pyr', pyr, '_', chw_cov, 'CHW_',district_subset,'_withIRS.png'), gg, dpi=600, width=7, height=12, units='in')
  }
  return(gg)
}









