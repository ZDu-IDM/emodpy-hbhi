#  plot_PBO_impact.R

# show the difference between the BAU scenario without PBO nets and with PBO nets. Results subset to only the districts that are scheduled to receive either PBO or IG2 nets

library(raster)
library(rgdal)

text_size = 15
save_plots = TRUE

source(paste0(script_hbhi_dir, '/simulation/plot_results_analyses/optimize_LLIN_IRS/optimization_functions.R'))
source(paste0(script_hbhi_dir, '/simulation/plot_results_analyses/compare_seedwise_scenario_burden_functions.R'))  # for get_relative_burden() and get_cumulative_burden()

plot_relative_burden_intervention_barplots = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                         barplot_start_year, barplot_end_year, 
                                         pyr, chw_cov,
                                         experiment_names, inter_name='PBO', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  admin_pop = read.csv(pop_filepath)
  
  # burden metrics
  burden_colnames = c('average_PfPR_U5', 'average_PfPR_all', 'incidence_U5', 'incidence_all')#, 'death_rate_mean_U5', 'death_rate_mean_all')
  burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)')#, 'mortality (U5)', 'mortality (all ages)')
  
  # first experiment is without PBO/IRS, second experiment is with PBO/IRS
  comparison_experiment_name = experiment_names[1]
  reference_experiment_name = experiment_names[2]
  # calculating the burden reduction of all metrics relative to no PBO/IRS (seedwise comparisons, so one output for each run). 
  relative_burden_df = get_relative_burden(sim_output_filepath=sim_future_output_dir, reference_experiment_name=reference_experiment_name, comparison_experiment_name=comparison_experiment_name, comparison_scenario_name='removePBO', start_year=barplot_start_year, end_year=barplot_end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # only save relevant columns for plotting
  relative_burden_df = relative_burden_df[,which(colnames(relative_burden_df) %in% c('scenario', 'Run_Number', burden_colnames))]

  # get minimum and maximum reductions - these will be used if they are smaller / greater than the current min/max
  standard_min_x = -0.001
  standard_max_x = 0.2
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
    geom_errorbar(aes(x=burden_metric, ymin=min_rel, ymax=max_rel), width=0.4, colour="black", alpha=0.9, size=1) +
    ylim(standard_min_x, standard_max_x) + 
    ylab(paste0('(GF without ', inter_name, ' - GF) / GF')) + 
    geom_hline(yintercept=0, color='black') +
    ggtitle(paste0('Comparison of burden in GF ', inter_name, ' districts')) + 
    theme_classic()+ 
    theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size), 
          axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          plot.margin=unit(c(0,1,1,0), 'cm'))

  if(save_plots){
    ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', inter_name, '_relative_burden_', pyr, '_', chw_cov, 'CHW.png'), gg, dpi=600, width=5.2, height=5.2, units='in')
  }
  
  return(gg)
}



plot_relative_burden_intervention_barplots_reversed = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                                      barplot_start_year, barplot_end_year, 
                                                      pyr, chw_cov,
                                                      experiment_names, inter_name='PBO', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  admin_pop = read.csv(pop_filepath)
  
  # burden metrics
  burden_colnames = c('average_PfPR_U5', 'average_PfPR_all', 'incidence_U5', 'incidence_all')#, 'death_rate_mean_U5', 'death_rate_mean_all')
  burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)')#, 'mortality (U5)', 'mortality (all ages)')
  
  # first experiment is with PBO/IRS, second experiment is without PBO/IRS
  comparison_experiment_name = experiment_names[2]
  reference_experiment_name = experiment_names[1]
  # calculating the burden reduction of all metrics relative to no PBO/IRS (seedwise comparisons, so one output for each run). 
  relative_burden_df = get_relative_burden(sim_output_filepath=sim_future_output_dir, reference_experiment_name=reference_experiment_name, comparison_experiment_name=comparison_experiment_name, comparison_scenario_name='removePBO', start_year=barplot_start_year, end_year=barplot_end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  # only save relevant columns for plotting
  relative_burden_df = relative_burden_df[,which(colnames(relative_burden_df) %in% c('scenario', 'Run_Number', burden_colnames))]
  
  # get minimum and maximum reductions - these will be used if they are smaller / greater than the current min/max
  standard_min_x = -0.2
  standard_max_x = 0.001
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
    geom_errorbar(aes(x=burden_metric, ymin=min_rel, ymax=max_rel), width=0.4, colour="black", alpha=0.9, size=1) +
    ylim(standard_min_x, standard_max_x) + 
    ylab(paste0('(GF - GF without ', inter_name, ') / GF without ', inter_name)) + 
    geom_hline(yintercept=0, color='black') +
    ggtitle(paste0('Comparison of burden in GF ', inter_name, ' districts')) + 
    theme_classic()+ 
    theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), text = element_text(size = text_size), legend.text=element_text(size = text_size), 
          axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.line.x=element_blank(),
          plot.margin=unit(c(0,1,1,0), 'cm'))
  
  if(save_plots){
    ggsave(paste0(sim_future_output_dir, '/_plots/barplot_', inter_name, '_relative_burden_reversed_', pyr, '_', chw_cov, 'CHW.png'), gg, dpi=600, width=5.2, height=5.2, units='in')
    print(paste0('plot saved here: ', sim_future_output_dir, '/_plots/barplot_', inter_name, '_relative_burden_reversed_', pyr, '_', chw_cov, 'CHW.png'))
  }
  # print(paste0(burden_metric_names[2], ' relative difference: ', rel_burden_agg[2,2]))
  # print(paste0(burden_metric_names[4], ' relative difference: ', rel_burden_agg[4,2]))
  print( rel_burden_agg)
  
  
  return(gg)
}












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


#####################################################################
# plot maps of burden with and without the intervention
#####################################################################
plot_burden_maps_with_without_inter = function(sim_future_output_dir, pop_filepath, cur_admins,
                                 barplot_start_year, barplot_end_year,
                                 experiment_names, admin_shapefile_filepath, shapefile_admin_colname='NOMDEP', inter_name='PBO'){

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
  noPBO_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, cur_admins=cur_admins)
  # with intervention burden df
  experiment_name = experiment_names[2]
  option_name = experiment_names_descriptions[2]
  PBO_burden_df = get_total_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year, cur_admins=cur_admins)

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






# # plot population sizes in each admin
# pop_ordered = data.frame('ds_ordered'=admin_shapefile[[shapefile_admin_colname]], 'value'=rep(NA, length(admin_shapefile[[shapefile_admin_colname]])))
# for (i_ds in 1:length(pop_ordered$ds_ordered)){
#   cur_ds = pop_ordered$ds_ordered[i_ds]
#   pop_ordered$value[i_ds] = admin_pop$pop_size[which(toupper(admin_pop$admin_name) == toupper(cur_ds))]
# }
# num_colors=40
# min_value = min(c(pop_ordered$value), na.rm=TRUE)
# max_value = max(c(pop_ordered$value), na.rm=TRUE)
# colorscale = colorRampPalette(brewer.pal(9, 'BuGn'))(num_colors)
# col_cur = colorscale[sapply(floor((num_colors)*(pop_ordered$value - min_value) / (max_value - min_value))+1, min, num_colors)]
# col_cur[is.na(col_cur)] = 'grey'
# plot(admin_shapefile, col=col_cur, border=rgb(0.3,0.3,0.3))
# 
# # legend
# legend_label_vals = seq(min_value/1000, max_value/1000, length.out=5)
# legend_image = as.raster(matrix(rev(colorscale[sapply(floor((num_colors)*(legend_label_vals - min_value) / (max_value - min_value))+1, min, num_colors)]), ncol=1))
# plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = c('population size', '(thousands)'))
# text(x=1.5, y = seq(0,1,length.out=5), labels = round(legend_label_vals))
# rasterImage(legend_image, 0, 0, 1,1)





