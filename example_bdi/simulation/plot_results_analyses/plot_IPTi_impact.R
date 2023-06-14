#  plot_IPTi_impact.R

library(raster)
library(rgdal)

text_size = 15
save_plots = FALSE

# get cumulative burden over specified years (for all malaria metrics, separate values for each seed)
get_cumulative_U1_burden = function(sim_output_filepath, experiment_name, start_year, end_year, admin_pop, district_subset=district_subset, cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
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
  output_filename = paste0(sim_output_filepath, '/', experiment_name, '/cumulativeBurden_IPTi_', start_year, '_', end_year, '_', district_subset, llin2y_string, '.csv')
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
    
    # U1 metrics - rescaled to full population
    df$pop_size_U1 = df$pop_size * (df$Pop_U1 / df$Statistical_Population)  # assumes fraction of individual U1 in simulation is same as fraction in full population
    df$positives_U1 = df$PfPR_U1 * df$pop_size_U1
    df$cases_U1 = df$New_clinical_cases_U1 * df$pop_size_U1 / df$Pop_U1
    df$deaths_1_U1 = df$total_mortality_U1_1 * df$pop_size_U1 / df$Pop_U1
    df$deaths_2_U1 = df$total_mortality_U1_2 * df$pop_size_U1 / df$Pop_U1
    
    df_aggregated = df %>% dplyr::group_by(Run_Number) %>%
      dplyr::summarize(pop_U1_sum = sum(pop_size_U1),
                cases_U1_sum = sum(cases_U1),
                positives_U1_sum = sum(positives_U1),
                deaths_1_U1_sum = sum(deaths_1_U1),
                deaths_2_U1_sum = sum(deaths_2_U1),
                num_values_grouped = n())
    
    
    # clinical incidence (annual): sum of number of cases over all months / (pop size) / (number of years) * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months /  number of months) / (number of months / 12)  * 1000
    #     = sum of number of cases over all months / (sum of pop size over all months / 12)  * 1000
    df_aggregated$incidence_U1 = (df_aggregated$cases_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_1_U1 = (df_aggregated$deaths_1_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_2_U1 = (df_aggregated$deaths_2_U1_sum / (df_aggregated$pop_U1_sum / 12) * 1000)
    df_aggregated$death_rate_mean_U1 = (df_aggregated$death_rate_1_U1 + df_aggregated$death_rate_2_U1) / 2
    df_aggregated$average_PfPR_U1 = (df_aggregated$positives_U1_sum  / (df_aggregated$pop_U1_sum))

    df_aggregated = df_aggregated[,which(colnames(df_aggregated) %in% c('Run_Number','cases_U1_sum', 'deaths_1_U1_sum', 'deaths_2_U1_sum',
                                                                        'incidence_U1', 'death_rate_1_U1', 'death_rate_2_U1', 
                                                                        'death_rate_mean_U1', 'average_PfPR_U1'))]
    write.csv(df_aggregated, output_filename, row.names=FALSE)
  }
  return(df_aggregated)
}




get_relative_U1_burden = function(sim_output_filepath, reference_experiment_name, comparison_experiment_name, start_year, end_year, admin_pop, district_subset='allDistricts', cur_admins='all', LLIN2y_flag=FALSE, overwrite_files=FALSE){
  reference_df = get_cumulative_U1_burden(sim_output_filepath=sim_output_filepath, experiment_name=reference_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
  comparison_df = get_cumulative_U1_burden(sim_output_filepath=sim_output_filepath, experiment_name=comparison_experiment_name, start_year=start_year, end_year=end_year, admin_pop=admin_pop, district_subset=district_subset, cur_admins=cur_admins, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
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
  relative_burden_df$scenario = 'withIPTi'
  return(relative_burden_df)
}








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
plot_IPTi_burden_maps = function(sim_future_output_dir, pop_filepath, cur_admins, 
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
  noIPTi_burden_df = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year)
  # IPTi burden df
  experiment_name = experiment_names[2]
  option_name = experiment_names_descriptions[2]
  IPTi_burden_df = get_total_U1_burden(sim_output_filepath=sim_future_output_dir, experiment_name=experiment_name, admin_pop=admin_pop[which(admin_pop$admin_name %in% cur_admins),], comparison_start_year=barplot_start_year, comparison_end_year=barplot_end_year)
  
  
  
  
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





