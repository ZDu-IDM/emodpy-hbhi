# plot_counterfactual_timeseries.R

hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
script_base_filepath = paste0('C:/Users/moniqueam/Documents/malaria-sle-hbhi')

seed_subset = 'all'
LLIN2y_flag = FALSE
district_subset_name = 'all health districts'
plot_by_month = FALSE
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
min_year=2012
max_year=2021
overwrite_files=FALSE
noInterName='noInter'

source(paste0(script_base_filepath, '/bdi_shiny/scripts/process_sim_output_functions.R'))
  
  

if(seed_subset == 'all'){
  sim_output_dir = paste0(hbhi_dir, '/simulation_output/simulations_to_present/_counterfactuals')
# } else if(seed_subset == 'more sensitive'){
#   sim_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLog')
# } else if(seed_subset == 'less sensitive'){
#   sim_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLogLog')
}


experiment_names = c(paste0('BDI_2010_2020_CM_IPTp_ITNr_ITNm_IRS'),
                     paste0('BDI_2010_2020_CM_IPTp_ITNr_ITNm'),
                     paste0('BDI_2010_2020_CM_IPTp_ITNr'),
                     paste0('BDI_2010_2020_CM_IPTp'),
                     paste0('BDI_2010_2020_CM'),
                     paste0('BDI_2010_2020_noInter'),
                     paste0('BDI_2010_2020_IRS'),
                     paste0('BDI_2010_2020_ITNm_IRS'),
                     paste0('BDI_2010_2020_ITNr_ITNm_IRS'),
                     paste0('BDI_2010_2020_IPTp_ITNr_ITNm_IRS')
                    )
if(LLIN2y_flag){
  experiment_names = paste0(experiment_names, '_2yLLIN')
}
scenario_filepaths = paste0(sim_output_dir, '/', experiment_names)
scenario_names = gsub('BDI_2010_2020_', '', experiment_names)
# set colors for each scenario
scenario_palette = c('add IRS'=rgb(0.1,0.9,0.4), 
                     'add ITNm'=rgb(1,0.6,1), 
                     'add ITNr'=rgb(0.5,0.0,0.1), 
                     'add IPTp'=rgb(0.9,0.7,0.3), 
                     'add CM'=rgb(0.0,0.4,1), 
                     'no interventions'=rgb(0,0,0)
                     )


if(district_subset_name == 'all health districts'){
  cur_admins = 'all'
  district_subset = 'districtsAll'
} else if(input$district_subset_name1 == 'only IRS districts'){
  cur_admins = irs_districts
  district_subset = 'districtsIRS'
} else if(input$district_subset_name1 == 'only districts that receive community LLIN distribution'){
  cur_admins = comm_llin_districts
  district_subset = 'districtsCommLLIN'
} else if(input$district_subset_name1 == 'only districts receiving PBO/IG2 in GF plan'){
  cur_admins = pbo_gf_districts
  district_subset = 'districtsPBO'
} else if(input$district_subset_name1 == 'DistrictsOther'){
  cur_admins = other_districts
  district_subset = 'DistrictsOther'
} else{
  warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
  cur_admins = 'all'
  district_subset = 'districtsAll'
}
  




######################################################################
# create plot panel with all burden metrics, no intervention info
######################################################################

plot_simulation_output_burden_all = function(sim_output_dir, pop_filepath, district_subset, cur_admins, 
                                             plot_by_month, min_year, max_year, 
                                             scenario_filepaths, scenario_names, experiment_names, scenario_palette, 
                                             LLIN2y_flag=FALSE, overwrite_files=FALSE, noInterName='noInter'){
  
  
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
  if(!dir.exists(paste0(sim_output_dir, '/_plots'))) dir.create(paste0(sim_output_dir, '/_plots'))
  if(!dir.exists(paste0(sim_output_dir, '/_plots/timeseries_dfs'))) dir.create(paste0(sim_output_dir, '/_plots/timeseries_dfs'))
  if(plot_by_month){
    time_string = 'monthly'
  } else time_string = 'annual'
  
  
  # ----- malaria burden ----- #
  burden_metrics = c('PfPR', 'PfPR', 'incidence', 'incidence', 'mortality', 'mortality')
  burden_metric_names = c('PfPR (U5)', 'PfPR (all ages)', 'incidence (U5)', 'incidence (all ages)', 'mortality (U5)', 'mortality (all ages)')
  burden_colnames = c('PfPR_U5', 'PfPR_MiP_adjusted', 'New_clinical_cases_U5', 'New_Clinical_Cases', 'total_mortality_U5_1', 'total_mortality_1')
  
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
    burden_df_filepath = paste0(sim_output_dir, '/_plots/timeseries_dfs/df_burden_',time_string,'Timeseries_', burden_metric, '_', age_plotted, '_',district_subset, llin2y_string,'.csv')
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
      
      write.csv(burden_df, burden_df_filepath, row.names=FALSE)
    }
    
    
    
    
    ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
    # create plots of timeseries (burden and burden averted)
    ### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ###
    
    # figure out plotting order looking at scenario names
    single_inter = scenario_names[!grepl('_', scenario_names)]
    single_inter = single_inter[single_inter != noInterName]

    if(length(single_inter) %in% c(1,2)){
      plot_twice = TRUE
      sorted_inter_list = list()
      for(ii in 1:length(single_inter)){
        # get ordered vector for plotting for the ordering starting with this intervention
        ss = scenario_names[scenario_names != noInterName]
        ss = ss[grepl(single_inter[ii], ss)]
        sorted_inters = rev(ss[order(nchar(ss))])
        sorted_inters[length(sorted_inters)+1] = noInterName
        sorted_inter_list[[ii]] = sorted_inters
      }
    }else{
      warning(paste0(length(single_inter), ' scenarios with a single intervention detected. Only one or two orders for adding interventions is currently supported.'))
    }
    
    for(ii in 1:length(sorted_inter_list)){
      burden_df_cur = burden_df[burden_df$scenario %in% sorted_inter_list[[ii]],]
      burden_df_cur$added_inter_name = 'no interventions'
      
      # burden averted relative to no interventions
      burden_df_no_inter = burden_df_cur[burden_df_cur$scenario == noInterName,]
      burden_df_no_inter$no_inter_baseline = burden_df_no_inter$mean_burden
      burden_df_no_inter = burden_df_no_inter[,c('year', 'no_inter_baseline')]
      burden_df_cur = merge(burden_df_cur, burden_df_no_inter, by='year')
      burden_df_cur$burden_averted = burden_df_cur$no_inter_baseline - burden_df_cur$mean_burden
      burden_df_cur$max_burden_averted = NA
      burden_df_cur$min_burden_averted = NA
      burden_df_cur$percent_reduction = (burden_df_cur$no_inter_baseline - burden_df_cur$mean_burden)/burden_df_cur$no_inter_baseline * 100
      burden_df_cur$max_percent_reduction = NA
      burden_df_cur$min_percent_reduction = NA
      
      for(inter in 1:(length(sorted_inter_list[[ii]])-1)){
        burden_df_cur$added_inter_name[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = paste0('add ', gsub('_', '', gsub(sorted_inter_list[[ii]][inter+1], '', sorted_inter_list[[ii]][inter])))
        # set minimum and maximum of shaded area (size of impact): minimum is simulation mean and maximum is the burden without that intervention
        burden_df_cur$max_burden[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$mean_burden[burden_df_cur$scenario == sorted_inter_list[[ii]][inter+1]]
        burden_df_cur$min_burden[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$mean_burden[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]]
        
        # set minimum and maximum of shaded area for burden averted by each intervention
        burden_df_cur$max_burden_averted[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$burden_averted[burden_df_cur$scenario == sorted_inter_list[[ii]][inter+1]]
        burden_df_cur$min_burden_averted[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$burden_averted[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]]
        
        # set minimum and maximum of shaded area for percent of burden averted by each intervention
        burden_df_cur$max_percent_reduction[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$percent_reduction[burden_df_cur$scenario == sorted_inter_list[[ii]][inter+1]]
        burden_df_cur$min_percent_reduction[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]] = burden_df_cur$percent_reduction[burden_df_cur$scenario == sorted_inter_list[[ii]][inter]]
      }
      # remove the min/max bounds for the no-intervention scenario (no shaded region on plot)
      burden_df_cur$max_burden[burden_df_cur$scenario == noInterName] = NA
      burden_df_cur$min_burden[burden_df_cur$scenario == noInterName] = NA
      

      
      # ----- create plots of burden timeseries with different intervention combinations ----- #
      
      if(plot_by_month){
        gg = ggplot(burden_df_cur, aes(x=as.Date(date), y=mean_burden, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=scenario), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('date') + 
          ylab(burden_metric_name) + 
          ylim(0,NA) +
          xlim(as.Date(paste0(min_year, '-01-01')), as.Date(paste0(max_year, '-01-01'))) +
          theme_classic()+ 
          theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
        
        
      } else{
        gg = ggplot(burden_df_cur, aes(x=year, y=mean_burden, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=added_inter_name), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('year') + 
          ylab(burden_metric_name) + 
          ylim(0,NA) +
          theme_classic()+ 
          theme(legend.position = "right", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
      }
      ggsave(filename=paste0(sim_output_dir, '/_plots/timeseries_counterfactuals_', burden_colname, '_', time_string, '_order', ii, '.png'), plot=gg, width=7, height=4, units='in', dpi=900)
      
      if((bb==1) & (ii==2)){
        num_scen = length(sorted_inter_list[[ii]])
        for(jj in 1:num_scen){
          burden_df_build = burden_df_cur[burden_df_cur$scenario %in% sorted_inter_list[[ii]][(num_scen-jj+1):num_scen],]
          gg = ggplot(burden_df_build, aes(x=year, y=mean_burden, color=added_inter_name)) +
            geom_ribbon(aes(ymin=min_burden, ymax=max_burden, fill=added_inter_name), alpha=0.1, color=NA)+
            scale_fill_manual(values = scenario_palette) + 
            geom_line(size=1) + 
            scale_color_manual(values = scenario_palette) + 
            xlab('year') + 
            ylab(burden_metric_name) + 
            ylim(0,0.38) +
            theme_classic()+ 
            theme(legend.position = "right", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
          ggsave(filename=paste0(sim_output_dir, '/_plots/timeseries_counterfactuals_', burden_colname, '_', time_string, '_order', ii, '_build', jj, '.png'), plot=gg, width=7, height=4, units='in', dpi=900)
          
        } 
        
        # calculate fraction of total burden reduction attributable to each intervention for the final simulation year
        final_year = max(burden_df_cur$year, na.rm=TRUE)
        burden_df_yy = burden_df_cur[burden_df_cur$year == final_year,]
        total_reduction = max(burden_df_yy$mean_burden, na.rm=TRUE) - min(burden_df_yy$min_burden, na.rm=TRUE)
        for(jj in 1:num_scen){
          xx = burden_df_yy$max_burden[burden_df_yy$scenario == sorted_inter_list[[ii]][jj]] - burden_df_yy$min_burden[burden_df_yy$scenario == sorted_inter_list[[ii]][jj]]
          print(paste0('In year ', final_year, ', ', round(xx/total_reduction, 4), ' of the total reduction was attributable to ', burden_df_yy$added_inter_name[burden_df_yy$scenario == sorted_inter_list[[ii]][jj]]))
        }
      }
      
    
    
      
      
      
      # ----- create timeseries plots with burden averted by each intervention ----- #
      
      if(plot_by_month){
        gg = ggplot(burden_df_cur, aes(x=as.Date(date), y=burden_averted, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_burden_averted, ymax=max_burden_averted, fill=scenario), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('date') + 
          ylab(paste0(burden_metric_name, ' averted')) + 
          xlim(as.Date(paste0(min_year, '-01-01')), as.Date(paste0(max_year, '-01-01'))) +
          theme_classic()+ 
          theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
        
        
      } else{
        gg = ggplot(burden_df_cur, aes(x=year, y=burden_averted, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_burden_averted, ymax=max_burden_averted, fill=added_inter_name), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('year') + 
          ylab(paste0(burden_metric_name, ' averted')) + 
          theme_classic()+ 
          theme(legend.position = "right", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
      }
      ggsave(filename=paste0(sim_output_dir, '/_plots/timeseries_burden_averted_counterfactuals_', burden_colname, '_', time_string, '_order', ii, '.png'), plot=gg, width=7, height=4, units='in', dpi=900)
      
      
      
      # ----- create timeseries plots with percent burden averted by each intervention ----- #
      
      if(plot_by_month){
        gg = ggplot(burden_df_cur, aes(x=as.Date(date), y=percent_reduction, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_percent_reduction, ymax=max_percent_reduction, fill=scenario), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('date') + 
          ylab(paste0('percent reduction in ', burden_metric_name, ' relative to no interventions')) + 
          xlim(as.Date(paste0(min_year, '-01-01')), as.Date(paste0(max_year, '-01-01'))) +
          theme_classic()+ 
          theme(legend.position = "top", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
        
        
      } else{
        gg = ggplot(burden_df_cur, aes(x=year, y=percent_reduction, color=added_inter_name)) +
          geom_ribbon(aes(ymin=min_percent_reduction, ymax=max_percent_reduction, fill=added_inter_name), alpha=0.1, color=NA)+
          scale_fill_manual(values = scenario_palette) + 
          geom_line(size=1) + 
          scale_color_manual(values = scenario_palette) + 
          xlab('year') + 
          ylab(paste0('percent reduction in ', burden_metric_name, '\n relative to no interventions')) + 
          theme_classic()+ 
          theme(legend.position = "right", legend.box='horizontal', legend.title = element_blank(), legend.text=element_text(size = 10))
      }
      ggsave(filename=paste0(sim_output_dir, '/_plots/timeseries_percent_burden_averted_counterfactuals_', burden_colname, '_', time_string, '_order', ii, '.png'), plot=gg, width=7, height=4, units='in', dpi=900)
      

    }
  }
}





