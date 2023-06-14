# compare_simOutput_interventionCoverage_functions.R

# functions needed to run Shiny app and to run compare_simOutput_interventionCoverage.R


library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)


####################################################################
# functions for reading and distilling in simulation output
####################################################################

text_size = 15
save_plots = FALSE


# ----- malaria burden ----- #
# function to subset simulation output to appropriate admin and time period, and calculate monthly mean, min, max burdens across all runs
get_burden_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, burden_colname, age_plotted, plot_by_month=TRUE){
  # read in simulation information, subset to appropriate years
  cur_sim_output = fread(paste0(exp_filepath, '/malariaBurden_withAdjustments.csv'))
  cur_sim_output = cur_sim_output[intersect(which(cur_sim_output$year >= min_year), which(cur_sim_output$year <= max_year)),]
  # subset to appropriate admins
  cur_sim_output = cur_sim_output[cur_sim_output$admin_name %in% cur_admins,]  
  
  # merge to get real-world population sizes in each admin
  cur_sim_output = merge(cur_sim_output, pop_sizes, by='admin_name')
  
  # get simulation population denominator
  if(age_plotted == 'U5'){
    cur_sim_output$population = cur_sim_output$Pop_U5
  } else{
    cur_sim_output$population = cur_sim_output$Statistical_Population
  }
  # get the real-world population size in each admin
  if(age_plotted == 'U5'){
    cur_sim_output$true_population = cur_sim_output$pop_size * cur_sim_output$Pop_U5 / cur_sim_output$Statistical_Population
  } else{
    cur_sim_output$true_population = cur_sim_output$pop_size
  }
  
  
  # calculate average over included admins
  if(grepl('PfPR', burden_colname)){
    # get total number of positives (PfPR * true population for all included admins), then divide by total population size of all admins
    cur_sim_output$positives = cur_sim_output[[burden_colname]] * cur_sim_output$true_population
    cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(month, year, date, admin_name, positives, true_population, Run_Number) %>%
      dplyr::group_by(month, year, date, Run_Number) %>%  # take sum of positives and population across admins
      dplyr::summarise(total_positives = sum(positives),
                total_population = sum(true_population))
    cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$total_positives / cur_sim_output_agg_admin$total_population
    if(!plot_by_month){
      cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% dplyr::group_by(year, Run_Number) %>% # take average PfPR across months
        dplyr::summarise(burden = mean(burden))
    }
    
  } else{
    # rescale case (or death) numbers to number present in full admin (with true population instead of simulated population size)
    cur_sim_output$true_burden = cur_sim_output[[burden_colname]] * cur_sim_output$true_population / cur_sim_output$population
    # take sum of the number of cases (or deaths) and population sizes acrsoss all included admins
    select_col_names = c('true_burden', 'month', 'year', 'date', 'true_population', 'Run_Number')
    cur_sim_output_agg_admin = as.data.frame(cur_sim_output) %>% dplyr::select(match(select_col_names, names(.))) %>%
      dplyr::group_by(month, year, date, Run_Number) %>%
      dplyr::summarise_all(sum)
    cur_sim_output_agg_admin$burden = cur_sim_output_agg_admin$true_burden / cur_sim_output_agg_admin$true_population * 1000
    if(!plot_by_month){
      cur_sim_output_agg_admin = cur_sim_output_agg_admin %>% dplyr::group_by(year, Run_Number) %>% # take average PfPR across months
        dplyr::summarise(burden = sum(burden))
    }
  }
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(month, year, date, burden) %>%
      dplyr::group_by(month, year, date) %>%
      dplyr::summarise(mean_burden = mean(burden),
                max_burden = max(burden),
                min_burden = min(burden))
  } else{
    cur_sim_output_agg = as.data.frame(cur_sim_output_agg_admin) %>% dplyr::select(year, burden) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_burden = mean(burden),
                max_burden = max(burden),
                min_burden = min(burden))
  }
  
  cur_sim_output_agg$scenario = exp_name
  return(cur_sim_output_agg)
}





# ----- LLIN and IRS intervention coverage ----- #
# plot ITN use rates through time in entire population (always shows all-age, even when burden plot shows U5)
# function to subset simulation output to appropriate admin(s) and time period, and calculate monthly net usage and net distribution across all runs
get_net_use_timeseries_exp = function(exp_filepath, exp_name, cur_admins, pop_sizes, min_year, max_year, indoor_protection_fraction, plot_by_month=TRUE){
  # read in simulation information, merge to single dataframe, subset to appropriate years
  net_use_all = fread(paste0(exp_filepath, '/MonthlyUsageLLIN.csv'))
  net_dist_all = fread(paste0(exp_filepath, '/monthly_Event_Count.csv'))
  net_dist_all = net_dist_all[,c('admin_name', 'date', 'Run_Number', 'Bednet_Got_New_One', 'Received_IRS')]
  net_use_all = merge(net_use_all, net_dist_all, by=c('admin_name', 'date', 'Run_Number'))
  net_use_all$date = as.Date(net_use_all$date)
  net_use_all$year = lubridate::year(net_use_all$date)
  net_use_all = net_use_all[intersect(which(net_use_all$year >= min_year), which(net_use_all$year <= max_year)),]
  
  # subset to appropriate admins
  net_use_all = net_use_all[net_use_all$admin_name %in% cur_admins,]  
  colnames(net_use_all) = gsub(' ','.',colnames(net_use_all))
  
  # rescale numbers of nets received, nets used, and IRS received to the true population in each admin, rather than the simulated population size
  net_use_all = merge(net_use_all, pop_sizes, by='admin_name')
  colnames(net_use_all)[colnames(net_use_all) == 'pop_size'] = 'true_population'
  net_use_all$Bednet_Using = net_use_all$Bednet_Using * net_use_all$true_population / net_use_all$Statistical.Population
  net_use_all$Bednet_Got_New_One = net_use_all$Bednet_Got_New_One * net_use_all$true_population / net_use_all$Statistical.Population
  net_use_all$Received_IRS = net_use_all$Received_IRS * net_use_all$true_population / net_use_all$Statistical.Population
  
  # get sum of numbers across all included admins (keeping runs and months separate)
  net_use_sums = net_use_all %>% dplyr::select(year, date, Run_Number, Bednet_Using, Bednet_Got_New_One, Received_IRS, true_population) %>% dplyr::group_by(date, year, Run_Number) %>%
    dplyr::summarise_all(sum)
  net_use_sums$coverage = net_use_sums$Bednet_Using / net_use_sums$true_population / indoor_protection_fraction
  net_use_sums$new_net_per_cap = net_use_sums$Bednet_Got_New_One / net_use_sums$true_population
  net_use_sums$irs_per_cap = net_use_sums$Received_IRS / net_use_sums$true_population
  
  # get average coverage accross months in a year for the annual report
  if(!plot_by_month){
    net_use_sums = net_use_sums %>% dplyr::group_by(year, Run_Number) %>% # take average across months
      dplyr::summarise(coverage = mean(coverage),
                new_net_per_cap = sum(new_net_per_cap),
                irs_per_cap = sum(irs_per_cap))
  }
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, date, coverage, new_net_per_cap, irs_per_cap) %>%
      dplyr::group_by(year, date) %>%
      dplyr::summarise(mean_coverage = mean(coverage),
                max_coverage = max(coverage),
                min_coverage = min(coverage),
                new_net_per_cap = mean(new_net_per_cap),
                irs_per_cap = mean(irs_per_cap))
  } else{
    net_use_agg = as.data.frame(net_use_sums) %>% dplyr::select(year, coverage, new_net_per_cap, irs_per_cap) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_coverage = mean(coverage),
                max_coverage = max(coverage),
                min_coverage = min(coverage),
                new_net_per_cap = mean(new_net_per_cap),
                irs_per_cap = mean(irs_per_cap))
  }
  
  net_use_agg$scenario = exp_name
  return(net_use_agg)
}


# ----- CM intervention coverage ----- #
get_cm_timeseries_exp = function(cm_filepath, pop_sizes, end_year, exp_name, cur_admins, min_year, plot_by_month=TRUE){
  
  cm_input = read.csv(cm_filepath)
  # subset to appropriate admins
  cm_input = cm_input[cm_input$admin_name %in% cur_admins,]  
  
  # CM is sometimes repeated for several years but only listed once; change to repeate the appropriate number of times
  cm_input$years_repeated = cm_input$duration/365
  if(any(cm_input$years_repeated>1)){
    cur_cm_years = unique(cm_input$year)
    for(yy in cur_cm_years){
      # get first instance of this year
      cur_year = cm_input[cm_input$year==yy,]
      if(cur_year$years_repeated[1]>1){
        for(rr in 1:(cur_year$years_repeated[1] - 1)){
          temp_year = cur_year
          temp_year$year = cur_year$year + rr
          temp_year$simday = cur_year$simday + rr*365
          cm_input = rbind(cm_input, temp_year)
        }
      }
    }
  }
  if(any(cm_input$duration==-1) & (max(cm_input$year)<end_year)){
    cm_repeated = cm_input[cm_input$duration == -1,]
    for(rr in 1:(end_year - cm_repeated$year[1])){
      temp_year = cm_repeated
      temp_year$year = cm_repeated$year + rr
      temp_year$simday = cm_repeated$simday + rr*365
      cm_input = rbind(cm_input, temp_year)
    }
  }
  # if there are multiple values in a single year (for a single DS/LGA), take the mean of those values
  cm_input <- cm_input %>% group_by(year, admin_name, seed) %>%
    summarise_all(mean) %>% ungroup()
  
  cm_input = cm_input[intersect(which(cm_input$year >= min_year), which(cm_input$year <= end_year)),]
  
  
  # get population-weighted CM coverage across admins
  cm_input = merge(cm_input, pop_sizes, by='admin_name')
  cm_input$multiplied_U5_cm = cm_input$U5_coverage * cm_input$pop_size
  
  # get sum of population sizes and multiplied CM coverage across included admins
  cm_input_agg_admin <- cm_input %>% dplyr::select(year, seed, multiplied_U5_cm, pop_size) %>% group_by(year, seed) %>%
    summarise_all(sum) %>% ungroup()
  # get population-weighted U5 coverage across all included admin by dividing by su  of population sizes
  cm_input_agg_admin$U5_coverage = cm_input_agg_admin$multiplied_U5_cm / cm_input_agg_admin$pop_size
  
  
  # take average, max, and min burdens across simulation seeds
  if(plot_by_month){
    # subdivide year values and add dates
    # date dataframe
    included_years = unique(cm_input_agg_admin$year)
    all_months = as.Date(paste0(rep(included_years, each=12),'-',c('01','02','03','04','05','06','07','08','09','10','11','12'), '-01' ))
    date_df = data.frame(year=rep(included_years, each=12), date=all_months)
    cm_input_agg_admin_monthly = merge(cm_input_agg_admin, date_df, by='year', all.x=TRUE, all.y=TRUE)
    cm_agg = as.data.frame(cm_input_agg_admin_monthly) %>% dplyr::select(year, date, U5_coverage) %>%
      dplyr::group_by(year, date) %>%
      dplyr::summarise(mean_coverage = mean(U5_coverage),
                max_coverage = max(U5_coverage),
                min_coverage = min(U5_coverage))
  } else{
    cm_agg = as.data.frame(cm_input_agg_admin) %>% dplyr::select(year, U5_coverage) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(mean_coverage = mean(U5_coverage),
                max_coverage = max(U5_coverage),
                min_coverage = min(U5_coverage))
  }
  
  cm_agg$scenario = exp_name
  return(cm_agg)
  
}










plot_simulation_intervention_output = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                               plot_by_month, min_year, max_year, sim_end_years, 
                                               burden_metric, age_plotted, 
                                               pyr, chw_cov,
                                               scenario_filepaths, scenario_names, scenario_input_references, experiment_names, scenario_palette, 
                                               indoor_protection_fraction=0.75, LLIN2y_flag=FALSE, overwrite_files=FALSE){

  
  ####################################################################
  # combine simulation output from multiple scenarios
  ####################################################################
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
  
  
  
  
  
  
  
  
  ####################################################################
  # create scenario-comparison plots
  ####################################################################
  
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

















######################################################################
# create plot panel with all burden metrics, no intervention info
######################################################################

plot_simulation_output_burden_all = function(sim_future_output_dir, pop_filepath, district_subset, cur_admins, 
                                               plot_by_month, min_year, max_year, sim_end_years, 
                                               pyr, chw_cov,
                                               scenario_filepaths, scenario_names, experiment_names, scenario_palette, LLIN2y_flag=FALSE, overwrite_files=FALSE){
  
  
  ####################################################################
  # combine simulation output from multiple scenarios
  ####################################################################
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
    
    
    
    
    ####################################################################
    # create scenario-comparison plots
    ####################################################################

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













