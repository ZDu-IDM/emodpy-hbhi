# create_NGA_bioassay_mortality_input.R
# November 2022
# contact: mambrose
#
# For 2010-2017, use insecticide mortality rasters (admin means) from MAP. After 2017 (when MAP data ends),
#   use the 2020 VectorLink resistance dataset. This has information from a few sites in a handful of states,
#   so to get estimates for all states, there are two options
#   a) Use similar VectorLink state values
#     1) take average across sample sites in states with VectorLink data
#     2) for states without VectorLink data, use 2017 MAP data to determine which state _with_ vectorlink data 
#          is closest and use that value. 
#   b) continue the 2017 value


library(dplyr)


script_dir = 'C:/Users/moniqueam/Documents/malaria-nga-snt22'
source(paste0(script_dir,'/standardize_admin_names.R'))


create_NGA_bioassay_mortality_input = function(hbhi_dir_base, hbhi_dir, ds_pop_df_filename, mortality_df_name){
  ###################################################################
  # specifiy filepaths and read in data
  ###################################################################
  # flag indicating whether the States without VectorLink data should simply continue to use its 2017 value or should use the value from a similar state with VectorLink data
  use_vl_values_after_2017 = FALSE
  use_who_values_after_2017 = FALSE
  
  
  # MAP data from 2010-2017 for each LGA
  map_data_filepath = paste0(hbhi_dir_base, '/ento/insecticide_resistance_DS_means/Permethrin_mortality_DS_means.csv')
  # VectorLink data from 2022 for select states
  vl_data_filepath = paste0(hbhi_dir, '/ento/insecticide_resistance/Entomological-Monitoring-Report-Nigeria-2019-2020_Table8.csv')
  # WHO vector resistance data (shared Feb 2022)
  who_data_filepath1 = paste0(hbhi_dir, '/ento/insecticide_resistance/Nigeria_WHO_IR_shared20220215_locationInfo.csv')
  who_data_filepath2 = paste0(hbhi_dir, '/ento/insecticide_resistance/Nigeria_WHO_IR_shared20220215_mortalityTest.csv')
  
  archetype_info = read.csv(ds_pop_df_filename)
  map_data = read.csv(map_data_filepath)
  vl_data = read.csv(vl_data_filepath)
  who_data1 = read.csv(who_data_filepath1)
  who_data2 = read.csv(who_data_filepath2)
  
  
  new_years = 2018:2022
  
  
  
  ###################################################################
  # standardize names to match archetype_info
  ###################################################################
  update_lga_name_match = function(lga_name){
    lga_name = str_replace_all(lga_name, pattern=' ', replacement='-')
    lga_name = str_replace_all(lga_name, pattern='/', replacement='-')
    lga_name = toupper(lga_name)
    return(lga_name)
  }
  archetype_info$admin_name = archetype_info$LGA
  archetype_info$matched_LGA = sapply(archetype_info$LGA, update_lga_name_match)
  map_data$matched_LGA = sapply(map_data$DS, update_lga_name_match)
  
  all(map_data$matched_LGA %in% archetype_info$matched_LGA)
  all(vl_data$State %in% archetype_info$State)
  
  # update names from resistance dataframe to match those expected in simulations
  map_data = merge(map_data, archetype_info[,c('admin_name', 'State', 'matched_LGA')], by='matched_LGA', all=TRUE)
  
  
  
  
  
  if(use_vl_values_after_2017){
    ###################################################################
    # assign vectorlink states to states that weren't sampled in vectorlink
    ###################################################################
    # get vector of vectorlink states
    vl_states = unique(vl_data$State)
    
    # get 2017 state averages
    map_state_averages = map_data[, c('State', 'X2017')] %>% group_by(State) %>%
      summarise(mortality = mean(X2017))
    map_state_averages_vl_states = map_state_averages[map_state_averages$State %in% vl_states,]
    
    # fill in the name of the vectorlink state that had the closest resistance value in 2017
    map_state_averages$rep2020 = NA
    for(ss in 1:nrow(map_state_averages)){
      map_state_averages$rep2020[ss] = map_state_averages_vl_states$State[which.min(abs(map_state_averages_vl_states$mortality - map_state_averages$mortality[ss]))]
    }
    
    
    ###################################################################
    # use 2020 state averages from VectorLink data for years after 2017
    ###################################################################
    # get state-level vector-link average
    vl_data_state = vl_data[,c('State', 'mortality')] %>% group_by(State) %>%
      summarise(mortality_2020 = mean(mortality))
    colnames(vl_data_state)[which(colnames(vl_data_state)=='State')] = 'rep2020'
    
    state_mort_2020 = merge(map_state_averages[,c('State', 'rep2020')], vl_data_state[c('rep2020', 'mortality_2020')])
    
    # expand state value out to all LGA
    lga_mort_2020 = merge(map_data[,c('admin_name', 'State')], state_mort_2020[,c('State', 'mortality_2020')])
    
    # add into all new years
    for(yy in new_years){
      map_data = merge(map_data, lga_mort_2020, all=TRUE)
      colnames(map_data)[colnames(map_data)=='mortality_2020'] = paste0('X',yy)
    }
  } else if(use_who_values_after_2017){

    ###################################################################
    # process the WHO data - use mean across tests from 2018-2022 for all remaining years after 2017
    ###################################################################
    who_data1 = who_data1[,c('WHO_ADMIN1', 'WHO_ADMIN2', 'WHO_SITE_CODE')]
    who_data2 = who_data2[,c('WHO_TEST_TYPE', 'TEST_ID', 'WHO_ASSAY_TYPE', 'METHODDETAILS', 'WHO_TEST_TIME', 'WHO_SPECIES',  "WHO_TEST_INSECTICIDE_CLASS", "WHO_INSECTICIDE_TYPE", "WHO_YEAR_START", "WHO_YEAR_END", 'WHO_SITE_CODE', 'WHO_TEST_NUMBER', 'WHO_TEST_MORTALITY_NUMBER', 'WHO_TEST_MORTALITY_PERCENT', 'WHO_TEST_MORTALITY_ADJUSTED', 'WHO_INSTITUTE', 'WHO_REPORTING_INSTITUTION_TYPE', 'WHO_CITATION')]
    who_data = merge(who_data2, who_data1, by='WHO_SITE_CODE', all.x=TRUE)
    who_data$WHO_TEST_MORTALITY_ADJUSTED = as.numeric(who_data$WHO_TEST_MORTALITY_ADJUSTED)
    who_data$WHO_YEAR_START = as.numeric(who_data$WHO_YEAR_START)
    # subset to pyrethroid mortality data after 2017
    who_data = who_data[who_data$WHO_YEAR_START > 2017,]
    who_data = who_data[who_data$WHO_TEST_INSECTICIDE_CLASS == 'Pyrethroids',]
    
    remove_pmi=TRUE
    if(remove_pmi){
      who_data = who_data[who_data$WHO_CITATION != "U.S. President's Malaria Initiative",]
    }
    
    
    # # plot average timeseries within State
    # who_data_state = who_data[, c('WHO_ADMIN1', 'WHO_YEAR_START', 'WHO_TEST_MORTALITY_ADJUSTED')] %>% group_by(WHO_ADMIN1, WHO_YEAR_START) %>%
    #   summarise(mortality = mean(WHO_TEST_MORTALITY_ADJUSTED))
    # ggplot(who_data_state, aes(x=WHO_YEAR_START, y=mortality, color=WHO_ADMIN1))+
    #   geom_line() +
    #   theme(legend.position='none')
    
    # get average mortality across all LGAs and relevant years in the state
    who_data_state = who_data[, c('WHO_ADMIN1', 'WHO_TEST_MORTALITY_ADJUSTED')] %>% group_by(WHO_ADMIN1) %>%
       summarise(mortality = mean(WHO_TEST_MORTALITY_ADJUSTED))
    who_data_state = who_data_state[!is.na(who_data_state$mortality),]
    colnames(who_data_state)[colnames(who_data_state)=='WHO_ADMIN1'] = 'State'
    who_states = unique(who_data_state$State)
    
    # get 2017 MAP state averages to use when WHO data isn't available
    map_state_averages = map_data[, c('State', 'X2017')] %>% group_by(State) %>%
      summarise(mortality = mean(X2017))
    map_state_averages_not_in_who = map_state_averages[which(!(map_state_averages$State %in% who_states)),]
    map_state_averages_in_who = map_state_averages[which((map_state_averages$State %in% who_states)),]
    colnames(map_state_averages_in_who)[colnames(map_state_averages_in_who)=='mortality'] = 'mortality_map'
    
    compare_df = merge(who_data_state, map_state_averages_in_who)
    plot(compare_df$mortality/100, compare_df$mortality_map, pch=20, bty='L')
    abline(a=0,b=1)
    
    # fill in the name of the vectorlink state that had the closest resistance value in 2017
    map_state_averages$rep2020 = NA
    for(ss in 1:nrow(map_state_averages)){
      map_state_averages$rep2020[ss] = map_state_averages_vl_states$State[which.min(abs(map_state_averages_vl_states$mortality - map_state_averages$mortality[ss]))]
    }
    
    map_state_averages_who_states = map_state_averages[map_state_averages$State %in% who_states,]
    # fill in the name of the who state that had the closest resistance value in 2017
    map_state_averages$rep2020 = NA
    for(ss in 1:nrow(map_state_averages)){
      map_state_averages$rep2020[ss] = map_state_averages_who_states$State[which.min(abs(map_state_averages_who_states$mortality - map_state_averages$mortality[ss]))]
    }
  } else{
    ###################################################################
    # repeat 2017 value for all remaining years
    ###################################################################
    lga_mort_2017 = map_data[,c('admin_name', 'X2017')]
    colnames(lga_mort_2017)[colnames(lga_mort_2017)=='X2017'] = 'mortality_2017'
    # add into all new years
    for(yy in new_years){
      map_data = merge(map_data, lga_mort_2017, all=TRUE)
      colnames(map_data)[colnames(map_data)=='mortality_2017'] = paste0('X',yy)
    }
  }
  
  
  ###################################################################
  # format and write results to csv
  ###################################################################
  map_data = map_data[,c('admin_name', paste0('X', c(2005:2017, new_years)))]
  write.csv(map_data, mortality_df_name)
  
  
  
  ###################################################################
  # plots
  ###################################################################
  
  # plot results through time
  mortality_df_0 = map_data
  colnames(mortality_df_0) = gsub('X','',colnames(mortality_df_0))  # get rid of the X in front of years in column names
  mortality_df = reshape2::melt(mortality_df_0, id.vars = 'admin_name')
  colnames(mortality_df)[which(colnames(mortality_df)=='variable')] = 'year'
  colnames(mortality_df)[which(colnames(mortality_df)=='value')] = 'bio_mortality'
  mortality_df$year = as.integer(as.character(mortality_df$year))
  mortality_df = merge(mortality_df, archetype_info[,c('admin_name', 'State')])
  gg = ggplot()+
    geom_line(data=mortality_df, aes(x=year, y=bio_mortality, color=admin_name))+
    theme_bw() +
    theme(legend.position='none')+
    facet_wrap('State', nrow=5)
  ggsave(filename=paste0(hbhi_dir,'/simulation_inputs/plots/permethrin_mortality_through_time_estimates.png'), gg, width=5, height=4, units='in')
  
  
  # look at the VL LGAs
  vl_lgas = unique(archetype_info$admin_name[archetype_info$State %in% vl_data$State])
  gg=ggplot()+
    geom_line(data=mortality_df[(mortality_df$admin_name %in% vl_lgas),], aes(x=year, y=bio_mortality, color=admin_name))+
    theme_bw() +
    theme(legend.position='none')+
    facet_wrap('State', nrow=5)
  ggsave(filename=paste0(hbhi_dir,'/simulation_inputs/plots/permethrin_mortality_through_time_estimates_subetVectorLink.png'), gg, width=5, height=4, units='in')
  
}




# runs only when script is run by itself, not sourced
if (sys.nframe() == 0){
  hbhi_dir_base = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
  hbhi_dir = paste0(hbhi_dir_base, '/snt_2022')
  
  ds_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
  mortality_df_name = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/insecticide_resistance/permethrin_mortality_admin_estimates.csv')
  
  create_NGA_bioassay_mortality_input(hbhi_dir_base, hbhi_dir, ds_pop_df_filename, mortality_df_name)
}