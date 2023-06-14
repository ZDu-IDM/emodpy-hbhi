# create_NGA_LLIN_mass_dist_dates.R
# contact: mambrose
# November 2022
#
# Purpose:  create a csv giving the dates of mass net campaigns in each of Nigeria's LGAs over the past decade.
#
# Background: There are several files giving the dates of mass distributions in different states/LGAs as well 
#    as the number of nets distributed and the target populations. Unfortunately, the LGA names are not consistent 
#    across years and the coverage (nets distributed divided by target population) is also often inconsistent or 
#    unrealistic. Similarly, sometimes the same LGA is listed as receiving the same number of nets in consecutive 
#    years (e.g., all LGAs in a state are reported to have received identical numbers of nets in 2018 and in 2019, 
#    which is more likely due to duplication across years than two identical campaigns in adjacent years). However, 
#    it does seem that mass distributions tend to take place during the same month and year within each state, even 
#    if different states have distributions in different years. So we create a dataframe with the month and year of 
#    distributions for each state, and assume that all LGAs in the state share that date. We make sure that distributions 
#    do not occur in subsequent years (when, for example, identical entries are recorded for 2018 and 2019, we use only 
#    the 2018 values)

library(ggplot2)


create_llin_mass_dist_dates = function(hbhi_dir, ds_pop_df_filename, llin_mass_date_filename, itn_distributions_by_admin_filename, create_plots=FALSE){

  ###################################
  # read in and plot data
  ###################################
  
  # read in csv with dates of distributions in each LGA/state, which was data provided by the WHO, combined into a single csv
  llin_info = read.csv(llin_mass_date_filename)
  # length(unique(llin_info$adm2))  # note that there are more than 774 LGAs because naming was not consistent between datasets/years
  # length(unique(llin_info$State))  # state names seem consistent and more reliable
  colnames(llin_info)[which(colnames(llin_info)=='adm1')] = 'State'
  # fix mismatched name
  llin_info$State[llin_info$State=='Edo '] = 'Edo'
  llin_info$State[llin_info$State=='Akwa Ibom'] = 'Akwa lbom'
  
  # set the type of net
  llin_info$llin_type[is.na(llin_info$llin_type)] = 'standard'
  llin_info$llin_type[(llin_info$llin_type %in% c('Delta or Alpha-based', 'Alphacypemetrin - based'))] = 'standard'
  # set the number of nets distributed if not specified (for plotting visualization, this information is not used for simulations)
  llin_info$llins_num = gsub(',','',llin_info$llins_num)
  llin_info$llins_num = as.numeric(llin_info$llins_num)
  llin_info$llins_num[is.na(llin_info$llins_num)] = -19999 + runif(n=sum(is.na(llin_info$llins_num)),min=0,max=19999)
  
  # add date. when month is not available, assume distribution occurred in June
  llin_info$month = llin_info$X
  llin_info$month[is.na(llin_info$month)] = 6
  llin_info$date = as.Date(paste0(llin_info$year,'-', llin_info$month,'-01'))
  
  if(create_plots){
    # plot results by year for all LGAs
    ggplot(llin_info, aes(y=llins_num, x=year, color=adm2))+
      geom_point(aes(shape=llin_type)) + 
      geom_line()+
      theme(legend.position='none')
    
    # plot results by year for all LGAs, separated into a grid by state
    ggplot(llin_info, aes(y=llins_num, x=year, color=adm2))+
      geom_point(aes(shape=llin_type)) + 
      geom_line()+
      theme(legend.position='none')+
      facet_wrap(nrow=6, 'State')
    
    # plot results by date for all LGAs, separated into a grid by state
    ggplot(llin_info, aes(y=llins_num, x=date, color=adm2))+
      geom_point(aes(shape=llin_type)) + 
      geom_line()+
      theme(legend.position='none')+
      facet_wrap(nrow=6, 'State')
  }
  
  
  
  
  ######################################################################
  # get dates corresponding to distributions for each state from records
  ######################################################################
  llin_info_state = llin_info[,c('State', 'date', 'year', 'llin_type')]
  llin_info_state = distinct(llin_info_state)
  # check whether there are multiple net types per year for a single state (to avoid giving all LGAs two mass distributions, one with each net type)
  if (nrow(distinct(llin_info[,c('State', 'year')])) != nrow(llin_info_state)){
    warning(paste0('PROBLEM DETEcTED: at least one state has multiple LLIN types in a single year or multiple distribution dates in a single year. As currently coded, we assume all LGAs in a state get the same net type on the same day. To avoid giving *all* LGAs in that state multiple distributions need to change input dataset.'))
  }
  
  
  if(create_plots){
    # check that the dates are captured correctly by state
    ggplot()+
      geom_point(data=llin_info, aes(y=llins_num, x=date, color=adm2, shape=llin_type)) + 
      geom_line(data=llin_info, aes(y=llins_num, x=date, color=adm2))+
      geom_point(data=llin_info_state, aes(x=date, y=0), color='black', size=2)+
      theme(legend.position='none')+
      facet_wrap(nrow=6, 'State')
  }
  
  
  # remove distributions that occur in adjacent years
  states = unique(llin_info_state$State)
  for(ss in 1:length(states)){
    state_years = sort(unique(llin_info_state$year[llin_info_state$State==states[ss]]))
    state_years_plus_1 = state_years + 1
    # remove duplicate years
    if(any(state_years %in% state_years_plus_1)){
      llin_info_state = llin_info_state[-intersect(which(llin_info_state$State==states[ss]), which(llin_info_state$year %in% state_years_plus_1)),]
    }
  }
  
  if(create_plots){
    # check that the dates are captured correctly by state, with duplicate years removed
    ggplot()+
      geom_point(data=llin_info, aes(y=llins_num, x=date, color=adm2, shape=llin_type)) + 
      geom_line(data=llin_info, aes(y=llins_num, x=date, color=adm2))+
      geom_point(data=llin_info_state, aes(x=date, y=0), color='black', size=2)+
      theme(legend.position='none')+
      facet_wrap(nrow=6, 'State')
  }
  
  
  
  ######################################################################
  # add in base distributions when date not recorded by state
  ######################################################################
  #  some states did not have any distributions recorded or have apparent gaps in the years recorded. 
  #   Looking at the DHS survey results, sometimes there are low ITN rates for states without distributions, 
  #   but certainly not always. Since it appears not all distributions are recorded, the current plan is to 
  #   assume a distribution in Jan 2010, 2013, 2015, 2018  if a state did not already report one in the two years before/after.
  #   If there really was not a distribution then, the DHS data should reflect the low net use, so the coverage will be
  #   very low in the simulations and the false 'distribution' shouldn't impact results much
  base_dist_years = c(2010, 2013, 2015, 2018)
  
  # add in the LGAs for all of the states
  archetype_info = read.csv(ds_pop_df_filename)
  archetype_info = archetype_info[,c('LGA', 'State')]
  unique(llin_info_state$State)[which(!(unique(llin_info_state$State) %in% unique(archetype_info$State)))]
  unique(archetype_info$State)[which(!(unique(archetype_info$State) %in% unique(llin_info_state$State)))]
  states = unique(c(states, archetype_info$State))
  
  for(ss in 1:length(states)){
    for(bb in (base_dist_years)){
      state_years = sort(unique(llin_info_state$year[llin_info_state$State==states[ss]]))
      if(!(any(c(bb-1, bb, bb+1) %in% state_years))){
        llin_info_state = merge(llin_info_state, data.frame('State'=states[ss], 'year'=bb, 'date'=as.Date(paste0(bb, '-01-01')), 'llin_type'='standard'), all=TRUE)
      }
    }
  }
  
  if(create_plots){
    # check that the dates are captured correctly by state, with duplicate years removed and with base years added
    ggplot()+
      geom_point(data=llin_info, aes(y=llins_num, x=date, color=adm2, shape=llin_type)) + 
      geom_line(data=llin_info, aes(y=llins_num, x=date, color=adm2))+
      geom_point(data=llin_info_state, aes(x=date, y=0), color='black', size=2)+
      theme(legend.position='none')+
      facet_wrap(nrow=6, 'State')
  }
  
  
  
  ######################################################################
  # get dates corresponding to distributions for each LGA
  ######################################################################
  llin_info_lga = merge(llin_info_state, archetype_info, all=TRUE)
  llin_info_lga$llin_type[is.na(llin_info_lga$llin_type)] = 'standard'
  llin_info_lga$llin_type[which(llin_info_lga$llin_type == '')] = 'standard'
  colnames(llin_info_lga)[colnames(llin_info_lga)=='LGA'] = 'admin_name'
  write.csv(llin_info_lga, itn_distributions_by_admin_filename)
  
}





# runs only when script is run by itself, not sourced
if (sys.nframe() == 0){
  hbhi_dir_base = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
  hbhi_dir = paste0(hbhi_dir_base, '/snt_2022')
  ds_pop_df_filename = paste0(hbhi_dir, '/admin_pop_archetype.csv')
  llin_mass_date_filename = paste0(hbhi_dir, '/interventions/NGA_net_distributions_modifiedFromWHO.csv')
  # name for output file
  itn_distributions_by_admin_filename = paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_mass_dist_dates_LGA.csv')
  # flag to indicate whether checking/verification plots should be created during run
  create_plots = FALSE
  
  # call function
  create_llin_mass_dist_dates(hbhi_dir, ds_pop_df_filename, llin_mass_date_filename, itn_distributions_by_admin_filename, create_plots=create_plots)
}
