# create_NGA_LLIN_ANC_date_coverage.R
# contact: mambrose
# November 2022
#
# Purpose:  create a csv giving the coverages of ANC net distributions for each year. 
#
# Background: The 2019 version of the analysis tarted ANC net distribution in 2018, based on the fraction
#    of anc_total that were preg_llin (I am not certain where those values were taken from, but presumably
#    the routine surveillance data, since numbers are sometimes quite small). The same values were used
#    for all years after 2018. It is not clear to me whether those values are sufficiently reliable, both
#    from the small sample sizes and from the fact that individuals who were recorded as part of the ANC
#    system do not include all pregnant individuals not attending ANC (who would not have received a net),
#    so we might expect an overestimate on coverage. Plus, it seems likely that there could be reporting biases,
#    either from individuals being more likely to appear in the records if they received a net, or from some
#    facilities not recording net distributions (I don't have any data suggesting relative rates of these).
# Nonetheless, it may be the best source of information at an LGA level. The alternative might be to use
#    a national-level estimate or reported number of routine nets distributed per state, if such information
#    were available. To start, I'll plot the 2019-analysis assumption.
# Current plan is to use the rescaled 2018 values for years after 2018, then feed dataframe into function to 
#   format for simulations which uses a linear decline to 0 coverage in 2010


library(ggplot2)






modify_llin_anc_date_coverage_from_2019_sim = function(hbhi_dir, ds_pop_df_filename, sim_2019_llin_anc_filename, anc_itn_access_filename, years_anc_nets=2018:2022){
  
  ###################################
  # setup and read in data
  ###################################
  
  # read in csv with dates of distributions in each LGA/state, which was data provided by the WHO, combined into a single csv
  llin_info = read.csv(sim_2019_llin_anc_filename)
  archetype_info = read.csv(ds_pop_df_filename)
  
  # add state name from each LGA
  all(llin_info$LGA %in% archetype_info$LGA)
  llin_info = merge(llin_info, archetype_info[,c('LGA', 'State')], all=TRUE)
  
  
  ###################################
  # rescale and set data maximums
  ###################################
  
  # account for individuals who did not seek ANC in public facility where they would have received a net
  # from 2018 DHS, around 67% of individuals reported at least one attendance at an ANC and 
  llin_info$coverage_orig = llin_info$coverage
  llin_info$coverage = (llin_info$coverage_orig * 0.67 + 0) / 1
  
  # median value
  median_value = as.numeric(quantile(llin_info$coverage, probs=.50))
  
  # set maximum ANC coverage (to max 98 percentile)
  max_value = as.numeric(quantile(llin_info$coverage, probs=0.98))
  llin_info$coverage = sapply(llin_info$coverage, min, max_value)
  
  if(FALSE){
    # create plot of results, separated by state
    ggplot(llin_info, aes(x=State, y=coverage))+
      geom_violin() +
      # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1)
      geom_hline(yintercept=median_value, color='red') +
      geom_jitter(shape=16, position=position_jitter(0.2))+
      ylab('estimated ANC LLIN coverage')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
    
    # create plot of results, separated by state
    ggplot(llin_info, aes(x=State, y=coverage))+
      # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.1)
      geom_jitter(shape=16, position=position_jitter(0.2))+
      ylab('estimated ANC LLIN coverage')+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
  }

  
  gg = ggplot(llin_info, aes(x=coverage))+
    geom_histogram(bins=20, color='black', fill=rgb(31/255,112/255, 195/255)) +
    xlab('estimated ANC LLIN coverage')+
    ylab('number of LGAs')+
    theme_bw()
  ggsave(filename=paste0(hbhi_dir, '/simulation_inputs/plots/anc_llin_coverage_distribution.png'), gg, width=4, height=3, units='in')
    
  
  ######################################################################
  # create input dataframe with all LGAs and years
  ######################################################################
  years_df = data.frame('year'=years_anc_nets)
  colnames(llin_info)[colnames(llin_info)=='LGA'] = 'adm2'
  colnames(llin_info)[colnames(llin_info)=='coverage'] = 'cov_llins_anc'
  llin_info_input = merge(llin_info[,c('adm2','cov_llins_anc','type', 'State')], years_df, all=TRUE)
  llin_info_input$duration = 365
  write.csv(llin_info_input, anc_itn_access_filename)
}




# runs only when script is run by itself, not sourced
if (sys.nframe() == 0){
  hbhi_dir_base = 'C:/Users/moniqueam/Dropbox (IDM)/NU_collaboration/hbhi_nigeria'
  hbhi_dir = paste0(hbhi_dir_base, '/snt_2022')
  ds_pop_df_filename = paste0(hbhi_dir, '/admin_pop_archetype.csv')
  # csv used in 2019 simulations with dates of ANC LLIN distributions in each LGA
  sim_2019_llin_anc_filename = paste0(hbhi_dir_base, '/simulation_inputs/projection_csvs/2010_2020_LGA_intervention_files/ITN/ITN_ANC_current_coverage.csv')
  # output filename
  anc_itn_access_filename=paste0(hbhi_dir, '/simulation_inputs/intermediate_files/ITN_ANC_dates_LGA.csv')
  # years over which ANC LLINs should be distributed at the 2018 estimated coverage
  years_anc_nets=2018:2022
  
  # call function
  modify_llin_anc_date_coverage_from_2019_sim(hbhi_dir, ds_pop_df_filename, sim_2019_llin_anc_filename, anc_itn_access_filename, years_anc_nets)
}
