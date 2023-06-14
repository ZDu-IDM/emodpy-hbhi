# plot_from_past_to_future_all_admins_seeds.R

library(ggplot2)

sim_output_filepath = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/simulation_output"
admin_pop_df_filename = paste(hbhi_dir, '/admin_pop_archetype.csv', sep='')
admin_pop = read.csv(admin_pop_df_filename)
admin_pop = admin_pop[,c('admin_name', 'pop_size')]

# future malariaBurden_withAdjustments.csv directory
future_burden = read.csv(paste0(sim_output_filepath, '/simulations_future/BDI_projection_64pyr/malariaBurden_withAdjustments.csv'))
# future_burden = read.csv(paste0(sim_output_filepath, '/simulations_future/BDI_projection_64pyr_no2020Camp/malariaBurden_withAdjustments.csv'))
first_year_future = 2021
last_year_plotted = 2030
future_burden = future_burden[future_burden$year <= last_year_plotted,]

# past malariaBurden_withAdjustments.csv directory
past_burden = read.csv(paste0(sim_output_filepath, '/simulations_to_present/BDI_2010_2020_allInter/malariaBurden_withAdjustments.csv'))
# past_burden = read.csv(paste0(sim_output_filepath, '/simulations_to_present/BDI_2010_2020_no2020Camp/malariaBurden_withAdjustments.csv'))
first_year_plotted = 2011
past_burden = past_burden[past_burden$year < first_year_future,]
past_burden = past_burden[past_burden$year >= first_year_plotted,]



if(all(colnames(past_burden) == colnames(future_burden))){
  
  # combine data frames
  all_years_burden = rbind(past_burden, future_burden)
  all_years_burden$date = as.Date(all_years_burden$date)
  
  # plot single admin and single seed as an example
  burden_cur = all_years_burden[intersect(which(all_years_burden$Run_Number == 0), which(all_years_burden$admin_name == 'Bubanza')),]
  ggplot(burden_cur, aes(x=date, y=New_Clinical_Cases)) + 
    geom_line()
  
  # get annual values
  burden_cur_annual = burden_cur %>% group_by(admin_name, Run_Number, year) %>%
    summarise(PfPR_all = mean(PfPR_MiP_adjusted),
              PfPR__U5 = mean(PfPR_U5),
              cases_all = sum(New_Clinical_Cases),
              cases_U5 = sum(Cases_U5),
              mortality_1_all = sum(total_mortality_1),
              mortality_2_all = sum(total_mortality_2),
              mortality_1_U5 = sum(total_mortality_U5_1),
              mortality_2_U5 = sum(total_mortality_U5_2))
  ggplot(burden_cur_annual, aes(x=year, y=cases_all)) + 
    geom_line()
  
  
  # plot all admins and single seed
  seed_plot = 0
  burden_cur = all_years_burden[which(all_years_burden$Run_Number == seed_plot),]
  # ggplot(burden_cur, aes(x=date, y=New_Clinical_Cases, group=admin_name)) + 
  #   geom_line(aes(color=admin_name))+
  #   theme(legend.position = "none")
  
  # get annual values
  burden_cur_annual = burden_cur %>% group_by(admin_name, Run_Number, year) %>%
    summarise(PfPR_all = mean(PfPR_MiP_adjusted),
              PfPR__U5 = mean(PfPR_U5),
              cases_all = sum(New_Clinical_Cases),
              cases_U5 = sum(Cases_U5),
              mortality_1_all = sum(total_mortality_1),
              mortality_2_all = sum(total_mortality_2),
              mortality_1_U5 = sum(total_mortality_U5_1),
              mortality_2_U5 = sum(total_mortality_U5_2))
  ggplot(burden_cur_annual, aes(x=year, y=cases_all, group=admin_name)) + 
    geom_line(aes(color=admin_name))+
    theme(legend.position = "none")
  
  # get aggregated annual burden, weighted by population size in each DS
  
  
  
  # which admins increase more than frac_increase between 2019 and 2024?
  base_year = 2017
  comp_year = 2024
  frac_increase = 0.4
  biggest_increasers = c()
  for(aa in 1:length(unique(burden_cur_annual$admin_name))){
    if(burden_cur_annual$cases_all[intersect(which(burden_cur_annual$year == comp_year), which(burden_cur_annual$admin_name == unique(burden_cur_annual$admin_name)[aa]))] > 
       (1+frac_increase) * burden_cur_annual$cases_all[intersect(which(burden_cur_annual$year == base_year), which(burden_cur_annual$admin_name == unique(burden_cur_annual$admin_name)[aa]))]){
      biggest_increasers = c(biggest_increasers, unique(burden_cur_annual$admin_name)[aa])
    }
  }
  biggest_increasers
  
  ggplot(burden_cur_annual[burden_cur_annual$admin_name %in% biggest_increasers,], aes(x=year, y=cases_all, group=admin_name)) + 
    geom_line(aes(color=admin_name))+
    theme(legend.position = "none")
  
  # plot - broken out by seeds (all admins in same plot)
  
  
  # plot - broken out by admin (all seeds in same plot)
  
  
  
} else{
  warning("PROBLEM DETECTED: not all columns of the future and past dataframes match.")
}













#######################################################
# compare results with and without 2020 ITN campaign



sim_output_filepath = "C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/simulation_output"

# future malariaBurden_withAdjustments.csv directory
future_burden_w = read.csv(paste0(sim_output_filepath, '/simulations_future/BDI_projection_64pyr/malariaBurden_withAdjustments.csv'))
future_burden_wo = read.csv(paste0(sim_output_filepath, '/simulations_future/BDI_projection_64pyr_no2020Camp/malariaBurden_withAdjustments.csv'))
first_year_future = 2021
last_year_plotted = 2030
future_burden_w = future_burden_w[future_burden_w$year <= last_year_plotted,]
future_burden_wo = future_burden_wo[future_burden_wo$year <= last_year_plotted,]

# past malariaBurden_withAdjustments.csv directory
past_burden_w = read.csv(paste0(sim_output_filepath, '/simulations_to_present/BDI_2010_2020_allInter/malariaBurden_withAdjustments.csv'))
past_burden_wo = read.csv(paste0(sim_output_filepath, '/simulations_to_present/BDI_2010_2020_no2020Camp/malariaBurden_withAdjustments.csv'))
first_year_plotted = 2011
past_burden_w = past_burden_w[past_burden_w$year < first_year_future,]
past_burden_w = past_burden_w[past_burden_w$year >= first_year_plotted,]
past_burden_wo = past_burden_wo[past_burden_wo$year < first_year_future,]
past_burden_wo = past_burden_wo[past_burden_wo$year >= first_year_plotted,]



# combine data frames - with mass distribution
all_years_burden_w = rbind(past_burden_w, future_burden_w)
all_years_burden_w$date = as.Date(all_years_burden_w$date)
all_years_burden_w$with_2020_ITN = 1

# plot single admin and single seed as an example
burden_cur_w = all_years_burden_w[intersect(which(all_years_burden_w$Run_Number == 0), which(all_years_burden_w$admin_name == 'Bubanza')),]
ggplot(burden_cur_w, aes(x=date, y=New_Clinical_Cases)) + 
  geom_line()


# combine data frames - without mass distribution
all_years_burden_wo = rbind(past_burden_wo, future_burden_wo)
all_years_burden_wo$date = as.Date(all_years_burden_wo$date)
all_years_burden_wo$with_2020_ITN = 0

# plot single admin and single seed as an example
burden_cur_wo = all_years_burden_wo[intersect(which(all_years_burden_wo$Run_Number == 0), which(all_years_burden_wo$admin_name == 'Bubanza')),]
ggplot(burden_cur_wo, aes(x=date, y=New_Clinical_Cases)) + 
  geom_line()



# combine dataframes with and without - plot monthly output for both
burden_cur = rbind(burden_cur_w, burden_cur_wo)
ggplot(burden_cur, aes(x=date, y=New_Clinical_Cases, group=with_2020_ITN)) + 
  geom_line(aes(color=with_2020_ITN))


# aggregate to annual values and plot both
burden_cur_annual = burden_cur %>% group_by(admin_name, Run_Number, year, with_2020_ITN) %>%
  summarise(PfPR_all = mean(PfPR_MiP_adjusted),
            PfPR__U5 = mean(PfPR_U5),
            cases_all = sum(New_Clinical_Cases),
            cases_U5 = sum(Cases_U5),
            mortality_1_all = sum(total_mortality_1),
            mortality_2_all = sum(total_mortality_2),
            mortality_1_U5 = sum(total_mortality_U5_1),
            mortality_2_U5 = sum(total_mortality_U5_2))
ggplot(burden_cur_annual, aes(x=year, y=cases_all, group=with_2020_ITN)) + 
  geom_line(aes(color=with_2020_ITN))



