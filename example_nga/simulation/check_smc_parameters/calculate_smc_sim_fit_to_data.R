# examine_smc_test_sims.R
# simulations run with a variety of changes to smc delivery in the dtk... plot a comparison of U5 incidence to see whether the changes occurred correctly

library(dplyr)
library(ggplot2)
library(stringr)
library(viridis)

# sim output folder
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
script_dir = paste0(user_path, '/Documents/malaria-nga-snt22')
source(paste0(script_dir, '/simulation/sim_check_params/functions_smc_sim_fit.R'))

sim_output_base_filepath = paste(user_path, '/Dropbox (IDM)/NU_collaboration/hbhi_nigeria/snt_2022/simulation_output/check_smc_efficacy', sep='')
drug_day = 365 *2

drug_factors=c('SP_C50',  'SP_kill', 'Amod_C50', 'Amod_kill')
comparison_day=50  # days after drug treatment that we look at whether treatment was successful

# treatment PE: reference targets - these are typically for PCR detection. I worry that DTK sometimes has too many sub-PCR infections that bounce back, so may use true prevalence instead.
treat_pe_target_spaq = 0.99
treat_pe_target_sp = 0.88
treat_pe_target_aq = 0.72

# preventative PE: reference targets
prevent_pe_28day = 0.83
prevent_pe_29_42day = 0.6
prevent_pe_100day = 0

# color for error between reference and simulation
scale_high_color=rgb(1,0.6,0.6)
scale_mid_color='black'
scale_low_color=rgb(0.3,1,1)


plot_subset = data.frame(SP_kill=c(0.8, 0.9, 0.99, 0.6,0.6,0.5), SP_C50=c(50,50,50,10,10,10), Amod_kill=c(0.6,0.6,0.6,0.4,0.5,0.5), Amod_C50=rep(100,6))
plot_subset = data.frame(SP_kill=c(0.6,0.6,0.7, 0.7), SP_C50=c(10,10,10,10), Amod_kill=c(0.4,0.5,0.4,0.5), Amod_C50=rep(100,4))
plot_subset = data.frame(SP_kill=c(0.28, 0.28,0.29, 0.28), SP_C50=c(0.8, 0.9, 0.9, 1), Amod_kill=c(0.2, 0.2, 0.2, 0.2), Amod_C50=c(55, 55, 70, 55))
plot_subset = data.frame(SP_kill=c(0.28, 0.28), SP_C50=c(0.8, 0.9), Amod_kill=c(0.2, 0.2), Amod_C50=c(55, 55))

################################################################################
########            efficacy at clearing existing infections            ########
################################################################################
experiment_names = c('SMC_effect_treat_control_v3', 'SMC_efficacy_treat_SPAQ_v3a', 'SMC_efficacy_treat_SP_v3a', 'SMC_efficacy_treat_SP_v3b','SMC_efficacy_treat_AQ_v3a')
experiment_names = c('SMC_effect_treat_control_v4', 'SMC_efficacy_treat_SPAQ_v4a', 'SMC_efficacy_treat_SP_v4a', 'SMC_efficacy_treat_AQ_v4a')
experiment_names = c('SMC_effect_treat_control_v5', 'SMC_efficacy_treat_SPAQ_v5a', 'SMC_efficacy_treat_SP_v5a', 'SMC_efficacy_treat_AQ_v5a',
                     'SMC_efficacy_treat_SPAQ_v5b', 'SMC_efficacy_treat_SP_v5b', 'SMC_efficacy_treat_AQ_v5b')
experiment_names = c('SMC_effect_treat_control_v6', 'SMC_efficacy_treat_SPAQ_v6b', 'SMC_efficacy_treat_SP_v6b', 'SMC_efficacy_treat_AQ_v6b')
sim_output = combine_experiments(sim_output_base_filepath=sim_output_base_filepath, experiment_names=experiment_names)
sim_results = analyze_treat_efficacy(sim_output, drug_day, comparison_day=comparison_day, drug_factors=drug_factors)
sim_results$diff_from_target=NA
# see which parameter sets are closest to target
spaq_eligible_rows = intersect(which(sim_results$Amod_kill>0), which(sim_results$SP_kill>0))
sim_results$diff_from_target[spaq_eligible_rows] = sim_results$pe_true_prevalence[spaq_eligible_rows] - treat_pe_target_spaq
reasonable_matches_spaq = sim_results[spaq_eligible_rows,][intersect(which(sim_results$True_Prevalence[spaq_eligible_rows]>0),which(abs(sim_results$pe_true_prevalence[spaq_eligible_rows] - treat_pe_target_spaq)<0.2)),]
sp_eligible_rows = intersect(which(sim_results$Amod_kill==0), which(sim_results$SP_kill>0))
sim_results$diff_from_target[sp_eligible_rows] = sim_results$pe_true_prevalence[sp_eligible_rows] - treat_pe_target_sp
reasonable_matches_sp = sim_results[sp_eligible_rows,][intersect(which(sim_results$True_Prevalence[sp_eligible_rows]>0),which(abs(sim_results$pe_true_prevalence[sp_eligible_rows] - treat_pe_target_sp)<0.16)),]
aq_eligible_rows = intersect(which(sim_results$Amod_kill>0), which(sim_results$SP_kill==0))
sim_results$diff_from_target[aq_eligible_rows] = sim_results$pe_true_prevalence[aq_eligible_rows] - treat_pe_target_aq
reasonable_matches_aq = sim_results[aq_eligible_rows,][intersect(which(sim_results$True_Prevalence[aq_eligible_rows]>0), which(abs(sim_results$pe_true_prevalence[aq_eligible_rows] - treat_pe_target_aq)<0.1)),]


if(TRUE){
  ggplot()+
    geom_point(data=sim_results[spaq_eligible_rows,], aes(x=SP_kill, y=Amod_kill, color=diff_from_target), size=2)+
    geom_point(data=reasonable_matches_spaq, aes(x=SP_kill, y=Amod_kill), color='black', shape='o', size=3)+
    ggtitle('Potential SPAQ parameter sets (filled circles)')+
    scale_color_gradient2(high=scale_high_color, mid=scale_mid_color, low=scale_low_color, midpoint=0)+
    facet_grid(SP_C50~Amod_C50)
  
  # plot areas where SP parameters are most reasonable matches
  ggplot()+
    geom_point(data=sim_results[sp_eligible_rows,], aes(x=SP_kill, y=SP_C50, color=diff_from_target, color=diff_from_target), size=2)+
    geom_point(data=reasonable_matches_sp, aes(x=SP_kill, y=SP_C50), color='black', shape='o', size=3) +
    scale_color_gradient2(high=scale_high_color, mid=scale_mid_color, low=scale_low_color, midpoint=0)+
    ggtitle('Potential SP parameter sets (filled circles)')
  
  # plot areas where AQ parameters are most reasonable matches
  ggplot()+
    geom_point(data=sim_results[aq_eligible_rows,], aes(x=Amod_kill, y=Amod_C50, color=diff_from_target), size=2)+
    geom_point(data=reasonable_matches_aq, aes(x=Amod_kill, y=Amod_C50), color='black', shape='o', size=3) +
    scale_color_gradient2(high=scale_high_color, mid=scale_mid_color, low=scale_low_color, midpoint=0)+
    ggtitle('Potential AQ parameter sets (filled circles)')


}

# plot how prevalence changes as kill rate increases
if(FALSE){
  # plot pattern of prevalence against amodiaquine kill parameter, faceted by other drug factors
  ggplot(sim_results, aes(x=Amod_kill, y=True_Prevalence, group_by=interaction(SP_C50, SP_C50, Amod_C50), color=Amod_C50))+
    geom_point()+
    geom_line()+
    coord_cartesian(ylim=c(0,1))+
    facet_grid(SP_kill~SP_C50)
  ggplot(sim_results, aes(x=Amod_kill, y=Micros_Prevalence, group_by=interaction(SP_C50, SP_C50, Amod_C50), color=Amod_C50))+
    geom_point()+
    geom_line()+
    coord_cartesian(ylim=c(0,1))+
    facet_grid(SP_kill~SP_C50)
}


# Which parameter sets are close enough for all of the criteria?



################################################################################
########             efficacy at preventing new infections              ########
################################################################################
experiment_names = c('SMC_efficacy_prevent_control_v3a', 'SMC_efficacy_prevent_v3a')
experiment_names = c('SMC_efficacy_prevent_control_v4a', 'SMC_efficacy_prevent_SPAQ_v4a', 'SMC_efficacy_prevent_SPAQ_v4b', 'SMC_efficacy_prevent_SPAQ_v4c')
experiment_names = c('SMC_efficacy_prevent_control_v5a', 'SMC_efficacy_prevent_SPAQ_v5a', 'SMC_efficacy_prevent_SPAQ_v5b', 'SMC_efficacy_prevent_SPAQ_v5c', 'SMC_efficacy_prevent_SPAQ_v5d',  'SMC_efficacy_prevent_SPAQ_v5e')
experiment_names = c('SMC_efficacy_prevent_control_v5a',  'SMC_efficacy_prevent_SPAQ_v5c',  'SMC_efficacy_prevent_SPAQ_v5d',  'SMC_efficacy_prevent_SPAQ_v5e')
experiment_names = c('SMC_efficacy_prevent_control_v5a', 'SMC_efficacy_prevent_SPAQ_v5a', 'SMC_efficacy_prevent_SPAQ_v5b', 'SMC_efficacy_prevent_SPAQ_v5c', 'SMC_efficacy_prevent_SPAQ_v5d',  'SMC_efficacy_prevent_SPAQ_v5e', 
                     'SMC_efficacy_prevent_SPAQ_v5f', 'SMC_efficacy_prevent_SPAQ_v5g', 'SMC_efficacy_prevent_SPAQ_v5h',  'SMC_efficacy_prevent_SPAQ_v5i','SMC_efficacy_prevent_SPAQ_v5j')
# experiment_names = c('SMC_efficacy_prevent_control_v5a',  'SMC_efficacy_prevent_SP_v5b',  'SMC_efficacy_prevent_AQ_v5b')
sim_output = combine_experiments(sim_output_base_filepath=sim_output_base_filepath, experiment_names=experiment_names)
sim_results = analyze_prevent_efficacy(sim_output, drug_day=drug_day, drug_factors=drug_factors)

# see which parameter sets are closest to target
error_tolerance = 0.03
period_pe_sim_28 = get_pe_in_time_period(sim_results, challenge_day_min=0, challenge_day_max=16, drug_factors=drug_factors)
period_pe_sim_28$period='a'
eligible_rows_28 = intersect(which(period_pe_sim_28$Amod_kill>0), which(period_pe_sim_28$SP_kill>0))
reasonable_matches_28 = period_pe_sim_28[eligible_rows_28,][intersect(which(period_pe_sim_28$pe_clinical[eligible_rows_28]<1), which(abs(period_pe_sim_28$pe_clinical[eligible_rows_28] - prevent_pe_28day)<error_tolerance)),]
period_pe_sim_42 = get_pe_in_time_period(sim_results, challenge_day_min=17, challenge_day_max=30, drug_factors=drug_factors)
period_pe_sim_42$period='b'
eligible_rows_42 = intersect(which(period_pe_sim_42$Amod_kill>0), which(period_pe_sim_42$SP_kill>0))
reasonable_matches_42 = period_pe_sim_42[eligible_rows_42,][intersect(which(period_pe_sim_42$pe_clinical[eligible_rows_42]<1), which(abs(period_pe_sim_42$pe_clinical[eligible_rows_42] - prevent_pe_29_42day)<error_tolerance)),]
period_pe_sim_100 = get_pe_in_time_period(sim_results, challenge_day_min=31, challenge_day_max=200, drug_factors=drug_factors)
period_pe_sim_100$period='c'
eligible_rows_100 = intersect(which(period_pe_sim_100$Amod_kill>0), which(period_pe_sim_100$SP_kill>0))
reasonable_matches_100 = period_pe_sim_100[eligible_rows_100,][intersect(which(period_pe_sim_100$pe_clinical[eligible_rows_100]<1), which(abs(period_pe_sim_100$pe_clinical[eligible_rows_100] - prevent_pe_100day)<error_tolerance)),]

reasonable_matches_all = inner_join(reasonable_matches_28[,colnames(reasonable_matches_28) %in% drug_factors],
                                    inner_join(reasonable_matches_42[,colnames(reasonable_matches_42) %in% drug_factors],
                                               reasonable_matches_100[,colnames(reasonable_matches_100) %in% drug_factors]))
nrow(reasonable_matches_all)
if(TRUE){
  ggplot()+
    geom_point(data=sim_results, aes(x=SP_C50, y=Amod_C50), color='blue', size=2)+
    geom_point(data=reasonable_matches_all, aes(x=SP_C50, y=Amod_C50), color='black', shape='o', size=3) +
    ggtitle('Potential parameter sets (filled blue circles) for all time intervals')+
    facet_grid(Amod_kill~SP_kill)
  
  pe_df_sim = rbind(period_pe_sim_28, period_pe_sim_42, period_pe_sim_100)
  
  pe_ref_df = data.frame(times=c(10,28,29,42,100,110)-12,  # c(1,28,29,42,90,100)
                          pe_vacc_target = rep(c(prevent_pe_28day, prevent_pe_29_42day, prevent_pe_100day), each=2),
                          interval=rep(c('a','b','c'), each=2))
  
  unique_drug_combos = distinct(sim_results[,drug_factors])
  unique_drug_combos$drug_id = 1:nrow(unique_drug_combos)
  sim_results2 = merge(sim_results, unique_drug_combos, all=TRUE)
  ggplot() +
    geom_point(data=sim_results2, aes(x=challenge_interval, y=pe_clinical, group=drug_id, shape=factor(Amod_C50)))+
    geom_line(data=sim_results2, aes(x=challenge_interval, y=pe_clinical, group=drug_id, color=SP_C50)) +
    geom_point(data=pe_df_sim, aes(x=challenge_interval, y=pe_clinical, group=period, color=SP_C50), size=10, shape='_') + 
    geom_line(data=pe_ref_df, aes(x=times, y=pe_vacc_target, group=interval), col='red') + 
    facet_wrap(Amod_kill~SP_kill)
  
  
  sim_results3 = merge(plot_subset, sim_results2, all.x=TRUE, all.y=FALSE)
  pe_df_sim3 = merge(plot_subset, pe_df_sim, all.x=TRUE, all.y=FALSE)
  gg1 = ggplot() +
    geom_point(data=sim_results3, aes(x=challenge_interval, y=pe_clinical, group=drug_id, shape=factor(Amod_C50)))+
    geom_line(data=sim_results3, aes(x=challenge_interval, y=pe_clinical, group=drug_id, color=SP_C50)) +
    geom_point(data=pe_df_sim3, aes(x=challenge_interval, y=pe_clinical, group=period, color=SP_C50), size=10, shape='_') + 
    geom_line(data=pe_ref_df, aes(x=times, y=pe_vacc_target, group=interval), col='red') + 
    facet_wrap(Amod_kill~SP_kill)
  
  gg1b = ggplot() +
    geom_point(data=sim_results3[sim_results3$SP_kill==0.6 & sim_results3$Amod_kill==0.4,], aes(x=challenge_interval, y=pe_clinical, group=drug_id, shape=factor(Amod_C50)))+
    geom_line(data=sim_results3[sim_results3$SP_kill==0.6 & sim_results3$Amod_kill==0.4,], aes(x=challenge_interval, y=pe_clinical, group=drug_id, color=SP_C50)) +
    geom_point(data=pe_df_sim3[pe_df_sim3$SP_kill==0.6 & pe_df_sim3$Amod_kill==0.4,], aes(x=challenge_interval, y=pe_clinical, group=period, color=SP_C50), size=10, shape='_') + 
    geom_line(data=pe_ref_df, aes(x=times, y=pe_vacc_target, group=interval), col='red')
  
}

# plot combinations of C50s that work for each time period
if(FALSE){
  sim_results$diff_from_target=NA
  sim_results$diff_from_target[eligible_rows_28] = sim_results$pe_clinical[eligible_rows_28] - prevent_pe_28day
  ggplot()+
    geom_point(data=sim_results[eligible_rows_28,], aes(x=SP_C50, y=Amod_C50, color=diff_from_target), shape='o')+
    # geom_point(data=reasonable_matches_28, aes(x=SP_C50, y=Amod_C50) ,col='blue')+
    ggtitle('Potential parameter sets (filled blue circles) for 0-28 days')+
    facet_grid(SP_kill~Amod_kill)
  
  ggplot()+
    geom_point(data=sim_results, aes(x=SP_C50, y=Amod_C50), shape='o')+
    geom_point(data=reasonable_matches_42, aes(x=SP_C50, y=Amod_C50) ,col='blue')+
    ggtitle('Potential parameter sets (filled blue circles) for 29-42 days')+
    facet_grid(SP_kill~Amod_kill)
  
  ggplot()+
    geom_point(data=sim_results, aes(x=SP_C50, y=Amod_C50), shape='o')+
    geom_point(data=reasonable_matches_100, aes(x=SP_C50, y=Amod_C50) ,col='blue')+
    ggtitle('Potential parameter sets (filled blue circles) for 100 days')+
    facet_grid(SP_kill~Amod_kill)
  
}

View(reasonable_matches_all)




################################################################################
########             plot PKPD              ########
################################################################################
sp_Cmax=105.8
sp_decay=11.5
sp_C50=10#c(0.1,10,50)

aq_Cmax=270
aq_decay1=0.7
aq_decay2=15.9
aq_C50=100#c(0.01,0.1,10,50, 100)


par(mfrow=c(1,2))
plot(seq(1:100), sp_Cmax*exp(seq(1:100) * -1/sp_decay), type='l', bty='L', ylab='SP concentration', xlab='days after administration', log='y')
for(cc in 1:length(sp_C50)){
  abline(h=sp_C50[cc], col='red')
  text(x=5, y=sp_C50[cc], labels=sp_C50[cc], col='darkred')
}

plot(seq(1:100), (aq_Cmax*exp(seq(1:100) * -1/aq_decay1))/2+(aq_Cmax*exp(seq(1:100) * -1/aq_decay2))/2, type='l', bty='L', ylab='AQ concentration', xlab='days after administration', log='y')
for(cc in 1:length(aq_C50)){
  abline(h=aq_C50[cc], col='red')
  text(x=5, y=aq_C50[cc], labels=aq_C50[cc], col='darkred')
}






################################################################################
########          efficacy at preventing new infections with vaccSMC    ########
################################################################################
experiment_names = c('SMC_efficacy_prevent_control_v5a', 'SMC_efficacy_prevent_vaccSMC_v5j')
sim_output = combine_experiments(sim_output_base_filepath=sim_output_base_filepath, experiment_names=experiment_names)
sim_results = analyze_prevent_efficacy(sim_output, drug_day=drug_day, drug_factors=drug_factors)

# see which parameter sets are closest to target
error_tolerance = 0.15
period_pe_sim_28 = get_pe_in_time_period(sim_results, challenge_day_min=0, challenge_day_max=16, drug_factors=drug_factors)
period_pe_sim_28$period='a'
eligible_rows_28 = which(period_pe_sim_28$drug_combo=='vaccSMC')
reasonable_matches_28 = period_pe_sim_28[eligible_rows_28,][intersect(which(period_pe_sim_28$pe_clinical[eligible_rows_28]<1), which(abs(period_pe_sim_28$pe_clinical[eligible_rows_28] - prevent_pe_28day)<error_tolerance)),]
period_pe_sim_42 = get_pe_in_time_period(sim_results, challenge_day_min=17, challenge_day_max=30, drug_factors=drug_factors)
period_pe_sim_42$period='b'
eligible_rows_42 = which(period_pe_sim_42$drug_combo=='vaccSMC')
reasonable_matches_42 = period_pe_sim_42[eligible_rows_42,][intersect(which(period_pe_sim_42$pe_clinical[eligible_rows_42]<1), which(abs(period_pe_sim_42$pe_clinical[eligible_rows_42] - prevent_pe_29_42day)<error_tolerance)),]
period_pe_sim_100 = get_pe_in_time_period(sim_results, challenge_day_min=31, challenge_day_max=200, drug_factors=drug_factors)
period_pe_sim_100$period='c'
eligible_rows_100 = which(period_pe_sim_100$drug_combo=='vaccSMC')
reasonable_matches_100 = period_pe_sim_100[eligible_rows_100,][intersect(which(period_pe_sim_100$pe_clinical[eligible_rows_100]<1), which(abs(period_pe_sim_100$pe_clinical[eligible_rows_100] - prevent_pe_100day)<error_tolerance)),]

reasonable_matches_all = inner_join(reasonable_matches_28[,colnames(reasonable_matches_28) %in% drug_factors],
                                    inner_join(reasonable_matches_42[,colnames(reasonable_matches_42) %in% drug_factors],
                                               reasonable_matches_100[,colnames(reasonable_matches_100) %in% drug_factors]))
if(TRUE){
  pe_vacc_df = data.frame(times=c(14, 35, 100)-12,
                          pe_vacc=c(period_pe_sim_28$pe_clinical[1], period_pe_sim_42$pe_clinical[1], period_pe_sim_100$pe_clinical[1]),
                          pe_vacc_target = c(prevent_pe_28day, prevent_pe_29_42day, prevent_pe_100day))

  
  # ggplot() +
  #   geom_point(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical))+
  #   geom_line(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical)) +
  #   geom_point(data=pe_vacc_df, aes(x=times, y=pe_vacc), shape='_', size=20) +
  #   geom_point(data=pe_vacc_df, aes(x=times, y=pe_vacc_target), shape='_', size=20, col='red')
  

  
  
  pe_df_sim = rbind(period_pe_sim_28, period_pe_sim_42, period_pe_sim_100)
  
  pe_ref_df = data.frame(times=c(10,28,29,42,100,110)-12,  # c(1,28,29,42,90,100)
                         pe_vacc_target = rep(c(prevent_pe_28day, prevent_pe_29_42day, prevent_pe_100day), each=2),
                         interval=rep(c('a','b','c'), each=2))
  
  unique_drug_combos = distinct(sim_results[,drug_factors])
  unique_drug_combos$drug_id = 1:nrow(unique_drug_combos)
  sim_results2 = merge(sim_results, unique_drug_combos, all=TRUE)
  ggplot() +
    geom_point(data=sim_results2, aes(x=challenge_interval, y=pe_clinical, color=drug_combo))+
    geom_line(data=sim_results2, aes(x=challenge_interval, y=pe_clinical, color=drug_combo))+
    geom_point(data=pe_df_sim, aes(x=challenge_interval, y=pe_clinical, group=period, color=drug_combo), size=10, shape='_') + 
    geom_line(data=pe_ref_df, aes(x=times, y=pe_vacc_target, group=interval), col='red')
  
}

gg2 = gg1 +  geom_point(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical), color='thistle')+
  geom_line(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical), color='thistle') 
  
gg2b = gg1b +  geom_point(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical), color='thistle')+
  geom_line(data=sim_results[sim_results$drug_combo=='vaccSMC',], aes(x=challenge_interval, y=pe_clinical), color='thistle')+
  geom_point(data=pe_df_sim, aes(x=challenge_interval, y=pe_clinical, group=period), size=10, shape='_', color='thistle')  
  



















pop_size = 100   # population size should be specified to be the same in all experiments and through time
num_runs = 3  # needs to be specified and the same for all simulations

# approximate values from slide shared by Paul Milligan
days_incub = 12
milligan_days = sapply(c(7,14,21,28,35,42) - days_incub, max, 0)
milligan_values = c(0.96, 0.91, 0.87, 0.72, 0.43, 0.05)

experiment_name_noSMC = 'SMC_efficacy_prevent_control_v5a'
experiment_name_SMC = 'SMC_efficacy_prevent_SPAQ_v5c'

# look at number of new clinical cases in the U5 population through the years in each experiment
sim_data_noSMC = read.csv(paste0(sim_output_base_filepath, '/', experiment_name_noSMC, '/dailyNewInfections.csv'))
sim_data_SMC = read.csv(paste0(sim_output_base_filepath, '/', experiment_name_SMC, '/dailyNewInfections.csv'))
experiment_list = list(experiment_name_SMC)

days_delay_challenge = sort(unique(sim_data_SMC$challenge_interval))

# subset to after SMC distribution date and subtract so that SMC distribution date is day 0
sim_data_noSMC = sim_data_noSMC[which(sim_data_noSMC$Day >= drug_day),]
sim_data_noSMC$Day = sim_data_noSMC$Day - drug_day
sim_data_SMC = sim_data_SMC[which(sim_data_SMC$Day >= drug_day),]
sim_data_SMC$Day = sim_data_SMC$Day - drug_day


# create a new plot for each C50 and kill combination
colnames(sim_data_SMC) = str_replace_all(colnames(sim_data_SMC), pattern='SP', replacement='Sulf')
if(!('Sulf_C50' %in% colnames(sim_data_SMC))){
  sim_data_SMC$Sulf_C50=10.8
  sim_data_SMC$Sulf_kill=1.6
}
# subset as necessary
Sulf_kill_subset=c(0.99)
Sulf_C50_subset = c(50)
if(length(Sulf_kill_subset)>0){
  sim_data_SMC = sim_data_SMC[sim_data_SMC$Sulf_kill %in% Sulf_kill_subset,]
}
if(length(Sulf_C50_subset)>0){
  sim_data_SMC = sim_data_SMC[sim_data_SMC$Sulf_C50 %in% Sulf_C50_subset,]
}
c50_values = sort(unique(sim_data_SMC$Sulf_C50))
kill_values = sort(unique(sim_data_SMC$Sulf_kill))
ncol_plot = length(c50_values)
nrow_plot = length(kill_values)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   plot of efficacy of SMC at preventing new clinical cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# get the number of individuals with clinical cases after challenge for the no-SMC simulations
num_cases_challenge_noSMC = rep(NA, length(days_delay_challenge))
for(dd in 1:length(days_delay_challenge)){
  num_cases_challenge_noSMC[dd] = sum(sim_data_noSMC$New_Clinical_Cases[sim_data_noSMC$challenge_interval == days_delay_challenge[dd]])/num_runs/pop_size*100
}

par(mfrow=c(nrow_plot, ncol_plot))
# get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
for (kk in 1:nrow_plot){
  for(cc in 1:ncol_plot){
    # get relevant section of simulations
    cur_sim_data_SMC = sim_data_SMC[intersect(which(sim_data_SMC$Sulf_C50 == c50_values[cc]), which(sim_data_SMC$Sulf_kill == kill_values[kk])),]
    # save the total number infected following the challenges on each challenge day
    num_cases_challenge = rep(NA, length(days_delay_challenge))
    for(dd in 1:length(days_delay_challenge)){
      num_cases_challenge[dd] = sum(cur_sim_data_SMC$New_Clinical_Cases[cur_sim_data_SMC$challenge_interval == days_delay_challenge[dd]])/num_runs/pop_size*100
    }

    plot(NA, ylim=c(0,pop_size*1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab='percent without clinical symptoms from challenge', xlab='days between SMC dose and exposure', bty='L', main=paste0('Sulf C50: ', c50_values[cc], '; kill: ', kill_values[kk]))
    lines(c(0,28-days_incub), c(0.85,0.85)*100, col=rgb(1,0,1,0.1), lwd=2)
    lines(c(29-days_incub,42-days_incub), c(0.61,0.61)*100, col=rgb(1,0,1,0.1), lwd=2)
    polygon(c(0,28-days_incub,28-days_incub,0), c(76,76,95,95), border=NA, col=rgb(1,0,1,0.05))
    polygon(x=c(29-days_incub,42-days_incub,42-days_incub,29-days_incub), y=c(47,47,72,72), border=NA, col=rgb(1,0,1,0.05))
    lines(days_delay_challenge, (100-num_cases_challenge_noSMC), col='lightgrey', type='b', pch=20, cex=1.5)
    lines(days_delay_challenge, (100-num_cases_challenge), col='darkblue', type='b', pch=20, cex=1.5)
    # lines(milligan_days, milligan_values*100, col=rgb(0,1,0,0.2), lwd=4)
  }
}
# par(mfrow=c(1,1))
# plot(NA, ylim=c(0,pop_size*1.1), xlim=c(min(days_delay_challenge), 50), ylab=c('percent without clinical','symptoms from challenge'), xlab='days between SMC dose and exposure', bty='L', main='')
# cols = c(rgb(102,194,165,maxColorValue=255), rgb(252,141,98,maxColorValue=255), rgb(141,160,203,maxColorValue=255), rgb(231,138,195,maxColorValue=255), rgb(255,217,47,maxColorValue=255))# rgb(166,216,84,maxColorValue=255))
# # for (kk in 1:nrow_plot){
# kk=6
# for(cc in 1:ncol_plot){
#   # get relevant section of simulations
#   cur_sim_data_SMC = sim_data_SMC[intersect(which(sim_data_SMC$Sulf_C50 == c50_values[cc]), which(sim_data_SMC$Sulf_kill == kill_values[kk])),]
#   # save the total number infected following the challenges on each challenge day
#   num_cases_challenge = rep(NA, length(days_delay_challenge))
#   for(dd in 1:length(days_delay_challenge)){
#     num_cases_challenge[dd] = sum(cur_sim_data_SMC$New_Clinical_Cases[cur_sim_data_SMC$challenge_interval == days_delay_challenge[dd]])/num_runs/pop_size*100
#   }
# 
#   # plot(NA, ylim=c(0,pop_size*1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab='percent without clinical symptoms from challenge', xlab='days between SMC dose and exposure', bty='L', main=paste0('Sulf C50: ', c50_values[cc], '; kill: ', kill_values[kk]))
#   lines(days_delay_challenge, (100-num_cases_challenge), col=viridis(5)[cc], type='l', pch=20, cex=1.5, lty=1, lwd=3)
#   lines(milligan_days, milligan_values*100, col=rgb(0,1,0,0.2), lwd=4)
#   # }
# }
# legend('bottomleft', legend=paste0('Sulf C50 = ', c50_values), bty='n', col=viridis(5), lwd=3)
# 
# 
# 
# 
# 
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   plot of efficacy of SMC at preventing new clinical cases -new  parameters only
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# sulf_c50_cur = 0.2
# sulf_kill_cur = 0.3
# pyri_c50_cur = 8
# pyri_kill_cur = 0.24
# par(mfrow=c(1,1))
# # get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
# 
# # get relevant section of simulations
# cur_sim_data_SMC = sim_data_SMC[intersect(intersect(intersect(which(sim_data_SMC$Sulf_C50 == sulf_c50_cur), which(sim_data_SMC$Sulf_kill == sulf_kill_cur)),which(sim_data_SMC$Pyri_C50 == pyri_c50_cur)), which(sim_data_SMC$Pyri_kill == pyri_kill_cur)),]
# # save the total number infected following the challenges on each challenge day
# num_cases_challenge = rep(NA, length(days_delay_challenge))
# for(dd in 1:length(days_delay_challenge)){
#   num_cases_challenge[dd] = sum(cur_sim_data_SMC$New_Clinical_Cases[cur_sim_data_SMC$challenge_interval == days_delay_challenge[dd]])/num_runs/pop_size*100
# }
# 
# plot(NA, ylim=c(0,pop_size*1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab='percent without clinical symptoms from challenge', xlab='days between SMC dose and exposure', bty='L', main=paste0('Sulf C50: ', sulf_c50_cur, '; kill: ', sulf_kill_cur))
# lines(c(0,28), c(0.85,0.85)*100, col=rgb(1,0,1,0.1), lwd=2)
# lines(c(29,42), c(0.61,0.61)*100, col=rgb(1,0,1,0.1), lwd=2)
# polygon(c(0,28,28,0), c(76,76,95,95), border=NA, col=rgb(1,0,1,0.05))
# polygon(x=c(29,42,42,29), y=c(47,47,72,72), border=NA, col=rgb(1,0,1,0.05))
# # lines(days_delay_challenge, (100-num_cases_challenge_noSMC), col='lightgrey', type='b', pch=20, cex=1.5)
# lines(days_delay_challenge, (100-num_cases_challenge), col='darkblue', type='b', pch=20, cex=1.5)
# # lines(milligan_days, milligan_values*100, col=rgb(0,1,0,0.2), lwd=4)
# 
# 
# 
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   plot of efficacy of SMC at reducing true prevalence
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 
# # get the average true prevalence on day 20 after challenge for the no-SMC simulations
# test_num_days_after_challenge = 20
# true_prev_challenge_noSMC = rep(NA, length(days_delay_challenge))
# for(dd in 1:length(days_delay_challenge)){
#   true_prev_challenge_noSMC[dd] = mean(sim_data_noSMC$True_Prevalence[intersect(which(sim_data_noSMC$challenge_interval == days_delay_challenge[dd]), which(sim_data_noSMC$Day == (days_delay_challenge[dd] + test_num_days_after_challenge)))])
# }
# 
# par(mfrow=c(nrow_plot, ncol_plot))
# 
# # get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
# for (kk in 1:nrow_plot){
#   for(cc in 1:ncol_plot){
#     # get relevant section of simulations
#     cur_sim_data_SMC = sim_data_SMC[intersect(which(sim_data_SMC$Sulf_C50 == c50_values[cc]), which(sim_data_SMC$Sulf_kill == kill_values[kk])),]
#     # save the total number infected following the challenges on each challenge day
#     true_prev_challenge = rep(NA, length(days_delay_challenge))
#     for(dd in 1:length(days_delay_challenge)){
#       true_prev_challenge[dd] = mean(sim_data_SMC$True_Prevalence[intersect(which(sim_data_SMC$challenge_interval == days_delay_challenge[dd]), which(sim_data_SMC$Day == (days_delay_challenge[dd] + test_num_days_after_challenge)))])
#     }
#     
#     plot(NA, ylim=c(0,1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab=paste('true prevalence', test_num_days_after_challenge, 'days after challenge'), xlab='days between SMC dose and exposure', bty='L', main=paste0('Sulf C50: ', c50_values[cc], '; kill: ', kill_values[kk]))
#     lines(days_delay_challenge, true_prev_challenge_noSMC, col='lightgrey', type='b', pch=20, cex=1.5)
#     lines(days_delay_challenge, true_prev_challenge, col='darkblue', type='b', pch=20, cex=1.5)
#   }
# }
# par(mfrow=c(1,1))
# 
# 
# 
# 
# 
# 
# 
# 
# experiment_lty = 1
# experiment_colors = c('black')
# experiment_plot_names = experiment_list[[1]]
# plot(NA, ylim=c(0,100), xlim=c(10,60))
# for(ee in length(experiment_list):1){
#   experiment_names = experiment_list[[ee]]
#   new_infections_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#   new_cases_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#   for(dd in 1:length(experiment_names)){
#     # look at number of new clinical cases in the U5 population through the years in each experiment
#     new_cases_data = read.csv(paste0(sim_output_base_filepath, '/', experiment_names[dd], '/dailyNewInfections.csv'))
#     # get rid of days before SMC
#     if('Day' %in% colnames(new_cases_data)){
#       new_cases_data = new_cases_data[new_cases_data$Day>drug_day[ee],]
#     }
#     for(rr in 1:num_runs){
#       new_cases_data_rr = new_cases_data[new_cases_data$Run_Number == (rr-1),]
#       # number of new infections in the scenario
#       new_infections_matrix[dd,rr] = sum(new_cases_data_rr$New_Infections)
#       # number of new clinical cases in the scenario
#       new_cases_matrix[dd,rr] = sum(new_cases_data_rr$New_Clinical_Cases)
#     }
#   }
#   # plot a line for each run
#   for(rr in 1:num_runs){
#     lines(days_delay_challenge, new_cases_matrix[,rr], col=experiment_colors[ee], type='b', lwd=1, lty=experiment_lty[ee])
#   }
# }
# legend('bottomright', experiment_plot_names, col=experiment_colors, lwd=2, bty='n', lty=experiment_lty)
# 
# 
# weeks_after_challenge = 15
# plot(NA, ylim=c(0,1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab=paste('true prevalence', weeks_after_challenge, ' weeks after challenge'), xlab='days between SMC dose and exposure', bty='L', main='SMC efficacy through time')
# 
# for(ee in 1:length(experiment_list)){
#   experiment_names = experiment_list[[ee]]
#   true_prev_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#   for(dd in 1:length(experiment_names)){
#     # look at number of new clinical cases in the U5 population through the years in each experiment
#     new_cases_data = read.csv(paste0(sim_output_base_filepath, '/', experiment_names[dd], '/dailyNewInfections.csv'))
#     for(rr in 1:num_runs){
#       new_cases_data_rr = new_cases_data[new_cases_data$Run_Number == (rr-1),]
#       # true prevalence three weeks after challenge
#       true_prev_matrix[dd,rr] = new_cases_data_rr$True_Prevalence[drug_day[ee]+(days_delay_challenge[dd]+7*weeks_after_challenge)]
#     }
#   }
#   # plot a line for each run
#   for(rr in 1:num_runs){
#     lines(days_delay_challenge, true_prev_matrix[,rr], col=experiment_colors[ee], type='b', lwd=1)
#   }
# }
# legend('bottomright', experiment_plot_names, col=experiment_colors, lwd=2, bty='n')
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ##################################################################################
# # efficacy of SP at clearing infections (reducing microscopy PfPR)
# ##################################################################################
# # sim output filder
# user = Sys.getenv("USERNAME")
# user_path = file.path("C:/Users",user)
# dropbox_filepath = paste(user_path, '/Dropbox (IDM)/Malaria Team Folder/projects', sep='')
# sim_output_base_filepath = paste0(dropbox_filepath, "/smc_effect_size/simulation_output")
# pop_size = 100   # population size should be specified to be the same in all experiments and through time
# num_runs = 5  # needs to be specified and the same for all simulations
# drug_day = 365 *2
# 
# experiment_name_SMC = "BF treatFail after SMC_v6"
# 
# # look at number of new clinical cases in the U5 population through the years in each experiment
# sim_data_SMC = read.csv(paste0(sim_output_base_filepath, '/', experiment_name_SMC, '/dailyNewInfections.csv'))
# sim_data_SMC = sim_data_SMC %>% group_by(Day, Sulf_C50, Sulf_kill, Pyri_C50, Pyri_kill) %>% summarise_all(mean) %>% ungroup()
# 
# # subset to after SMC distribution date and subtract so that SMC distribution date is day 0
# sim_data_SMC = sim_data_SMC[which(sim_data_SMC$Day >= drug_day),]
# sim_data_SMC$Day = sim_data_SMC$Day - drug_day
# 
# 
# # create a new plot for each C50 and kill combination
# c50_values = sort(unique(sim_data_SMC$Sulf_C50))
# kill_values = sort(unique(sim_data_SMC$Sulf_kill))
# pyri_c50_values = sort(unique(sim_data_SMC$Pyri_C50))
# pyri_kill_values = sort(unique(sim_data_SMC$Pyri_kill))
# 
# ncol_plot = length(c50_values)
# nrow_plot = length(kill_values)
# 
# 
# 
# 
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# #   plot of efficacy of SP at clearing infections (reducing microscopy PfPR)
# # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# 
# par(mfrow=c(nrow_plot, ncol_plot))
# # get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
# for (kk in 1:nrow_plot){
#   for(cc in 1:ncol_plot){
#     # get relevant section of simulations
#     cur_sim_data_SMC = sim_data_SMC[intersect(intersect(which(sim_data_SMC$Sulf_C50 == c50_values[cc]), which(sim_data_SMC$Sulf_kill == kill_values[kk])), which(sim_data_SMC$Day <40)),]
#     
#     plot(NA, ylim=c(0,1), xlim=c(0,40), ylab='percent with positive microscopy', xlab='days after SP', bty='L', main=paste0('Sulf C50: ', c50_values[cc], '; kill: ', kill_values[kk]))
#     lines(cur_sim_data_SMC$Day, cur_sim_data_SMC$Micros_Prevalence, col='darkblue', type='b', pch=20, cex=1.5)
#     text(x=20, y=0.9, paste0('failure=',round(max(cur_sim_data_SMC$Micros_Prevalence[20:39])*100), '%'))
#   }
# }
# par(mfrow=c(1,1))
# 
# 
# 
# 
# ncol_plot = length(pyri_kill_values) * length(pyri_c50_values)
# 
# par(mfrow=c(nrow_plot, ncol_plot))
# # get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
# for (kk in 1:nrow_plot){
#   for(cc in 1:length(pyri_c50_values)){
#     for(k2 in 1:length(pyri_kill_values)){
#       # get relevant section of simulations
#       cur_sim_data_SMC = sim_data_SMC[intersect(intersect(intersect(which(sim_data_SMC$Pyri_C50 == pyri_c50_values[cc]), which(sim_data_SMC$Pyri_kill == pyri_kill_values[k2])), which(sim_data_SMC$Sulf_kill == kill_values[kk])), which(sim_data_SMC$Day <40)),]
#       
#       plot(NA, ylim=c(0,1), xlim=c(0,40), ylab='percent with positive microscopy', xlab='days after SP', bty='L', main=paste0('Pyri C50: ', pyri_c50_values[cc],'; Pyri kill: ', pyri_kill_values[k2], '; kill: ', kill_values[kk]))
#       lines(cur_sim_data_SMC$Day, cur_sim_data_SMC$Micros_Prevalence, col='darkblue', type='b', pch=20, cex=1.5)
#       text(x=20, y=0.9, paste0('failure=',round(max(cur_sim_data_SMC$Micros_Prevalence[20:39])*100), '%'))
#       
#     }
#   }
# }
# 
# 
# 
# # which cominations are within some bounds of desired failure rate?
# lower_bound = 0.25
# upper_bound = 0.35
# # get 28-day failure rate (maximum microscopy prevalence within 40 days after treatment) for each drug PD parameter set
# failure_rates_array = array(NA, dim=c(length(kill_values), length(c50_values), length(pyri_kill_values), length(pyri_c50_values)))
# # get the number of individuals with clinical cases after challenge for the with-SMC simulations and plot
# for (kk in 1:length(kill_values)){
#   for(cc in 1:length(c50_values)){
#     for(k2 in 1:length(pyri_kill_values)){
#       for(c2 in 1:length(pyri_c50_values)){
#         # get relevant section of simulations
#         cur_sim_data_SMC = sim_data_SMC[intersect(intersect(intersect(intersect(which(sim_data_SMC$Pyri_C50 == pyri_c50_values[c2]), which(sim_data_SMC$Pyri_kill == pyri_kill_values[k2])), which(sim_data_SMC$Sulf_C50 == c50_values[cc])), which(sim_data_SMC$Sulf_kill == kill_values[kk])), which(sim_data_SMC$Day <60)),]
#         failure_rates_array[kk,cc,k2,c2] = max(cur_sim_data_SMC$Micros_Prevalence[20:29])
#         if(max(cur_sim_data_SMC$Micros_Prevalence[20:29])>lower_bound & max(cur_sim_data_SMC$Micros_Prevalence[20:29])<upper_bound){
#           print(paste0('28-day failure: ', round(max(cur_sim_data_SMC$Micros_Prevalence[20:29]), 2), ' - Pyri C50: ', pyri_c50_values[c2], '; Pyri kill: ', pyri_kill_values[k2], '; Sulf C50: ', c50_values[cc], '; Sulf kill: ', kill_values[kk]))
#         }
#       }
#     }
#   }
# }
# # length(intersect(which((failure_rates_array>(target - bound)), arr.ind=TRUE), which((failure_rates_array<(target + bound)), arr.ind=TRUE)))
# sets_in_range = (which((failure_rates_array>lower_bound)& failure_rates_array<upper_bound, arr.ind=TRUE))
# sort(kill_values[unique(sets_in_range[,1])])
# sort(c50_values[unique(sets_in_range[,2])])
# sort(pyri_kill_values[unique(sets_in_range[,3])])
# sort(pyri_c50_values[unique(sets_in_range[,4])])
# 
# sulf_k = kill_values[sets_in_range[,1]]
# sulf_c = c50_values[sets_in_range[,2]]
# pyri_k = pyri_kill_values[sets_in_range[,3]]
# pyri_c = pyri_c50_values[sets_in_range[,4]]
# params_in_range = data.frame(sulf_k, sulf_c, pyri_k, pyri_c)
# 
# par(mfrow=c(3,3))
# for(ii in 1:nrow(params_in_range)){
#   # get relevant section of simulations
#   cur_sim_data_SMC = sim_data_SMC[intersect(intersect(intersect(intersect(which(sim_data_SMC$Pyri_C50 == params_in_range$pyri_c[ii]), which(sim_data_SMC$Pyri_kill == params_in_range$pyri_k[ii])), which(sim_data_SMC$Sulf_C50 ==params_in_range$sulf_c[ii])), which(sim_data_SMC$Sulf_kill ==params_in_range$sulf_k[ii])), which(sim_data_SMC$Day <60)),]
#   
#   plot(NA, ylim=c(0,1), xlim=c(0,60), ylab='percent with positive microscopy', xlab='days after SP', bty='L', main=paste0('Pyri C50: ', params_in_range$pyri_c[ii],'; Pyri kill: ', params_in_range$pyri_k[ii], '; Sulf C50: ', params_in_range$sulf_c[ii], '; Sulf kill: ', params_in_range$sulf_k[ii]))
#   lines(cur_sim_data_SMC$Day, cur_sim_data_SMC$Micros_Prevalence, col='darkblue', type='b', pch=20, cex=1.5)
#   text(x=20, y=0.9, paste0('28-day failure=',round(max(cur_sim_data_SMC$Micros_Prevalence[20:29])*100), '%'))
# }
# 
# 
# # what is failure rate for current default parameters
# cur_sim_data_SMC = sim_data_SMC[intersect(intersect(intersect(intersect(which(sim_data_SMC$Pyri_C50 == 2), which(sim_data_SMC$Pyri_kill == 0.6)), which(sim_data_SMC$Sulf_C50 == 0.2)), which(sim_data_SMC$Sulf_kill == 0.5)), which(sim_data_SMC$Day <60)),]
# max(cur_sim_data_SMC$Micros_Prevalence[20:29])
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ######################
# # old plots, not finished updating
# 
# 
# 
# experiment_names_noSMC = c(
#   'SMC_effect_challenge_control_v1'
# )
# experiment_names_wSMC = c(
#   'SMC_effect_challenge_v1'
# )
# 
# 
# experiment_list = list(experiment_names_noSMC, experiment_names_wSMC)
# experiment_plot_names = c('no SMC', 'with SMC')
# days_delay_challenge = c(20,40,60,80,100)
# num_each = length(days_delay_challenge)
# drug_day = c(rep(365*2, 2))
# experiment_colors = c('darkred','blue')
# experiment_lty = c(3,3)
# 
# 
# 
# 
# plot(NA, ylim=c(0,pop_size*1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab='number with clinical symptoms from challenge', xlab='days between SMC dose and exposure', bty='L', main='SMC efficacy through time')
# 
# for(ee in length(experiment_list):1){
#   experiment_names = experiment_list[[ee]]
#   for(e2 in 1:length(experiment_names)){
#     new_infections_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#     new_cases_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#     new_cases_data = read.csv(paste0(sim_output_base_filepath, '/', experiment_names[e2], '/dailyNewInfections.csv'))
#     for(dd in 1:length(days_delay_challenge)){
#       cur_days_delay = days_delay_challenge[dd]
#       # look at number of new clinical cases in the U5 population through the years in each experiment
#       # get rid of days before SMC
#       if('Day' %in% colnames(new_cases_data)){
#         new_cases_data = new_cases_data[new_cases_data$Day>drug_day[ee],]
#       }
#       for(rr in 1:num_runs){
#         new_cases_data_rr = new_cases_data[new_cases_data$Run_Number == (rr-1),]
#         # number of new infections in the scenario
#         new_infections_matrix[dd,rr] = sum(new_cases_data_rr$New_Infections)
#         # number of new clinical cases in the scenario
#         new_cases_matrix[dd,rr] = sum(new_cases_data_rr$New_Clinical_Cases)
#       }
#     }
#     # plot a line for each run
#     for(rr in 1:num_runs){
#       lines(days_delay_challenge, new_cases_matrix[,rr], col=experiment_colors[ee], type='b', lwd=1, lty=experiment_lty[ee])
#     }
#   }
#  
# }
# legend('bottomright', experiment_plot_names, col=experiment_colors, lwd=2, bty='n', lty=experiment_lty)
# 
# 
# weeks_after_challenge = 15
# plot(NA, ylim=c(0,1.1), xlim=c(min(days_delay_challenge), max(days_delay_challenge)), ylab=paste('true prevalence', weeks_after_challenge, ' weeks after challenge'), xlab='days between SMC dose and exposure', bty='L', main='SMC efficacy through time')
# 
# for(ee in 1:length(experiment_list)){
#   experiment_names = experiment_list[[ee]]
#   true_prev_matrix = matrix(NA, nrow=length(days_delay_challenge), ncol=num_runs)  # rows are day of challenge, columns are runs
#   for(dd in 1:length(experiment_names)){
#     # look at number of new clinical cases in the U5 population through the years in each experiment
#     new_cases_data = read.csv(paste0(sim_output_base_filepath, '/', experiment_names[dd], '/dailyNewInfections.csv'))
#     for(rr in 1:num_runs){
#       new_cases_data_rr = new_cases_data[new_cases_data$Run_Number == (rr-1),]
#       # true prevalence three weeks after challenge
#       true_prev_matrix[dd,rr] = new_cases_data_rr$True_Prevalence[drug_day[ee]+(days_delay_challenge[dd]+7*weeks_after_challenge)]
#     }
#   }
#   # plot a line for each run
#   for(rr in 1:num_runs){
#     lines(days_delay_challenge, true_prev_matrix[,rr], col=experiment_colors[ee], type='b', lwd=1)
#   }
# }
# legend('bottomright', experiment_plot_names, col=experiment_colors, lwd=2, bty='n')
# 
