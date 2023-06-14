# explore_within_admin_heterogeneity.R
# how far from admin-level mean are the individual cluster samles?

# ie. if recorded mean is true mean, what is probability of observing DHS cluster-level samples?


country = 'BDI'  #'SLE'  # 'BDI'
dta_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/data'
variables = c('mic','itn_all','itn_u5','itn_o5','iptp','cm','blood_test')
num_samples = 50

if(country =='BDI'){
  hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi'
  years = c(2010, 2012, 2016)
} else if(country == 'SLE'){
  hbhi_dir = 'C:/Users/mambrose/Dropbox (IDM)/Malaria Team Folder/projects/SierraLeone_hbhi'
  years = c(2008, 2013, 2016, 2019)
}

max_survey_size = 300

for(yy in 1:length(years)){
  # admin-level results
  admin_sums = read.csv(paste0(hbhi_dir, '/estimates_from_DHS/DHS_admin_minN30_', years[yy], '.csv'))[,-1]
  
  # cluster-level results
  cluster_sums = read.csv(paste0(hbhi_dir, '/estimates_from_DHS/DHS_cluster_outputs_', years[yy], '.csv'))[,-1]
  
  png(paste0('C:/Users/mambrose/Downloads/within_admin_heterogeneity_',years[yy],'.png'), width=8.5, height=5.5, units='in',res=300)
  par(mfrow=c(2,3))
  for(i_var in 1:6){#length(variables)){
    var = variables[i_var]
    plot(NA, xlim=c(0,1), ylim=c(0,1), xlab='estimated admin-level value', ylab='observed cluster-level values', bty='L', main=paste0(var, '; year ', years[yy]))
    lines(c(0,1), c(0,1))
    if(paste0(var,'_num_total') %in% colnames(cluster_sums)){
      
      
      # iterate through admins, extracting mean and then looking at how often we reject the null hypothesis that survey is drawn from same population, given survey sizes
      for(i_admin in 1:nrow(admin_sums)){
        # which clusters are in this admin
        cluster_rows = which(cluster_sums$NAME_3 == admin_sums$NAME_3[i_admin])
        cur_mean = admin_sums[[paste0(var, '_rate')]][i_admin]
        if(length(cluster_rows)>0){
          for(i_clust in 1:length(cluster_rows)){
            within_95_interval_col = rgb(1,0.5,0.5,0.5)
            clust_size = cluster_sums[[paste0(var,'_num_total')]][cluster_rows[i_clust]]
            if(!is.na(clust_size)){
              observed_true = cluster_sums[[paste0(var,'_num_true')]][cluster_rows[i_clust]]
              # given the assumed 'true' mean and the survey size, 95% of of the time, we would expect the survey result to fall within these bounds
              bounds_95 = c(qbinom(p=0.025, size=clust_size, prob=cur_mean), qbinom(p=0.975, size=clust_size, prob=cur_mean))
              if((observed_true>=bounds_95[1]) & (observed_true<=bounds_95[2])){
                # within_95_interval[i_clust] = 1
                within_95_interval_col = rgb(0.5,0.5,1,0.5)
              }
              # points(cur_mean, observed_true/clust_size, cex=clust_size/(max_survey_size/3), pch=20, col=rainbow(140, alpha=0.5)[100-round(abs(cur_mean - observed_true/clust_size)/cur_mean*100)])
              points(cur_mean, observed_true/clust_size, cex=clust_size/(max_survey_size/4), pch=20, col=rgb(0,0,0,0.5))#col=within_95_interval_col)
            }
          }
        }
      }
    }
  }
  dev.off()
}
