# plot_map_xLH_each_admin_each_seed.R


library(pals)
library(GISTools) 
library(rgdal)
library(raster)
library(prettyGraphs)

# ============================================================================ #
# =========================     USER SPECIFIED     =========================== #
# ============================================================================ #
country = 'BDI'  #'SLE'  # 'BDI'
expt_name = 'PfPR_sweep_main_BDI_v2'

if(country =='BDI'){
  hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'
  # read in shapefile with admin boundaries
  admin_shape = shapefile(paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp'))
  admin_shape$NAME_1 = admin_shape$NOMREGION  # NOMDEP should be admin level where modeling occurs (e.g., DS, LGA), NOMREGION should be one admin above that, NAME_1 should be one above that
  admin_shape$NOMREGION = admin_shape$NOMREGION
  admin_shape$admin_name = admin_shape$NOMDEP
}

xLHs = read.csv(paste0(hbhi_dir, '/simulation_inputs/larval_habitats/larval_habitat_multipliers_v1.csv'))
shapefile_admins = admin_shape$admin_name
min_log_xLH = min(log(xLHs$Habitat_Multiplier))
max_log_xLH = max(log(xLHs$Habitat_Multiplier))
colors_range_min_to_max = add.alpha(pals::parula(101), alpha=0.5)  # specify colorscale. To go from a value vv to a color, take colors_range_0_to_max[1+round(vv*100)]

pdf(paste0(hbhi_dir, '/simulation_output/transmission_intensity_calibration/', expt_name, '/plot_map_xLH_each_seed.pdf'), useDingbats=FALSE, width=4*3, height=5*ceiling((length(unique(xLHs$Run_Number))+1)/3))
par(mfrow=c(ceiling((length(unique(xLHs$Run_Number))+1)/3), 3))
for(rr in unique(xLHs$Run_Number)){
  xLHs_cur = xLHs[xLHs$Run_Number == rr,]
  admin_shape_xLH = merge(admin_shape, xLHs_cur, by='admin_name')
  plot(admin_shape_xLH, col=colors_range_min_to_max[round((log(admin_shape_xLH$Habitat_Multiplier) - min_log_xLH) / (max_log_xLH-min_log_xLH) * 100) + 1], main=paste0('seed ', rr))
  
}
# legend - colorbar
legend_image = as.raster(matrix(rev(colors_range_min_to_max[1+round(seq((min_log_xLH - min_log_xLH) / (max_log_xLH-min_log_xLH), (max_log_xLH - min_log_xLH) / (max_log_xLH-min_log_xLH), length.out=20)*100)]), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'habitat multiplier')
text(x=1.2, y = seq(0,1,length.out=5), labels = round(exp(seq(min_log_xLH,max_log_xLH,length.out=5))))
rasterImage(legend_image, 0.8, 0, 1.1, 1)

dev.off()
