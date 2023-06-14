# compare_IR_MAP2017_versus_report2019
# plot comparison of 2017 IR from MAP versus 2019 IR from Burundi report

library(ggplot2)

hbhi_dir = 'C:/Users/moniqueam/Dropbox (IDM)/Malaria Team Folder/projects/burundi_hbhi/snt_2023'
ir_compare = read.csv(paste0(hbhi_dir, '/ento/insecticide_resistance_DS_means/IR_MAP2017_versus_report2019.csv'))

gg = ggplot(ir_compare, aes(x=1-X2017, y=1-X2019))+
  geom_point(color=rgb(0.1,0.4,0.85), size=2)+
  geom_abline(intercept=0, slope=1)+
  ylab('Resistance from 2019 report') +
  xlab('Resistance from 2017 MAP estimate') +
  xlim(0, 0.5)+
  ylim(0,0.5)+
  theme_bw()


ggsave(filename=paste0(hbhi_dir, '/ento/insecticide_resistance_DS_means/IR_MAP2017_versus_report2019.png'), gg, width=3, height=3)


