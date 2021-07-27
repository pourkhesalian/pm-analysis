#new graphs from the averaged data
library(ggplot2)

avgdata <- read.csv('avgDriftForR.csv')
str(avgdata)
ggplot(data= avgdata, aes(x= vg, y= kgdriftpabs, color= device))+
  geom_point(aes(size= fliq, shape= packing))+
  ylim(c(0,5))+ 
  geom_smooth(method = 'lm')+
  theme_bw()+
  labs(title = 'Solvent loss VS air velocity for different devices and defferent flowrates', 
       x='Air velocity (m/s)', 
       y= 'kg Solvent loss per ton of CO2 absorbed (kg/ton)')
#avg analysis