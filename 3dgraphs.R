library(ggplot2)
library(plotly)
library(dplyr)
library(reshape2)
library(tidyr)
?gather
smpsData <- read.csv(file='AIM1.csv', as.is = 1)
head(smpsData)
smpsLong <- smpsData[,-2] %>%
  gather(key= smpno, value = nconc, X2:X23)
head(smpsLong)
smpsLong$smpno <- as.numeric(extract_numeric(smpsLong$smpno))

ggplot(data = smpsLong, aes(x= diameter, y= smpno, fill=nconc))+
  geom_tile()+scale_x_log10()

ggplot(smpsLong, aes(diameter, smpno)) +
  geom_raster(aes(fill = nconc), interpolate = TRUE)+scale_x_log10()+
  
  theme_bw()+
  scale_fill_gradientn(colours=c("blue","red", "yellow"))
heatmap(as.matrix(smpsLong))
head(smpsLong)

hist(rnorm(5000, mean = 100, sd= 30))
