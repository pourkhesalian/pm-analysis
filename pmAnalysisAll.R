library(lubridate); library(ggplot2); library(dplyr); 
library(stringr);library(devtools);library(GGally); 
library(dplyr);library(reshape2)
library(tidyr)
library(caret)
library(corrr)

      rm(list = ls())
#read the csv file
      originalData <- read.csv(file = 'all_for_R.csv', as.is = TRUE)
      originalData$mass[1:11] <- paste(originalData$mass[1:11],
                                       '(µg/m³)', sep = '') #adding 

# converting to numeric variable
      originalData$mass <- str_sub(originalData$mass, end = -8)
      originalData$vol <- str_sub(originalData$vol, end = -10)
      originalData$nc <- str_sub(originalData$nc , end = -8)
      originalData$Dustrak <- as.numeric(originalData$Dustrak)
      originalData$mass <- as.numeric(originalData$mass)
      originalData$vol <- as.numeric(originalData$vol)
      originalData$nc <- as.numeric(originalData$nc)
      originalData$pmmghour <- as.numeric(originalData$pmmghour)
      originalData$driftmgkg <- as.numeric(originalData$driftmgkg)



#to lower case

      colnames(originalData) <- tolower(names(originalData))



#preliminary graphs
      ggplot(originalData, aes(x= vg, y= dustrak))+ 
        geom_point(aes(color= type))+ geom_smooth(method = 'lm', aes(color= type))+ theme_bw()+scale_y_log10()
      
      ggplot(originalData, aes(x= fliq, y= dustrak))+ 
        geom_point(aes(color= type))+ geom_smooth(method = 'lm', aes(color= type))+
        scale_x_log10()+xlim(c(0,400))

#separating different types
      coolingtower <- originalData %>%
        filter(type=='ct' & (disb=='hc' | disb=='fc'| disb=='sp_3arm'))
      rls <- originalData %>% 
        filter(type=='rls')
      packcolumn <-  originalData %>% 
        filter(type=='pc' | type== 'pci')

#selecting variables
      coolingtower <- coolingtower %>% select(-time, -run.,-type, -tliqo, -medium,-fliqkghour, -flowrate1, -tliqo, -vol,-pmmghour)
      rls <- rls %>% select(-time, -run.,-type, -tliqo, -packing, -disb,-medium, -fliq,-fliqkghour, -flowrate1, -tliqo, -vol,-pmmghour)
      packcolumn <- packcolumn %>%
        select(-time, -run.,-type, -disb,-fliqkghour, -flowrate1,-tgout, -tliqo, -vol,-pmmghour)

#more graph
    plot01<-  ggplot(coolingtower, aes(x= disb, y= mass)) + 
        ylim(c(0,50))+
        geom_boxplot()+ theme_bw()+
        ggtitle('PM10 emissions from cooling tower by different distributors')+
        xlab('Distributor type')+ylab('PM10 emissions (\u03BCgr/m3)')

#perparation for imputing NAs
#converting factors from chars to numbers
#for cooing tower
      unique(coolingtower$packing)
      unique(coolingtower$disb)
      unique(coolingtower$medium)
      coolingtower$packing <- ifelse(coolingtower$packing=='o',1,2) # 1=original packing, 2= new packing
      coolingtower$disb[coolingtower$disb=='hc']<- 1 #1= half cone, original nozzle
      coolingtower$disb[coolingtower$disb=='sp_3arm']<- 2 #2= 3-arm sprinkler
      coolingtower$disb[coolingtower$disb=='fc']<- 3 #3= full cone nozzle
      
      coolingtowernumeric <- apply(coolingtower,2, as.numeric)
      head(coolingtowernumeric)
#for packcolumn
      unique(packcolumn$medium)
      packcolumn$medium <- ifelse(packcolumn$medium=='caustic', 0,1) # 0=caustic, 1= taurate      
      unique(packcolumn$packing)
      packcolumn$packing[packcolumn$packing=='ss'] <- 1 #1=  ss304     
      packcolumn$packing[packcolumn$packing=='pp'] <- 2 #2= plastic      
      packcolumn$packing[packcolumn$packing=='cp'] <- 3 #3= curtin uni structured packing      
      packcolumnnumeric <- apply(packcolumn, 2,as.numeric)  
#for rls
      rlsnumeric <- apply(rls, 2,as.numeric)
head(rls)      


#impute for nas 
      packcolumnpreprocess<- preProcess(packcolumnnumeric, method = 'bagImpute')
      packcolumnimputed <- predict(packcolumnpreprocess, packcolumnnumeric)       

      rlspreprocess <- preProcess(rlsnumeric, method = 'bagImpute')
      rlsimputed <- predict(rlspreprocess, rlsnumeric)      

      coolingtowerpreprocess <- preProcess(coolingtowernumeric, method = 'bagImpute')
      coolingtowerimputed <- predict(coolingtowerpreprocess, coolingtowernumeric)            
#correaltion findig

      #cooling tower      
   coolingtowerimputed %>% 
     correlate() %>%
     network_plot(min_cor = .3, 
                  colours= c('blue','white','red'),
                  repel =1)
   
#rls tower      
   rlsimputed %>% 
     correlate() %>%
     network_plot(min_cor = .3, 
                  colours= c('blue','white','red'),
                  repel =1)
#pack column      
   packcolumnimputed %>% 
     correlate() %>%
     network_plot(min_cor = .3, 
                  colours= c('blue','white','red'),
                  repel =1)
#rejoin the dataframe
  rlsanalysis <-  rlsimputed %>%
    data.frame()
  packcolumanalysis <- packcolumnimputed %>%
    data.frame()
  coolingtoweranalysis <- coolingtowerimputed %>%
    data.frame()
rlspackcolumnanalysis <- full_join(rlsanalysis,packcolumanalysis)

allanalysis<- full_join(rlspackcolumnanalysis, coolingtoweranalysis)
plot01
