
library(ggplot2); library(dplyr); library(corrr):library(caret)
#
      alldata<- read.csv('all_for_graphs.csv')
     # alldata.orig<- alldata
      alldata$vg <- 0.3*0.1*alldata$vg #here i converted vg from velosity to flowrate of intake air
      str(alldata)
      
      
# tg out for pack column is filled with NAs to fix this, I use preprocess
      
      tgoutpreprocess <- preProcess(alldata[, 1:6], method = 'bagImpute')
      tgoutimpuned <- predict(tgoutpreprocess, alldata[,1:6])
      alldata$tgout <- tgoutimpuned$tgout
      
#rls

      rls.data <- alldata[alldata$device=='rls',]
      rls.data[,1:16] %>%
        correlate()%>%
        network_plot(colours = c('blue', 'white', 'red'),
                     min_cor=.5)
      ggplot(rls.data, aes(x= vg, y= dustrak))+
        geom_point()
      
    plot01<-  ggplot(rls.data,  aes(x= vg, y=driftmgkg))+
        geom_point()+stat_smooth(method = 'lm')+
        labs(title = 'Solvent drift VS Air flowrate in RLS', x= 'Air flowrate (m3/s)', y= 'Solvent drift (mg/kg)')+
        theme_bw()
      
      plot02<- ggplot(rls.data,  aes(x= vg, y=dustrak))+
        geom_point()+stat_smooth(method = 'lm')+
        labs(title = 'PM10 emissions VS Air flowrate', x= 'Air flowrate (m3/s)', y= 'PM10 emissions (mg/m3)')+
        theme_bw()

#Cooling tower
      ct.data <- alldata[alldata$device=='ct',]
      ct.data[,1:16] %>%
        correlate()%>%
        network_plot(colours = c('blue', 'white', 'red'),
                     min_cor=.5)
      plot03<- ggplot(ct.data, aes(x= disb, y= mass/1000))+geom_boxplot()+ylim(c(0,0.050))+
        theme_bw()+labs(title = 'PM10 emissions VS Dristributor type in cooling tower',x= 'Distributor type',y= 'PM10 emissions (mg/m3)')
      
      
      
      
      plot04<- ggplot(ct.data, aes(x= vg, y= driftmgkg))+
        geom_point()+facet_wrap(.~packing)+
        ylim(c(0.02,38/1000))+stat_smooth(method = 'lm')+ 
        theme_bw()+ labs(title = 'Solvent drift VS Air flowrate in cooling tower', x= 'Air flowrate (m3/s)', y= 'Solvent drift (%)')
      
      plot05 <- ggplot(ct.data, aes(x= vg, y= dustrak))+
        geom_point()+facet_wrap(.~packing)+
        ylim(c(0,38/1000))+stat_smooth(method = 'lm')+
        theme_bw()+ labs(title = 'PM10 emissions VS Air flowrate in cooling tower', x= 'Air flowrate (m3/s)', y= 'PM10 emissions (mg/m3)')

      plot06 <- ggplot(ct.data, aes(x= vg, y= dustrak))+
        geom_point()+facet_wrap(packing~disb)+
        ylim(c(0,40/1000))+stat_smooth(method = 'lm')+
        theme_bw()+ labs(title = 'PM10 emissions VS Air flowrate in cooling tower', x= 'Air flowrate (m3/s)', y= 'PM10 emissions (mg/m3)')
      
            
      plot07 <- ggplot(ct.data, aes(x=as.factor(ct.data$fliq)))+
        geom_boxplot(aes( y= dustrak), color='blue')+ ylim(c(0,0.05))+
        theme_bw()+labs(title = 'PM10 emissions VS Solvent flow rate in cooling tower', x='Solvent flowrate (lit/min)', y= 'Particulate matter mass concentration (mg/m3)')+
        annotate("text", x =1, y = 0.01, label = "PM2.5",color='red')+
        geom_boxplot(aes( y= mass/2000), color= 'red')+
        annotate("text", x =1, y = 0.04, label = "PM10",color='blue')
        #geom_boxplot(aes(y= driftmgkg), color='green', alpha=.4) if you want to add the drift the pm figure
        
      #ggplot(ct.data, aes(x=as.factor(ct.data$fliq)))+
      #geom_boxplot(aes(y= mass/1000))+ ylim(c(0,.02))+
      #  theme_bw()+labs(title = 'PM2.5 emissions VS Solvent flow rate', x='Solvent flowrate (lit/min)', y= 'PM2.5 mg/m3')
      
      plot08 <- ggplot(ct.data, aes(x= as.factor(ct.data$fliq)))+
        geom_boxplot(aes(y= driftmgkg)) + ylim(c(0,.1))+
        theme_bw()+labs(title = 'Solvent drift Vs Solvent flow rate', x='Solvent flowrate (lit/min)', y= 'Drift (mg/kg)')
      
#pack column
      pc.data <- alldata[alldata$device=='pc',]
      
      
      pc.data[,1:16] %>% correlate() %>%
        network_plot(colours = c('blue', 'white', 'red'),
                     min_cor=.3)
      
      
      ggplot(pc.data, aes(x= as.factor(pc.data$fliq), y= dustrak/2+mass/2000))+geom_point()+facet_wrap(.~packing)

      ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= mass))+geom_point()+facet_wrap(medium~packing)
      
      ggplot(pc.data,aes(x= ambpm, y= dustrak))+geom_point()
      
      ggplot(pc.data, aes(x= fliq, y= driftmgkg))+geom_point()+stat_smooth(method = 'lm')+ylim(c(0,.01))
      ggplot(pc.data, aes(x= vg, y= driftmgkg))+geom_point()+stat_smooth(method = 'lm')+ylim(c(0,.0075))
      #ggplot(pc.data, aes(x= dp, y= driftmgkg))+geom_point()+ stat_smooth(method = 'lm')      
      plot09 <- ggplot(pc.data, aes(x= vg, y= dp))+
        geom_point(aes(color= packing))+
        stat_smooth(method = 'lm', aes(color= packing))+
        theme_bw()+
        labs(title= 'Pressure drop VS air flowrate in packed column')

      
      plot10 <- ggplot(pc.data, aes(x= as.factor(pc.data$fliq), y= dustrak/2+mass/2000))+
      geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Solvent flowrate in packed column', x='Sovent flowrate (lit/min)', y= 'PM10 emissions (mg/m3)')
      
      
      
      
      plot11 <- ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= dustrak/2+mass/2000))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Air flowrate in packed column', x='Air flowrate (m3/s)', y= 'PM10 emissions (mg/m3)')
      
      plot12 <- ggplot(pc.data, aes(x= as.factor(pc.data$fliq), y= driftmgkg))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'Sovent drift VS Solvent in packed column', x='Sovent flowrate (lit/min)', y= 'Solvent drift (mgr/kg)')
      
      plot13 <- ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= driftmgkg))+geom_boxplot()+facet_wrap(.~packing)+
        theme_bw()+labs(title = 'Sovent drift VS Air flowrate in packed column', x='Air flowrate (m3/s)', y= 'Solvent drift (mgr/kg)')
      
      plot14 <- ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= dustrak/2+mass/2000))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Air flowrate in packed column', x='Air flowrate (m3/s)', y= 'PM10 emissions (mg/m3)')

      
      
      
#all   
    plot15 <- ggplot(alldata, aes(x= co2in, y= nc))+
        geom_point(aes(color= device))+
      # geom_jitter()+
        xlim(c(350,450))+ylim(c(0,100000))+
        stat_smooth(method= 'lm', aes(color= device))+
        theme_bw()+
        labs(title= 'Particle number concentration VS ambient CO2 concentration', 
             x= ' Carbon dioxide concentration (PPM)',
             y= 'Number concentration (#/cm3)')
        
      
      plot16 <- ggplot(alldata, aes(x= device, y= nc)) +
        geom_boxplot()+ theme_bw()+ xlim(c( 'pc','ct', 'rls'))+
        ylim(c(0, 60000))+
        labs(title= 'Particle number concentration for different contactors', 
             x= ' Device',
             y= 'Number concentration (#/cm3)')

      plot17 <- ggplot(alldata, aes(x= device, y= driftmgkg)) +
        geom_boxplot()+ theme_bw()+ ylim(c(0,.1))+
        labs(title= 'Solvent drift for different contactors', 
             x= ' Device',
             y= 'Drift (mg/kg)')
      
      
      
      plot18 <- ggplot(alldata, aes(x= device, y= cmd)) +
        geom_boxplot()+ theme_bw()+ xlim(c( 'ct','rls', 'pc'))+
        labs(title= 'Particulate matter median diameter for different contactors', 
             x= ' Device',
             y= 'Median diameter (nm)')+
        ylim(c(20, 100))
      
      
      ggplot(alldata, aes(x= vg, y= dustrak))+ 
        facet_grid(.~device)+
        geom_point(aes(size=log10(fliq), color= packing), alpha=.5)+
        stat_smooth(method = 'lm', aes(color= packing))+theme_bw()
     #drift for all   
      plot19 <- ggplot(alldata, aes(x= vg, y= driftmgkg))+ 
        facet_grid(.~device)+
        geom_point(aes(size=fliq, color= packing), alpha=.5)+
        stat_smooth(method = 'lm', aes(color= packing))+theme_bw()+
        labs(title='Solvent loss VS Air flowrate', x='Air flowrate (m3/s)', y='Drift (mg/kg)')+ ylim(c(0,.1))
      
      #solvwnt loss mg per hr
      
      #drift for all   
      plot20 <- ggplot(alldata, aes(x= vg, y= driftmgkg*fliq*1.2*60))+ 
        facet_grid(.~device)+
        geom_point(aes(size=fliq, color= packing), alpha=.5)+
        stat_smooth(method = 'lm', aes(color= packing))+theme_bw()+
        labs(title='Solvent loss VS Air flowrate', x='Air flowrate (m3/s)', y='Solvent loss rate (mg/hr)')

     
        
     alldatatabulte<-alldata %>% 
        group_by( device,packing,disb, medium, fliq) %>% 
        summarise(avgdrift= mean(driftmgkg), 
                  avgairflowrate=mean(vg), 
                  avgsolventloss= mean(driftmgkg*fliq*1.2*60), 
                  avgco2diff= mean(co2diff/co2in*100))  
    alldatatabulte[,5:9] <-  round(alldatatabulte[,5:9],4)
     write.csv(alldatatabulte, 'avgdata.csv')
     round(alldatatabulte[,5:9],4)
     
     
#outputs of the code     
plot01
plot02
plot03     
plot04
plot05
plot06
plot07
plot08
plot09
plot10
plot11
plot12
plot13
plot14
plot15
plot16
plot17
plot18
plot19
plot20
#tabulated analysed data
write.csv(alldatatabulte, 'avgdata.csv')
round(alldatatabulte[,5:9],4)