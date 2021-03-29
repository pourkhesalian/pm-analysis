
library(ggplot2); library(dplyr); library(corrr):library(caret)
#
      alldata<- read.csv('all_for_graphs.csv')
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
      
      ggplot(rls.data,  aes(x= vg, y=driftmgkg))+
        geom_point()+ ylim(c(0, 6e-4))+stat_smooth(method = 'lm')+
        labs(title = 'Solvent drift VS intake air velocity', x= 'Intake air velocity (m/s)', y= 'Solvent drift (mg/kg)')+
        theme_bw()
      
      ggplot(rls.data,  aes(x= vg, y=dustrak))+
        geom_point()+stat_smooth(method = 'lm')+
        labs(title = 'PM10 emissions VS intake air velocity', x= 'Intake air velocity (m/s)', y= 'PM10 emissions (mg/m3)')+
        theme_bw()

#Cooling tower
      ct.data <- alldata[alldata$device=='ct',]
      ct.data[,1:16] %>%
        correlate()%>%
        network_plot(colours = c('blue', 'white', 'red'),
                     min_cor=.5)
      ggplot(ct.data, aes(x= disb, y= mass/1000))+geom_boxplot()+ylim(c(0,0.050))+
        theme_bw()+labs(title = 'PM10 emissions VS Dristributor type',x= 'Distributor type',y= 'PM10 emissions (mg/m3)')
      
      
      
      
      ggplot(ct.data, aes(x= vg, y= driftmgkg))+
        geom_point()+facet_wrap(.~packing)+
        ylim(c(0.02,38/1000))+stat_smooth(method = 'lm')+ 
        theme_bw()+ labs(title = 'Solvent drift VS intake air velocity', x= 'Intake air velocity (m/s)', y= 'Solvent drift (%)')
      
      ggplot(ct.data, aes(x= vg, y= dustrak))+
        geom_point()+facet_wrap(.~packing)+
        ylim(c(0,38/1000))+stat_smooth(method = 'lm')+
        theme_bw()+ labs(title = 'PM10 emissions VS intake air velocity', x= 'Intake air velocity (m/s)', y= 'PM10 emissions (mg/m3)')

      ggplot(ct.data, aes(x= vg, y= dustrak))+
        geom_point()+facet_wrap(packing~disb)+
        ylim(c(0,40/1000))+stat_smooth(method = 'lm')+
        theme_bw()+ labs(title = 'PM10 emissions VS intake air velocity', x= 'Intake air velocity (m/s)', y= 'PM10 emissions (mg/m3)')
      
            
      ggplot(ct.data, aes(x=as.factor(ct.data$fliq)))+
        geom_boxplot(aes( y= dustrak), color='blue')+ ylim(c(0,0.05))+
        theme_bw()+labs(title = 'PM10 emissions VS Solvent flow rate', x='Solvent flowrate (lit/min)', y= 'Particulate matter mass concentration (mg/m3)')+
        annotate("text", x =1, y = 0.01, label = "PM2.5",color='red')+
        geom_boxplot(aes( y= mass/2000), color= 'red')+
        annotate("text", x =1, y = 0.04, label = "PM10",color='blue')
        #geom_boxplot(aes(y= driftmgkg), color='green', alpha=.4) if you want to add the drift the pm figure
        
      #ggplot(ct.data, aes(x=as.factor(ct.data$fliq)))+
      #geom_boxplot(aes(y= mass/1000))+ ylim(c(0,.02))+
      #  theme_bw()+labs(title = 'PM2.5 emissions VS Solvent flow rate', x='Solvent flowrate (lit/min)', y= 'PM2.5 mg/m3')
      
      ggplot(ct.data, aes(x= as.factor(ct.data$fliq)))+
        geom_boxplot(aes(y= driftmgkg)) + ylim(c(0,.02))+
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
      ggplot(pc.data, aes(x= vg, y= dp))+geom_point(aes(color= packing))+stat_smooth(method = 'lm', aes(color= packing))

      
      ggplot(pc.data, aes(x= as.factor(pc.data$fliq), y= dustrak/2+mass/2000))+
      geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Solvent flowrate', x='Sovent flowrate (lit/min)', y= 'PM10 emissions (mg/m3)')
      
      
      
      
      ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= dustrak/2+mass/2000))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Intake air velocity', x='Intake air velocity (m/s)', y= 'PM10 emissions (mg/m3)')
      
      ggplot(pc.data, aes(x= as.factor(pc.data$fliq), y= driftmgkg))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'Sovent drift VS Solvent flowrate', x='Sovent flowrate (lit/min)', y= 'Solvent drift (%)')
      
      ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= driftmgkg))+geom_boxplot()+facet_wrap(.~packing)+
        theme_bw()+labs(title = 'Sovent drift VS intake air velocity', x='Intake air velocity (m/s)', y= 'Solvent drift (%)')
      
      ggplot(pc.data, aes(x= as.factor(pc.data$vg), y= dustrak/2+mass/2000))+geom_boxplot()+facet_wrap(.~packing)+
      theme_bw()+labs(title = 'PM10 emissions VS Intake air velocity', x='Intake air velocity (m/s)', y= 'PM10 emissions (mg/m3)')

      
      
      
#all   
      ggplot(alldata, aes(x= co2in, y= nc))+geom_point()+
        xlim(c(350,450))+ylim(c(0,100000))+
        stat_smooth(method= 'lm')
      
      
      ggplot(alldata, aes(x= device, y= nc)) +
        geom_boxplot()+ theme_bw()+ xlim(c( 'pc','ct', 'rls'))
      ggplot(alldata, aes(x= device, y= driftmgkg)) +
        geom_boxplot()+ theme_bw()
      ggplot(alldata, aes(x= device, y= cmd)) +
        geom_boxplot()+ theme_bw()+ xlim(c( 'ct','rls', 'pc'))
      ggplot(alldata, aes(x= vg, y= dustrak))+ 
        facet_grid(.~device)+
        geom_point(aes(size= fliq, color= packing), alpha=.5)+
        stat_smooth(method = 'lm', aes(color= packing))+theme_bw()
        

             