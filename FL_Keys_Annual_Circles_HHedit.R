library(seacarb)
library(tidyverse)
library(patchwork)
library(MuMIn)
library(ggtext)

# install.packages("devtools")
library(devtools)
# devtools::install_github("coatless/cetcolor")
# install_github('coatless/cetcolor')
library(cetcolor)

se=function(x){return(sd(x,na.rm=T)/sqrt(length(x)))}

mod=read.csv("/Users/heidi.k.hirsh/Desktop/GBC_Submission_2024/Figures/FLK.Manuscript.Figures/6days_modification/CCmod.allHab.csv")

carbAL=carb(flag=15,var1=mod$AL.mTA/10^6,var2=mod$AL.mDIC/10^6,S = mod$Salinity_Bottle,T=mod$Temperature_C)
carbCC=carb(flag=15,var1=mod$CC.mTA/10^6,var2=mod$CC.mDIC/10^6,S = mod$Salinity_Bottle,T=mod$Temperature_C)
carbSG=carb(flag=15,var1=mod$SG.mTA/10^6,var2=mod$SG.mDIC/10^6,S = mod$Salinity_Bottle,T=mod$Temperature_C)
mod$AL.mOmA=carbAL$OmegaAragonite
mod$SG.mOmA=carbSG$OmegaAragonite
mod$CC.mOmA=carbCC$OmegaAragonite

mod$AL.mTemp=mod$Temperature_C
mod$SG.mTemp=mod$Temperature_C
mod$CC.mTemp=mod$Temperature_C
mod$AL.mPAR8=mod$PAR_MODIS_8DAY
mod$SG.mPAR8=mod$PAR_MODIS_8DAY
mod$CC.mPAR8=mod$PAR_MODIS_8DAY
mod$AL.mPARm=mod$PAR_MODIS_MON
mod$SG.mPARm=mod$PAR_MODIS_MON
mod$CC.mPARm=mod$PAR_MODIS_MON

mod1=mod %>% filter(mod==1)
mod2=mod %>% filter(mod==2)

#need to keep timestamp in addition to jday to plot with month too.
thisout=NULL
win=8*7#days aka 8 weeks
step=7
for(i in seq(1,366,by=step)){
  min.jday=(i-win/2)%%366
  jdayset=(min.jday:(min.jday+win))%%366
  thisref.df=mod %>%
    filter(mod==1,jday%in%jdayset) %>%
    pivot_longer(cols = c("AL.mDIC","AL.mTA","AL.mOmA","SG.mDIC","SG.mTA","SG.mOmA","CC.mTA","CC.mDIC","CC.mOmA","AL.mTemp","SG.mTemp","CC.mTemp","AL.mPAR8","SG.mPAR8","CC.mPAR8","AL.mPARm","SG.mPARm","CC.mPARm"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thisref.df$mod=1;thisref.df$jday=i;
  
  thismod2.df=mod %>%
    filter(mod==2,jday%in%jdayset) %>% 
    pivot_longer(cols = c("AL.mDIC","AL.mTA","AL.mOmA","SG.mDIC","SG.mTA","SG.mOmA","CC.mTA","CC.mDIC","CC.mOmA","AL.mTemp","SG.mTemp","CC.mTemp","AL.mPAR8","SG.mPAR8","CC.mPAR8","AL.mPARm","SG.mPARm","CC.mPARm"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thismod2.df$mod=2;thismod2.df$jday=i;
  
  thismod.5.df=mod %>%
    filter(mod==0.5,jday%in%jdayset) %>% 
    pivot_longer(cols = c("AL.mDIC","AL.mTA","AL.mOmA","SG.mDIC","SG.mTA","SG.mOmA","CC.mTA","CC.mDIC","CC.mOmA","AL.mTemp","SG.mTemp","CC.mTemp","AL.mPAR8","SG.mPAR8","CC.mPAR8","AL.mPARm","SG.mPARm","CC.mPARm"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thismod.5.df$mod=0.5;thismod.5.df$jday=i;
  
  thisout=rbind(thisout,thisref.df,thismod2.df,thismod.5.df)
  print(i)
} #should loop through 365 days

thisout.=thisout %>% 
  ungroup() %>% 
  pivot_longer(cols=c("mn","se"),names_to = "summary",values_to = "value")  %>%
  pivot_wider(names_from = c("Metric",'summary'),values_from = "value") %>% 
  arrange(mod,Sub_region,HAB,jday)

thisout=thisout.

pertLU=c("Half Cover","Unperturbed","Double Cover");names(pertLU)=c("0.5","1","2")
thisout$Perturbation=pertLU[as.character(thisout$mod)]
# write.csv(thisout,"C:/Users/Thomas.Oliver/Downloads/FL_Keys_Annual_Ring_TADIC.csv")

thisout$Sub_region=factor(thisout$Sub_region,levels=c("BB","UK","MK","LK"))
refout=thisout %>% filter(mod==1) %>% arrange(Sub_region,HAB,jday)
DubPlot=thisout %>% filter(mod %in% c(2)) %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=mDIC_mn,y=mTA_mn,
             xmin=mDIC_mn-mDIC_se,ymin=mTA_mn-mTA_se,
             xmax=mDIC_mn+mDIC_se,ymax=mTA_mn+mTA_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(Sub_region~HAB)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_errorbarh(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  scale_color_distiller(name="Annual Day",palette = "Spectral")+
  theme_bw()+ylab("TA")+xlab("DIC")
DubPlot

sc=1.25
# ggsave(DubPlot,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_Double_Cover_Perturbation.jpg",width=11*sc,height=8.5*sc)


#Just MK
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
DubPlotMK=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=mDIC_mn,y=mTA_mn,
             xmin=mDIC_mn-mDIC_se,ymin=mTA_mn-mTA_se,
             xmax=mDIC_mn+mDIC_se,ymax=mTA_mn+mTA_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(Sub_region~HAB)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_errorbarh(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Total Alkalinity (umol/kg)")+xlab("Dissolved Inorganic Carbon (umol/kg)")+theme(legend.position = "bottom")

DubPlotMK

sc=1.25
# ggsave(DubPlotMK,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_Double_Cover_PerturbationMK.jpg",width=11*sc,height=(11/3)*sc)

#Just MK - Omega A
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
DubPlotMK_TOm=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=mTemp_mn,y=mOmA_mn,
             xmin=mTemp_mn-mTemp_se,ymin=mOmA_mn-mOmA_se,
             xmax=mTemp_mn+mTemp_se,ymax=mOmA_mn+mOmA_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(Sub_region~HAB)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_errorbarh(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Aragonite Saturation State")+xlab("Temperature (deg C)")+theme(legend.position = "bottom")
DubPlotMK_TOm
# ggsave(DubPlotMK_TOm,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_Double_Cover_PerturbationMK_tempOmega.jpg",width=11*sc,height=(11/3)*sc)


#Just MK - PAR
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
DubPlotMK_PAROm=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=mPARm_mn,y=mOmA_mn,
             xmin=mPARm_mn-mPARm_se,ymin=mOmA_mn-mOmA_se,
             xmax=mPARm_mn+mPARm_se,ymax=mOmA_mn+mOmA_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(Sub_region~HAB)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_errorbarh(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Aragonite Saturation State")+xlab("Photosynthetically Active Radiation (Monthly-uE)")+theme(legend.position = "bottom")
DubPlotMK_PAROm
# ggsave(DubPlotMK_PAROm,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_Double_Cover_PerturbationMK_PARomega.jpg",width=11*sc,height=(11/3)*sc)


TADC_TeOm=DubPlotMK/DubPlotMK_TOm/DubPlotMK_PAROm+plot_layout(guides = "collect")&theme(legend.position = "bottom")
TADC_TeOm
sc=1.25
# ggsave(TADC_TeOm,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_Double_Cover_TADIC_TempOmA_PAROmA.jpg",width=11*sc,height=(12)*sc)



# #plot by jday
# #stack omega plots for AL, CC, SG
# #change colors to be cyclical
# refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
# A=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
#   ggplot(aes(x=jday,y=mOmA_mn,
#              ymin=mOmA_mn-mOmA_se,
#              ymax=mOmA_mn+mOmA_se,
#              color=jday,shape=Perturbation))+
#   facet_grid(HAB~Sub_region)+
#   # facet_wrap(.~HAB,ncol=1)+
#   geom_errorbar(alpha=.5)+
#   geom_point(size=3)+
#   geom_path(linewidth=1)+
#   
#   geom_errorbar(alpha=.5,data=refout,color="gray50")+
#   # geom_errorbarh(alpha=.5,data=refout,color="gray50")+
#   geom_point(size=2,color="gray50",data=refout,alpha=.5)+
#   geom_line(color="gray50",data=refout,alpha=.5)+
#   scale_color_distiller(name="Annual Day",palette = "Spectral")+
#   theme_bw()+ylab("Omega")+xlab("Jday")+ #really want monthly labels eventually
#   theme(legend.position="bottom") 
# # jdayPlot
# 
# #add Temperature
# B=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
# ggplot(aes(x=jday,y=mTemp_mn,  #mPARm_mn
#            color=jday))+
#   facet_grid(~Sub_region)+
#   geom_point(size=3)+
#   geom_line(linewidth=1)+
#   scale_color_distiller(name="Annual Day",palette = "Spectral")+
#   theme_bw()+ylab("Temperature")+xlab("Jday")+ #really want monthly labels eventually
#   theme(legend.position="none") 
# 
# #add PAR
# C=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
#   ggplot(aes(x=jday,y=mPARm_mn,
#              color=jday))+
#   facet_grid(~Sub_region)+
#   geom_point(size=3)+
#   geom_line(linewidth=1)+
#   scale_color_distiller(name="Annual Day",palette = "Spectral")+
#   theme_bw()+ylab("PAR")+xlab("Jday")+ #really want monthly labels eventually
#   theme(legend.position="none") 
# 
# jdayPlot=A/B/C
# jdayPlot

#ok now loop to explore. 
# myPal = rev(cet_pal(12, name='c1s')) #other options: c1, c1s, c2, c2s,
myPal=cet_pal(12, name='c1s')
# display_cet_attribute(attribute = "cyclic")
# cet_pal(4, name='c1')
#Just Par Omega A
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_OmA_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% 
  ggplot(aes(x=as.Date(jday, origin = as.Date("2018-01-01")),
             # x=jday,
             y=mOmA_mn,
             ymin=mOmA_mn-mOmA_se,
             ymax=mOmA_mn+mOmA_se,
             color=jday,shape=Perturbation))+
  facet_grid(HAB~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  # scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  # scale_color_gradientn(colors = c(rainbow(12), "red"))+
  scale_color_gradientn(colors = myPal)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+ 
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Aragonite Saturation State")+xlab("")+theme(legend.position = "bottom")
Time_OmA_Dub
# ggsave(Time_OmA_Dub,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/Time_OmA_Dub.jpg",width=11,height=12)


#Just Par Omega A
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_DIC_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=as.Date(jday, origin = as.Date("2018-01-01")),
             # x=jday,
             y=mDIC_mn,
             ymin=mDIC_mn-mDIC_se,
             ymax=mDIC_mn+mDIC_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(HAB~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  # scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  # scale_color_gradientn(colors = c(rainbow(12), "red"))+
  scale_color_gradientn(colors = myPal)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+ 
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Dissolved Inorganic Carbon (umol/kg)")+xlab("Julian Day of Year")+theme(legend.position = "bottom")
Time_DIC_Dub
# ggsave(Time_DIC_Dub,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/Time_DIC_Dub.jpg",width=11,height=12)

refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_TA_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=as.Date(jday, origin = as.Date("2018-01-01")),
             # x=jday,
             y=mTA_mn,
             ymin=mTA_mn-mTA_se,
             ymax=mTA_mn+mTA_se,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(HAB~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  # scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  # scale_color_gradientn(colors = c(rainbow(12), "red"))+
  scale_color_gradientn(colors = myPal)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+ 
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Total Alkalinity (umol/kg)")+xlab("Julian Day of Year")+theme(legend.position = "bottom")
Time_TA_Dub
# ggsave(Time_TA_Dub,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/Time_TA_Dub.jpg",width=11,height=12)

Time_Temp=thisout %>% filter(mod %in% c(2),Sub_region=="MK",HAB=="CC") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=as.Date(jday, origin = as.Date("2018-01-01")),
             # x=jday,
             y=mTemp_mn,
             ymin=mTemp_mn-mTemp_se,
             ymax=mTemp_mn+mTemp_se,
             color=jday))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(.~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  # scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  # scale_color_gradientn(colors = c(rainbow(12), "red"))+
  scale_color_gradientn(colors = myPal)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+ 
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Temperature")+xlab("Julian Day of Year")+
  theme(legend.position = "bottom")
# Time_Temp



Time_PAR=thisout %>% filter(mod %in% c(2),Sub_region=="MK",HAB=="CC") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=as.Date(jday, origin = as.Date("2018-01-01")),y=mPARm_mn, #x=jday,
             ymin=mPARm_mn-mPARm_se,
             ymax=mPARm_mn+mPARm_se,
             color=jday))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(.~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  # scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  # scale_color_gradientn(colours=rainbow(365))+
  # scale_color_gradientn(colors = c(rainbow(12), "red"))+
  scale_color_gradientn(colors = myPal)+
  scale_x_date(date_labels = "%b",date_breaks="1 month")+ #https://stackoverflow.com/questions/68684064/how-to-add-months-on-top-of-daily-data-on-the-x-axis-in-ggplot2
  # scale_color_gradientn(colors = cet_pal(12, name='c2s'))+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Photosynthetically \nActive Radiation (Monthly-uE)")+xlab("")+theme(legend.position = "bottom")
Time_PAR
  
  
Time_XOm=Time_OmA_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")
Time_XTA=Time_TA_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")
Time_XDIC=Time_DIC_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")

Time_XOm
Time_XTA
Time_XDIC


sc=.9
ggsave(Time_XOm,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_TimeOnX_OMA.jpg",width=8.5*sc,height=(12)*sc)
ggsave(Time_XDIC,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_TimeOnX_DIC.jpg",width=8.5*sc,height=(12)*sc)
ggsave(Time_XTA,filename = "/Users/heidi.k.hirsh/Desktop/Forecast_Home/Perturbation_Plots/FLKeys_TimeOnX_TA.jpg",width=8.5*sc,height=(12)*sc)



