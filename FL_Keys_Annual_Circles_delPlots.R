library(seacarb)
library(tidyverse)
library(patchwork)
library(metR)
# mod=read.csv("C:/Users/Thomas.Oliver/Downloads/CCmod.allHab.csv")
mod=read.csv("/Users/heidi.k.hirsh/Desktop/6days_modification/CCmod.allHab.csv")

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
se=function(x){return(sd(x,na.rm=T)/sqrt(length(x)))}

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
}

thisout.=thisout %>% 
  ungroup() %>% 
  pivot_longer(cols=c("mn","se"),names_to = "summary",values_to = "value")  %>%
  pivot_wider(names_from = c("Metric",'summary'),values_from = "value") %>% 
  arrange(mod,Sub_region,HAB,jday)

thisout=thisout.

pertLU=c("Half Cover","Unperturbed","Double Cover");names(pertLU)=c("0.5","1","2")
thisout$Perturbation=pertLU[as.character(thisout$mod)]
thisout$Sub_region=factor(thisout$Sub_region,levels=c("BB","UK","MK","LK"))
# write.csv(thisout,"C:/Users/Thomas.Oliver/Downloads/FL_Keys_Annual_Ring_TADIC.csv")

# thisout=read.csv("C:/Users/Thomas.Oliver/Downloads/FL_Keys_Annual_Ring_TADIC.csv")
head(thisout)

thisout2=thisout %>% filter(mod%in%1:2,Sub_region=="MK") %>% 
  # select(!1:2) %>% 
  pivot_wider(id_cols = c("Sub_region","HAB","jday"),
              names_from = "mod",
              values_from = c("mDIC_mn","mDIC_se","mOmA_mn","mOmA_se","mPAR8_mn","mPAR8_se","mPARm_mn","mPARm_se","mTA_mn","mTA_se","mTemp_mn","mTemp_se" ))
# thisout2

thisout3=thisout %>% filter(mod<2,Sub_region=="MK") %>% 
  # select(!1:2) %>% 
  pivot_wider(id_cols = c("Sub_region","HAB","jday"),
              names_from = "mod",
              values_from = c("mDIC_mn","mDIC_se","mOmA_mn","mOmA_se","mPAR8_mn","mPAR8_se","mPARm_mn","mPARm_se","mTA_mn","mTA_se","mTemp_mn","mTemp_se" ))
# thisout3

#keep all 3 mods
thisout4=thisout %>% filter(Sub_region=="MK") %>% 
  pivot_wider(id_cols = c("Sub_region","HAB","jday","mod"),
              names_from = "mod",
              values_from = c("mDIC_mn","mDIC_se","mOmA_mn","mOmA_se","mPAR8_mn","mPAR8_se","mPARm_mn","mPARm_se","mTA_mn","mTA_se","mTemp_mn","mTemp_se" ))
thisout4


jday_qtr=c(1,92,183,274)
qtr_name=c("Jan","Mar","Jun","Sep");qtr_name
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday) %>% rename(mDIC_mn_1=mDIC_mn,mTA_mn_1=mTA_mn)
qtr_ann=refout %>% filter(jday%in%jday_qtr)
DelJday=thisout2 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Weekly Climatology of modeled TA/DIC, Perturbed by doubling 'biomass'")
DelTemp=thisout2 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=mTemp_mn_1))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Water Temperature (deg C)",palette = "RdYlBu")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")
DelPAR=thisout2 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=mPAR8_mn_1))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  # scale_color_viridis(name="Photosynthetically Active Radiation (uE)",palette = "inferno")+
  scale_color_distiller(name="Photosynthetically Active Radiation (uE)",palette = "PRGn")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")
DelPlots=DelJday/DelTemp/DelPAR+plot_layout(guides = "collect")&theme(legend.position = "bottom")

DelPlots
sc=1.25
# ggsave(DelPlots,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_DeltaDICTAArrows_DoubleCover.jpg",width=11*sc,height=11*sc)
ggsave(DelPlots,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_DeltaDICTAArrows_DoubleCover2.jpg",width=11*sc,height=11*sc)

ggsave(DelJday,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_DeltaDICTAArrows_DoubleCover_DelJday.jpg",width=12*sc,height=5*sc)



#delta plots for halving biomass
jday_qtr=c(1,92,183,274)
qtr_name=c("Jan","Mar","Jun","Sep");qtr_name
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday) %>% rename(mDIC_mn_1=mDIC_mn,mTA_mn_1=mTA_mn)
qtr_ann=refout %>% filter(jday%in%jday_qtr)
DelJday.5=thisout3 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Weekly Climatology of modeled TA/DIC, Perturbed by halving 'biomass'")
DelTemp.5=thisout3 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=mTemp_mn_1))+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Water Temperature (deg C)",palette = "RdYlBu")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")
DelPAR.5=thisout3 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=mPAR8_mn_1))+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  facet_grid(Sub_region~HAB)+
  # scale_color_viridis(name="Photosynthetically Active Radiation (uE)",palette = "inferno")+
  scale_color_distiller(name="Photosynthetically Active Radiation (uE)",palette = "PRGn")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  theme_bw()+ylab("TA")+xlab("DIC")
DelPlots.5=DelJday.5/DelTemp.5/DelPAR.5+plot_layout(guides = "collect")&theme(legend.position = "bottom")
DelPlots.5
DelJday.5
sc=1.25
# ggsave(DelPlots,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_DeltaDICTAArrows_DoubleCover.jpg",width=11*sc,height=11*sc)
ggsave(DelPlots.5,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_DeltaDICTAArrows_HalfCover2.jpg",width=11*sc,height=11*sc)
ggsave(DelJday.5,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_DeltaDICTAArrows_HalfCover_DelJday.jpg",width=12*sc,height=5*sc)
 
###
# compare seagrass: 
sgDelJday.5=thisout3 %>% filter(HAB=="SG") %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  # facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  ggtitle("Half Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")#+ggtitle("Weekly Climatology of modeled TA/DIC, Perturbed by halving 'biomass'")

sgDelJday=thisout2  %>% filter(HAB=="SG") %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=1)+
  # facet_grid(Sub_region~HAB)+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  ggtitle("Double Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")#+ggtitle("Weekly Climatology of modeled TA/DIC, Perturbed by doubling 'biomass'")
sgMOD = sgDelJday.5+sgDelJday
sgMOD



SGf =thisout4 %>% filter(HAB=="SG") %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#5ab4ac')+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#d8b365')+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  ggtitle("Half Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Perturb Seagrass Biomass")

ALf =thisout4 %>% filter(HAB=='AL') %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#5ab4ac')+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#d8b365')+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  ggtitle("Half Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Perturb NonCalc Algae Biomass")

CCf =thisout4 %>% filter(HAB=="CC") %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#5ab4ac')+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#d8b365')+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  ggtitle("Half Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Perturb Calcifier Biomass")

biof= ALf+CCf+SGf
biof

# ggsave(SGf,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/half_and_double_SG.jpg",width=5*sc,height=5*sc)
sc=1.25
# ggsave(biof,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/oppposing.biomass.mod.jpg",width=12*sc,height=5*sc)


#facet by hab
# New facet label names for HAB
hab.labs <- c("Seagrass", "Calcifiers", "Noncalcifying Algae")
names(hab.labs) <- c("SG", "CC", "AL")

sr.labs <- c("Biscayne Bay", "Upper Keys", "Middle Keys", "Lower Keys")
names(sr.labs) <- c("BB", "UK", "MK", "LK")

ALL =thisout4 %>% ggplot(aes(x=mDIC_mn_1,y=mTA_mn_1,color=jday))+
  geom_segment(aes(xend=mDIC_mn_2,yend=mTA_mn_2),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#5ab4ac')+
  geom_segment(aes(xend=mDIC_mn_0.5,yend=mTA_mn_0.5),arrow = arrow(length = unit(0.5, "cm")),linewidth=.75,color='#d8b365')+
  facet_grid(Sub_region~HAB,labeller=labeller(HAB=hab.labs, Sub_region=sr.labs))+
  scale_color_distiller(name="Julian Day",palette = "Spectral")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  # ggtitle("Half Seagrass Biomass")+
  theme_bw()+ylab("TA")+xlab("DIC")+ggtitle("Weekly Climatology of modeled TA/DIC, Perturbed by halving OR doubling 'biomass'")
  
ALL
sc=1.25
ggsave(ALL,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/ALL.oppposing.biomass.mod.jpg",width=12*sc,height=5*sc)



#Plots
# md=rep(31,12);md[2]=28;md[c(4,6,9,11)]=30
# monjday=c(0,cumsum(md))+1
qjday=1+seq(0,300,by=7*12)
mod12_qjday=thisout %>% filter(mod%in%1:2,jday%in%qjday) %>% 
  # select(!1:2) %>% 
  pivot_wider(id_cols = c("Sub_region","HAB","jday"),
              names_from = "mod",
              values_from = c("mDIC_mn","mDIC_se","mOmA_mn","mOmA_se","mPAR8_mn","mPAR8_se","mPARm_mn","mPARm_se","mTA_mn","mTA_se","mTemp_mn","mTemp_se" ))

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
# ggsave(DubPlot,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_Double_Cover_Perturbation.jpg",width=11*sc,height=8.5*sc)
ggsave(DubPlot,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_Double_Cover_Perturbation.jpg",width=11*sc,height=11*sc)


#Just MK
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
qdayout=mod12_qjday %>% filter(Sub_region=="MK") %>% arrange(Sub_region,HAB,jday) %>% rename(mDIC_mn=mDIC_mn_1,mTA_mn=mTA_mn_1)
qdayout$Perturbation=pertLU[2]
DubPlotMK=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x=mDIC_mn,y=mTA_mn,
             color=jday,shape=Perturbation))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(Sub_region~HAB)+
  geom_errorbar(aes(ymin=mTA_mn-mTA_se,ymax=mTA_mn+mTA_se),alpha=.5)+
  geom_errorbarh(aes(xmin=mDIC_mn-mDIC_se,xmax=mDIC_mn+mDIC_se),alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(aes(ymin=mTA_mn-mTA_se,ymax=mTA_mn+mTA_se),alpha=.5,data=refout,color="gray50")+
  geom_errorbarh(aes(xmin=mDIC_mn-mDIC_se,xmax=mDIC_mn+mDIC_se),alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  
  geom_segment(data=qdayout,aes(xend=mDIC_mn_2,yend=mTA_mn_2),color="black",linewidth=2)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Total Alkalinity (umol/kg)")+xlab("Dissolved Inorganic Carbon (umol/kg)")+theme(legend.position = "bottom")

DubPlotMK

sc=1.25
# ggsave(DubPlotMK,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_Double_Cover_PerturbationMK.jpg",width=11*sc,height=(11/3)*sc)
ggsave(DubPlotMK,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_Double_Cover_PerturbationMK.jpg",width=11*sc,height=11*sc)


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
ggsave(DubPlotMK_TOm,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/aragonite_modification.jpg",width=11*sc,height=6*sc)



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


TADC_TeOm=DubPlotMK/DubPlotMK_TOm/DubPlotMK_PAROm+plot_layout(guides = "collect")&theme(legend.position = "bottom")


sc=1.25
# ggsave(TADC_TeOm,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_Double_Cover_TADIC_TempOmA_PAROmA.jpg",width=11*sc,height=(12)*sc)
ggsave(TADC_TeOm,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_Double_Cover_TADIC_TempOmA_PAROmA.jpg",width=11*sc,height=11*sc)


#Just Par Omega A
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_OmA_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x = as.Date(jday, origin = as.Date("2018-01-01")),
             y=mOmA_mn,
             ymin=mOmA_mn-mOmA_se,
             ymax=mOmA_mn+mOmA_se,
             color=jday,shape=Perturbation))+
  scale_x_date(date_labels = "%b")+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(HAB~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  
  geom_errorbar(alpha=.5,data=refout,color="gray50")+
  geom_point(size=2,color="gray50",data=refout,alpha=.5)+
  geom_path(color="gray50",data=refout,alpha=.5)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Aragonite Saturation State")+xlab("")+#xlab("Julian Day of Year")+
  theme(legend.position = "bottom")
Time_OmA_Dub

#Just Par Omega A
refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_DIC_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x = as.Date(jday, origin = as.Date("2018-01-01")),
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
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Dissolved Inorganic Carbon (umol/kg)")+xlab("")+theme(legend.position = "bottom")
Time_DIC_Dub

refout=thisout %>% filter(mod==1,Sub_region=="MK") %>% arrange(Sub_region,HAB,jday)
Time_TA_Dub=thisout %>% filter(mod %in% c(2),Sub_region=="MK") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x = as.Date(jday, origin = as.Date("2018-01-01")),
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
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Total Alkalinity (umol/kg)")+xlab("")+theme(legend.position = "bottom")
Time_TA_Dub


Time_Temp=thisout %>% filter(mod %in% c(2),Sub_region=="MK",HAB=="CC") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x = as.Date(jday, origin = as.Date("2018-01-01")),
             y=mTemp_mn,
             ymin=mTemp_mn-mTemp_se,
             ymax=mTemp_mn+mTemp_se,
             color=jday))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(.~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Temperature")+xlab("")+theme(legend.position = "bottom")
Time_Temp

Time_PAR=thisout %>% filter(mod %in% c(2),Sub_region=="MK",HAB=="CC") %>% #,HAB=="CC",Sub_region=="MK") %>% 
  ggplot(aes(x = as.Date(jday, origin = as.Date("2018-01-01")),
             y=mPARm_mn,
             ymin=mPARm_mn-mPARm_se,
             ymax=mPARm_mn+mPARm_se,
             color=jday))+
  #facet_wrap(Sub_region~HAB,scales="free")+
  facet_grid(.~Sub_region)+
  geom_errorbar(alpha=.5)+
  geom_point(size=3)+
  geom_path(linewidth=1)+
  scale_color_distiller(name="Julian Day of Year",palette = "Spectral")+
  scale_x_date(date_breaks="1 month", date_labels="%B")+
  #scale_color_gradientn(colours = c("yellow", "blue", "red", "green", "yellow"), values = c(0, 90, 180, 270, 360)/360)+
  theme_bw()+ylab("Photosynthetically \nActive Radiation (Monthly-uE)")+xlab("")+theme(legend.position = "bottom")
Time_PAR

Time_XOm=Time_OmA_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")
Time_XTA=Time_TA_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")
Time_XDIC=Time_DIC_Dub/Time_Temp/Time_PAR+plot_layout(heights = c(3,1,1),guides="collect")&theme(legend.position = "bottom")

sc=.9

ggsave(Time_XOm,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_TimeOnX_OMA3.jpg",width=8.5*sc,height=(12)*sc)
ggsave(Time_XDIC,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_TimeOnX_DIC3.jpg",width=8.5*sc,height=(12)*sc)
ggsave(Time_XTA,filename = "/Users/heidi.k.hirsh/Desktop/6days_modification/FLKeys_TimeOnX_TA3.jpg",width=8.5*sc,height=(12)*sc)


