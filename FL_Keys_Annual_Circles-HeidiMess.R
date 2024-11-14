# mod=read.csv("C:/Users/Thomas.Oliver/Downloads/CCmod.allHab.csv")
mod=read.csv("/Users/heidi.k.hirsh/Desktop/GBC_Submission_2024/Figures/FLK.Manuscript.Figures/6days_modification/CCmod.allHab.csv")


library(tidyverse)
library(patchwork)
library(MuMIn)
library(PNWColors)
library(ghibli)
library(ggtext)


unique(mod$Zone)

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
    pivot_longer(cols = c("AL.mDIC","AL.mTA","SG.mDIC","SG.mTA","CC.mTA","CC.mDIC"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value","Season","Zone")) %>% 
    # select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric,Season,Zone) %>% 
    # group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thisref.df$mod=1;thisref.df$jday=i;
  
  thismod2.df=mod %>%
    filter(mod==2,jday%in%jdayset) %>% 
    pivot_longer(cols = c("AL.mDIC","AL.mTA","SG.mDIC","SG.mTA","CC.mTA","CC.mDIC"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value","Season","Zone")) %>% 
    # select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric,Season,Zone) %>% 
    # group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thismod2.df$mod=2;thismod2.df$jday=i;
  
  thismod.5.df=mod %>%
    filter(mod==0.5,jday%in%jdayset) %>% 
    pivot_longer(cols = c("AL.mDIC","AL.mTA","SG.mDIC","SG.mTA","CC.mTA","CC.mDIC"),
                 names_to = c("HAB","Metric"),names_sep = "\\.") %>%
    select(c("HAB","Sub_region","mod","Metric","value","Season","Zone")) %>% 
    # select(c("HAB","Sub_region","mod","Metric","value")) %>% 
    group_by(Sub_region,HAB,Metric,Season,Zone) %>% 
    # group_by(Sub_region,HAB,Metric) %>% 
    summarize(mn=mean(value),
              se=se(value))
  thismod.5.df$mod=0.5;thismod.5.df$jday=i;
  
  thisout=rbind(thisout,thisref.df,thismod2.df,thismod.5.df)
  print(i)
}

thisout=thisout %>% 
  pivot_longer(cols=c("mn","se"),names_to = "summary",values_to = "value")  %>%
  pivot_wider(names_from = c("Metric",'summary'),values_from = "value") %>% 
  arrange(mod,Sub_region,HAB,jday)

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

# sc=1.25
# ggsave(DubPlot,filename = "C:/Users/Thomas.Oliver/Downloads/FLKeys_Double_Cover_Perturbation.jpg",width=11*sc,height=8.5*sc)


thisout$Sub_region=factor(thisout$Sub_region,levels=c("BB","UK","MK","LK"))
names(thisout)
unique(thisout$Zone)

SGplot = thisout %>% filter(mod %in% c(2)) %>% 
  ggplot(aes(x=mDIC_mn,y=mTA_mn,
             xmin=mDIC_mn-mDIC_se,ymin=mTA_mn-mTA_se,
             xmax=mDIC_mn+mDIC_se,ymax=mTA_mn+mTA_se,
             xend=0, yend=0, 
             shape=Sub_region,color=Season))+
  geom_segment()+
  geom_point(size=4)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  # labs(title=paste0('Seagrass Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  facet_wrap(Season~Sub_region)+
  coord_fixed( ratio=1)+
  theme_bw()
SGplot


# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/SG.noSite.mod=",fmod,".png"),plot=SGplot,width = 12, height = 8, dpi = 300)




#plot point vectors again. 
