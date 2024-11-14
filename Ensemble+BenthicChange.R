#RUN new ensemble model

#Oct17 try rerunning this script 
#What does it do:compare degree of modification in each benthic class over seasons and regions
#should limit model to non-oceanic sites to follow the submitted manuscript

rm(list = ls()) #clear environment

#edit April 29 to summarize over zone (not sites within zones)

library(tidyverse)
library(patchwork)
library(MuMIn)
library(PNWColors)
library(ghibli)
library(ggtext)

se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}

CCc=read.csv("/Users/heidi.k.hirsh/Documents/GitHub/test-flk/CC_complete_cases_12Feb2024.csv")
CCc$Sub_region = factor(CCc$Sub_region, levels=c("BB","UK","MK","LK"))
dim(CCc)
CCc$CosHr=cos(2*pi*CCc$hrod.lst/24)
CCc$SinHr=sin(2*pi*CCc$hrod.lst/24)
CCc$CosMOY=cos(2*pi*CCc$MoY/12)
CCc$SinMOY=sin(2*pi*CCc$MoY/12)




#modeled metabolic contribution (put this inside loop?)
CCc=CCc %>% mutate(
  CALC_DV=CALC_m2*ndays*inverseVol,
  CALC_cHV=CALC_m2*CosHr*inverseVol,
  CALC_sHV=CALC_m2*SinHr*inverseVol,
  CALC_cMV=CALC_m2*CosMOY*inverseVol,
  CALC_sMV=CALC_m2*SinMOY*inverseVol,
  ALGi_DV=ALGi_m2*ndays*inverseVol,
  ALGi_cHV=ALGi_m2*CosHr*inverseVol,
  ALGi_sHV=ALGi_m2*SinHr*inverseVol,
  ALGi_cMV=ALGi_m2*CosMOY*inverseVol,
  ALGi_sMV=ALGi_m2*SinMOY*inverseVol,
  SGi_DV=SGi_m2*ndays*inverseVol,
  SGi_cHV=SGi_m2*CosHr*inverseVol,
  SGi_sHV=SGi_m2*SinHr*inverseVol,
  SGi_cMV=SGi_m2*CosMOY*inverseVol,
  SGi_sMV=SGi_m2*SinMOY*inverseVol
)

names(CCc)

# CCc_s=subset(CCc,ndays==7)
nrun=100
tf=.8
#Prep
TA_ModList=list()
DIC_ModList=list()
# CCc_s=subset(CCc,ndays==7)

# CCc_s=subset(CCc,ndays==7)
#do 6 days instead
CCc_s=subset(CCc,ndays==6)

#Build Model Ensemble Loop
for (i in 1:nrun) {
  train_i = sample(1:nrow(CCc_s), tf * nrow(CCc_s), replace = F)
  test_i = setdiff(1:nrow(CCc_s), train_i)
  
  
  DIC_m0.7 = lm(
    DIC_umol_kg ~
      (1 - FBfraction) * DICoce_mean +
      FBfraction * DICrefN +
      PAR_MODIS_MON +
      Salinity_Bottle +
      Temperature_C +
      Chla +
      NO3 +
      CALC_DV +
      CALC_cHV +
      CALC_sHV +
      CALC_cMV +
      CALC_sMV +
      ALGi_DV +
      ALGi_cHV +
      ALGi_sHV +
      ALGi_cMV +
      ALGi_sMV +
      SGi_DV +
      SGi_cHV +
      SGi_sHV +
      SGi_cMV +
      SGi_sMV +
      Year +
      Sub_region,
    data = CCc_s[train_i, ]
  )
  
  
  TA_m0.7 = lm(
    TA_umol_kg ~
      (1 - FBfraction) *  TAoce_mean +
      FBfraction * TArefN +
      PAR_MODIS_MON +
      Salinity_Bottle +
      Temperature_C +
      Chla +
      NO3 +
      CALC_DV +
      CALC_cHV +
      CALC_sHV +
      CALC_cMV +
      CALC_sMV +
      ALGi_DV +
      ALGi_cHV +
      ALGi_sHV +
      ALGi_cMV +
      ALGi_sMV +
      SGi_DV +
      SGi_cHV +
      SGi_sHV +
      SGi_cMV +
      SGi_sMV +
      Year +
      Sub_region,
    data = CCc_s[train_i, ]
  )
  DIC_ModList[[i]] = DIC_m0.7
  TA_ModList[[i]] = TA_m0.7
  
  print(i)
}

DIC_ma=model.avg(DIC_ModList)
TA_ma=model.avg(TA_ModList)

summary(DIC_ma)
summary(TA_ma)

length(coef(DIC_ma))
length(coef(TA_ma))
dim(CCc_s)

DIC_ma_fit=lm(predict(DIC_ma,newdata=CCc_s)~DIC_umol_kg,data=CCc_s)
TA_ma_fit=lm(predict(TA_ma,newdata=CCc_s)~TA_umol_kg,data=CCc_s)

sDICma=summary(DIC_ma_fit)
sTAma=summary(TA_ma_fit)

#calculate predicted DIC and TA
#why [[2]]?
CCc_s$pDIC_ma=as.vector(predict(DIC_ModList[[2]], newdata = CCc_s))
CCc_s$pTA_ma=as.vector(predict(TA_ModList[[2]], newdata = CCc_s))

DIC_RMSE=round(sqrt(mean((CCc_s$pDIC_ma-CCc_s$DIC_umol_kg)^2)),1)
TA_RMSE=round(sqrt(mean((CCc_s$pTA_ma-CCc_s$TA_umol_kg)^2)),1)




#___________________change benthic indices______________
CCc=read.csv("/Users/heidi.k.hirsh/Documents/GitHub/test-flk/CC_complete_cases_12Feb2024.csv")
CCc$Sub_region = factor(CCc$Sub_region, levels=c("BB","UK","MK","LK"))
CCc$Season=factor(CCc$Season, levels = c("Winter","Spring","Summer","Fall"))
CCc$Month=factor(CCc$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

CCc$CosHr=cos(2*pi*CCc$hrod.lst/24)
CCc$SinHr=sin(2*pi*CCc$hrod.lst/24)
CCc$CosMOY=cos(2*pi*CCc$MoY/12)
CCc$SinMOY=sin(2*pi*CCc$MoY/12)
CCc_s=subset(CCc,ndays==7)
#only look at 7 days
unique(CCc_s$ndays)

modifier = c(0.0, 0.1, 0.25, 0.5, 0.75, 1.0, 1.1, 1.25, 1.5, 1.75, 2.0)
length(modifier)

new.DIC = NULL
new.TA = NULL

for(m_i in 1:length(modifier)) {
#just seagrass first
new.df = CCc_s
new.df$SGi_m2 = modifier[m_i]*CCc_s$SGi_m2

#recalculate metabolic contributions 
new.df=new.df %>% mutate(
  CALC_DV=CALC_m2*ndays*inverseVol,
  CALC_cHV=CALC_m2*CosHr*inverseVol,
  CALC_sHV=CALC_m2*SinHr*inverseVol,
  CALC_cMV=CALC_m2*CosMOY*inverseVol,
  CALC_sMV=CALC_m2*SinMOY*inverseVol,
  ALGi_DV=ALGi_m2*ndays*inverseVol,
  ALGi_cHV=ALGi_m2*CosHr*inverseVol,
  ALGi_sHV=ALGi_m2*SinHr*inverseVol,
  ALGi_cMV=ALGi_m2*CosMOY*inverseVol,
  ALGi_sMV=ALGi_m2*SinMOY*inverseVol,
  SGi_DV=SGi_m2*ndays*inverseVol,
  SGi_cHV=SGi_m2*CosHr*inverseVol,
  SGi_sHV=SGi_m2*SinHr*inverseVol,
  SGi_cMV=SGi_m2*CosMOY*inverseVol,
  SGi_sMV=SGi_m2*SinMOY*inverseVol
)
# names(new.df)

#predict new DIC: **substitute in the model average from the ensemble modeling
pDIC.sg=as.vector(predict(DIC_ModList[[2]], newdata = new.df))
new.DIC=cbind(new.DIC,pDIC.sg)

#predict new TA: **substitute in the model average from the ensemble modeling
pTA.sg=as.vector(predict(TA_ModList[[2]], newdata = new.df))
new.TA=cbind(new.TA,pTA.sg)

}

#output is 830 rows (one for each sample) and 11 columns (one for each modification including no mod)
colnames(new.DIC)=modifier
colnames(new.TA)=modifier

new.DIC = as.data.frame(new.DIC)
new.TA = as.data.frame(new.TA)

#try adding visitID then pivoting longer so it can be matched with full df
new.DIC$visitID = new.df$visitID
new.TA$visitID = new.df$visitID
new.TA.long = new.TA %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="TA")
new.DIC.long = new.DIC %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="DIC")
length(new.TA.long)==length(new.DIC.long)

newCCsg = new.TA.long
newCCsg$DIC = new.DIC.long$DIC
# View(newCCsg)

sgMod= newCCsg %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  # geom_point(data=subset(newCCsg, mod==1),aes(x=DIC, y=TA))+
  labs(title='Seagrass Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  scale_color_brewer(palette="PRGn")+
  theme_bw()
sgMod
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modSeagrass",Sys.time(),".png"),plot=sgMod,width = 9, height = 8, dpi = 300)


#merge with original dataframe again: 
dim(CCc_s)#830
length(unique(CCc_s$visitID)) #830
dim(newCCsg)
9130/11 #830

CCmod.sg = left_join(newCCsg, CCc_s, by=c("visitID"))
dim(CCmod.sg)
#SAVE modified sg: 
# write.csv(CCmod.sg,file="/Users/heidi.k.hirsh/Desktop/6days_modification/CCmod.sg.csv",row.names=FALSE)

sgModz= CCmod.sg %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  # geom_point(data=subset(newCCsg, mod==1),aes(x=DIC, y=TA))+
  scale_color_brewer(palette="PRGn")+
  facet_wrap("Zone")+
  labs(title='Seagrass Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
sgModz
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modSeagrass_zone", Sys.time(),".png"),plot=sgModz,width = 9, height = 8, dpi = 300)


sgModsr= subset(CCmod.sg,Zone=='Inshore') %>% ggplot()+
  # geom_point(aes(x=DIC, y=TA, color=mod))+
  geom_point(aes(x=DIC, y=TA, color=mod, shape=Season),size=3)+
  # geom_point(data=subset(newCCsg, mod==1),aes(x=DIC, y=TA))+
  scale_color_brewer(palette="PRGn")+
  facet_wrap(Sub_region~Season)+
  labs(title='Seagrass Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
sgModsr
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modSeagrass_SR_inshore",Sys.time(),".png"),plot=sgModsr,width = 9, height = 8, dpi = 300)


#modify calcifiers______________(just copy same code for now)
CCc=read.csv("/Users/heidi.k.hirsh/Documents/GitHub/test-flk/CC_complete_cases_12Feb2024.csv")
CCc$Sub_region = factor(CCc$Sub_region, levels=c("BB","UK","MK","LK"))
CCc$Season=factor(CCc$Season, levels = c("Winter","Spring","Summer","Fall"))
CCc$Month=factor(CCc$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

CCc$CosHr=cos(2*pi*CCc$hrod.lst/24)
CCc$SinHr=sin(2*pi*CCc$hrod.lst/24)
CCc$CosMOY=cos(2*pi*CCc$MoY/12)
CCc$SinMOY=sin(2*pi*CCc$MoY/12)
CCc_s=subset(CCc,ndays==7)

modifier = c(0.0, 0.1, 0.25, 0.5, 0.75, 1.0, 1.1, 1.25, 1.5, 1.75, 2.0)
length(modifier)

new.DIC = NULL
new.TA = NULL

for(m_i in 1:length(modifier)) {
  #CALC
  new.df = CCc_s
  new.df$CALC_m2 = modifier[m_i]*CCc_s$CALC_m2
  
  #recalculate metabolic contributions 
  new.df=new.df %>% mutate(
    CALC_DV=CALC_m2*ndays*inverseVol,
    CALC_cHV=CALC_m2*CosHr*inverseVol,
    CALC_sHV=CALC_m2*SinHr*inverseVol,
    CALC_cMV=CALC_m2*CosMOY*inverseVol,
    CALC_sMV=CALC_m2*SinMOY*inverseVol,
    ALGi_DV=ALGi_m2*ndays*inverseVol,
    ALGi_cHV=ALGi_m2*CosHr*inverseVol,
    ALGi_sHV=ALGi_m2*SinHr*inverseVol,
    ALGi_cMV=ALGi_m2*CosMOY*inverseVol,
    ALGi_sMV=ALGi_m2*SinMOY*inverseVol,
    SGi_DV=SGi_m2*ndays*inverseVol,
    SGi_cHV=SGi_m2*CosHr*inverseVol,
    SGi_sHV=SGi_m2*SinHr*inverseVol,
    SGi_cMV=SGi_m2*CosMOY*inverseVol,
    SGi_sMV=SGi_m2*SinMOY*inverseVol
  )
  # names(new.df)
  
  #predict new DIC: **substitute in the model average from the ensemble modeling
  pDIC.calc=as.vector(predict(DIC_ModList[[2]], newdata = new.df))
  new.DIC=cbind(new.DIC,pDIC.calc)
  
  #predict new TA: **substitute in the model average from the ensemble modeling
  pTA.calc=as.vector(predict(TA_ModList[[2]], newdata = new.df))
  new.TA=cbind(new.TA,pTA.calc)
  
}

#output is 830 rows (one for each sample) and 11 columns (one for each modification including no mod)
colnames(new.DIC)=modifier
colnames(new.TA)=modifier

new.DIC = as.data.frame(new.DIC)
new.TA = as.data.frame(new.TA)

#try adding visitID then pivoting longer so it can be matched with full df
new.DIC$visitID = new.df$visitID
new.TA$visitID = new.df$visitID
new.TA.long = new.TA %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="TA")
new.DIC.long = new.DIC %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="DIC")
length(new.TA.long)==length(new.DIC.long)

newCCcalc = new.TA.long
newCCcalc$DIC = new.DIC.long$DIC
# View(newCCcalc)

cMod= newCCcalc %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  scale_color_brewer(palette="RdBu")+
  labs(title='Coral Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
cMod
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modCoral",Sys.time(),".png"),plot=cMod,width = 9, height = 8, dpi = 300)




#merge with original dataframe again: 
CCmod.cc = left_join(newCCcalc, CCc_s, by=c("visitID"))
dim(CCmod.cc)
#SAVE modified calc: 
# write.csv(CCmod.cc,file="/Users/heidi.k.hirsh/Desktop/6days_modification/CCmod.cc.csv",row.names=FALSE)

cModz= CCmod.cc %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  scale_color_brewer(palette="RdBu")+
  facet_wrap("Zone")+
  labs(title='Calcifier Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
cModz
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modCoral_zone",Sys.time(),".png"),plot=cModz,width = 9, height = 8, dpi = 300)

# CCmod.cc$Season=factor(CCmod.cc$Season, levels = c("Winter","Spring","Summer","Fall"))

cModsr= subset(CCmod.cc,Zone=='Inshore') %>% ggplot()+
  # geom_point(aes(x=DIC, y=TA, color=mod, shape=Season),size=3)+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  scale_color_brewer(palette="RdBu")+
  facet_wrap("Sub_region")+
  # facet_wrap(Sub_region~Season)+
  labs(title='Calcifier Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
cModsr
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modCoral_SR_inshore",Sys.time(),".png"),plot=cModsr,width = 9, height = 8, dpi = 300)

# CCmod.cc$Month=factor(CCmod.cc$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
                                              
#note: this conflates original observations and perturbed model predictions.  Better to compare modeled 1 and modeled x2


CCmod.cc.md = CCmod.cc %>% mutate(delTA = TA-TA_umol_kg, delDIC = DIC-DIC_umol_kg) %>% filter(mod==2) %>% 
  # group_by(SiteID, Season, Zone, Sub_region) %>% summarize(md.delTA=median(delTA, na.rm=T),
  group_by(Season, Zone, Sub_region) %>% summarize(md.delTA=median(delTA, na.rm=T),
                                         md.delDIC=median(delDIC, na.rm=T),
                                         se.delTA=se(delTA, na.rm=T),
                                         se.delDIC=se(delDIC, na.rm=T)
                                         )

CCmod.cc.md %>% 
  filter(Zone=="Inshore") %>% 
  ggplot(aes(x=md.delTA,y=md.delDIC,xmin=md.delTA-se.delTA,xmax=md.delTA+se.delTA,
             xend=0, yend=0, 
             ymin=md.delDIC-se.delDIC,ymax=md.delDIC+se.delDIC,shape=Sub_region,color=Sub_region))+
  # ymin=md.delDIC-se.delDIC,ymax=md.delDIC+se.delDIC,shape=Sub_region,color=Season))+
  geom_segment()+
  geom_point(size=4)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  coord_flip()+ #dic and ta are on wrong axes
  facet_wrap("Season")+
  # facet_wrap("SiteID")+
  theme_bw()
  


#modify algae______________(just copy same code for now)
CCc=read.csv("/Users/heidi.k.hirsh/Documents/GitHub/test-flk/CC_complete_cases_12Feb2024.csv")
CCc$Sub_region = factor(CCc$Sub_region, levels=c("BB","UK","MK","LK"))
CCc$Season=factor(CCc$Season, levels = c("Winter","Spring","Summer","Fall"))
CCc$Month=factor(CCc$Month,levels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

CCc$CosHr=cos(2*pi*CCc$hrod.lst/24)
CCc$SinHr=sin(2*pi*CCc$hrod.lst/24)
CCc$CosMOY=cos(2*pi*CCc$MoY/12)
CCc$SinMOY=sin(2*pi*CCc$MoY/12)
CCc_s=subset(CCc,ndays==7)

modifier = c(0.0, 0.1, 0.25, 0.5, 0.75, 1.0, 1.1, 1.25, 1.5, 1.75, 2.0)
length(modifier)

new.DIC = NULL
new.TA = NULL

for(m_i in 1:length(modifier)) {
  #noncalcifying ALGAE
  new.df = CCc_s
  new.df$ALGi_m2 = modifier[m_i]*CCc_s$ALGi_m2
  
  #recalculate metabolic contributions 
  new.df=new.df %>% mutate(
    CALC_DV=CALC_m2*ndays*inverseVol,
    CALC_cHV=CALC_m2*CosHr*inverseVol,
    CALC_sHV=CALC_m2*SinHr*inverseVol,
    CALC_cMV=CALC_m2*CosMOY*inverseVol,
    CALC_sMV=CALC_m2*SinMOY*inverseVol,
    ALGi_DV=ALGi_m2*ndays*inverseVol,
    ALGi_cHV=ALGi_m2*CosHr*inverseVol,
    ALGi_sHV=ALGi_m2*SinHr*inverseVol,
    ALGi_cMV=ALGi_m2*CosMOY*inverseVol,
    ALGi_sMV=ALGi_m2*SinMOY*inverseVol,
    SGi_DV=SGi_m2*ndays*inverseVol,
    SGi_cHV=SGi_m2*CosHr*inverseVol,
    SGi_sHV=SGi_m2*SinHr*inverseVol,
    SGi_cMV=SGi_m2*CosMOY*inverseVol,
    SGi_sMV=SGi_m2*SinMOY*inverseVol
  )
  # names(new.df)
  
  #predict new DIC: **substitute in the model average from the ensemble modeling
  pDIC.alg=as.vector(predict(DIC_ModList[[2]], newdata = new.df))
  new.DIC=cbind(new.DIC,pDIC.alg)
  
  #predict new TA: **substitute in the model average from the ensemble modeling
  pTA.alg=as.vector(predict(TA_ModList[[2]], newdata = new.df))
  new.TA=cbind(new.TA,pTA.alg)
  
}

#output is 830 rows (one for each sample) and 11 columns (one for each modification including no mod)
colnames(new.DIC)=modifier
colnames(new.TA)=modifier

new.DIC = as.data.frame(new.DIC)
new.TA = as.data.frame(new.TA)

#try adding visitID then pivoting longer so it can be matched with full df
new.DIC$visitID = new.df$visitID
new.TA$visitID = new.df$visitID
new.TA.long = new.TA %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="TA")
new.DIC.long = new.DIC %>% pivot_longer(cols=-c(visitID),names_to="mod", values_to="DIC")
length(new.TA.long)==length(new.DIC.long)

newCCalg = new.TA.long
newCCalg$DIC = new.DIC.long$DIC
# View(newCCalg)

aMod= newCCalg %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  # scale_color_brewer(palette="BrBG")+
  scale_color_brewer(palette="PiYG")+
  labs(title='Noncalcifying Algae Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
aMod
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modAlgae",Sys.time(),".png"),plot=aMod,width = 9, height = 8, dpi = 300)


#merge with original dataframe again: 
CCmod.a = left_join(newCCalg, CCc_s, by=c("visitID"))
dim(CCmod.a)
#SAVE modified alg: 
# write.csv(CCmod.a,file="/Users/heidi.k.hirsh/Desktop/6days_modification/CCmod.a.csv",row.names=FALSE)

aModz= CCmod.a %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  scale_color_brewer(palette="PiYG")+
  facet_wrap("Zone")+
  labs(title='Noncalcifying Algae Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
aModz
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modAlgae_zone",Sys.time(),".png"),plot=aModz,width = 9, height = 8, dpi = 300)


aModsr= subset(CCmod.a,Zone=='Inshore') %>% ggplot()+
  geom_point(aes(x=DIC, y=TA, color=mod))+
  scale_color_brewer(palette="PiYG")+
  facet_wrap("Sub_region")+
  labs(title='Noncalcifying Algae Modification', x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  theme_bw()
aModsr
ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/perturbations/modAlgae_SR_inshore",Sys.time(),".png"),plot=aModsr,width = 9, height = 8, dpi = 300)

