#Combine habitat modifications in one plot
#modify April29 to summarize by zone (not sites within zone)


#oct 16 2024 starting perturbation thinking again... does this script work?
#yes this script works (10/17) - output is modification in DIC/TA space

rm(list = ls()) #clear environment

library(tidyverse)
library(patchwork)
library(MuMIn)
library(PNWColors)
library(ghibli)
library(ggtext)
library(mapview)
library(sf)

se = function(x,na.rm=T){return(sd(x,na.rm=na.rm)/sqrt(length(x)))}


#########  
#Read in modification dataframes for each benthic index category
CCmod.cc=read.csv("/Users/heidi.k.hirsh/Desktop/GBC_Submission_2024/Figures/FLK.Manuscript.Figures/6days_modification/CCmod.cc.csv")
CCmod.sg=read.csv("/Users/heidi.k.hirsh/Desktop/GBC_Submission_2024/Figures/FLK.Manuscript.Figures/6days_modification/CCmod.sg.csv")
CCmod.a=read.csv("/Users/heidi.k.hirsh/Desktop/GBC_Submission_2024/Figures/FLK.Manuscript.Figures/6days_modification/CCmod.a.csv")

ggplot(data=CCmod.cc, aes(x=jday.lst,y=DIC, color=Sub_region))+
  geom_jitter()+
  theme_bw()

#rename columns to specify ecosystem and combine. 
# CCmod.cc %>% rename_at(c('DIC','TA','mod'),~c('CC.mDIC','CC.mTA','CC.mod')) 
# CCmod.sg %>% rename_at(c('DIC','TA','mod'),~c('SG.mDIC','SG.mTA','SG.mod')) 
# CCmod.a %>% rename_at(c('DIC','TA','mod'),~c('AL.mDIC','AL.mTA','AL.mod')) 
#actually they can share the same mod column

summary(CCmod.cc$mod == CCmod.sg$mod) #all true
summary(CCmod.cc$mod == CCmod.a$mod) #all true

CCmod.cc = CCmod.cc %>% rename_at(c('DIC','TA'),~c('CC.mDIC','CC.mTA')) 
CCmod.sg.sub = CCmod.sg %>% rename_at(c('DIC','TA'),~c('SG.mDIC','SG.mTA')) %>% select(visitID, SG.mDIC, SG.mTA, mod)
# names(CCmod.sg.sub)
CCmod.a.sub = CCmod.a %>% rename_at(c('DIC','TA'),~c('AL.mDIC','AL.mTA')) %>% select(visitID, AL.mDIC, AL.mTA, mod)
# names(CCmod.a.sub)

#combine (I want CCmod.cc plus the unique new columns (3 each) for seagrass and alg)
dim(CCmod.cc) # 9130  129
# names(CCmod.cc)
dim(CCmod.sg.sub) # 9130    4
# names(CCmod.sg.sub)
dim(CCmod.a.sub) # 9130    4

# CCmod.cc.sg = left_join(CCmod.sg.sub, CCmod.cc, by=c('visitID'='visitID','mod'='mod'))
CCmod.cc.sg = left_join(CCmod.sg.sub, CCmod.cc, by=c('visitID','mod'))
dim(CCmod.cc.sg) # 9130  131 (yay added 2 cols)
CCmod.cc.sg.a = left_join(CCmod.a.sub, CCmod.cc.sg, by=c('visitID','mod'))
dim(CCmod.cc.sg.a) # 9130  133
CCmod.ben = CCmod.cc.sg.a
dim(CCmod.ben)
head(CCmod.ben)

# write.csv(CCmod.ben,file="/Users/heidi.k.hirsh/Desktop/6days_modification/CCmod.allHab.csv",row.names=FALSE)


#then I think we can do the summarize piping on everything at once

# CCmod.cc.md = CCmod.cc %>% mutate(delTA = TA-TA_umol_kg, delDIC = DIC-DIC_umol_kg) %>% filter(mod==2) %>% 
#   group_by(SiteID, Season, Zone, Sub_region) %>% summarize(md.delTA=median(delTA, na.rm=T),
#                                                            md.delDIC=median(delDIC, na.rm=T),
#                                                            se.delTA=se(delTA, na.rm=T),
#                                                            se.delDIC=se(delDIC, na.rm=T))

CCmod.ben.md=NULL
CCmod.ben.md = CCmod.ben %>% 
  mutate(CC.delTA = CC.mTA-TA_umol_kg, CC.delDIC = CC.mDIC-DIC_umol_kg,
                    SG.delTA = SG.mTA-TA_umol_kg, SG.delDIC = SG.mDIC-DIC_umol_kg,
                    AL.delTA = AL.mTA-TA_umol_kg, AL.delDIC = AL.mDIC-DIC_umol_kg) %>% 
  filter(mod==2) %>%
  # filter(mod==0.5) %>%
  filter(Zone=='Inshore') %>% 
  # group_by(SiteID, Season, Zone, Sub_region) %>% 
  group_by(Season, Zone, Sub_region) %>% 
  summarize(md.CC.delTA=median(CC.delTA, na.rm=T),
            md.CC.delDIC=median(CC.delDIC, na.rm=T),
            se.CC.delTA=se(CC.delTA, na.rm=T),
            se.CC.delDIC=se(CC.delDIC, na.rm=T),
            #seagrass
            md.SG.delTA=median(SG.delTA, na.rm=T),
            md.SG.delDIC=median(SG.delDIC, na.rm=T),
            se.SG.delTA=se(SG.delTA, na.rm=T),
            se.SG.delDIC=se(SG.delDIC, na.rm=T),
            #algae
            md.AL.delTA=median(AL.delTA, na.rm=T),
            md.AL.delDIC=median(AL.delDIC, na.rm=T),
            se.AL.delTA=se(AL.delTA, na.rm=T),
            se.AL.delDIC=se(AL.delDIC, na.rm=T)
            )

names(CCmod.ben.md)

fmod=2 #what is our chosen modifier (put in title and file name)
# fmod=0.5

# CCmod.ben.md.sf = st_as_sf(CCmod.ben.md, coords = c("Longitude.n","Latitude.n"), crs = st_crs(4326))
#dont have coords anymore

# CCmod.cc.sf = st_as_sf(CCmod.cc, coords = c("Longitude.n","Latitude.n"), crs = st_crs(4326))
# mapThis=subset(CCmod.cc.sf,Zone=='Inshore')
# unique(mapThis$Zone)
# mapview(mapThis, zcol = "SiteID")

# unique(mapThis$SiteID)
# length(unique(mapThis$SiteID))

# CCmod.ben.md$SiteID=factor(CCmod.ben.md$SiteID, levels = c("1","EK_IN","4","5","UK_IN","7","10","13","16","19","24"))
# length(unique(CCmod.ben.md$SiteID))

CCplot = CCmod.ben.md %>% 
  ggplot(aes(x=md.CC.delDIC,y=md.CC.delTA,
             xmin=md.CC.delDIC-se.CC.delDIC,xmax=md.CC.delDIC+se.CC.delDIC,
             ymin=md.CC.delTA-se.CC.delTA,ymax=md.CC.delTA+se.CC.delTA,
             xend=0, yend=0, 
             shape=Sub_region,color=Season))+
  geom_segment()+
  geom_point(size=4)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title=paste0('Calcifier Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  facet_wrap("Season")+
  coord_fixed( ratio=1)+
  theme_bw()
CCplot
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/CALC.noSite.mod=",fmod,".png"),plot=CCplot,width = 12, height = 8, dpi = 300)


SGplot = CCmod.ben.md %>% 
  ggplot(aes(x=md.SG.delDIC,y=md.SG.delTA,
             xmin=md.SG.delDIC-se.SG.delDIC,xmax=md.SG.delDIC+se.SG.delDIC,
             ymin=md.SG.delTA-se.SG.delTA,ymax=md.SG.delTA+se.SG.delTA,
             xend=0, yend=0, 
             shape=Sub_region,color=Season))+
  geom_segment()+
  geom_point(size=4)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title=paste0('Seagrass Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  facet_wrap("Season")+
  coord_fixed( ratio=1)+
  theme_bw()
SGplot
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/SG.noSite.mod=",fmod,".png"),plot=SGplot,width = 12, height = 8, dpi = 300)


ALplot = CCmod.ben.md %>% 
  ggplot(aes(x=md.AL.delDIC,y=md.AL.delTA,
             xmin=md.AL.delDIC-se.AL.delDIC,xmax=md.AL.delDIC+se.AL.delDIC,
             ymin=md.AL.delTA-se.AL.delTA,ymax=md.AL.delTA+se.AL.delTA,
             xend=0, yend=0, 
             shape=Sub_region,color=Season))+
  geom_segment()+
  geom_point(size=4)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title=paste0('Noncalcifying Algae Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  facet_wrap("Season")+
  coord_fixed( ratio=1)+
  theme_bw()
ALplot
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/ALG.noSite.mod=",fmod,".png"),plot=ALplot,width = 12, height = 8, dpi = 300)



##### Combine habitats together and facet by season and subregion
#the correct way to set up this dataframe is to pivot longer so there is one column for modification and a column for habitat being changed but they all share the same modTA and modDIC columns

#pivot to combine md.delTA, md.delDIC, se.delTA, se.delDIC
#color points by habitat
head(CCmod.ben.md)
names(CCmod.ben.md)
dim(CCmod.ben.md) #44 16
# CCmod.ben.md.long.mddelTA = CCmod.ben.md %>% pivot_longer(cols=c('md.CC.delTA','md.SG.delTA','md.AL.delTA'), names_to='habTA', values_to="md.delTA")
# dim(CCmod.ben.md.long.mddelTA) #132  15. (x3 because CC,SG,AL are stacked)
# CCmod.ben.md.long.mddelTA.mddelDIC = CCmod.ben.md.long.mddelTA  %>% pivot_longer(cols=c('md.CC.delDIC','md.SG.delDIC','md.AL.delDIC'), names_to='habDIC', values_to="md.delDIC")
# dim(CCmod.ben.md.long.mddelTA.mddelDIC) #396  14


## ok I'm doing something extra dumb. I'm going to separate into separate dfs for  md.delTA, md.delDIC, se.delTA, se.delDIC
#then pivot_longer
#then bind together again


# md.delTA.df = select(CCmod.ben.md, c('SiteID','Season','Zone','Sub_region','md.CC.delTA','md.SG.delTA','md.AL.delTA'))
md.delTA.df = select(CCmod.ben.md, c('Season','Zone','Sub_region','md.CC.delTA','md.SG.delTA','md.AL.delTA'))
md.delTA.L = md.delTA.df %>% pivot_longer(cols=c('md.CC.delTA','md.SG.delTA','md.AL.delTA'), names_to='hab', values_to='md.delTA') 
#rename habs column to just benthos category (CC, SG, or AL)
md.delTA.L[,'hab.name']=NA
# hab.name=NULL
for(i in 1:nrow(md.delTA.L)){
  split = strsplit(md.delTA.L$hab[i], "[.]")
  # # split
  # name = split[[1]][[2]]
  # # name
  # hab.name=rbind(hab.name,name)
  md.delTA.L$hab.name[i]=split[[1]][[2]]
}
# table(hab.name) #even for each
# md.delTA.L$hab.name=hab.name
md.delTA.L$hab.name
#drop hab column
md.delTA.L =md.delTA.L %>% select(-c('hab'))
# names(md.delTA.L)

####Do se delta TA
# se.delTA.df = select(CCmod.ben.md, c('SiteID','Season','Zone','Sub_region','se.CC.delTA','se.SG.delTA','se.AL.delTA'))
se.delTA.df = select(CCmod.ben.md, c('Season','Zone','Sub_region','se.CC.delTA','se.SG.delTA','se.AL.delTA'))
names(se.delTA.df)
se.delTA.L = se.delTA.df %>% pivot_longer(cols=c('se.CC.delTA','se.SG.delTA','se.AL.delTA'), names_to='hab', values_to='se.delTA') 
se.delTA.L[,'hab.name']=NA

for(i in 1:nrow(se.delTA.L)){
  split = strsplit(se.delTA.L$hab[i], "[.]")
  se.delTA.L$hab.name[i]=split[[1]][[2]]
}
se.delTA.L =se.delTA.L %>% select(-c('hab'))
# names(se.delTA.L)

####Do median delta DIC
# md.delDIC.df = select(CCmod.ben.md, c('SiteID','Season','Zone','Sub_region','md.CC.delDIC','md.SG.delDIC','md.AL.delDIC'))
md.delDIC.df = select(CCmod.ben.md, c('Season','Zone','Sub_region','md.CC.delDIC','md.SG.delDIC','md.AL.delDIC'))
names(md.delDIC.df)
md.delDIC.L = md.delDIC.df %>% pivot_longer(cols=c('md.CC.delDIC','md.SG.delDIC','md.AL.delDIC'), names_to='hab', values_to='md.delDIC') 
md.delDIC.L[,'hab.name']=NA

for(i in 1:nrow(md.delDIC.L)){
  split = strsplit(md.delDIC.L$hab[i], "[.]")
  md.delDIC.L$hab.name[i]=split[[1]][[2]]
}
md.delDIC.L =md.delDIC.L %>% select(-c('hab'))
# names(md.delDIC.L)

####Do se delta DIC
# se.delDIC.df = select(CCmod.ben.md, c('SiteID','Season','Zone','Sub_region','se.CC.delDIC','se.SG.delDIC','se.AL.delDIC'))
se.delDIC.df = select(CCmod.ben.md, c('Season','Zone','Sub_region','se.CC.delDIC','se.SG.delDIC','se.AL.delDIC'))
names(se.delDIC.df)
se.delDIC.L = se.delDIC.df %>% pivot_longer(cols=c('se.CC.delDIC','se.SG.delDIC','se.AL.delDIC'), names_to='hab', values_to='se.delDIC') 
se.delDIC.L[,'hab.name']=NA

for(i in 1:nrow(se.delDIC.L)){
  split = strsplit(se.delDIC.L$hab[i], "[.]")
  se.delDIC.L$hab.name[i]=split[[1]][[2]]
}
se.delDIC.L =se.delDIC.L %>% select(-c('hab'))
# names(se.delDIC.L)



#OK. now join the four long dataframes again 
dim(md.delDIC.L)
dim(se.delDIC.L)
dim(md.delTA.L)
dim(se.delTA.L)
#all 132x6

#want 132x9
#combine DIC md and se
# CCmod.ben.dicL = left_join(md.delDIC.L, se.delDIC.L, by=c("SiteID","Season","Zone","Sub_region","hab.name"))
CCmod.ben.dicL = left_join(md.delDIC.L, se.delDIC.L, by=c("Season","Zone","Sub_region","hab.name"))
dim(CCmod.ben.dicL) #132   7

#cobmine TA md and se
# CCmod.ben.taL = left_join(md.delTA.L, se.delTA.L, by=c("SiteID","Season","Zone","Sub_region","hab.name"))
CCmod.ben.taL = left_join(md.delTA.L, se.delTA.L, by=c("Season","Zone","Sub_region","hab.name"))
dim(CCmod.ben.taL) #132   7

# CCmod.BEN.L = left_join(CCmod.ben.dicL, CCmod.ben.taL, by=c("SiteID","Season","Zone","Sub_region","hab.name"))
CCmod.BEN.L = left_join(CCmod.ben.dicL, CCmod.ben.taL, by=c("Season","Zone","Sub_region","hab.name"))
dim(CCmod.BEN.L) #132   9
names(CCmod.BEN.L) #"SiteID"     "Season"     "Zone"       "Sub_region" "md.delDIC"  "hab.name"   "se.delDIC"  "md.delTA"   "se.delTA"  

#ok ready to roll
#FINALLY

CCmod.BEN.L$Sub_region=factor(CCmod.BEN.L$Sub_region, levels = c("BB","UK","MK","LK"))
CCmod.BEN.L$Season=factor(CCmod.BEN.L$Season, levels = c("Winter","Spring","Summer","Fall"))

# hab_colors = c('#803590','#196D51','#C0A86A')
# hab_colors = c('#785ef0','#009E73','#882255')
hab_colors = c('#785ef0','#009E73','coral')
# hab_colors = c('#CC79A7','#009E73','#E69F00')
hab_color_scale  <- scale_color_manual(name = "hab.name", values = hab_colors,
                                         breaks = c('CC','SG','AL'),
                                         labels = c('Calcifiers','Seagrass','Noncalcifying Algae'))
season_colors = c('#1E88E5','#D81B60','#FFC107','#004D40')
season_color_scale  <- scale_color_manual(name = "Season", values = season_colors,
                                       breaks = c("Winter","Spring","Summer","Fall"),
                                       labels = c("Winter","Spring","Summer","Fall"))



hab_shape = c(16,15,17)
# hab_shape = c(1,0,5)
hab_shape_scale  <- scale_shape_manual(name = "hab.name", values = hab_shape,
                                       breaks = c('CC','SG','AL'),
                                       labels = c('Calcifiers','Seagrass','Noncalcifying Algae'))

# season_shape = c(21,22,23,24)
season_shape = c(15,16,17,18)
season_shape_scale  <- scale_shape_manual(name = "Season", values = season_shape,
                                          breaks = c("Winter","Spring","Summer","Fall"),
                                          labels = c("Winter","Spring","Summer","Fall"))


BEN = CCmod.BEN.L %>%
  ggplot(aes(x=md.delDIC,y=md.delTA,
             xmin=md.delDIC-se.delDIC,xmax=md.delDIC+se.delDIC,
             ymin=md.delTA-se.delTA,ymax=md.delTA+se.delTA,
             xend=0, yend=0,
             shape=hab.name,color=hab.name))+
  hab_color_scale+
  hab_shape_scale+
  geom_segment()+
  geom_point(size=4)+ #,lwd=2)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title=paste0('Benthic Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  # facet_wrap(Sub_region~Season)+
  facet_grid(Sub_region~Season)+
  coord_fixed( ratio=1)+
  theme_bw()
BEN
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/1.allHab.BenthicModificaiton.noSite.mod=",fmod,".png"),plot=BEN,width = 10, height = 10, dpi = 300)

#facet by habitat and zone instead
SEASON = CCmod.BEN.L %>%
  ggplot(aes(x=md.delDIC,y=md.delTA,
             xmin=md.delDIC-se.delDIC,xmax=md.delDIC+se.delDIC,
             ymin=md.delTA-se.delTA,ymax=md.delTA+se.delTA,
             xend=0, yend=0,
             shape=Season,color=Season))+
  season_color_scale+
  season_shape_scale+
  geom_segment()+
  geom_point(size=4)+ #,lwd=2)+
  geom_errorbar(alpha=.5)+
  geom_errorbarh(alpha=.5)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  labs(title=paste0('Benthic Modification (mod=',fmod,')'), x='modeled DIC (umol/kg)',y='modeled TA (umol/kg)')+
  # facet_wrap("SiteID")+
  # facet_wrap(Sub_region~Season)+
  facet_grid(Sub_region~hab.name)+
  coord_fixed( ratio=1)+
  theme_bw()
SEASON
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/1.allHab.BenthicModificaiton.noSite.habFacet.mod=",fmod,".png"),plot=SEASON,width = 12, height = 10, dpi = 300)
# ggsave(filename=paste0("/Users/heidi.k.hirsh/Desktop/NewModelPlots/allHab.BenthicModificaiton.noSite.habFacet.mod=",fmod,".png"),plot=SEASON, dpi = 300)

