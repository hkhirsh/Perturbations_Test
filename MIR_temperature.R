#MIR Temperature
#Cheeca and Horseshoe
#Read in data from Taylor

#add libraries

Tfiles=read.csv("/Users/heidi.k.hirsh/Desktop/ChecaHorseshoeTemp/csv") #tell it to open all csv files


#read in separately: 
sn11131=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Middle_Keys_02m_sn11131_tag11131_cheeca_2023-09-22_2024-08-21_raw.csv")
sn11125=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Middle_Keys_03m_sn11125_tag11125_cheeca_2023-03-29_2024-05-22_raw.csv")
sn11182=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Middle_Keys_03m_sn11182_tag11182_cheeca_2023-03-29_2024-05-22_raw.csv")
sn11188=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Middle_Keys_04m_sn11188_tag11188_cheeca_2023-08-19_2024-08-21_raw.csv")
sn11123=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Upper_Keys_03m_sn11123_tag11123_horseshoe_2023-03-28_2024-05-23_raw.csv")
sn11136=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Upper_Keys_03m_sn11136_tag11136_horseshoe_2023-08-18_2024-05-23_raw.csv")
sn11179=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Upper_Keys_03m_sn11179_tag11179_horseshoe_2023-03-28_2024-05-23_raw.csv")
sn11118=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Upper_Keys_04m_sn11118_tag11118_horseshoe_2023-03-28_2024-05-23_raw.csv")
sn11185=read.csv("/Users/heidi.k.hirsh/Desktop/CheecaHorseshoeTemp/FL_Keys_Upper_Keys_04m_sn11185_tag11185_horseshoe_2023-03-28_2024-05-23_raw.csv")


head(Tfiles)



#write a loop to open file 
#read in each file (9 total
#parse and add values to represent time and site. 


#make a datetime value
ggplot()+
  geom_point()