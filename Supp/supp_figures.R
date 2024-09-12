library(tidyverse)
library(lubridate)
library(conflicted)
library(ggpubr)
library(ggplot2)
library(ggbeeswarm)
library(ggtext)
library(dplyr)
library(tidyr)
library(cowplot)
library(here)

conflict_prefer("filter", "dplyr")

#set wd
setwd<-here() 

#set theme for plotting 
aesthetic <- 
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = ggplot2::element_line(size = 0.25, colour = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,face='plain', size=8),
    axis.text.y = element_text(face = "plain", size = 8),
    strip.text.y = element_text(size=10),        
    axis.title.y = element_text(size = 10, angle=90,face = "plain"),
    panel.border = element_blank(),
    strip.text = element_text(face = "plain", size = 8))

#################
#load in data
################
#qPCR

pivot_amp<-readRDS('../Data/controls_tidy.rds')

########
# PLot for Blanks

pivot_amp$species <- factor(pivot_amp$species , 
                                   levels = c('A. duodenale','A. lumbricoides','N. americanus','T. trichiura','IAC',"B. atrophaeus"))

blank_labels <- data.frame(country = c("Benin", "India"), label = c("", "Not tested"))

blanks<-pivot_amp%>%
  filter(sample_type==c('field blank'))%>%
  ggplot(aes(x= species, y =percent , color= country, fill = country))+
  geom_quasirandom()+
  facet_grid(country~.)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "Country",
                     labels = c("Benin (n=193)", "India (n=155)")) +
  scale_fill_manual(values = c("#29103e","#F37413"), name = "Country",
                    labels = c("Benin (n=193)", "India (n=155)")) +
  geom_text(x = 1, y = 20,aes(label = label), data = blank_labels,size=2.8,color='black', angle=90,inherit.aes = FALSE)+
  aesthetic+
  theme(legend.text=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'),
        axis.text.x = element_text(angle = 45,face='italic', size=10))

blanks

#save_plot("../figures/Supp_figs/blanks.pdf",blanks, base_height = 6, base_width = 6)


#IAC species
pivot_amp<- pivot_amp%>%
  mutate(sample_type = ifelse( sample_type == 'STANDARD', 'standard', sample_type))%>%
  mutate(sample_type = ifelse( sample_type == 'no template ', 'NTC',sample_type))

pivot_amp$sample_type <- factor(pivot_amp$sample_type , 
                                levels = c("grab","moore","moore T2", "sediment" , "C. water" , "OD Area, center"  , "OD Area, entrance" ,"OD Area, edge" ,"Market, entrance" , "Market, other" , "Market, register" ,  "School, class" , "School, entrance" , "School, latrine" ,"Household","standard","field blank", "NTC", "NSC"), 
                                labels  = c("grab","Moore T1", "MooreT2","sediment","community water",
                                            "od center","od entrance","od edge","market other","market entrance","market register",
                                            'school class','school entrance','school latrine',"household","standard","blank","NTC",'NSC'))

pivot_amp_sp<-pivot_amp%>%
  filter(sample_type == 'standard') %>%
  mutate(specie = ifelse(species == "B. atrophaeus", '5', species))

pivot_amp_sp$specie <- factor(pivot_amp_sp$specie , 
                            labels = c('A. duodenale','A. lumbricoides','N. americanus','T. trichiura','IAC'))


IACsp<-pivot_amp_sp%>%
  filter(specie != 'IAC')%>%
  ggplot(aes(x= specie, y =as.numeric(percent) , color= country, fill = country))+
  geom_quasirandom()+
  facet_grid(country~.)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "Country",
                     labels = c("Benin (n=193)", "India (n=155)")) +
  scale_fill_manual(values = c("#29103e","#F37413"), name = "Country",
                    labels = c("Benin (n=193)", "India (n=155)")) +
  scale_y_continuous(expand=c(0,0), limits=c(-5,120), breaks=c(0,25,50,75,100))+
  geom_text(x = 1, y = 20,aes(label = label), data = blank_labels,size=2.8,color='black', angle=90,inherit.aes = FALSE)+
  aesthetic+
  theme(legend.text=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'),
        axis.text.x = element_text(angle = 45,face='plain', size=10))

IACsp
save_plot("../figures/Supp_figs/IACsp.pdf",IACsp, base_height = 6, base_width = 6)


#plot for IACs for each sample type 

pivot_amp_st<-pivot_amp%>%
  filter(sample_type == 'standard' & percent != 0)%>%
  group_by(country)%>%
  mutate(percent = mean(percent))%>%
  mutate_all(~replace(., is.na(.), 'IAC'))%>%# replace NAs  
mutate(specie = species)
  
pivot_amp_con<-pivot_amp%>%
  filter(sample_type!='standard')%>%
  mutate(specie = ifelse(sample_type == 'NSC', "5", species))%>%
  rbind(.,pivot_amp_st)


pivot_amp_con$specie <- factor(pivot_amp_con$specie , 
                            labels = c('A. duodenale','A. lumbricoides','N. americanus','T. trichiura','IAC','B. atrophaeus','A. duodenale','A. lumbricoides','IAC','N. americanus','T. trichiura'))


#plot to look at sample dif by country, Fig 2 E 
sup_labels <- data.frame(country = c("Benin", "India"), label = c("", "Not tested"))

IACs<-pivot_amp_con%>%
  filter(specie == 'IAC'| sample_type %in% c('standard','NTC'), sample_type != 'blank')%>%
  ggplot(aes(x= sample_type, y =as.numeric(percent) , color= country, fill = country))+
  geom_quasirandom()+
  facet_grid(country~.)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "Country",
                     labels = c("Benin (n=193)", "India (n=155)")) +
  scale_fill_manual(values = c("#29103e","#F37413"), name = "Country",
                    labels = c("Benin (n=193)", "India (n=155)")) +
  geom_vline(xintercept=15.5, size=1)+
  annotate("text", x=8, y=110, label = "IAC", size = 3, fontface=2) +
  annotate("text", x=16.5, y=110, label = "Other", size = 3, fontface=2) +
  scale_y_continuous(expand=c(0,0), limits=c(-5,120), breaks=c(0,25,50,75,100))+
  geom_text(x = 18, y = 20,aes(label = label), data = sup_labels,size=2.8,color='black', angle=90,inherit.aes = FALSE)+
  geom_text(x = 15, y = 20,aes(label = label), data = sup_labels,size=2.8,color='black', angle=90,inherit.aes = FALSE)+
  aesthetic+
  theme(legend.text=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'),
        axis.text.x = element_text(angle = 45,face='plain', size=10))

IACs
save_plot("../figures/Supp_figs/IAC.pdf",IACs, base_height = 6, base_width = 6)


