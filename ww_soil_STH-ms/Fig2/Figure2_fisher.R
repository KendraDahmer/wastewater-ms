library(tidyverse)
library(grid)
library(mgcv)
library(lubridate)
library(conflicted)
library(ggbeeswarm)
library(ggpubr)
library(ggtext)
library(gplots)
library(dplyr)
library(cowplot)
library(magick)
library(pdftools)
library(broom)
library(RVAideMemoire)
library(rstatix)
library(corrplot)
library(ggcorrplot)
library(ggpubr)
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

prev_sum<-readRDS('../Data/qPCR_prev_sum.RDS')
prev_summary<-readRDS('../Data/qPCR_prev_summary.RDS')

#####################
#plots
####################
#plots for prevalence

# set plotting axis order 
prev_sum$sample_label <- factor(prev_sum$sample_label , 
                                levels = c("grab","Moore \n swab", "sediment","community \n water",
                                           "open \n defecation","market",'school',"household"))

prev_summary$sample_label <- factor(prev_summary$sample_label , 
                                    levels = c("grab","Moore \n swab", "sediment","community \n water",
                                               "open \n defecation","market",'school',"household"))
###########################
#plot for any STH, Fig 2 A
###########################

any_species_plot<- prev_sum%>%
  filter(!sample_label %in% c('standard', 'field blank', 'no template '))%>%
  ggplot(aes(x= country, y = prev*100, color= country, fill = country))+
  geom_bar(position="dodge", stat="identity")+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("white","white"),name = "Country",
                     labels = c("Benin (n=185)", "India (n=155)")) +
  scale_fill_manual(values = c("#29103e","#F37413"), name = "Country",
                    labels = c("Benin (n=185)", "India (n=155)")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,50))+
  geom_label(aes(label=paste0("N=",total," (", round(prev*100,0),"%)")), size=2.5)+ 
  aesthetic+
  theme( legend.position = "none",
         axis.text.x = element_text(angle = 0, hjust = .5,face='plain', size=10))
any_species_plot

###############################################################
#plot for overall species prevalence in each country, Fig 2 B 
###############################################################

prev_plot_species<- prev_summary%>%
  filter(!sample_label %in% c('standard', 'field blank', 'no template '))%>%
  ggplot(aes(x= species, y = species_percent, color= country, fill = country))+
  geom_bar(position="dodge", stat="identity")+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("white","white"),name = "Country",
                     labels = c("Benin (n=185)", "India (n=155)")) +
  scale_fill_manual(values = c("#29103e","#F37413"), name = "Country",
                    labels = c("Benin (n=185)", "India (n=155)")) +
  scale_y_continuous(expand=c(0,0), limits=c(0,40))+
  geom_text(aes(1.2, 20,label = 'Not tested'), size=2.4,color='black', angle=90)+
  geom_text(aes(3.8, 20.3,label = 'Not detected'), size=2.4,color='black', angle=90)+
  geom_text(aes(4.2, 20.3,label = 'Not detected'), size=2.4,color='black', angle=90)+
  aesthetic+
  theme( legend.position = "right",
         axis.text.x = element_text(angle = 45, hjust = 1,face='italic', size=8))
prev_plot_species

######################################################
#prev by sample type (soil vs wastewater), Fig 2 C
######################################################

prev_plot_sample<- prev_sum%>%
  filter(! sample_type %in% c('standard', 'field blank', 'no template '))%>%
  ggplot(aes(x= sample, y = prev_sample*100, color= country, fill = country))+
  geom_bar(position = 'dodge',stat = 'identity', width=.5)+
  facet_wrap(~country)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("white","white"),name = "", guide ='none') +
  scale_fill_manual(values = c("#29103e","#F37413"),name = "" ) +
  scale_y_continuous(expand=c(0,0), limits=c(0,70), breaks=c(10,20,30,40,50,60,70))+
  aesthetic+
  geom_label(aes(label=paste0("N=",total_sample," (", round(prev_sample*100,0),"%)")), size=2.5)+ 
  theme(legend.text=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'))

prev_plot_sample

################################
#prev by sample label, Fig 2 D 
################################

#set labels for geom_text in fig D
D_labels <- data.frame(country = c("Benin", "India"), label = c("", "Not tested"))

#plot
prev_plot_country<- prev_sum%>%
  filter(! sample_type %in% c('standard', 'field blank', 'no template '))%>%
  ggplot(aes(x= sample_label, y = prev_label*100, color= country, fill = country))+
  geom_bar(position = 'dodge',stat = 'identity', width=.5)+
  facet_wrap(~country)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "", guide ='none') +
  scale_fill_manual(values = c("#29103e","#F37413"),name = "") +
  scale_y_continuous(expand=c(0,0), limits=c(0,70), breaks=c(10,20,30,40,50,60,70))+
  coord_flip()+
  aesthetic+
  theme(legend.text=element_blank(),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'))+
  #guides(color=guide_legend(nrow=2))+ 
  geom_text(x = 8, y = 20, aes(label = label), data = D_labels,size=2.8,color='black', angle=0)+
  geom_hline(yintercept= 0, size=1.2)+
  geom_vline(xintercept=3.5)+
  geom_text(aes(6, 60,label = 'Soil'), size=2.2,color='black', angle=270)+
  geom_text(aes(1.6, 60,label = 'Wastewtaer'), size=2.2,color='black', angle=270)

prev_plot_country #can also use: geom_bar(position= 'dodge',stat = 'identity')+

#################################################
#plot to look at sample dif by country, Fig 2 E 
#################################################

#set labels for geom_text in fig E
E_labels <- data.frame(country = c("Benin", "India"), label = c("", "Not tested"))

#plot
prev_plot_country_soil<- prev_summary%>%
  #filter(sample == 'soil')%>%
  filter(! sample_type %in% c('standard', 'field blank', 'no template '))%>%
  ggplot(aes(x= sample_label, y = percent_label, fill = species))+
  geom_bar(position = 'dodge',stat = 'identity', width=.5)+
  facet_wrap(~country)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "", guide ='none') +
  scale_fill_manual(values = c("#662A0F","#008080","#FAD510","grey"),name = "Species") +
  scale_y_continuous(expand=c(0,0), limits=c(0,70), breaks=c(10,20,30,40,50,60,70))+
  coord_flip()+
  aesthetic+
  theme(legend.text=element_text(face='italic'),
        legend.key.size = unit(.5, 'cm'),
        legend.position = 'right',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'))+
  #guides(color=guide_legend(nrow=2))+ 
  geom_text(x = 7.9, y = 25,aes(label = label), data = E_labels,size=2.8,color='black', angle=0,inherit.aes = FALSE)+
  geom_hline(yintercept= 0, size=1.2)+
  geom_vline(xintercept=3.5)+
  geom_text(aes(6, 60,label = 'Soil'), size=2.2,color='black', angle=270)+
  geom_text(aes(1.9, 60,label = 'Wastewtaer'),size=2.2, color='black', angle=270)
      
prev_plot_country_soil #can also use: geom_bar(position= 'dodge',stat = 'identity')+

#########################################################
# plot to look at locations and Moore processing Fig 2 F
#########################################################

#set order and labels for plotting 
prev_summary$sample_type <- factor(prev_summary$sample_type , 
                                   levels = c("grab","moore","moore T2", "sediment" , "C. water" , "OD Area, center"  , "OD Area, entrance" ,"OD Area, edge" ,"Market, entrance" , "Market, other" , "Market, register" ,  "School, class" , "School, entrance" , "School, latrine" ,"Household"), 
                                   labels  = c("grab","Moore \n swab T1", "Moore \n swab T2","sediment","community \n water",
                                               "open \n defecation center","open \n defecation entrance","open \n defecation edge","market other","market entrance","market register",
                                               'school class','school entrance','school latrine',"household"))
#plot
prev_plot_types_soil<- prev_summary%>%
  filter(! sample_type %in% c('standard', 'field blank', 'no template ', 'household','grab','sediment', 'community \n water'))%>%
  ggplot(aes(x= sample_type, y = percent, color= country, fill = country))+
  geom_bar(position= 'dodge',stat = 'identity', width=.5)+
  facet_wrap(~country)+
  labs(x = "", y = "Detection (%)") +
  scale_color_manual(values = c("#29103e","#F37413"),name = "", guide ='none') +
  scale_fill_manual(values = c("#29103e","#F37413"),name = "Species") +
  scale_y_continuous(expand=c(0,0), limits=c(0,50), breaks=c(10,20,30,40,50))+
  coord_flip()+
  aesthetic+
  theme(
        legend.position = 'none',
        axis.title.x=element_text(size=10),
        axis.title.y = element_text(size=10),
        strip.background = element_blank(),
        strip.text.x = element_text(size=10, face ='plain'))+
  #guides(color=guide_legend(nrow=2))+ 
  geom_hline(yintercept= 0, size=1.2)+
  geom_vline(xintercept=8.5, linetype="dotted")+
  geom_vline(xintercept=5.5, linetype="dotted")+
  geom_vline(xintercept=2.5)+
  geom_text(aes(7, 45,label = 'Soil'), size=2.2,color='black', angle=270)+
  geom_text(aes(1.5, 45,label = 'Wastewtaer'), size=2.2,color='black', angle=270)

prev_plot_types_soil #can also use for stacked bars: geom_bar(stat = 'summary')+

#####################
#Fisher exact stats
#####################

library(rstatix)

################ sample (soil vs ww) Fig 2 c ##################################

#both countries combined 
stats_df<- prev_sum%>%
  select(2,3,21)%>%
  drop_na()

stats_df$any_pos <-factor(
  stats_df$any_pos, levels = c(1, 0),
  labels = c("positive", "negative")
)

xtabs( ~ any_pos+sample+country,
       data=stats_df)
#is there a difference by sample type 
fisher.test(stats_df$any_pos, stats_df$sample,workspace=2e9)


#Benin 
stats_df.b<- prev_sum%>%
  select(2,3,21)%>%
  filter(country=='Benin')%>%
  drop_na()

stats_df.b$any_pos <-factor(
  stats_df.b$any_pos, levels = c(1, 0),
  labels = c("positive", "negative")
)

xtabs( ~ any_pos+sample,
       data=stats_df.b)
#is there a difference by sample type 
fisher.test(stats_df.b$any_pos, stats_df.b$sample,workspace=2e9)

#India
stats_df.i<- prev_sum%>%
  select(2,3,21)%>%
  filter(country=='India')%>%
  drop_na()

stats_df.i$any_pos <-factor(
  stats_df.i$any_pos, levels = c(1, 0),
  labels = c("positive", "negative")
)

xtabs( ~ any_pos+sample,
       data=stats_df.i)

#is there a difference by sample type 
fisher.test(stats_df.i$any_pos, stats_df.i$sample,workspace=2e9)

############## sample label Fig 2 D ###########################################

#Benin 

stats_df.b.2<- prev_sum%>%
  select(2,20,21)%>%
  filter(country == "Benin")%>%
  drop_na()

stats_df_benin_pw<-stats_df.b.2%>%select(2,3)%>%
  group_by(sample_label)%>%
  count(any_pos)%>%
  pivot_wider(names_from = sample_label, values_from = n)%>%
  column_to_rownames(var="any_pos")
stats_df_benin_pw[is.na(stats_df_benin_pw)] <- 0

#is there a difference by sample sites  
pairwise_BJ<-pairwise_fisher_test(as.matrix(stats_df_benin_pw), p.adjust.method = "none")

#write.csv(pairwise_BJ, '../stats_csv/fisher/stat_Fig2D_fisher_BJ.csv')

#India 

stats_df.i.2<- prev_sum%>%
  select(2,20,21)%>%
  filter(country == "India")%>%
  drop_na()

stats_df_india_pw<-stats_df.i.2%>%select(2,3)%>%
  group_by(sample_label)%>%
  count(any_pos)%>%
  pivot_wider(names_from = sample_label, values_from = n)%>%
  column_to_rownames(var="any_pos")
stats_df_india_pw[is.na(stats_df_india_pw)] <- 0

#is there a difference by sample sites 
pairwise_IN<-pairwise_fisher_test(as.matrix(stats_df_india_pw), p.adjust.method = "none")

#write.csv(pairwise_IN, '../stats_csv/fisher/stat_Fig2D_fisher_IN.csv')


########## sample type species Fig 2 E ########################################

#Benin 

stats_df.b.3<- prev_summary%>%
  select(2,4,16,17,18)%>%
  filter(country == 'Benin', species== 'N.americanus')%>% #replace with species of interest 
  drop_na()

#stats_df.b.3$combine<- paste(stats_df.b.3$sample_label,stats_df.b.3$species) #use this for all species in df


stats_df_benin_pw2<-stats_df.b.3%>%select(3,4,5)%>%
  group_by(sample_label)%>%
  count(amplification)%>%
  pivot_wider(names_from = sample_label, values_from = n)%>%
  column_to_rownames(var="amplification")
stats_df_benin_pw2[is.na(stats_df_benin_pw2)] <- 0

#is there a difference by sample sites for species composition
pairwise_BJ2<-pairwise_fisher_test(as.matrix(stats_df_benin_pw2), p.adjust.method = "none")

#write.csv(pairwise_BJ2, '../stats_csv/fisher/stat_Fig2E_AD_fisher_BJ.csv')

#India

stats_df.i.3<- prev_summary%>%
  select(2,4,16,17,18)%>%
  filter(country == 'India', species == 'A.lumbricoides')%>%#replace with species of interest
  drop_na()

#stats_df.i.3$combine<- paste(stats_df.i.3$sample_label,stats_df.i.3$species) #use this for all species in df

stats_df_india_pw2<-stats_df.i.3%>%select(3,4,5)%>%
  group_by(sample_label)%>%
  count(amplification)%>%
  pivot_wider(names_from = sample_label, values_from = n)%>%
  column_to_rownames(var="amplification")
stats_df_india_pw2[is.na(stats_df_india_pw2)] <- 0

#is there a difference by sample sites for species composition
pairwise_IN2<-pairwise_fisher_test(as.matrix(stats_df_india_pw2), p.adjust.method = "none")

#write.csv(pairwise_IN2, '../stats_csv/fisher/stat_Fig2E_NA_fisher_IN.csv')

################### sample type  Fig 2 F ###############################

#benin 
stats_df.b.4<- prev_summary%>%
  select(2,4,16,17,18)%>%
  filter(country =='Benin', !sample_type %in% c('grab','sediment','household', 'community \n water'))%>%
  drop_na()

stats_df_benin_pw3<-stats_df.b.4%>%
  group_by(sample_type)%>%
  count(amplification)%>%
  pivot_wider(names_from = sample_type, values_from = n)%>%
  column_to_rownames(var="amplification")
stats_df_benin_pw3[is.na(stats_df_benin_pw3)] <- 0

#is there a difference by location within site 
pairwise_BJ3<-pairwise_fisher_test(as.matrix(stats_df_benin_pw3), p.adjust.method = "none")

#write.csv(pairwise_BJ3, '../stats_csv/fisher/stat_Fig2F_fisher_BJ.csv')

#India
stats_df.i.4<- prev_summary%>%
  select(2,4,16,17,18)%>%
  filter(country =='India', !sample_type %in% c('grab','sediment', 'community \n water'))%>%
  drop_na()

stats_df_india_pw3<-stats_df.i.4%>%
  group_by(sample_type)%>%
  count(amplification)%>%
  pivot_wider(names_from = sample_type, values_from = n)%>%
  column_to_rownames(var="amplification")
stats_df_india_pw3[is.na(stats_df_india_pw3)] <- 0

#is there a difference by location within site 
pairwise_IN3<-pairwise_fisher_test(as.matrix(stats_df_india_pw3), p.adjust.method = "none")

#write.csv(pairwise_IN3, '../stats_csv/fisher/stat_Fig2F_fisher_IN.csv')

###############
#figure plot 
###############                 

topblock<- plot_grid(any_species_plot, prev_plot_species,prev_plot_sample,prev_plot_country, labels=c('A','B','C','D'), ncol=2, align = 'hv',axis = 'ltb',rel_widths = c(.8,1,1,1),rel_heights = c(.6,1,1.8,1.8) )
topblock


bottom<-plot_grid(prev_plot_country_soil, prev_plot_types_soil, labels=c('E','F'), ncol=2, align = 'v', axis = 'lr', rel_widths = c(1.2,.8))
bottom


figure2<- plot_grid(topblock, bottom, ncol=1, align = 'vh',axis = 'btlr')
figure2

save_plot("../figures/fig2_updated.pdf",figure2, base_height = 10, base_width = 8)
             
