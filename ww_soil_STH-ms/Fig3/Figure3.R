library(tidyverse)
library(ggpubr)
library(ggtext)
library(gplots)
library(dplyr)
library(cowplot)
library(magick)
library(pdftools)
library(grImport2)
library(here)

#Set wd

setwd<-here()

# Read in all data 

Data<-readRDS('../Data/TAC_data.RDS')

#set theme 
aesthetic <- 
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),  axis.line = ggplot2::element_line(size = 0.25, colour = "black"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    axis.title.x=element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1,face='plain', size=8),
    axis.text.y = element_text(face = "plain", size = 8),
    strip.text.y = element_blank(),        
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    strip.text = element_text(face = "plain", size = 8))

#remove control data for plotting 
tac_all <- Data %>% 
  # remove controls
  filter(Pathogen != "Control - 16S" & Pathogen != "Control - 18S" & 
           Pathogen != "CrAssphage fecal indicator" & Pathogen != "Control")%>%
  filter(sample_type != 'Control'& sample_type != 'NTC' & sample_type != 'Moore2')

#counts for each sample type and country
counts<-tac_all%>%
 select(Sample,sample_type,country)%>%
  unique()%>%
  group_by(sample_type,country)%>%
  count()
  
  
# pathogen hits by sample type
sum_tac <- tac_all %>% 
  mutate(Detect = if_else(Call == "Presence", 1, 0)) %>% 
  group_by(country, sample_type, Pathogen) %>% 
  summarise(Sum_Detect = sum(Detect)) %>% 
  mutate(Detect = as.factor(if_else(Sum_Detect > 0, "Yes", "No")))%>% #collapse data into single output for each target, sample type and country
  ungroup()

  
#Benin sample order 

#set plotting order
sum_tac$Pathogen <- factor(sum_tac$Pathogen , 
                                    levels = c("Adenovirus","Astrovirus",'Enterovirus',"Norovirus",
                                               "Rotavirus","Sapovirus","Aeromonas","Bacteroides fragilis",
                                               "Campylobacter","Clostridium difficile","E. coli","E. coli/Shigella",
                                               "EAEC","EHEC 0157H7","EPEC","ETEC","Helicobacter pylori","P. shigelloides",
                                               "STEC","Salmonella Typhi","Shigella flexneri", "Vibrio cholerae","Y. enterocolitica",    
                                                "EHEC", "Ascaris", "Cryptosporidium","Cyclospora" , "Entamoeba histolytica",
                                              "Giardia" , "Necator", "Strongyloides", "Trichuris"))
#set facet groups 
sum_tac<-sum_tac%>%
  mutate(pathogen_type= case_when(
   Pathogen %in% c("Adenovirus","Astrovirus",'Enterovirus',"Norovirus",
                "Rotavirus","Sapovirus") ~ 'Virus' ,
   Pathogen %in% c("Aeromonas","Bacteroides fragilis",
                 "Campylobacter","Clostridium difficile","E. coli","E. coli/Shigella",
                 "EAEC","EHEC 0157H7","EPEC","ETEC","Helicobacter pylori","P. shigelloides",
                 "STEC","Salmonella Typhi","Shigella flexneri", "Vibrio cholerae","Y. enterocolitica",    
                 "EHEC") ~ 'Bacteria',
   Pathogen %in% c("Ascaris", "Cryptosporidium","Cyclospora" , "Entamoeba histolytica",
                 "Giardia" , "Necator", "Strongyloides", "Trichuris") ~ 'Parasite'
  ))

#order facet groups 
sum_tac$pathogen_type <- factor(sum_tac$pathogen_type , 
                           levels = c('Virus', 'Bacteria', 'Parasite'))
#########################
#Figure 3 A Plot Benin
#########################

fig3A_benin<-sum_tac %>% 
   filter(country=='Benin')%>%
  ggplot(aes(y=sample_type, x=Pathogen, fill=Detect)) +
  geom_tile(color='white') +
  scale_fill_manual(values=c("#dbd7d2","#29103e"), name= 'Detect Benin') +
  scale_y_discrete(expand=c(0,0))+
  facet_grid(cols= vars(pathogen_type), scale='free_x', space='free')+
  theme_bw() +
  labs(x="") +
  aesthetic+
  theme_minimal() +
  theme( axis.title.y = element_blank(),
        axis.text.x = element_text(size=8, angle=75, hjust=1.05, vjust=1.05, face= 'italic'),
        panel.border = element_rect(color="black", fill=NA), 
        )
fig3A_benin

#########################
#Figure 3 B Plot India
#########################

fig3B_india<-sum_tac %>% 
  filter(country=='India')%>%
  ggplot(aes(y=sample_type, x=Pathogen, fill=Detect)) +
  geom_tile(color = "white") +
  scale_fill_manual(values=c("#dbd7d2","#F37413"), name= 'Detect India') +
  scale_y_discrete(expand=c(0,0))+
  facet_grid(cols= vars(pathogen_type), scale='free_x', space='free')+
  theme_bw() +
  labs(x="") +
  aesthetic+
  theme_minimal() +
  theme(    axis.title.y = element_blank(),
        axis.text.x = element_text(size=8, angle=75, hjust=1.05, vjust=1.05, face= 'italic'),
        panel.border = element_rect(color="black", fill=NA))
fig3B_india

###################
# figure plot 
###################

illust.fig3C <- image_read_pdf("../Figures/Schematics/Abstract schematic.pdf", density = 600)

Fig3C <- ggdraw() +
  draw_image(illust.fig3C, scale = 1)


top<- plot_grid(fig3A_benin,fig3B_india, labels = c('A','B'), axis = 'l',ncol=2, rel_widths = c(1.2,.9))
fig3<-plot_grid(top,Fig3C, labels = c('','C'), axis = 'l',ncol=1)
  
  
save_plot("../figures/fig3.pdf",fig3, base_height = 8, base_width = 10)

