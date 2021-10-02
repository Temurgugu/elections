rm(list=ls())    # clear the workspace

#=== Temur Gugushvii === 

library(tidyr)
library(tidyverse)
library(ggplot2)
library(readxl)
library(extrafont)
library(cowplot)
library(hrbrthemes)
library(plotly)
library(viridis)


election_2021 <- read_excel("data/election_2021.xlsx")
#tg_data_1$round <- factor(election_2021$round, 
#                          levels = c("პირველი ტური", "მეორე ტური"))


tg_p_voting <- ggplot(election_2021, aes(time , region, fill =percentage)) +
  geom_tile() +
  geom_text(aes(label = percentage), size = 3)+
  theme_gray(base_family="Sylfaen")+
  
  scale_fill_viridis(option = "plasma", direction = -1,
                     begin = 0, end = 1, discrete=FALSE)+
  theme_ipsum(base_family="Sylfaen") +
  theme(axis.title.x = element_text(colour="black", size=12, hjust=0.5),
        axis.title.y = element_text(colour="black", size=12, hjust=0.5),
        axis.text.x=element_text(angle = 90,  hjust=0.5, size=14, colour="black"),
        axis.text.y=element_text(size=12),
        plot.caption = element_text(size=14, colour="black", hjust=0),
        plot.title=element_text(colour="black", size=12),
        legend.title=element_text(size=12),
        legend.text=element_text(size=12))+
  labs(title = "",
       subtitle ="",
       caption = "წყარო:ცენტრალური საარჩევნო კომისიის (ცესკო)",
       color="")+
  xlab("")+
  ylab("რეგიონი")+ 
  labs(fill = "ამომრჩეველთა \nაქტივობა %")+
  scale_x_discrete()+
  facet_grid(cols = vars(year))


#Save the ggplot
ggsave("visualization/tg_p_voting.png", 
       plot = tg_p_voting)     
