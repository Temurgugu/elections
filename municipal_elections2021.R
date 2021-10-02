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


election_2021 <- read_excel("data/MunicipalElections.xlsx")
#tg_data_1$round <- factor(election_2021$round, 
#                          levels = c("პირველი ტური", "მეორე ტური"))


election_2021_compare <-  election_2021 %>%
  select(region, percentage, time, year)  %>%
  pivot_wider(names_from = year, values_from = percentage) %>%
  data.table::setnames(old = c("2014", "2017", "2021"), new = c("y2014", "y2017", "y2021"))  %>%
  mutate(year20142021 = y2021 - y2014) %>%
  mutate(year20172021 = y2021 - y2017) %>%
  select(region, time, year20142021, year20172021) %>%
  pivot_longer(cols = starts_with("year"), names_to = "year", values_to = "difference")

election_2021_compare$difference <- base::round(election_2021_compare$difference , 1)




PMunicipalElections <- ggplot(election_2021, aes(time , region)) +
  geom_tile(aes(fill =percentage)) +
  geom_text(aes(label = percentage), size = 3, color = "black", show.legend = FALSE)+
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
       caption = "წყარო: ცენტრალური საარჩევნო კომისიის (ცესკო)",
       color="")+
  xlab("")+
  ylab("რეგიონი")+ 
  labs(fill = "ამომრჩეველთა \nაქტივობა %")+
  scale_x_discrete()+
  facet_grid(cols = vars(year))

PMunicipalElections

#Save the ggplot
ggsave("visualization/MunicipalElections.png", 
       plot = MunicipalElections)  





PMunicipalElectionsCompare <- ggplot(election_2021_compare, aes(time , region)) +
  geom_tile(aes(fill =difference)) +
  geom_text(aes(label = difference), size = 3,  color = "black", show.legend = FALSE)+
  theme_gray(base_family="Sylfaen")+
  scale_fill_viridis(option = "viridis", direction = -1,
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
       caption = "წყარო: ცენტრალური საარჩევნო კომისიის (ცესკო)",
       color="")+
  xlab("")+
  ylab("რეგიონი")+ 
  labs(fill = "ამომრჩეველთა \nაქტივობა %")+
  scale_x_discrete()+
  facet_grid(cols = vars(year))

PMunicipalElectionsCompare

#Save the ggplot
ggsave("visualization/PMunicipalElectionsCompare.png", 
       plot = PMunicipalElectionsCompare)     

