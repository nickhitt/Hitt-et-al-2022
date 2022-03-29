#### Supp Figure  for Bulk d13C Paper

#### This code produces plot the variance plot through time of EAuC2


library(dplyr)
library(readxl)
library(stats)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(zoo)
library(pracma)



setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC2",
                                       Coral == "64344" ~ "EAuC1",
                                       Coral == "47996" ~ "STF1")) %>%
  filter(Coral_name == "EAuC2")

supp_fig <- data %>%
  ggplot(mapping = aes(age, d13c)) +
  geom_point() + 
  geom_line() +
  xlim(0, 3000) +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank()) 

supp_fig
