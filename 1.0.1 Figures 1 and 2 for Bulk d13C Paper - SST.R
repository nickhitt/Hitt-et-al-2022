#### Figures 1 and 2 for Bulk d13C Paper

#### NOTE: Archived File last Used 9th, Dec, 2021 is 1.0 Figures 1 and 2 for Bulk d13C Paper - SST
#### This file has been modified and cleaned 9th, Dec, 2021 for archiving and store in paper in Archived Codes Folder

library(dplyr)
library(ggplot2)
library(readxl)
library(stats)
library(RColorBrewer)
library(broom)

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

## Data Import

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) 

############# Figure 1

figure_1 <- data %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC 2",
                                       Coral == "64344" ~ "EAuC 1",
                                       Coral == "47996" ~ "STF 1")) %>%
  ggplot(mapping = aes(age, d13c, group = Coral_name)) +
          geom_point(aes(colour=Coral_name)) + 
          geom_smooth(aes(colour=Coral_name), method = "loess", span = 0.15, show.legend = FALSE) +
          geom_smooth(aes(colour=Coral_name), method = "loess", span = 0.15, show.legend = TRUE, se = FALSE) +
          xlab("Time (cal BP)") +
          theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
                legend.box.background = element_rect(),
                legend.box.margin = margin(6, 6, 6, 6),
                legend.key = element_rect(colour = "transparent", fill = "white"),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) 
figure_1$labels$colour <- "Coral"
figure_1$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"))

figure_1

########### Figure 2

### Loading in detrended and interpolated d13C

interp_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Detrended d13C.xlsx")) 

# Making Figure

figure_2 <- interp_data %>% 
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC 2",
                                       Coral == "64344" ~ "EAuC 1",
                                       Coral == "47996" ~ "STF 1")) %>%
  ggplot(mapping = aes(age, d13c, group = Coral_name)) +
  geom_point(aes(colour = Coral_name)) + 
  geom_line(aes(colour = Coral_name), se = FALSE) +
  xlim(0, 1500) +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank()) 

figure_2$labels$colour <- "Coral"
figure_2$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))

figure_2


