### This script produces the Main Text Detrended d13C Figures for Hitt et al 2022


library(dplyr)
library(readxl)
library(stats)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(zoo)
library(pracma)

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/detrend_functions.R")

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::mutate(Coral_name = case_when(Coral == "35104" ~ "EAuC2",
                                       Coral == "64344" ~ "EAuC1",
                                       Coral == "47996" ~ "STF1"))

means <- aggregate(d13c ~ Coral_name, data, mean)

coral_steps <- rep(0,3)
for (i in 1:3){
  coral <- c("EAuC1", "EAuC2", "STF1")
  new_df <- data %>%
    filter(Coral_name %in% coral[i]) %>%
    select(age)
  new_age <- rep(0, length(new_df-1))
  for (q in 1:length(new_age)){
    new_age[q] <- new_df$age[q+1] - new_df$age[q]
  }
  coral_steps[i] <- max(new_age)
}

min_age <- ceiling(min(data[which(data$Coral_name == "STF1"),1])) 
max_age <- floor(max(data[which(data$Coral_name == "EAuC2"),1])) 

time_vec <- seq(from = min_age, to = max_age, by = ceiling(max(coral_steps)))
time_vec <- data.frame(time_vec) %>%
  rename(age_int = time_vec)

detrended_eauc1_raw <- detrend_coral(data, "EAuC1", "d13c") 
detrended_eauc2_raw <- detrend_coral(data, "EAuC2", "d13c") 
detrended_stf1_raw <- detrend_coral(data, "STF1", "d13c") 

detrended_eauc1 <- detrend_coral_interp(data, "EAuC1", "d13c", time_vec) 
detrended_eauc2 <- detrend_coral_interp(data, "EAuC2", "d13c", time_vec) 
detrended_stf1 <- detrend_coral_interp(data, "STF1", "d13c", time_vec) 

figure3a <- rbind(detrended_eauc1, detrended_eauc2, detrended_stf1) %>%
  rename(age = age_int, d13c = output) %>%
  ggplot(mapping = aes(age, d13c, group = Coral_name)) +
  geom_point(aes(colour = Coral_name)) + 
  geom_line(aes(colour = Coral_name)) +
  xlim(0, 3000) +
  xlab("Time (cal BP)") +
  scale_x_continuous(breaks=seq(0,3000,500)) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank()) 

figure3a$labels$colour <- "Coral"
figure3a$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))

figure3a$labels$title <- expression(paste("Southwest Pacific Corals Detrended Bulk ", "\u03B4" ^ "13", "C", " Resampled Data"))

figure3a

figure3b <- rbind(detrended_eauc1_raw, detrended_eauc2_raw, detrended_stf1_raw) %>%
  rename(age = dataframe_2.age, d13c = detrended) %>%
  ggplot(mapping = aes(age, d13c, group = Coral_name)) +
  geom_point(aes(colour = Coral_name)) + 
  geom_line(aes(colour = Coral_name)) +
  xlim(0, 3000) +
  xlab("Time (cal BP)") +
  scale_x_continuous(breaks=seq(0,3000,500)) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.minor = element_blank()) 

figure3b$labels$colour <- "Coral"
figure3b$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))

figure3b$labels$title <- expression(paste("Southwest Pacific Corals Detrended Bulk ", "\u03B4" ^ "13", "C", " Raw Data"))


figure3b


final_figure3 <- ggarrange(figure3a, figure3b, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2)

final_figure3

