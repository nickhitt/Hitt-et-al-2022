#### Figures 1 and 2 for Bulk d13C Paper

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

#### Interpolating to consistent resolution - FAILED DO NOT USE
# likely failed to difference between york_fit in matlab and lm in R - no york_fit available in R as of 23102021

# # Chopping Data
# 
# chop_data <- data %>%
#   subset(age > 0 & age < 1500)
# 
# steps <-  round(1500/50)
# 
# time_step <- seq(0,1500, steps)
# 
# interp_coral <- function(data, coral, time_steps){
#   coral_filtered <- data %>%
#     dplyr::filter(Coral == coral) %>%
#     dplyr::select(age, d13c)
#   output <- as.data.frame(approx(coral_filtered$age, coral_filtered$d13c,
#                                  time_steps, method = "linear", na.rm = TRUE)) %>%
#     dplyr::filter(!is.na(y)) %>%
#     rename(age = x, d13c = y) %>%
#     dplyr::mutate(Coral = ifelse(is.numeric(d13c), coral, 0))
# }
# 
# int_35104 <- interp_coral(chop_data, "35104", time_steps = time_step)
# int_64344 <- interp_coral(chop_data, "64344", time_steps = time_step)
# int_47996 <- interp_coral(chop_data, "47996", time_steps = time_step)
# 
# interp_data <- rbind(int_35104, int_64344, int_47996)
# 
# 
# # Fitting Models for Detrending
# 
# models <- interp_data %>%
#   group_by(Coral) %>% 
#   do(model = lm(d13c ~ age, data = .))
# 
# # Putting Models back into dataset
#   
# interp_data <- interp_data %>%
#   dplyr::mutate(detrended = ifelse(Coral == "35104", models[[2]][[1]]$residuals,0),
#                 detrended = ifelse(Coral == "47996", models[[2]][[2]]$residuals,detrended),
#                 detrended = ifelse(Coral == "64344", models[[2]][[3]]$residuals,detrended))

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
figure_2$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C"))

figure_2


