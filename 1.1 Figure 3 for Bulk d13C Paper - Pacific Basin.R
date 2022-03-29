#### Figure3  for Bulk d13C Paper

library(dplyr)
library(lattice)
library(readxl)
library(stats)
library(RColorBrewer)
library(latticeExtra)
library(gridExtra)

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

## Data Import

mcmahon <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/McMahon et al 2015 Data.xlsx")) %>%
  rename(age = Year) %>%
  dplyr::mutate(age = 1950 - age)

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::filter(Coral == "64344") %>%          
  dplyr::mutate(Coral_name = case_when(Coral == "64344" ~ "EAuC 2")) 

aa_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_C/csiaadataclean_with_lys.xlsx")) %>%
  dplyr::filter(Coral == "P") %>%          
  dplyr::mutate(Coral_name = case_when(Coral == "P" ~ "EAuC 2")) %>%
  dplyr::filter(sample.id != "P-2")

## Making Figure

# Top Panel

time_series_bulk <- xyplot(Bulk ~ age, data = mcmahon,
                           groups = factor(Coral),
                           pch = 20, auto.key = list(columns=2), type = "l",
                           xlim = c(-78, 1500))
  
  
time_series_Phe <- xyplot(Phe ~ age, data = mcmahon,
                          groups = factor(Coral, labels = c("GER9701","GER9702", "FFS694")),
                          pch = 20, auto.key = list(columns=2), 
                          type = "p", ylab = "Phe 13C",
                          xlim = c(-78, 1500))

figure1a <- doubleYScale(time_series_bulk, time_series_Phe, add.ylab2 = TRUE)

figure1a

## Bottom Panel

bulk_nz <- xyplot(d13c ~ age, data = data,
                           groups = factor(Coral),
                           pch = 20, auto.key = list(columns=1), type = "l",
                            xlim = c(-78, 1500))
phe_nz <- xyplot(Phe ~ age, data = aa_data,
                          groups = factor(Coral, labels = c("EAuC2")),
                          pch = 20, auto.key = list(columns=1), 
                          type = "p", ylab = "Phe 13C", ylim = c(-25, -21),
                          xlim = c(-78, 1500))

figure1b <- doubleYScale(bulk_nz, phe_nz, add.ylab2 = TRUE)

figure3 <- grid.arrange(figure1a, figure1b, nrow=2)


## All Coral Data Everywhere Figure - Including AA data to lattice used - not as neat....

pacific_data <- mcmahon %>%
  dplyr::select(Coral, age, Bulk, Phe)

glynn_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Glynn et al Data.xlsx")) %>%
  rename(age = Year) %>%
  subset(age < 3000) %>%
  dplyr::filter(d13C != "-") %>%
  select(-d15N) %>%
  rename(Bulk = d13C, Coral = ID) %>%
  dplyr::mutate(Phe = case_when(!is.na(Bulk) ~ NA)) %>%
  dplyr::filter(Coral != "35104") %>%
  dplyr::filter(Coral != "47996") %>%
  dplyr::filter(Coral != "64344")

pacific_data <- pacific_data %>%
  rbind(glynn_data) %>%
  dplyr::filter(Bulk != "-") %>%
  dplyr::mutate(Bulk = as.numeric(Bulk))

pacific_data <- transform(pacific_data, z = Coral)
pacific_data <- pacific_data[1:367,]


# ### Plotting
# 
# #Top Panel
# 
# time_series_bulk <- xyplot(Bulk ~ age, data = pacific_data,
#                            groups = factor(Coral),
#                            pch = 20, auto.key = list(columns=2), type = "l",
#                            xlim = c(-78, 3000))
# 
# 
# time_series_Phe <- xyplot(Phe ~ age, data = mcmahon,
#                           groups = factor(Coral),
#                           pch = 20, auto.key = list(columns=2), 
#                           type = "p", ylab = "Phe 13C",
#                           xlim = c(-78, 3000), ylim = c(-40, -20))
# 
# figure1a <- doubleYScale(time_series_bulk, time_series_Phe, add.ylab2 = TRUE)
# 
# figure1a
# 
# ## Bottom Panel
# 
data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::filter(Coral != "47996") %>%
  dplyr::mutate(Coral = case_when(Coral == "64344" ~ "EAuC1",
                                  Coral == "35104" ~ "EAuC2")) %>%
  dplyr::select(-uncert)

mean_d13c_nz <- aggregate(d13c ~ Coral, data, mean)

data <- data %>%
  dplyr::mutate(d13c_anom = case_when(Coral == "EAuC2" ~ d13c - mean_d13c_nz[2,2],
                                      Coral == "EAuC1" ~ d13c - mean_d13c_nz[1,2])) %>%
  relocate(age, d13c, d13c_anom, Coral)
# 
# bulk_nz <- xyplot(d13c_anom ~ age, data = data,
#                   groups = factor(Coral),
#                   pch = 20, auto.key = list(columns=1), type = "l",
#                   xlim = c(-78, 3000))
# # phe_nz <- xyplot(Phe ~ age, data = aa_data,
# #                  groups = factor(Coral, labels = c("EAuC2")),
# #                  pch = 20, auto.key = list(columns=1), 
# #                  type = "p", ylab = "Phe 13C", ylim = c(-25, -21),
# #                  xlim = c(-78, 3000))
# 
# 
# 
# figure1b <- doubleYScale(bulk_nz, phe_nz, add.ylab2 = TRUE)
# 
# figure3 <- grid.arrange(time_series_bulk, bulk_nz, nrow=2)

### Bulk Only Data - so GGplot Used, much neater

library(ggplot2)

## Calculating Anomalies for North Pacific Corals and Orgaising Dataframe

mean_d13c_pacific <- aggregate(Bulk ~ Coral, pacific_data, mean)

locale <- c("EAuC1", "EAuC2")

pacific_data <- pacific_data %>% 
  dplyr::select(-z, -Phe) %>%
  rename(d13c = Bulk) %>%
  dplyr::mutate(d13c_anom = case_when(Coral == "FFS694" ~ d13c - mean_d13c_pacific[1,2],
                                      Coral == "GER9701" ~ d13c - mean_d13c_pacific[2,2],
                                      Coral == "GER9702" ~ d13c - mean_d13c_pacific[3,2],
                                      Coral == "L1" ~ d13c - mean_d13c_pacific[4,2],
                                      Coral == "L2" ~ d13c - mean_d13c_pacific[5,2])) %>%
  relocate(age, d13c, d13c_anom, Coral) %>%
  rbind(data) %>%
  dplyr::mutate(basin = ifelse(Coral %in% locale, "South Pacific", "North Pacific")) %>%
  #dplyr::mutate(age = case_when((Coral == "EAuC2" & age < 2000 & age > 1000) ~ NA)) %>%
  dplyr::filter(!is.na(age)) 

pacific_data <- pacific_data[order(pacific_data$Coral, pacific_data$age),]

## Removing Bad 35104 Data

pacific_data[which(pacific_data$Coral == "EAuC2" & pacific_data$age < 2000 & pacific_data$age > 1000),] <- NA

pacific_data <- pacific_data %>%
  dplyr::filter(!is.na(age))

## Plotting Figure

pacific_figure <- pacific_data %>%
  ggplot(mapping = aes(age, d13c_anom, group = basin)) +
  geom_point(aes(colour = Coral)) + 
  geom_smooth(aes(colour = basin), method = "loess", se = TRUE, span = 0.225) +
  facet_grid(rows = vars(basin)) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
pacific_figure$labels$colour <- "Coral"
pacific_figure$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))

pacific_figure
                  
## Loess Filtered Analysis

# Pulling out each basin

north_pacific <- pacific_data %>%
  dplyr::filter(basin == "North Pacific")

south_pacific <- pacific_data %>%
  dplyr::filter(basin == "South Pacific")

# Loess Smoothing

smooth_north <- predict(loess(d13c_anom~age, north_pacific, span = 0.225))

smooth_south <- predict(loess(d13c_anom~age, south_pacific, span = 0.225))

# Putting Data Back

north_pacific <- north_pacific %>%
  cbind(smooth_north) %>%
  rename(smoothed = smooth_north)

south_pacific <- south_pacific %>%
  cbind(smooth_south) %>%
  rename(smoothed = smooth_south)

# Combining to one dataframe for plotting check

smoothed_pacific_data <- north_pacific %>%
  rbind(south_pacific)

figure4 <- smoothed_pacific_data %>%
  ggplot(mapping = aes(age, smoothed, group = basin)) +
  geom_line(aes(colour = basin)) +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
figure4$labels$colour <- "Coral"
figure4$labels$y <- expression(paste("Smoothed Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
figure4

# Interpolating

age_step1 <- south_pacific %>%
  dplyr::filter(Coral == "EAuC1") %>%
  dplyr::select(age)

age_cap <- north_pacific %>%
  dplyr::filter(Coral == "L2") %>%
  dplyr::select(age) %>%
  max()

age_step_eauc2 <- south_pacific %>%
  dplyr::filter(Coral == "EAuC2") %>%
  dplyr::select(age) %>%
  subset(age < age_cap)

age_interp <- rbind(age_step1, age_step_eauc2)

interval1_NP <- approx(as.numeric(north_pacific$age), 
                        as.numeric(north_pacific$smoothed), 
                        xout = as.numeric(age_step1), method = "linear")

interval1_SP <- south_pacific %>%
  subset(age < age_step1[,110])



