#### SI Figures 2-3 for Hitt et al 2022

library(dplyr)
library(lattice)
library(readxl)
library(stats)
library(RColorBrewer)
library(latticeExtra)
library(gridExtra)
library(ggplot2)
library(ggpubr)
library(egg)

setwd("~/Dropbox/R Codes")

source("~/Dropbox/R Codes/Base Stats Functions.R")

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

################################################################################
## Data Import, Clean and Analysis (and cleaning/reorgaising againn) for d13C anomaly figure

#Cleaning McMahon data
pacific_data <- mcmahon %>%
  dplyr::select(Coral, age, Bulk, Phe)

#Loading in Glynn Data
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

#Cleaning Glynn Data
pacific_data <- pacific_data %>%
  rbind(glynn_data) %>%
  dplyr::filter(Bulk != "-") %>%
  dplyr::mutate(Bulk = as.numeric(Bulk))

pacific_data <- transform(pacific_data, z = Coral)
pacific_data <- pacific_data[1:367,]

# Loading in my data + cleaning data
data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::filter(Coral != "47996") %>%
  dplyr::mutate(Coral = case_when(Coral == "64344" ~ "EAuC1",
                                  Coral == "35104" ~ "EAuC2")) %>%
  dplyr::select(-uncert)

# Finding mean d13C of my data to calculate d13C anom of my data
mean_d13c_nz <- aggregate(d13c ~ Coral, data, mean)

# Calculating d13C anom of my data
data <- data %>%
  dplyr::mutate(d13c_anom = case_when(Coral == "EAuC2" ~ d13c - mean_d13c_nz[2,2],
                                      Coral == "EAuC1" ~ d13c - mean_d13c_nz[1,2])) %>%
  relocate(age, d13c, d13c_anom, Coral)

# Calculating mean d13C of north Pacific corals
mean_d13c_pacific <- aggregate(Bulk ~ Coral, pacific_data, mean)

# Assigning Locale framework as Categorical Variable
locale <- c("EAuC1", "EAuC2")

#Calculating d13C anom of North Pacific Data and Organising Dataframe
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
  dplyr::filter(!is.na(age)) 

#Ordering dataframe by coral and time
pacific_data <- pacific_data[order(pacific_data$Coral, pacific_data$age),]

# Removing Bad 35104 Data
pacific_data[which(pacific_data$Coral == "EAuC2" & pacific_data$age < 2000 & pacific_data$age > 1000),] <- NA
pacific_data <- pacific_data %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::mutate(Interval = case_when(age < 1730 & basin == "North Pacific" ~ "Interval 1",
                                     age > 1730 & basin == "North Pacific" ~ "Interval 2",
                                     age < 1510 & basin == "South Pacific" ~ "Interval 1",
                                     age > 1510 & basin == "South Pacific" ~ "Interval 2"))

###############################################################################
# Interpolating Data for Correlation Analysis

#ordering for interpolate
north_pacific <- pacific_data %>%
  dplyr::filter(basin == "North Pacific")
south_pacific <- pacific_data %>%
  dplyr::filter(basin == "South Pacific")
north_pacific_ordered <- north_pacific[order(north_pacific$age),]
south_pacific_ordered <- south_pacific[order(south_pacific$age),]

# Making age vector for 130-1500BP
age_step1 <- south_pacific %>%
  dplyr::filter(Coral == "EAuC1") %>%
  dplyr::select(age)

# Finding First Age Step for 2000-2500BP
age_beg_cap <- south_pacific %>%
  dplyr::filter(Coral == "EAuC2") %>%
  dplyr::select(age) %>%
  min()

# Finding Last Age Step for 2000-2500BP
age_end_cap <- north_pacific %>%
  dplyr::filter(Coral == "L2") %>%
  dplyr::select(age) %>%
  max()

# That didn't work -> so just pulled out the ages for EAuC2 and cut it off at end of L2
age_step2 <- south_pacific %>%
  dplyr::filter(Coral == "EAuC2") %>%
  dplyr::select(age) %>%
  subset(age < age_end_cap) %>%
  subset(age > 1923)

# Pulling out EAuC1 Coral d13C anom Data
interval1_SP <- south_pacific %>%
  dplyr::filter(Coral == "EAuC1") %>%
  dplyr::select(age, d13c_anom) %>%
  dplyr::mutate(interval = case_when(!is.na(age) ~ "Interval 1"))

# Pulling out EAuC2 Coral d13C anom Data that overlaps with L2
interval2_SP <- south_pacific %>%
  dplyr::filter(Coral == "EAuC2") %>%
  dplyr::select(age, d13c_anom) %>%
  subset(age < age_end_cap) %>%
  subset(age > 1923) %>%
  dplyr::mutate(interval = case_when(!is.na(age) ~ "Interval 2"))

# Combining overlapping EAuC data to one vector
combined_SP <- rbind(interval1_SP, interval2_SP) %>%
  dplyr::mutate(basin = case_when(!is.na(age) ~ "South"))

## Interpolating NP data to EAuC2 Timesteps and putting into dataframe

interval1_NP <- approx(as.numeric(north_pacific_ordered$age), 
                       as.numeric(north_pacific_ordered$d13c_anom), 
                       xout = interval1_SP$age, method = "linear")

interval1_NP <- data.frame(cbind(interval1_NP[["x"]], interval1_NP[["y"]])) %>%
  rename(age = X1, smoothed = X2) %>%
  dplyr::mutate(interval = case_when(!is.na(age) ~ "Interval 1"))

## Interpolating NP data to EAuC2 Timesteps and putting into dataframe
interval2_NP <- approx(as.numeric(north_pacific_ordered$age), 
                       as.numeric(north_pacific_ordered$d13c_anom), 
                       xout = interval2_SP$age, method = "linear")

interval2_NP <- data.frame(cbind(interval2_NP$x, interval2_NP$y)) %>%
  rename(age = X1, smoothed = X2) %>%
  dplyr::mutate(interval = case_when(!is.na(age) ~ "Interval 2"))

# Combining both into 1 big dataframe
combined_NP <- rbind(interval1_NP, interval2_NP) %>%
  dplyr::mutate(basin = case_when(!is.na(age) ~ "North"))

## Correlation Tests

corr_all <- cor.test(combined_SP$d13c_anom, combined_NP$smoothed)

corr_int1 <- cor.test(interval1_SP$d13c_anom, interval1_NP$smoothed)

corr_int2 <- cor.test(interval2_SP$d13c_anom, interval2_NP$smoothed)

###############################################################################
## Plotting Figure of Regressions for all data, ad EAuC1 Interval and EAuC2 Interval

# Preparing Data

NP_smooth <- combined_NP %>%
  rename(North_smoothed = smoothed)

SP_smooth <- combined_SP %>%
  rename(South_smoothed  = d13c_anom)

combined_smoothed <- SP_smooth %>%
  dplyr::left_join(NP_smooth, by = c("age", "interval"))

## Plotting All Data

all_corals_regression <- combined_smoothed %>%
  ggplot(mapping = aes(North_smoothed, South_smoothed)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
all_corals_regression$labels$x <- expression(paste("North Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"], " (\u2030)"))
all_corals_regression$labels$y <- expression(paste("South Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"], " (\u2030)"))
all_corals_regression

model_all <- lm(South_smoothed ~ North_smoothed, combined_smoothed)

## Plot by Intervals

corals_panel_regressions <- combined_smoothed %>%
  dplyr::mutate(interval = case_when(interval == "Interval 1" ~ "130-1700 cal BP",
                                     interval == "Interval 2" ~ "1923-2550 cal BP")) %>%
  ggplot(mapping = aes(North_smoothed, South_smoothed, group = interval)) +
  geom_point(aes(colour = interval)) +
  geom_smooth(aes(colour = interval), method = "lm") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.2),
        legend.title = element_text(size = 7), 
        legend.text = element_text(size = 5)) 
corals_panel_regressions$labels$x <- expression(paste("North Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"], " (\u2030)"))
corals_panel_regressions$labels$y <- expression(paste("South Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"], " (\u2030)"))
corals_panel_regressions$labels$colour <- "Time Interval"
corals_panel_regressions

model_int1 <- lm(South_smoothed ~ North_smoothed, 
                 combined_smoothed[which(combined_smoothed$interval == "Interval 1"),])

model_int2 <- lm(South_smoothed ~ North_smoothed, 
                 combined_smoothed[which(combined_smoothed$interval == "Interval 2"),])

grid.arrange(all_corals_regression, corals_panel_regressions)

### Supplementary means plot

coral_47996 <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  filter(Coral == "47996")
means_47996 <- aggregate(d13c ~ Coral, coral_47996, mean)
std_47996 <- aggregate(d13c ~ Coral, coral_47996, std)%>%
  rename(stderr = d13c)
df_47996 <- means_47996 %>%
  left_join(std_47996) %>%
  mutate(Basin = "South Pacific", Coral = "STF1")

means <- aggregate(d13c ~ Coral, pacific_data, mean)
stderr <- aggregate(d13c ~ Coral, pacific_data, std) %>%
  rename(stderr = d13c)

means <- means %>%
  left_join(stderr) %>%
  dplyr::mutate(Basin = ifelse(Coral %in% locale, "South Pacific", "North Pacific")) %>%
  rbind(df_47996) %>%
  arrange(Basin)

mean_plot <- means %>%
  ggplot(mapping = aes(Basin, d13c)) +
  geom_point(aes(colour = Coral), size = 4, show.legend = FALSE) +
  geom_point(aes(colour = Coral), size = 4, show.legend = TRUE) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top",
        legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12),
        text = element_text(size = 14)) 
mean_plot$labels$y <- expression(paste("Mean Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))

mean_plot


