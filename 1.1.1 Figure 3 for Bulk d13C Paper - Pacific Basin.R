#### Figure3  for Bulk d13C Paper

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
### Making Figure of Bulk C and Phe C in North Pacific

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
## Plotting Figure of all d13C anom data in North and South Pacific

pacific_figure <- pacific_data %>%
  ggplot(mapping = aes(age, d13c_anom, group = basin)) +
  geom_point(aes(colour = Coral)) + 
  geom_smooth(aes(colour = basin, group = Interval), method = "loess", se = TRUE, span = 0.35, show.legend = FALSE) +
  geom_smooth(aes(colour = basin, group = Interval), method = "loess", se = FALSE, span = 0.35, show.legend = TRUE) +
  facet_grid(rows = vars(basin)) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(fill = NA), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box.margin = margin(1, 1, 1, 1), legend.position = "top",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        legend.title = element_text(size = 6), legend.text = element_text(size = 6))
pacific_figure$labels$colour <- "Coral"
pacific_figure$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))

pacific_figure

##############################################################################
# ENSO, Global Temp Comparison

moy_numevents <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Moy_ENSO Data.xlsx",
                             sheet = "Num Events"))

enso_numevents <- moy_numevents %>%
  ggplot(mapping = aes(Age, Number.of.Events)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, span = 0.2, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, span = 0.2, show.legend = TRUE) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  ylab("Number of Events") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(fill = NA),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
enso_numevents

pacific_figure <- pacific_data %>%
  ggplot(mapping = aes(age, d13c_anom, group = basin)) +
  geom_point(aes(colour = Coral)) + 
  geom_smooth(aes(colour = basin, group = Interval), method = "loess", se = TRUE, span = 0.35, show.legend = FALSE) +
  geom_smooth(aes(colour = basin, group = Interval), method = "loess", se = FALSE, span = 0.35, show.legend = TRUE) +
  facet_grid(rows = vars(basin)) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.position = "none",
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        strip.text.y = element_blank())
pacific_figure$labels$colour <- "Coral"
pacific_figure$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))

#pacific_figure <- ggplotGrob(pacific_figure)

pacific_figure

moy_raw <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Moy_ENSO Data.xlsx",
                                       sheet = "Raw"))

enso_red_intensity <- moy_raw %>%
  ggplot(mapping = aes(Age, Red.Intensity)) +
  geom_point() +
  #geom_smooth(method = "loess", se = TRUE, span = 0.175, show.legend = FALSE) +
  #geom_smooth(method = "loess", se = FALSE, span = 0.175, show.legend = TRUE) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  ylab("Red Intensity") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(fill = NA), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box.margin = margin(4, 4, 4, 4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#enso_red_intensity <- ggplotGrob(enso_red_intensity)

###############################################################################
### Temperature Figure
pages <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Pages2kTemp.xlsx")) %>%
  rename(temp = Full.ensemble.median) %>%
  dplyr::mutate(Year = 1950-Year)

temperature_figure <- pages %>%
  ggplot(mapping = aes(Year, temp)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE, span = 0.175, show.legend = FALSE) +
  geom_smooth(method = "loess", se = FALSE, span = 0.175, show.legend = TRUE) +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  ylab("Temperature Anomaly") +
  xlim(-50, 3000) +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(fill = NA), 
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.box.margin = margin(4, 4, 4, 4),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
temperature_figure
#temperature_figure <- ggplotGrob(temperature_figure)
#temperature_figure


###### Putting all figures together

#align_plots(pacific_figure, enso_red_intensity, temperature_figure, ncol=1)

combined <- plot_grid(pacific_figure, enso_red_intensity, temperature_figure, 
                      axis = "lr",align = "h", ncol = 1, greedy = TRUE,
                      rel_heights = c(1.5/4, 1/4, 1/4),
                      labels = c("A", "B", "C"))

ggsave("3panel_figure",
       plot = combined, device = "png", 
       path = "~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Figures",
       width = 6, height = 8, units = c("in"),
       dpi = 300)
  
###############################################################################   
## Loess Smoothing Analysis

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

#### Plotting Figure

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

###############################################################################
# Interpolating Data for Correlation Analysis

#ordering for interpolate

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

correlation_plot <- combined_smoothed %>%
  ggplot(mapping = aes(North_smoothed, South_smoothed)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
correlation_plot$labels$x <- expression(paste("North Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
correlation_plot$labels$y <- expression(paste("South Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
correlation_plot

model_all <- lm(South_smoothed ~ North_smoothed, combined_smoothed)

## Plot by Intervals

correlation_plot_intervals <- combined_smoothed %>%
  dplyr::mutate(interval = case_when(interval == "Interval 1" ~ "130-1500 cal BP",
                                     interval == "Interval 2" ~ "2000-2550 cal BP")) %>%
  ggplot(mapping = aes(North_smoothed, South_smoothed, group = interval)) +
  geom_point(aes(colour = interval)) +
  geom_smooth(aes(colour = interval), method = "lm") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
correlation_plot_intervals$labels$x <- expression(paste("North Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
correlation_plot_intervals$labels$y <- expression(paste("South Pacific Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
correlation_plot_intervals$labels$colour <- "Time Interval"
correlation_plot_intervals

model_int1 <- lm(South_smoothed ~ North_smoothed, 
                 combined_smoothed[which(combined_smoothed$interval == "Interval 1"),])

model_int2 <- lm(South_smoothed ~ North_smoothed, 
                 combined_smoothed[which(combined_smoothed$interval == "Interval 2"),])

