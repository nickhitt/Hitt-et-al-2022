#### Figure5  for Bulk d13C Paper

#### This code produces plot the figure 5 plot which shows the records by Pacific basin
#### and includes the ENSO panel from Moy et al
#### The code should: 1) load in the data 2) make the dataframes 3) process the data
#### 4) make the figure 5) Include the legend in the figure


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
  dplyr::rename(age = Year) %>%
  dplyr::mutate(age = 1950 - age)

data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Clean d13C Data.xlsx")) %>%
  dplyr::filter(Coral == "64344") %>%          
  dplyr::mutate(Coral_name = case_when(Coral == "64344" ~ "EAuC 2")) 

aa_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_C/csiaadataclean_with_lys.xlsx")) %>%
  dplyr::filter(Coral == "P") %>%          
  dplyr::mutate(Coral_name = case_when(Coral == "P" ~ "EAuC 2")) %>%
  dplyr::filter(sample.id != "P-2")

################################################################################
## Data Import, Clean and Analysis (and cleaning/reorganising again) for d13C anomaly figure

#Cleaning McMahon data
pacific_data <- mcmahon %>%
  dplyr::select(Coral, age, Bulk, Phe)

#Loading in Glynn Data
glynn_data <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Glynn et al Data.xlsx")) %>%
  dplyr::rename(age = Year) %>%
  subset(age < 3000) %>%
  dplyr::filter(d13C != "-") %>%
  select(-d15N) %>%
  dplyr::rename(Bulk = d13C, Coral = ID) %>%
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
  dplyr::rename(d13c = Bulk) %>%
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
#pacific_data[which(pacific_data$Coral == "EAuC2" & pacific_data$age < 2000 & pacific_data$age > 1500),] <- NA
pacific_data <- pacific_data %>%
  dplyr::filter(!is.na(age)) %>%
  dplyr::mutate(Interval = case_when(age < 1730 & basin == "North Pacific" ~ "Interval 1",
                                     age > 1730 & basin == "North Pacific" ~ "Interval 2"))
                                     #age < 1510 & basin == "South Pacific" ~ "Interval 1",
                                     #age > 1510 & basin == "South Pacific" ~ "Interval 2"))

orders <- colnames(pacific_data)

# Loading in and Cleaning Moy Data
Moy_ENSO_Data <- read_excel("Moy_ENSO Data.xlsx") %>%
  dplyr::rename(age = Age, d13c = "Red Intensity") %>%
  subset(age < 3000) %>%
  select(age, d13c) %>%
  dplyr::mutate(Coral = "Moy et al 2002",
                d13c_anom = d13c,
                basin = "Eastern Pacific",
                Interval = "Interval 3") %>%
  select(orders)

## Loading in and cleaning Oceans2k

oceans_2k <- read_excel("Oceans2k.xlsx", sheet = "Pacific") %>%
  dplyr::rename(d13c = SSTa) %>%
  dplyr::filter(d13c != "NaN") %>%
  dplyr::mutate(age = 1950-age,
                d13c_anom = d13c,
                Coral = "Oceans2k",
                basin = "Pacific Ocean",
                Interval = "Interal 4") %>%
  select(orders)

## Adding Moy and Oceans2k to Pacific Data

pacific_data <- pacific_data %>%
  rbind(Moy_ENSO_Data, oceans_2k) %>%
  dplyr::mutate(d13c_anom = round(as.numeric(d13c_anom), digits = 3)) %>%
  dplyr::mutate(smooth = case_when(basin != "Eastern Pacific" ~ 1,
                                   basin == "Eastern Pacific" ~ 0)) 

lab1 <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Anomaly"]))
lab2 <- expression(paste("Red Colour Intesity"))
lab3 <- expression(paste("SST"["Anomaly"]))


###############################################################################
## Plotting Figure of all d13C anom data in North and South Pacific

pacific_figure <- pacific_data %>%
  ggplot(mapping = aes(age, d13c_anom, group = basin)) +
  geom_point(data = subset(pacific_data, smooth==1),aes(colour = basin, group = Interval), show.legend = FALSE) + 
  geom_line(data = subset(pacific_data, smooth!=1),aes(colour = basin, group = Interval), show.legend = FALSE) +
  geom_smooth(data = subset(pacific_data, smooth==1 & basin == "North Pacific"), aes(color = "black",
              group = Interval), color = "black", method = "loess", se = TRUE, span = 0.35, show.legend = FALSE) +
  geom_smooth(data = subset(pacific_data, smooth==1 & basin == "South Pacific"), aes(color = "black",
              group = Interval), color = "black", method = "loess", se = TRUE, span = 0.2, show.legend = FALSE) + 
  geom_smooth(data = subset(pacific_data, smooth==1 & basin == "Pacific Ocean"), aes(color = "black",
              group = Interval), color = "black", method = "loess", se = TRUE, span = 0.5, show.legend = FALSE) + 
  facet_grid(rows = vars(factor(basin, levels=c('Eastern Pacific',
                                                'North Pacific',
                                                'South Pacific',
                                                'Pacific Ocean'))), scales = "free_y") +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pacific_figure$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Norm"], " (\u2030)"))
#pacific_figure$labels$y <- expression(paste("SST"["Anomaly"], " (", "\u00B0C", ")"))
#pacific_figure$labels$y <- expression(paste("Red Colour Intensity"))


pacific_figure


