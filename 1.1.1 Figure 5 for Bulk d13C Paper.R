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
## Data Import, Clean and Analysis (and cleaning/reorganising again) for d13C anomaly figure

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

orders <- colnames(pacific_data)

# Loading in and Cleaning Moy Data
Moy_ENSO_Data <- read_excel("Moy_ENSO Data.xlsx") %>%
  rename(age = Age, d13c = "Red Intensity") %>%
  subset(age < 3000) %>%
  select(age, d13c) %>%
  dplyr::mutate(Coral = "Moy et al 2002",
                d13c_anom = d13c,
                basin = "Eastern Pacific",
                Interval = "Interval 3") %>%
  select(orders)

## Loading in and cleaning Oceans2k

oceans_2k <- read_excel("Oceans2k.xlsx", sheet = "Pacific") %>%
  rename(d13c = SSTa) %>%
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
lab2 <- expression(paste("Red Intesity"))
lab3 <- expression(paste("SST"["Anomaly"]))


###############################################################################
## Plotting Figure of all d13C anom data in North and South Pacific

pacific_figure <- pacific_data %>%
  ggplot(mapping = aes(age, d13c_anom, group = basin)) +
  geom_point(aes(colour = basin), show.legend = FALSE) + 
  geom_smooth(data = subset(pacific_data, smooth==1), aes(color = "black",
              group = Interval), color = "black", method = "loess", se = TRUE, span = 0.5, show.legend = FALSE) +
  #geom_smooth(aes(colour = Coral, group = Interval), method = "loess", se = FALSE, span = 0.35, show.legend = FALSE) +
  facet_grid(rows = vars(factor(basin, levels=c('Eastern Pacific',
                                                'North Pacific',
                                                'South Pacific',
                                                'Pacific Ocean'))), scales = "free_y") +
  scale_colour_brewer(palette = "Paired") +
  xlab("Time (cal BP)") +
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  # theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
  #       legend.box.background = element_rect(fill = NA), 
  #       legend.key = element_rect(colour = "transparent", fill = "white"),
  #       legend.box.margin = margin(1, 1, 1, 1), legend.position = "top",
  #       panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #       legend.title = element_text(size = 6), legend.text = element_text(size = 6))
#pacific_figure$labels$colour <- "Bas"
pacific_figure$labels$y <- expression(paste("Bulk ", "\u03B4" ^ "13", "C"["Anomaly"], "(\u2030)"))
#pacific_figure$labels$y <- expression(paste("SST"["Anomaly"], " (", "\u00B0C", ")"))
#pacific_figure$labels$y <- expression(paste("Red Shift Intensity"))


pacific_figure


###############################################################################

# Data Analysis

### Regime shift

library(rshift)

## Interval 1

# regime shift of North Pacific

data <- pacific_data %>%
  subset(Interval == "Interval 1" & basin == "North Pacific") %>%
  arrange(desc(age))

shift <- Rodionov(data = data, col = "d13c_anom", time = "age", l = 100, prob = 0.95)



# regime shift of South Pacific

data <- pacific_data %>%
  subset(Interval == "Interval 1" & basin == "South Pacific") %>%
  arrange(desc(age))
shift <- Rodionov(data = data, col = "d13c_anom", time = "age", l = 50, prob = 0.95)

# regime shift of SSTs

data <- pacific_data %>%
  subset(basin == "Pacific Ocean") %>%
  arrange(desc(age))

loessMod35 <- loess(d13c_anom ~ age, data=pacific_data, span=0.35)
loess_out <- predict(loessMod35)
time <- seq(1950,50, -0.35)
data <- data.frame(cbind(time, loess_out))
  
shift <- Rodionov(data = data, col = "loess_out", time = "time", l = 100, prob = 0.95)

