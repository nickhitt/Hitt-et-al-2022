### This script explores a possible connection between the ∆R and d13C variability
setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/detrend_functions.R")
source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Detrended_d13c_MainFigures.R")

deltar <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/DeltaR_Data.xlsx")) %>%
  dplyr::rename(proxy_value = dR) %>%
  filter(Coral_name != "STF1") %>%
  mutate(record_type = "dR",
         dummy = case_when(Coral_name != "EAuC2" ~ 0,
                           Coral_name == "EAuC2" ~ 1))

deltar_detrended <- detrend(deltar$proxy_value)

deltar <- deltar %>%
  mutate(proxy_value = deltar_detrended)

all_detrend_dsc <- data.frame(rbind(detrended_eauc1_raw, detrended_eauc2_raw, detrended_stf1_raw)) %>%
  dplyr::rename(Age = dataframe_2.age, proxy_value = detrended) %>%
  mutate(record_type = "Stable Isotope",
         dummy = 1)

all_data <- rbind(deltar, all_detrend_dsc)
all_data <- all_data %>%
  ggplot(aes(Age, proxy_value)) +
  geom_point(aes(color = Coral_name)) +
  geom_line(aes(color = Coral_name)) +
  geom_smooth(data = subset(all_data, record_type == "dR"), aes(group = dummy), method = "loess", span = 0.1) +
  geom_smooth(data = subset(all_data, record_type == "Stable Isotope"), 
              method = "loess", span = 0.1) +
  facet_grid(rows = vars(factor(record_type, levels=c('dR',
                                                      'Stable Isotope'))), scales = "free_y")

all_data
           
