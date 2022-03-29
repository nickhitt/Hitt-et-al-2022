### This script explores a possible connection between the LMR and d13C variability
setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/detrend_functions.R")
source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Detrended_d13c_MainFigures.R")

library(stats)
library(ncdf4)
nc_data <- nc_open('sst_MCruns_ensemble_mean_LMRv2.1.nc')

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")/365

sst.array <- ncvar_get(nc_data, "sst") # store the data in a 3-dimensional array
dim(sst.array) 

sst.slice <- sst.array[which(lon %in% c(170)):which(lon %in% c(190)), which(lat %in% c(-30)):which(lat %in% c(-50)), , ]

fillvalue <- ncatt_get(nc_data, "sst", "_FillValue")

sst.slice[sst.slice == fillvalue$value] <- NA
mat <- matrix(0, length(t), 20)
for (i in 1:20){
  model <- sst.slice[,,i,]
  for (q in 1:length(t)){
    time_point <- model[,,q]
    mat[q,i] <- mean(time_point, na.rm = TRUE)
  }
}

nc_close(nc_data)

sst_models <- data.frame(t,mat) %>%
  melt(id.vars=c("t")) %>%
  dplyr::rename(Age = t, Coral_name = variable, sst_detrend = value) %>%
  mutate(Age = 1950 - Age, record_type = "LMR") 

sst_models
  

# mat_2 <- matrix(0, length(t), 2)
# for (z in 1:length(t)){
#   mat_2[z,1] <- median(as.numeric(sst_models[z,2:21]))
# }
# 
# sst_med <- data.frame(t,mat_2) %>%
#   dplyr::rename(Age = t, sst_detrend = X1) %>%
#   mutate(Age = 1950 - Age, record_type = "LMR", Coral_name = "LMR") %>%
#   select(-X2)


all_detrend_dsc <- data.frame(rbind(detrended_eauc1_raw, detrended_eauc2_raw, detrended_stf1_raw)) %>%
  dplyr::rename(Age = dataframe_2.age, sst_detrend = detrended) %>%
  mutate(record_type = "Deep Sea Corals",
         sst_detrend = sst_detrend/0.23) 


all_data <- rbind(sst_models, all_detrend_dsc)

figure4 <- all_data %>%
  ggplot(mapping = aes(Age, sst_detrend, group = Coral_name)) +
  geom_point(data = subset(all_data, record_type == "Deep Sea Corals")) + 
  geom_line(aes(color = Coral_name)) + 
  geom_smooth(method = "loess", span = 0.1) +
  facet_grid(rows = vars(factor(record_type, levels=c('Deep Sea Corals',
                                                'LMR'))), scales = "free_y")

figure4
  
