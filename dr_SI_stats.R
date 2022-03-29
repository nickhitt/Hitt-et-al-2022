# This script attempts to show the running correlation between ∆R and d13C

source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/deltar_comparison.R")

loess_coral <- function(dataframe, coral, proxy) {
  data <- dataframe %>%
    filter(Coral_name == coral & record_type == proxy)
  filtered <- loess(proxy_value ~ Age, data, na.rm = TRUE, span = 0.1)
  output <- data.frame(data$Age, filtered$fitted) %>%
    mutate(Coral_name = coral) 
  return(output)
}

all_data <- rbind(deltar, all_detrend_dsc)

all_dr_fitted <- rbind(loess_coral(all_data, "EAuC2", "dR"), loess_coral(all_data, "EAuC1", "dR"))
si <- all_data[all_data$record_type == "Stable Isotope",]
fitted_si <- loess(proxy_value ~ Age, si, na.rm = TRUE, span = 0.1)
all_si_fitted <- data.frame(all_data[all_data$record_type == "Stable Isotope",1], fitted_si$fitted)

