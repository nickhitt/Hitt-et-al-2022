### Percent Bulk N 15 Analysis by Amino Acid

library(dplyr)
library(ggplot2)
library(readxl)
library(stats)

## loading data

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_N/Percent Analysis")

batch_name1 <- c("M-8|M-25|M-52|M-77|M-121")

batch1 <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_N/Percent Analysis/01_Nick's Black Corals_Batch1of4_AJMS_2Mar2020_SB_FINAL.xlsx"
                                , sheet = "Raw data")) %>%
  subset(grepl(batch_name1, Identifier.1)) %>%
  dplyr::mutate(sample = ifelse(grepl("M-8", Identifier.1), "M-8", 0),
                sample = ifelse(grepl("M-25", Identifier.1), "M-25", sample),
                sample = ifelse(grepl("M-52", Identifier.1), "M-52", sample),
                sample = ifelse(grepl("M-77", Identifier.1), "M-77", sample),
                sample = ifelse(grepl("M-121", Identifier.1), "M-121", sample))

batch_name2 <- c("M-8|M-25|M-52|M-77|M-121")

batch2 <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_N/Percent Analysis/02_Nick's Black Corals_Batch2of4_AJMS_5Mar2020_SB_FINAL.xlsx"
                                , sheet = "Raw data")) %>%
  subset(grepl(batch_name1, Identifier.1)) %>%
  dplyr::mutate(sample = ifelse(grepl("M-8", Identifier.1), "M-8", 0),
                sample = ifelse(grepl("M-25", Identifier.1), "M-25", sample),
                sample = ifelse(grepl("M-52", Identifier.1), "M-52", sample),
                sample = ifelse(grepl("M-77", Identifier.1), "M-77", sample),
                sample = ifelse(grepl("M-121", Identifier.1), "M-121", sample))

batch_name3 <- c("M-8|M-25|M-52|M-77|M-121")

batch3 <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_N/Percent Analysis/03_Nick's Black Corals_Batch3of4_AJMS_6Mar2020_SB_FINAL.xlsx"
                                , sheet = "Raw data")) %>%
  subset(grepl(batch_name3, Identifier.1)) %>%
  dplyr::mutate(sample = ifelse(grepl("M-8", Identifier.1), "M-8", 0),
                sample = ifelse(grepl("M-25", Identifier.1), "M-25", sample),
                sample = ifelse(grepl("M-52", Identifier.1), "M-52", sample),
                sample = ifelse(grepl("M-77", Identifier.1), "M-77", sample),
                sample = ifelse(grepl("M-121", Identifier.1), "M-121", sample))

batch_name4 <- c("M-8|M-25|M-52|M-77|M-121")

batch4 <- data.frame(read_excel("~/Dropbox/Marsden Black Coral Project/R Codes/CSIAA_N/Percent Analysis/04_Nick's Black Corals_Batch4of4_AJMS_9Mar2020_SB_FINAL.xlsx"
                                , sheet = "Raw data")) %>%
  subset(grepl(batch_name4, Identifier.1)) %>%
  dplyr::mutate(sample = ifelse(grepl("M-8", Identifier.1), "M-8", 0),
                sample = ifelse(grepl("M-25", Identifier.1), "M-25", sample),
                sample = ifelse(grepl("M-52", Identifier.1), "M-52", sample),
                sample = ifelse(grepl("M-77", Identifier.1), "M-77", sample),
                sample = ifelse(grepl("M-121", Identifier.1), "M-121", sample))