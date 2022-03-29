## Comparison between all Pacific Corals d13C

library(dplyr)
library(ggplot2)
library(readxl)
library(stats)

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

scaleFUN <- function(x) sprintf("%.2f", x)

##### All Corals

all_corals_bulkC <- data.frame(read_excel("Glynn et al Data.xlsx", sheet = "Sheet1")) %>%
  dplyr::select(-d15N) %>%
  dplyr::filter(ID != "L3") %>%
  dplyr::filter(ID != "47996") %>%
  dplyr::filter(d13C != "-") %>%
  dplyr::mutate(d13C = as.numeric(d13C))

p1 <- ggplot(data = all_corals_bulkC, mapping = aes(Year, d13C, group = ID)) +
  geom_point(aes(colour=ID)) + geom_line(aes(colour=ID)) + 
  geom_smooth(method = "loess", aes(colour = ID)) +
  scale_y_continuous(labels=scaleFUN)
p1

#### Subset Corals

## Subset 35014 and interpolate

chop_35104 <- all_corals_bulkC %>%
  dplyr::filter(ID == "35104") %>%
  subset(Year > 1900 & Year < 2550)

## Subset L2

chop_l2 <- all_corals_bulkC %>%
  dplyr::filter(ID == "L2")%>%
  subset(Year > 1900 & Year < 2550)

sub_corals <- rbind(chop_35104, chop_l2)

p2 <- ggplot(data = sub_corals, mapping = aes(Year, d13C, group = ID)) +
  geom_point(aes(colour=ID)) + geom_line(aes(colour=ID)) + 
  geom_smooth(method = "loess", aes(colour = ID)) +
  scale_y_continuous(labels=scaleFUN)
p2

nz_loess <- loess(d13C ~ Year, chop_35104)
nz_loess_dat <- data.frame(cbind(nz_loess$x, nz_loess$fitted))%>%
  dplyr::mutate(ID = if_else(!is.na(Year), "35104","not"))
l2_loess <- loess(d13C ~ Year, chop_l2)
l2_loess_dat <- data.frame(cbind(l2_loess$x, l2_loess$fitted)) %>%
  dplyr::mutate(ID = if_else(!is.na(Year), "L2","not"))

#### Interpolating Data

int_35104 <- approx(nz_loess_dat$Year, nz_loess_dat$V2, 
                    xout = l2_loess_dat$Year, method = "linear")

cors <- cor.test(int_35104$y, l2_loess_dat$V2)

## Plotting Regression

combined_loess <- cbind(l2_loess_dat, int_35104$y) %>%
  rename(d13C_L2 = V2) %>%
  rename(d13C_35104 = int_35104$y)

p3 <- ggplot(data = combined_loess, mapping = aes(V2)) +
  geom_point() + geom_smooth(method = lm)
p3
