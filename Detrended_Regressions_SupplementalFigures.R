### This script produces the supplemental detrended correlation plots for Hitt et al 2022

setwd("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes")

source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/detrend_functions.R")
source("~/Dropbox/Marsden Black Coral Project/R Codes/Bulk Carbon Isotopes/Detrended_d13c_MainFigures.R")

correlation_plot(detrended_interp_all, "STF1")
correlations_detrend_interp(detrended_interp_all, "STF1")
correlation_plot(detrended_interp_all, "EAuC1")
correlations_detrend_interp(detrended_interp_all, "EAuC1")
correlation_plot(detrended_interp_all, "EAuC2")
correlations_detrend_interp(detrended_interp_all, "EAuC2")
