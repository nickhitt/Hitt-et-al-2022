### This script holds the Detrend Functions used in Detrended_d13c_MainFigures.R


detrend_coral <- function(dataframe, coral, proxy){
  dataframe_2 <- dataframe %>%
    dplyr::filter(Coral_name == coral) %>%
    select(age, proxy)
  dataframe_3 <- dataframe_2 %>%
    select(proxy)
  detrended <- detrend(as.numeric(unlist(dataframe_3)))
  new_frame <- data.frame(dataframe_2$age, detrended) %>%
    mutate(Coral_name = coral)
  return(new_frame)
}

detrend_coral_interp <- function(dataframe, coral, proxy, time_vec){
  dataframe_2 <- dataframe %>%
    dplyr::filter(Coral_name == coral) %>%
    select(age, proxy)
  dataframe_3 <- dataframe_2 %>%
    select(proxy)
  detrended <- detrend(as.numeric(unlist(dataframe_3)))
  xi <- time_vec %>%
    filter(age_int >= min(dataframe_2[,1])) %>% 
    filter(age_int <= max(dataframe_2[,1])) 
  output <- interp1(as.numeric(dataframe_2$age), as.numeric(detrended), xi[[1]], method = c("linear"))
  new_frame <- data.frame(xi, output) %>%
    mutate(Coral_name = coral)
  return(new_frame)
}

correlation_plot <- function(data, exclude_coral){
  excluded <- data %>%
    filter(Coral_name != exclude_coral)
  corals_left <- unique(excluded$Coral_name)
  min_coral1 <- excluded %>%
    filter(Coral_name == corals_left[1]) %>%
    select(age) %>%
    min()
  max_coral1 <- excluded %>%
    filter(Coral_name == corals_left[1]) %>%
    select(age) %>%
    max()
  min_coral2 <- excluded %>%
    filter(Coral_name == corals_left[2]) %>%
    select(age) %>%
    min()
  max_coral2 <- excluded %>%
    filter(Coral_name == corals_left[2]) %>%
    select(age) %>%
    max()
  if (min_coral2 > min_coral1) {
    excluded <- excluded %>%
      filter(age > min_coral2)
  } else {
    excluded <- excluded %>%
      filter(age > min_coral1)
  }
  if (max_coral2 < max_coral1) {
    excluded <- excluded %>%
      filter(age < max_coral2)
  } else {
    excluded <- excluded %>%
      filter(age < max_coral1)
  }
  reshaped_excluded <- reshape(excluded, v.names = "d13c", idvar = "age",
                               timevar = "Coral_name", direction = "wide") %>%
    select(-age) 
  colname1 <- colnames(reshaped_excluded)
  reshaped_excluded <- reshaped_excluded %>%
    rename(d13c1 = colname1[1], d13c2 = colname1[2])
  figure <- reshaped_excluded %>%
    ggplot(mapping = aes(d13c1, d13c2)) +
    geom_point() + 
    geom_smooth(method = "lm", se = TRUE) +
    theme(panel.background = element_rect(fill = "white", colour = "black", size = 1),
          legend.box.background = element_rect(),
          legend.box.margin = margin(6, 6, 6, 6),
          panel.grid.major = element_blank(), 
          legend.key = element_rect(colour = "transparent", fill = "white"),
          panel.grid.minor = element_blank()) 
  
  figure$labels$y <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))
  figure$labels$x <- expression(paste("Detrended Bulk ", "\u03B4" ^ "13", "C", " (\u2030)"))
  title(main = paste(corals_left[1], " vs. ", corals_left[2]))
  
  return(figure)
}

correlations_detrend_interp <- function(data, exclude_coral){
  excluded <- data %>%
    filter(Coral_name != exclude_coral)
  corals_left <- unique(excluded$Coral_name)
  min_coral1 <- excluded %>%
    filter(Coral_name == corals_left[1]) %>%
    select(age) %>%
    min()
  max_coral1 <- excluded %>%
    filter(Coral_name == corals_left[1]) %>%
    select(age) %>%
    max()
  min_coral2 <- excluded %>%
    filter(Coral_name == corals_left[2]) %>%
    select(age) %>%
    min()
  max_coral2 <- excluded %>%
    filter(Coral_name == corals_left[2]) %>%
    select(age) %>%
    max()
  if (min_coral2 > min_coral1) {
    excluded <- excluded %>%
      filter(age > min_coral2)
  } else {
    excluded <- excluded %>%
      filter(age > min_coral1)
  }
  if (max_coral2 < max_coral1) {
    excluded <- excluded %>%
      filter(age < max_coral2)
  } else {
    excluded <- excluded %>%
      filter(age < max_coral1)
  }
  reshaped_excluded <- reshape(excluded, v.names = "d13c", idvar = "age",
                               timevar = "Coral_name", direction = "wide") %>%
    select(-age) 
  colname1 <- colnames(reshaped_excluded)
  reshaped_excluded <- reshaped_excluded %>%
    rename(d13c1 = colname1[1], d13c2 = colname1[2])
  cors <- cor.test(reshaped_excluded$d13c1, reshaped_excluded$d13c2, method=c("pearson"))
  
  model <- lm(d13c2 ~ d13c1, data = reshaped_excluded, method = "qr")
  
  multi_return <- function(estimate, p.value, model) {
    my_list <- list("cor" = estimate, "pval" = p.value, "model" = summary(model))
    return(my_list) 
  }
  multi_return(cors$estimate, cors$p.value, model)

}
  
  