setwd('C:/Users/iyuo1/OneDrive/Documents/P-SAT/주제분석/백신 주제')

data <- read.csv('reg_imputed_train.csv')

library(tidyverse)

# NA 갯수
na <- data %>% 
  is.na() %>% as_tibble() %>% 
  summarise(across(everything(), sum))

library(VIM)

data_aggr <- data  %>%  
  aggr(combined = FALSE, numbers = TRUE, col = "skyblue")

summary(data_aggr)

# 핫덱
library(hot.deck)
library(VIM)

write.csv(hotdeck_data, 'C:/Users/iyuo1/OneDrive/Documents/P-SAT/주제분석/백신 주제/Hotdeck_data.csv')

hotdeck_data <- hotdeck(data=data)

hotdeck_data <- hotdeck_data %>% select(1:38)