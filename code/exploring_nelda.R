if(Sys.info()['user'] == "Maheep'"){
  base_dir = 'C:/Users/Maheep/Documents/R/Thesis/'
  data_dir = paste0(base_dir, 'data/')
  out_dir =   paste0(base_dir, 'output/')
  setwd(base_dir)
} else { 
  base_dir = ''
  print(paste0("Manually set base dir in script for this user - ", Sys.info()['user']))
}

#######################################
# load packages
#######################################
library(tidyverse)
library(dplyr)
library(haven)
library(readxl)
library(ggplot2)
library(patchwork) #to combine separate ggplots into the same graphic

# load the nelda data
df_nelda <- read_dta(paste0(data_dir, 'nelda_6.0/id & q-wide_share.dta'))
df_nelda %>% filter (year %in% 2007:2017) %>% select(year) %>% unique()
