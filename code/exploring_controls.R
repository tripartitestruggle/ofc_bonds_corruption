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
library(kableExtra)
library(broom) #for tidying lm into a df
library(modelsummary)

data_controls_dir = paste0(data_dir, 'controls/')
# load the chinn and ito financial openness/capital account openness
df_kaopen <- read_xls(paste0(data_controls_dir, 'kaopen_2020.xls'))
df_kaopen = df_kaopen %>% rename("Year" = "year") %>% filter (Year %in%  2007:2017)

# load IF export and import data
df_dots_e <- read_xlsx(paste0(data_controls_dir, 'Exports_and_Imports_by_Areas_and_Co.xlsx'), sheet = "Exports, FOB", skip = 5)
df_dots_e = df_dots_e %>% rename("Country" = "...1" )

df_dots_i <- read_xlsx(paste0(data_controls_dir, 'Exports_and_Imports_by_Areas_and_Co.xlsx'), sheet = "Imports, CIF", skip = 5)
df_dots_i = df_dots_i %>% rename("Country" = "...1" )

# load IMF WEO for GDP data
df_weo <- read_xlsx(paste0(data_controls_dir, 'WEOOct2022all.xlsx') )
df_weo = df_weo %>% rename_all(make.names)
df_gdp =  df_weo %>% filter(WEO.Subject.Code == "NGDPD")
df_gdp_gr = df_weo %>% filter(WEO.Subject.Code == "NGDP_RPCH")

#----------------real shit

#load the IFS data
df_ifs <- read_xlsx(paste0(data_controls_dir, "IFS_relevant.xlsx"), skip = 2)
df_ifs = df_ifs %>% rename(c("Country_Name" = "...1", "Country_Code" = "...2", "Year" = "...3" ))
df_ifs = df_ifs %>% mutate (Year = as.double(Year))
# #check if my country codes match ifs
# data_coppola_dir = paste0(data_dir, 'coppola_data/')
# #load the country codes
# df_cc <- read_csv(paste0(data_coppola_dir, 'country_codes.csv'))
# df_check = df_cc %>% left_join(df_ifs %>% select("Country_Code","Year"), by = c("Issuer" = "Country_Code" ))
# print(df_check %>% filter (is.na(Year)), n = 35)
# #also check the reverse join
# print(df_ifs %>% left_join(df_cc, by = c("Country_Code" = "Issuer")) %>% select("Country_Code", "Country_Name", "Issuer_Name") %>% filter (is.na(Issuer_Name)), n = 80)
# # All cool!

#

#load the WEO data
df_weo <- read_xlsx(paste0(data_controls_dir, "WEO_relevant.xlsx"))
df_weo = df_weo %>% rename_all(make.names)
df_weo_t = df_weo %>% pivot_longer(cols=4:15, names_to = "Year", values_to = "Values") 
df_weo_t = df_weo_t %>% pivot_wider(id_cols = c("ISO", "Country", "Year"), names_from = WEO.Subject.Code, values_from = Values )
df_weo_t = df_weo_t %>% mutate(Year = stri_sub(Year, 2)) %>% mutate(Year = as.integer(Year))
 
#check if my country codes match weo
df_check = df_cc %>% left_join(df_weo_t %>% select("Country","ISO"), by = c("Issuer" = "ISO" ))
print(df_check %>% filter (is.na(Country)), n = 50)
#also check the reverse join
print(df_weo %>% left_join(df_cc, by = c("ISO" = "Issuer")) %>% select("ISO", "Country", "Issuer_Name") %>% filter (is.na(Issuer_Name)), n = 80)
# All cool!

######################
# 1. capital account openness
######################
# ccode to "ISO Alpha-3 Code"; kaopen to the Chinn-Ito index; and ka_open to the Chinn-Ito index normalized to range between zero and one. 
df_model = df_model %>% left_join(df_kaopen %>% select(ccode, Year, kaopen, ka_open), by = c("Issuer" = "ccode", "Year" = "Year"))

# #check if my country codes match ifs
# df_check = df_cc %>% left_join(df_kaopen %>% select("ccode","Year","country_name"), by = c("Issuer" = "ccode" ))
# print(df_check %>% filter (is.na(Year)), n = 35)
# #also check the reverse join
# print(df_kaopen %>% left_join(df_cc, by = c("ccode" = "Issuer")) %>% select("ccode", "country_name", "Issuer_Name") %>% filter (is.na(Issuer_Name)), n = 80)
# # All cool!

######################
# 2 . Trade openness
######################
# total trade/gdp 
# base vars = ERI, EXP, IMP = 109 countries are with NA in atleast one...
df_ifs = df_ifs %>% mutate (GDPD = GDP*ERI) %>% mutate (TRA = EXP + IMP) %>% mutate(TRAO = TRA/GDPD)
#View(df_model %>% left_join(df_ifs %>% select(Country_Code, Year, TRAO), by = c("Issuer" = "Country_Code", "Year" = "Year")) %>% filter_all(any_vars(is.na(.))))
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, TRAO), by = c("Issuer" = "Country_Code", "Year" = "Year"))

######################
# 3 . Current account imbalance
######################
# mag of BOP/GDP
df_ifs = df_ifs %>% mutate (CAI = abs(BOP)/GDPD)
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, CAI), by = c("Issuer" = "Country_Code", "Year" = "Year"))

######################
# 4 . sophistication of banking sector
######################
# liquidity/GDP
df_ifs = df_ifs %>% mutate(SB = LIQ/GDPD)
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, SB), by = c("Issuer" = "Country_Code", "Year" = "Year"))

######################
# 5 . tax evasion
######################
# rough tax rate = tax revenue/gdp
df_model = df_model %>% left_join(df_weo_t %>% select(ISO, Year, GGR_NGDP), by = c("Issuer" = "ISO", "Year" = "Year")) %>% mutate (GGR_NGDP = as.double(GGR_NGDP)/100)

######################
# 6 . domestic interest rate
######################
# lending rate
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, LENR), by = c("Issuer" = "Country_Code", "Year" = "Year"))
df_model = df_model %>% mutate (LENR = LENR/100)

######################
# 7 . gdp growth rate
######################
# gdp(t)/gdp(t-1) - 1 
df_ifs