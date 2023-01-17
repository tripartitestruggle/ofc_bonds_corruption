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
library(plm)
library(lmtest)

#######################################
# load data
#######################################
data_coppola_dir = paste0(data_dir, 'coppola_data/')
data_nelda_dir = paste0(data_dir, 'nelda_6.0/')
data_controls_dir = paste0(data_dir, 'controls/')
#load the raw restatement data
df_rh_raw <-  read_dta(paste0(data_coppola_dir, 'Restated_Bilateral_External_Portfolios.dta'))
# load the restated holdings data that was filtered in excel
#df_rh <- read_csv(paste0(data_coppola_dir, 'restated_holdings_filtered.csv'))
#load the OFCs list
df_ofc <- read_xlsx(paste0(data_coppola_dir, 'ofc_list.xlsx'))
#load also the world ban list of tax havens
df_th <- read_xlsx(paste0(data_dir, 'list_tax_havens.xlsx'))
#load the bribe payers index data
df_bpi <- read_csv(paste0(data_dir, 'TI_BPI_2021.csv'))
#load the country codes
df_cc <- read_csv(paste0(data_coppola_dir, 'country_codes.csv'))
# load the nelda data
df_nelda_raw <- read_xls(paste0(data_nelda_dir, 'NELDA.xls'))
# load the list of indep states in nelda
df_lois <- read_xlsx(paste0(data_nelda_dir, 'list_of_indep_states_ISO.xlsx'))
# load the chinn and ito financial openness/capital account openness
df_kaopen_raw <- read_xls(paste0(data_controls_dir, 'kaopen_2020.xls'))
#load the IFS data
df_ifs_raw <- read_xlsx(paste0(data_controls_dir, "IFS_relevant.xlsx"), skip = 2)
# load IMF WEO for tax revenue data
df_weo_raw <- read_xlsx(paste0(data_controls_dir, "WEO_relevant.xlsx"))

#######################################
# DV
#######################################

#add the Issuer_Name_full to the restated_holdings df
df_rh_raw = df_rh_raw %>% left_join(df_cc %>% select("Issuer", "Issuer_Name_Full"), by = "Issuer")

#add the OFC information
df_ofc = df_ofc %>% mutate( Is_OFC = 1)
df_rh_raw = df_rh_raw %>% left_join(df_ofc %>% select(-c("Country_Name")), by = c("Issuer" = "ISO3_Code")) %>% replace_na(list(Is_OFC = 0))

#filter the restated-holdings data to make it meaningful for us
# Q = is it ok to filter out those with restated == 0?
df_rh = df_rh_raw %>% filter (Methodology == 1,
                      Year %in% 2007:2017,
                      Investor == "USA",
                      Asset_Class_Code == "BC",
                      Restatement_TH_Only > 0,
                      Is_OFC == 0) %>% 
  select(-c("Methodology","Investor_Name","Asset_Class","Issuer_Name","Restatement_Full","Restatement_Ex_Domestic","Restatement_Sales","Restatement_Sales_Ex_Domestic","Estimated_Common_Equity" )) %>%
  rename("Restatement_TH" = "Restatement_TH_Only")

#add the 3-letter country codes to the BPI df
df_bpi = df_bpi %>% left_join(df_cc %>% select("Issuer", "Issuer_Name_Full"), by = c(`COUNTRY/TERRITORY` = "Issuer_Name_Full"))

#add the columns to rh that says how much is coming from OFCs
df_rh = df_rh %>% mutate (Delta = Restatement_TH -  Position_Residency) %>% 
  mutate (Delta_perc = Delta*100/Restatement_TH) 

# get the rows that are not TH as per WBs, i.e. USA investment to 'normal' countries
l_th = df_th %>% drop_na("World Bank Code") %>% pull("World Bank Code")
df_rh %>% count()
df_rh = df_rh %>% filter(!Issuer %in% l_th)

# get rows that have +ve delta only, and do ln(delta)
df_rh = df_rh %>% filter(Delta > 0) %>% mutate(lnDelta = log(Delta))

## get list of countries that is intersection of BPI & non-OFC
#df_rh_bpi = df_rh %>% inner_join(df_bpi %>% select("Issuer", "SCORE"), by = "Issuer") %>% rename("Score_BPI" = "SCORE")



#######################################
# IV
#######################################
df_nelda = df_nelda_raw %>% mutate (Has_election = 1)

#check if my country codes match nelda
#df_check = df_cc %>% left_join(df_nelda %>% select("country","stateid"), by = c("Issuer" = "stateid" ))
#print(df_check %>% filter (is.na(country)), n = 50)
#also check the reverse join
#print(df_nelda %>% left_join(df_cc, by = c("stateid" = "Issuer")) %>% select("stateid", "country", "Issuer_Name") %>% filter (is.na(Issuer_Name)), n = 80) %>% unique()
# All cool!

df_nelda = df_nelda %>% left_join(df_lois %>% select(c("stateid","ISO")), by = "stateid") 
df_nelda = df_nelda %>% filter( year %in% 2006:2018, types %in% c("Legislative/Parliamentary", "Executive")) %>% select(ISO, year, Has_election) %>% rename(c("Year" = "year")) %>% distinct()
# lag and lead years of elections
df_nelda = df_nelda %>% mutate(Year_be = Year - 1, Lag_election = 1, Year_ae = Year + 1, Lead_election = 1)

df_model = df_rh %>% 
  left_join(df_nelda %>% select(ISO, Year, Has_election), by = c("Issuer" = "ISO", "Year" = "Year")) %>% replace_na(list(Has_election = 0)) %>%
  left_join(df_nelda %>% select(ISO, Year_be, Lag_election), by = c("Issuer" = "ISO", "Year" = "Year_be")) %>% replace_na(list(Lag_election = 0)) %>%
  left_join(df_nelda %>% select(ISO, Year_ae, Lead_election), by = c("Issuer" = "ISO", "Year" = "Year_ae")) %>% replace_na(list(Lead_election = 0))

#######################################
# Controls
#######################################

#clean up kaopen
df_kaopen = df_kaopen_raw %>% rename("Year" = "year") %>% filter (Year %in%  2007:2017)

#clean up ifs
df_ifs = df_ifs_raw %>% rename(c("Country_Name" = "...1", "Country_Code" = "...2", "Year" = "...3" ))
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

#clean the WEO data
df_weo = df_weo_raw %>% rename_all(make.names)
df_weo = df_weo %>% pivot_longer(cols=4:15, names_to = "Year", values_to = "Values") 
df_weo = df_weo %>% pivot_wider(id_cols = c("ISO", "Country", "Year"), names_from = WEO.Subject.Code, values_from = Values )
df_weo = df_weo %>% mutate(Year = stri_sub(Year, 2)) %>% mutate(Year = as.integer(Year))

# #check if my country codes match weo
# df_check = df_cc %>% left_join(df_weo %>% select("Country","ISO"), by = c("Issuer" = "ISO" ))
# print(df_check %>% filter (is.na(Country)), n = 50)
# #also check the reverse join
# print(df_weo %>% left_join(df_cc, by = c("ISO" = "Issuer")) %>% select("ISO", "Country", "Issuer_Name") %>% filter (is.na(Issuer_Name)), n = 80)
# # All cool!

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
df_model = df_model %>% left_join(df_weo %>% select(ISO, Year, GGR_NGDP), by = c("Issuer" = "ISO", "Year" = "Year")) %>% mutate (GGR_NGDP = as.double(GGR_NGDP)/100)

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
df_ifs = df_ifs %>% arrange(Country_Code, Year) %>% group_by(Country_Code) %>% mutate (GDPGR = GDPD/dplyr::lag(GDPD) - 1)
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, GDPGR), by = c("Issuer" = "Country_Code", "Year" = "Year"))           

######################
# 8 . exchange rate
######################
df_model = df_model %>% left_join(df_ifs %>% select(Country_Code, Year, ER, ERI), by = c("Issuer" = "Country_Code", "Year" = "Year")) 


#######################################
# plots
#######################################
df_rh_bpi = df_rh_bpi %>% mutate(Year_short = stri_sub(Year, 3)) %>% mutate(Year_short = as.factor(Year_short))
color1 = "#b36973"
color2 = "#69b3a9"

## ---- plot_prelim ----
patch <- list()
for (i in 1:length(l_eme)) {
  var = l_eme[i]
  p1 = ggplot(data = df_rh_bpi %>% filter(Issuer == var) , aes(x = Year_short)) +
    geom_point(color= color1, aes(y = Position_Residency), shape = "x", size = 2, stroke = 3) +
    geom_point(color=  color2, aes(y = Restatement_TH)) +
    scale_x_discrete(
      name = "Year"
    ) +
    scale_y_continuous(
      name = str_c("Position_Residency","(x)"),
      sec.axis = sec_axis(~.*1,name=str_c("Restatement_TH", "(.)")),
      limits = c(0,NA)
    ) +
    theme(
      axis.title.y = element_text(color = color1 ),
      axis.title.y.right = element_text(color = color2)
    ) +
    ggtitle(str_c(i,"a) ",var))
  
  p2 =ggplot(data = df_rh_bpi %>% filter(Issuer == var) , aes(x = Year_short, y = Delta_perc)) +
    geom_point() + 
    scale_x_discrete(
      name = "Year"
    ) +
    scale_y_continuous(
      name = "Delta_%age",
      limits = c(0,100)
    ) +
    ggtitle(str_c(i,"b) ",var))
  
  patch[[i]] <-  p1 + p2
}

wrap_plots(patch, ncol = 1)
## ---- end-plot_prelim ----

#######################################
# models
#######################################
# simple model
m1 <- lm(Delta_perc ~ Score_BPI, data = df_rh_bpi)
summary(m1)


#modelsummary(m1, output = "kableExtra") %>% landscape()
#m1 %>% tidy() %>% kbl() %>% landscape()

# model 1 : 
#m1.0 <- lm(lnDelta ~ Has_election + as.factor(Year), data = df_model)
df_model = df_model %>% mutate (Year = as.factor(Year))

# model 1.1: 
m1.1 <- plm(lnDelta ~ Has_election + ka_open + TRAO + CAI + SB  + LENR + GDPGR + ERI, data = df_model, index = c("Issuer", "Year") , effect = c('twoways'), model = "within")
summary(m1.1)

# model 2 - lag 
m1.2 <- plm(lnDelta ~ Has_election + Lag_election + ka_open + TRAO + CAI + SB + LENR + GDPGR + ERI, data = df_model, index = c("Issuer", "Year") , effect = c('twoways'), model = "within")
summary(m1.2)

# model 2 - lag and lead
m2.0 <- plm(lnDelta ~ Lag_election + Has_election + Lead_election + ka_open + TRAO + CAI + SB + LENR + GDPGR + ERI, data = df_model, index = c("Issuer", "Year") , effect = c('twoways'), model = "within")
summary(m1.3)


l_models = list("(1)" = m1.0, 
                "(2)" = m1.1, 
                "(3)" = m2.0)

d_cr = c("Has_election" = "Election in year t", 
         "Lag_election" = "Election in year t+1",
         "Lead_election" = "Election in year t-1",
         "ka_open" = "Capital account openness",
         "TRAO" = "Trade openness",
         "CAI" = "abs(Current account balance)",
         "SB" = "Sophistication of Banking Sector",
         "LENR" = "Lending rate",
         "GDPGR" = "GDP Growth rate",
         "ER" = "Exchange Rate")

## ---- reg_table1 ----
modelsummary(l_models, 
             title = "Effect of elections on foreign debt portfolio flows via OFCs",
             notes = list('Estimating a fixed effects linear model with individual and time fixed effects'),
             coef_rename = d_cr, 
             stars = TRUE, 
             output = "kableExtra") %>% 
  kable_styling(font_size = 10, position = "center", latex_options = "hold_position")
## ---- end-reg_table1 ----
