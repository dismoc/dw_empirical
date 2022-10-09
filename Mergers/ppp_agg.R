# Library ----
library(tidyverse)
library(mlr3verse)
library(plm)
library(pglm)
library(readxl)
library(dplyr)
library('DataCombine')
library('stargazer')
library('simpleboot')
library('Rpdb')
library('data.table')
library(ggplot2)
library('zoo')
library('fixest')
library('timeDate')
library('DescTools')
library('summarytools')
library(gtools)
library(lubridate)
library('fuzzyjoin')
library(readr)
library('ggpubr')
library('haven')

# Import
list <- list.files(path="D:/Research/DW lending empirical/Data/ppp_sba/", pattern=".csv")
dfp <- read_delim(paste0("D:/Research/DW lending empirical/Data/ppp_sba/",list[1]), show_col_types = FALSE) 
matchlb <- read_dta("D:/Downloads/matched_loans_banks_UPDATED_alldates.dta")

# Reading in and merging
                  
for (i in 2:length(list)){
  dfpn <- read_delim(paste0("D:/Research/DW lending empirical/Data/ppp_sba/",list[i]), show_col_types = FALSE) 
  dfp <- bind_rows(dfp,dfpn) %>% select(-contains("..."))
  print(i)
}

rm(dfpn)
dfp$DateApproved <- as.Date(dfp$DateApproved,'%m/%d/%Y')
dfp <- left_join(dfp, matchlb[,c('loannumber','rssd')], by=c('LoanNumber' = 'loannumber'))

ppb <- aggregate(InitialApprovalAmount ~ DateApproved, dfp, FUN = sum)

dfn <- left_join(aggregate(InitialApprovalAmount ~ rssd + DateApproved, dfp, sum), dfp %>% count(rssd, DateApproved))
dfn <- left_join(dfn, unique(dfp[,c('rssd','OriginatingLenderState')]))

write.csv(dfp,"D:\\Research\\DW lending empirical\\Data\\ppp_full.csv")
write.csv(ppb,"D:\\Research\\DW lending empirical\\Data\\ppp_daily.csv")
write.csv(dfn,"D:\\Research\\DW lending empirical\\Data\\ppp_bankmatched.csv")
