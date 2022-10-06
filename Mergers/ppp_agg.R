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

# Import

# Reading in and merging
list <- list.files(path="D:/Research/DW lending empirical/Data/ppp_sba/", pattern=".csv")
dfp <- read_delim(paste0("D:/Research/DW lending empirical/Data/ppp_sba/",list[1]), show_col_types = FALSE) 
                  
for (i in 2:length(list)){
  dfpn <- read_delim(paste0("D:/Research/DW lending empirical/Data/ppp_sba/",list[i]), show_col_types = FALSE) 
  dfp <- bind_rows(dfp,dfpn) %>% select(-contains("..."))
  print(i)
}

rm(dfpn)
dfp$DateApproved <- as.Date(dfp$DateApproved,'%m/%d/%Y')
pp1 <- aggregate(InitialApprovalAmount ~ DateApproved, dfp, FUN = sum)
