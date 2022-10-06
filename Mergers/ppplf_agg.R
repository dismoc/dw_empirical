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

# Reading in and merging
list <- list.files(path="D:/Research/DW lending empirical/Data/PPPLF/", pattern=".xlsx")

pp <- data.frame(read_excel(paste0("D:/Research/DW lending empirical/Data/PPPLF/",list[1])))

for (i in 2:length(list)){
  ppn <- data.frame(read_excel(paste0("D:/Research/DW lending empirical/Data/PPPLF/",list[i])))
  pp <- smartbind(pp,ppn) %>% select(-contains("..."))
}

pp$origin_date <- as.Date(ifelse(year(pp$Date.Of.Maturity) == 2022, 
                         as.Date(pp$Date.Of.Maturity, format = '%Y-%m-%d') - years(2),
                         as.Date(pp$Date.Of.Maturity, format = '%Y-%m-%d') - years(5)))
pp$processing_time <- difftime(pp$Date.Of.Advance,pp$origin_date, units = 'days')
