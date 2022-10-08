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


ppr <- read_delim("D:/Research/DW lending empirical/Data/ppp_borrower/query_20211221_082058.csv", 
                                    delim = "|", escape_double = FALSE, trim_ws = TRUE)
ppb <- aggregate(currentapprovalamount ~ cert + dateapproved, ppb, FUN = sum)
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

pp <- pp[!duplicated(pp[c('Date.Of.Advance','Original.Outstanding.Advance.Amount')]),]
pp$Date.Of.Advance <- as.Date(pp$Date.Of.Advance)

pp$processing_time <- Winsorize((pp$Date.Of.Advance-pp$origin_date),minval=0, probs=c(.00,1),na.rm=TRUE)

write.csv(pp,"D:\\Research\\DW lending empirical\\Data\\ppplf_full.csv")
