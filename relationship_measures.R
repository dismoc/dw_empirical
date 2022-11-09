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
library(marginaleffects)
library('timeperiodsR')
library('synthdid')
library('scales')
library('fitdistrplus')
library('ivprobit')
library('margins')
library('robomit')

# Importing Data SBA 7(a)----
sb7a <- rbind(read_csv("D:/Research/DW lending empirical/Data/sba_7a/LenderDetail_2018.csv"),
              read_csv("D:/Research/DW lending empirical/Data/sba_7a/LenderDetail_2019.csv"))
ppf <- read_csv("D:/Research/DW lending empirical/Data/ppp_borrower/query_20211221_082058.csv")


list <- unique(ppf[,c('OriginatingLender','rssd')])
i <- match(sb7a$Lender,list$OriginatingLender)
sb7a$RSSD <- list[i,2]; sb7a$ppp_lender <- list[i,1]

sb7a <- sb7a[!is.na(sb7a$RSSD)==T,]
sb7a$`Approval Amount` <- parse_number(sb7a$`Approval Amount`)
sb7a <- sb7a %>% group_by(RSSD) %>% summarise(sb_count = sum(`Approval Count`,na.rm = T), sb_quant = sum(`Approval Amount`,na.rm = T))

write.csv(sb7a,"D:\\Research\\DW lending empirical\\Data\\sb7.csv")
