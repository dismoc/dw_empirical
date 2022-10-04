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
library('fuzzyjoin')
library('lubridate')

# (signalling) Did DW borrowing during COVID affect fed funds rate that bnaks borrow at?
  # Strategy - do 2 regressions, pre and during covid.

# 1 SD change in size during covid led to 6% increase in borrowing prob, 1 SD change in size pre-covid led to a 1% increase in borrowing prob.
post <-feols(dwborrow_bin ~ size + age + exposure + ca + aq + roe + i(FED)| Date,
      data = subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') ),
      panel.id = c('IDRSSD','Date')) 

pre <-feols(dwborrow_bin ~ size + age + exposure + ca + aq + roe + i(FED)| Date,
             data = subset(df, as.Date(Date) <= as.Date('2020-01-01') ),
             panel.id = c('IDRSSD','Date')) 

etable(post,pre, cluster='FED')      

#small number of large banks in boston, new york,philly, SF (1,2,3,12), large number of small banks in chicago and KC. ----
left_join(aggregate(size ~ FED + Date, subset(df, as.Date(Date) >= as.Date('2020-01-01')), FUN = mean), subset(df, as.Date(Date) >= as.Date('2020-01-01')) %>% count(Date, FED))

#