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

dfpre <- subset(df, as.Date(Date) <= as.Date('2020-01-01'))
dfpost <- subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') )

dfpost$RCONLG26[is.na(dfpost$RCONLG26)] <- 0

# 1 SD change in size during covid led to 6.2% increase in borrowing prob, 1 SD change in size pre-covid led to a .89% (same as Ennis Klee) increase in borrowing prob.

post <-feols(dwborrow_bin ~ size + exposure + ci_loans_growth + reserve_asset_ratio + stmult + RCONLG26|FED + Date,
      data = dfpost,
      panel.id = c('IDRSSD','Date')) 

pre <- feols(dwborrow_bin ~ size + exposure + ci_loans_growth + reserve_asset_ratio + stmult + log(RCONLG26)|FED + Date,
             data = dfpre,
             panel.id = c('IDRSSD','Date')) 

etable(post,pre)

#small number of large banks in boston, new york,philly, SF (1,2,3,12), large number of small banks in chicago and KC. ----
left_join(aggregate(size ~ FED + Date, subset(df, as.Date(Date) >= as.Date('2020-01-01')), FUN = mean), subset(df, as.Date(Date) >= as.Date('2020-01-01')) %>% count(Date, FED))

# Exposure is highly volatile in the the three periods of COVID (SD: 11.03 vs .95 in the 3 periods before COVID)

descr(subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') )$exposure)
descr(subset(df, as.Date(Date) <= as.Date('2020-01-01') & as.Date(Date) >= as.Date('2019-04-01'))$exposure)


# Reserve to deposit ratio for 2018-2019 is 7.9% for all banks, post-COVID, non-borrower has 11.6% and borrowers had 7.8%
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, subset(dfpre, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, dfpost, FUN = mean)[1:3,3]) #nonborrower
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, dfpost, FUN = mean)[4:6,3]) #borrower

# Looking at total deposits in the banking system, there was an average of 1B precovid, then 1.6B for nonborrowers and 5.8B for borrowers
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01') ), FUN = mean)[,3]) #pre total
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre nonborrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre borrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, dfpost, FUN = mean)[1:3,3]) #nonborrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, dfpost, FUN = mean)[4:6,3]) #borrower