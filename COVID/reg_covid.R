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
library('car')

# (signalling) Did DW borrowing during COVID affect fed funds rate that bnaks borrow at?
  # Strategy - do 2 regressions, pre and during covid.

dfpre <- subset(df, as.Date(Date) <= as.Date('2020-01-01'))
dfpost <- subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') )

dfpost$dw_quant[is.na(dfpost$dw_quant)] <- 0

# 1 SD change in size during covid led to 6.2% increase in borrowing prob, 1 SD change in size pre-covid led to a .89% (same as Ennis Klee) increase in borrowing prob.

post <-feols(dwborrow_bin ~ bigsmall + ci_loans_growth + reserve_asset_ratio + stmult + log(1+nonppp_loans)| FED + Date,
      data = dfpost,
      panel.id = c('IDRSSD','Date')) 

pre <- feols(dwborrow_bin ~ bigsmall + ci_loans_growth + reserve_asset_ratio + stmult |FED + Date,
             data = dfpre,
             panel.id = c('IDRSSD','Date')) 

etable(post,pre)

# how does uncovered PPP loan affect DW borrowing
  # gains from this reg: larger banks more likely to borrow, increase in non-ppp loans to reserve weakly increases chance of borrowing, dw borrowing is made by liquidity constrained banks,
  # increase in borrowing from MMLF decreases chance of borrowing, 
regq <- feols(log(dw_quant+1) ~ size + nppp_loan_reserve_ratio + reserve_asset_ratio + log(1+nonppp_loans) + log(RCONLL61+1) +  log(ppp_advance+1)+ i(FED)| IDRSSD + Date, data = dfpost, cluster='FED'); 
regb <- update(regq, dwborrow_bin ~ .); etable(regq, regb)

  #Controlling for loan demand, how much is dw loan and ppp loan substitutes?
reg <- feols(log(dw_quant+1) ~ log(ppp_advance+1) + log(loans+1) + log(RCONLL58+1) + size| Date + FED, dfpost); etable(reg)
