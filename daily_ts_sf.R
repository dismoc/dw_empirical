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
ppb <- read_csv("D:/Research/DW lending empirical/Data/ppp_daily.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
pplf <- read_csv("D:/Research/DW lending empirical/Data/ppplf_full.csv")
pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")


# Aggregate Data 1: Correlation between dw borrowing (aggregate) and aggregate ppp loans-------
  # Transformation
  

  ppb2<- aggregate(Original.Outstanding.Advance.Amount ~ Date.Of.Advance, pplf, sum)
  sf <- full_join(ppb, ppb2, by=c('DateApproved' = 'Date.Of.Advance'))
  sf <- sf[order(sf$DateApproved),]
  sf[is.na(sf)] <- 0
  sf$tot_change <- sf$Original.Outstanding.Advance.Amount - sf$InitialApprovalAmount
  sf$cumu <- cumsum(sf[, 'tot_change'])
  
  dwborrow1 <- dwborrow %>% count(Loan.date)
  dwborrow2 <- aggregate(Loan.amount ~ Loan.date, dwborrow, FUN = sum)
  dwborrow1 <- subset(full_join(dwborrow2,dwborrow1), as.Date(Loan.date) >= as.Date('2020-01-01'))
  
  sf <- full_join(sf,dwborrow1, by=c('DateApproved' = 'Loan.date')) %>% select(-contains("..."))
  sf <- subset(sf, as.Date(DateApproved) <= as.Date('2020-10-01'))
  sf <- sf[order(sf$DateApproved),]
  
  
  ind <- which(is.na(sf$n) == TRUE)
  for (i in 2:length(ind)) {
    if (ind[i] == ind[i-1]+1) {
      ind <- ind[-i]
    }
    print(length(ind))
  }
  
  sf[ind+1,'InitialApprovalAmount'] <- sf[ind+1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  ind <- which(is.na(sf$n) == TRUE)
  ind <- ind[1:length(ind)-1]
  sf[ind+1,'InitialApprovalAmount'] <- sf[ind+1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  ind <- which(is.na(sf$n) == TRUE)
  sf[ind-1,'InitialApprovalAmount'] <- sf[ind-1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  
  rm(ind, dwborrow1, dwborrow2)
  # Creation
      sf <- sf[sf$InitialApprovalAmount != 0,]
  
  sf$cumu <- sf$cumu$tot_change
  sf$cumu_init <- 4075359310000 + sf$cumu
  sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, sf$InitialApprovalAmount)
  sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) >= as.Date('2020-08-10'), 0, sf$InitialApprovalAmount)
  sf$ppp_week_avg <- rollapply(sf$InitialApprovalAmount, 5, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$dw_quant_avg <- rollapply(sf$Loan.amount, 5, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$cum_quant_avg <- rollapply(sf$cumu, 5, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$tchange_avg <- rollapply(sf$tot_change, 5, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$id <- 1
  sf$signal <- ifelse(as.Date(sf$DateApproved) >= as.Date('2020-03-16') & as.Date(sf$DateApproved) <= as.Date('2020-03-21'), 1, 0)
  sf$preppp <- ifelse(as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, 1)
  # Figures
  ggplot(sf) +
    geom_line(aes(x = DateApproved, y = InitialApprovalAmount/3, colour ='PPP')) +
    geom_line(aes(x = DateApproved, y = Loan.amount, colour ='DW')) +
    scale_y_continuous(name = "Moving Avg of DW Loan", sec.axis = sec_axis(~.*3, name="Moving Avg of PPP Loan")) +
    labs(x="Date") + theme(legend.position = c(.9, .9))
  
  ggplot(sf) +
    geom_line(aes(x = DateApproved, y = log(InitialApprovalAmount), colour ='PPP')) +
    geom_line(aes(x = DateApproved, y = log(Loan.amount), colour ='DW')) +
    labs(x="Date") + theme(legend.position = c(.9, .9))
  
  ggplot(sf) +
    geom_line(aes(x = DateApproved, y = log(ppp_week_avg), colour ='PPP')) +
    geom_line(aes(x = DateApproved, y = log(dw_quant_avg), colour ='DW')) +
    labs(x="Date") + theme(legend.position = c(.9, .9))
  
  ggplot(subset(sf, as.Date(DateApproved) >= as.Date('2020-04-01') &as.Date(DateApproved) <= as.Date('2020-08-10'))) +
         geom_line(aes(x = DateApproved, y = log(ppp_week_avg+1), colour ='PPP')) +
         geom_line(aes(x = DateApproved, y = log(dw_quant_avg+1), colour ='DW')) +
         labs(x="Date") + theme(legend.position = c(.9, .9))
         
  
  #Regressions
  dict1 <- c('log(InitialApprovalAmount)' = 'PPP', 'log(quant_week_avg)' = 'Avg Weekly PPP', 'log(Loan.amount)' = 'DW Quant',
             'log(dw_quant_avg)' = 'Avg Weekly DW Quant')
  
  r1 <- feols(log(Loan.amount) ~ log(InitialApprovalAmount) , sf, panel.id = ~id + DateApproved, se='white')
  r3 <- feols(log(dw_quant_avg) ~ log(ppp_week_avg), sf, panel.id = ~id + DateApproved, se='iid')
  etable(r1,r3, dict= dict1,
         drop = '(Intercept)',
         se = 'white',
         tex = F)

# Aggregate Data 2: correlation between dw borrowing (binary) and uncovered ppp loans (ppp loans - ppplf advance) -------
# Bank Level Data: -----
  sf2 <- left_join(pppm,unique(pplf[,c('Institution.RSSD','Institution.ABA')]),by=c('rssd' = 'Institution.RSSD')) %>% select(-contains("..."))
  sf2 <- full_join(sf2, data.frame(aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + Date.Of.Advance, pplf, sum)),
                  by=c('rssd' = 'Institution.RSSD', 'DateApproved' = 'Date.Of.Advance')) %>% select(-contains("..."))
  sf2 <- left_join(sf2,dwborrow[,c('Loan.date','Borrower.ABA.number','Loan.amount')], by=c('Institution.ABA' = 'Borrower.ABA.number', 'DateApproved' = 'Loan.date'))
  temp <- subset(df, as.Date(Date) == as.Date('2020-03-31'))[,c('size','IDRSSD','reserve_asset_ratio','reserve_loan_ratio','RCON0010')]
  temp$IDRSSD <- as.numeric(as.character(temp$IDRSSD))
  sf2 <- left_join(sf2, temp, by=c('rssd' = 'IDRSSD'))
  
  sf2[is.na(sf2$Original.Outstanding.Advance.Amount),c('Original.Outstanding.Advance.Amount')] <- 0
  sf2[is.na(sf2$Loan.amount),c('Loan.amount')] <- 0
  setnames(sf2, old=c('DateApproved','InitialApprovalAmount', 'Original.Outstanding.Advance.Amount', 'Loan.amount','OriginatingLenderState'),
           new = c('Date','PPP','PPPLF','DW','State'))
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  sf2$r_delt <- -sf2$PPPLF + sf2$PPP  
  sf2 <- sf2[order(sf2$rssd,sf2$Date),]
  sf2$month <- month(sf2$Date)
  sf2 <- sf2 %>% mutate(quintile = ntile(r_delt, 5))
  sf2$demand_so_reserves <- sf2$r_delt/(sf2$RCON0010*1000)
  
  
  
  # Regression on demand share of reserves using reserve data from Q1 2020
  feols(dw_bin ~ demand_so_reserves + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se='white')
  feglm(dw_bin ~ demand_so_reserves + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se = 'white', family = binomial(link = "logit"))
  feglm(dw_bin ~ demand_so_reserves + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se = 'white', family = binomial(link = "probit"))
  
  feols(asinh(DW) ~ demand_so_reserves + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by
  feols(log(DW+1) ~ demand_so_reserves + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by
  
  
  # Binary regressions + robustness
  feols(dw_bin ~ asinh(r_delt) + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se='white')
  
  feols(dw_bin ~ quintile + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by
  feglm(dw_bin ~ quintile + reserve_asset_ratio + reserve_loan_ratio + size| State + month, sf2, se = 'white', family = binomial(link = "logit"))
  feglm(dw_bin ~ quintile + reserve_asset_ratio + reserve_loan_ratio + size + log(PPPLF+1)| State + month, sf2, se = 'white', family = binomial(link = "probit"))
 
  # Elasticity regressions
  feols(asinh(DW) ~ asinh(r_delt) + reserve_asset_ratio + reserve_loan_ratio + size| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by
  feols(log(DW) ~ quintile + reserve_asset_ratio + reserve_loan_ratio + size| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by

  