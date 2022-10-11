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
att <- read_csv("D:/Research/DW lending empirical/Data/ffiec/Atrributes_merged.csv")[,c('ID_ABA_PRIM','#ID_RSSD','STATE_ABBR_NM')]

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
    #only for april - august period
    sf <- sf[sf$InitialApprovalAmount != 0,]
    # PPP and DW loan correlation not log with cut
    ggplot(sf) +
      geom_line(aes(x = DateApproved, y = InitialApprovalAmount/3, colour ='PPP'), size=1.5) +
      geom_line(aes(x = DateApproved, y = Loan.amount, colour ='DW'), size=1.5) +
      scale_y_continuous(name = "Daily DW Loans", 
                         sec.axis = sec_axis(~.*3, name="Daily PPP Loans", labels = unit_format(unit = "B", scale = 1e-9)),
                         labels = unit_format(unit = "B", scale = 1e-9)) +
      labs(x="Date") + 
      theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
      annotate('text', label = paste0('Correlation = ',round(cor(sf$Loan.amount,sf$InitialApprovalAmount,'complete'),2)), 
               x=max(sf$DateApproved, na.rm=TRUE) - 21, y=10e9)
  
    #Now with the log values
    ggplot(sf) +
      geom_line(aes(x = DateApproved, y = log(InitialApprovalAmount+1), colour ='PPP'), size=1.5) +
      geom_line(aes(x = DateApproved, y = log(Loan.amount+1), colour ='DW'), size=1.5) +
      scale_y_continuous(name = "Log Value") +
      labs(x="Date") + 
      theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
      annotate('text', label = paste0('Correlation = ',round(cor(log(sf$Loan.amount+1),log(sf$InitialApprovalAmount+1),'complete'),2)), 
               x=max(sf$DateApproved, na.rm=TRUE) - 21, y=10)
    
    # using moving averages
    ggplot(sf) +
      geom_line(aes(x = DateApproved, y = ppp_week_avg/5, colour ='PPP'), size=1.5) +
      geom_line(aes(x = DateApproved, y = dw_quant_avg, colour ='DW'), size=1.5) +
      scale_y_continuous(name = "Mov. Avg. DW Loans", 
                         sec.axis = sec_axis(~.*5, name="Mov. Avg. PPP Loans", labels = unit_format(unit = "B", scale = 1e-9)),
                         labels = unit_format(unit = "B", scale = 1e-9)) +
      labs(x="Date") + 
      theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
      annotate('text', label = paste0('Correlation = ',round(cor(sf$ppp_week_avg,sf$dw_quant_avg,'complete'),2)), 
               x=max(sf$DateApproved, na.rm=TRUE) - 21, y=5e9)
    
    #Now with the log values
    ggplot(sf) +
      geom_line(aes(x = DateApproved, y = log(ppp_week_avg), colour ='PPP'), size=1.5) +
      geom_line(aes(x = DateApproved, y = log(dw_quant_avg), colour ='DW'), size=1.5) +
      scale_y_continuous(name = "MA Log Value") +
      labs(x="Date") + 
      theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
      annotate('text', label = paste0('Correlation = ',round(cor(log(sf$ppp_week_avg+1),log(sf$dw_quant_avg+1),'complete'),2)), 
               x=max(sf$DateApproved, na.rm=TRUE) - 21, y=22)
    
         
  
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
  
  temp <- subset(df, as.Date(Date) == as.Date('2020-03-31'))
  
  sf2 <- aggregate(InitialApprovalAmount ~ rssd + DateApproved, pppm, sum)
  sf2 <- left_join(sf2, att, by=c('rssd' = '#ID_RSSD'))
  sf2 <- full_join(pppm,unique(pplf[,c('Institution.RSSD','Institution.ABA')]),by=c('rssd' = 'Institution.RSSD')) %>% select(-contains("..."))
  sf2$ABA <- ifelse(sf2$ID_ABA_PRIM == 0 | is.na(sf2$ID_ABA_PRIM) == TRUE, sf2$Institution.ABA, sf2$ID_ABA_PRIM)
  sf2 <- data.frame(RSSD = sf2$rssd, Date = sf2$DateApproved, ABA = sf2$ABA, State = sf2$OriginatingLenderState, PPP = sf2$InitialApprovalAmount)
  
  sf2 <- full_join(sf2, data.frame(aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + Date.Of.Advance, pplf, sum)),
                  by=c('RSSD' = 'Institution.RSSD', 'Date' = 'Date.Of.Advance')) %>% select(-contains("..."))
  sf2 <- left_join(sf2,dwborrow[,c('Loan.date','Borrower.ABA.number','Loan.amount')], by=c('Institution.ABA' = 'Borrower.ABA.number', 'DateApproved' = 'Loan.date'))
  temp <- subset(df, as.Date(Date) == as.Date('2020-03-31'))[,c('size','IDRSSD','reserve_asset_ratio','reserve_loan_ratio','RCON0010','eqcaprat','age')]
  temp$IDRSSD <- as.numeric(as.character(temp$IDRSSD))
  sf2 <- left_join(sf2, temp, by=c('rssd' = 'IDRSSD'))
  
  sf2[is.na(sf2$Original.Outstanding.Advance.Amount),c('Original.Outstanding.Advance.Amount')] <- 0
  sf2[is.na(sf2$Loan.amount),c('Loan.amount')] <- 0
  setnames(sf2, old=c('DateApproved','InitialApprovalAmount', 'Original.Outstanding.Advance.Amount', 'Loan.amount','OriginatingLenderState'),
           new = c('Date','PPP','PPPLF','DW','State'))
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  sf2$PPPLF_ind <- ifelse(sf2$PPPLF > 0, 1, 0)
  sf2$r_delt <- -sf2$PPPLF + sf2$PPP
  sf2$cumu_shock <- 
  sf2 <- sf2[order(sf2$rssd,sf2$Date),]
  sf2$month <- month(sf2$Date)
  sf2$demand_so_reserves <- sf2$r_delt/(sf2$RCON0010*1000)
  sf2 <- sf2 %>% mutate(rd_quint = ntile(r_delt, 5))
  sf2 <- sf2 %>% mutate(ds_quint = ntile(demand_so_reserves, 5))
  
  
  
  # Regression on demand share of reserves using reserve data from Q1 2020
  dict1 <- c('dw_bin' = 'DW Prob', 'demand_so_reserves' = 'Demand Shock', 'reserve_asset_ratio' = 'RA Ratio',
             'size' = 'Log(Asset)', 'log(PPPLF+1)' = 'Log(PPPLF)', 'log(RCON0010)' = 'Log(Reserves)',
             'eqcaprat' = 'Equity Cap Ratio')
  
    #Table 1: Binary regression: 1) baseline 2-5) progressively more controls
    r1 <- feols(dw_bin ~ demand_so_reserves |State + month, subset(sf2, ds_quint>=0))
    r2 <- update(r1, .~.+reserve_asset_ratio)
    r3 <- update(r2, .~.+size)
    r4 <- update(r3, .~. + log(PPPLF+1))
    r5 <- update(r4, .~.  +eqcaprat)
  #Main
    etable(r1,r2,r3,r4,r5, dict=dict1,
           se='cluster', fitstat=~n+r2, tex=T)
  #Robustness
    r6 <- feglm(dw_bin ~ demand_so_reserves + reserve_asset_ratio + size + log(PPPLF+1) + eqcaprat| State + month, sf2, se = 'white', family = binomial(link = "logit"))
    r7 <- update(r6, family = binomial(link = "probit"))
    etable(r5,r6,r7, dict=dict1,
           se='cluster',
           tex=T)
    
    
  # Binary regressions + robustness
    r1 <- feols(log(DW+1) ~ log(demand_so_reserves) |State + month, subset(sf2, ds_quint>=0))
    r2 <- update(r1, .~.+reserve_asset_ratio)
    r3 <- update(r2, .~.+size)
    r4 <- update(r3, .~. + log(PPPLF+1))
    r5 <- update(r4, .~.  +eqcaprat)
    #Main
    etable(r1,r2,r3,r4,r5, dict=dict1,
           se='cluster', fitstat=~n+r2, tex=F)
    #Robustness
    r6 <- feglm(log(DW) ~ demand_so_reserves + reserve_asset_ratio + size + log(PPPLF+1) + eqcaprat| State + month, sf2, se = 'white', family = binomial(link = "logit"))
    r7 <- update(r6, family = binomial(link = "probit"))
    etable(r5,r6,r7, dict=dict1,
           se='cluster',
           tex=F)
    
  # Elasticity regressions
  feols(asinh(DW) ~ asinh(r_delt) + reserve_asset_ratio + reserve_loan_ratio + size| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by
  feols(log(DW) ~ quintile + reserve_asset_ratio + reserve_loan_ratio + size| State + month, sf2, se='white') #increasing reserve demand by 1 decile increases borrowing probability by

  