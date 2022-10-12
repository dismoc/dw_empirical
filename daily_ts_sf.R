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
library('scales')

# Import
ppb <- read_csv("D:/Research/DW lending empirical/Data/ppp_daily.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
pplf <- read_csv("D:/Research/DW lending empirical/Data/ppplf_full.csv")
pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")
att <- read_csv("D:/Research/DW lending empirical/Data/ffiec/Atrributes_merged.csv")[,c('ID_ABA_PRIM','#ID_RSSD','STATE_ABBR_NM')]

setnames(att,old=c('ID_ABA_PRIM','#ID_RSSD','STATE_ABBR_NM'), new =c('ABA','IDRSSD','STATE'))

  


    
         
  
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
  
  ls <- unique(data.frame(IDRSSD = df$IDRSSD, ABA = df$Primary.ABA.Routing.Number, STATE = df$Financial.Institution.State))
  ls <- unique(smartbind(ls,data.frame(att)))
  
  sf2 <- aggregate(InitialApprovalAmount ~ rssd + DateApproved, pppm, sum)
  sf2 <- full_join(sf2, unique(df[,c('IDRSSD','Primary.ABA.Routing.Number')]), by=c('rssd' = 'IDRSSD'))
  sf2 <- left_join(sf2, ls, by=c('rssd' = 'IDRSSD'))
  sf2$ABA <- coalesce(sf2$Primary.ABA.Routing.Number, sf2$ABA)
  sf2 <- data.frame(RSSD = sf2$rssd, Date = sf2$DateApproved, ABA = sf2$ABA, State = sf2$STATE, PPP = sf2$InitialApprovalAmount)
  sf2 <- full_join(sf2, 
                   left_join(data.frame(aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + Date.Of.Advance, pplf, sum)), 
                             pplf[,c('Institution.RSSD', 'Date.Of.Advance', 'origin_date')]),
                  by=c('RSSD' = 'Institution.RSSD', 'Date' = 'origin_date')) %>% select(-contains("..."))
  
  dwborrow1 <- dwborrow
  dwborrow1$Borrower.ABA.number <- as.numeric(dwborrow1$Borrower.ABA.number)
  dwborrow1 <- subset(dwborrow1,Loan.date >= as.Date('2020-04-03') & Loan.date <= as.Date('2020-08-08'))
  sf2 <- full_join(sf2,dwborrow1[,c('Loan.date','Borrower.ABA.number','Loan.amount')], by=c('ABA' = 'Borrower.ABA.number', 'Date' = 'Loan.date'))
  
  sf2[is.na(sf2$Original.Outstanding.Advance.Amount),c('Original.Outstanding.Advance.Amount')] <- 0
  sf2[is.na(sf2$Loan.amount),c('Loan.amount')] <- 0
  sf2[is.na(sf2$PPP) == TRUE, 'PPP'] <- 0
  sf2 <- sf2[!is.na(sf2$Date),]
  sf2[is.na(sf2$ABA) == TRUE,'ABA'] <- ls[match(sf2[is.na(sf2$ABA) == TRUE,'RSSD'],ls$IDRSSD),'ABA']
  sf2[is.na(sf2$RSSD) == TRUE,'RSSD'] <- ls[match(sf2[is.na(sf2$RSSD) == TRUE,'ABA'],ls$ABA),'IDRSSD']
  
  
  temp <- subset(df, as.Date(Date) >= as.Date('2020-03-31'))[,c('Date','size','IDRSSD','reserve_asset_ratio','reserve_loan_ratio','RCON0071','RCON0081','eqcaprat','age','borr_total','FED','exposure')]
  temp$Date <- as.Date(temp$Date)
  sf2$Quarter <- as.Date(quarter(sf2$Date, type='date_first')-1)
  sf2 <- left_join(sf2, temp, by=c('RSSD' = 'IDRSSD', 'Quarter' = 'Date'))
  
  setnames(sf2, old=c('Original.Outstanding.Advance.Amount', 'Loan.amount'),
           new = c('PPPLF','DW'))
  sf2$Reserves <- (sf2$RCON0081+sf2$RCON0071)*1000
  sf2$dw_so_res <- sf2$DW/sf2$Reserves
  sf2$dwbin_notest <- ifelse(sf2$dw_so_res > .01, 1,0)
  sf2$dwbin_notest <- ifelse(is.na(sf2$dwbin_notest) == TRUE, 0, sf2$dwbin_notest)
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  #sf2$dw_bin <- ifelse(is.na(sf2$dw_bin) == FALSE, sf2$dw_bin, 0)
  
  sf2$PPPLF_ind <- ifelse(sf2$PPPLF > 0, 1, 0)
  sf2$r_delt <- -sf2$PPPLF + sf2$PPP
  sf2 <- sf2[order(sf2$RSSD,sf2$Date),]
  sf2 <- sf2 %>% group_by(RSSD) %>% mutate(cs_ppp = cumsum(PPP), cs_delt = cumsum(r_delt))
  sf2$month <- month(sf2$Date)
  sf2$cs_ppp <- sf2$cs_ppp/sf2$Reserves
  sf2$ppp_so_reserves <- sf2$PPP/sf2$Reserves
  sf2$delt_so_reserves <- sf2$r_delt/sf2$Reserves
  sf2 <- sf2 %>% mutate(dsr_quint = ntile(delt_so_reserves, 20), psr_quint = ntile(ppp_so_reserves, 20))
  
  temp <-aggregate(PPPLF ~ RSSD, sf2, sum)
  temp$PPPLF_i <- ifelse(temp$PPPLF > 0, 1, 0)
  sf2 <- left_join(sf2,temp[,c('RSSD','PPPLF_i')])
  
  # create series on whether bank has used DW 5 years pre-covid
  dwborrow1 <- subset(dwborrow, Loan.date >= '2015-04-03' & Loan.date < '2020-04-03')
  ls <- intersect(unique(sf2$ABA),unique(dwborrow1$Borrower.ABA.number))
  sf2[sf2$ABA %in% ls, 'precovdw'] <- 1
  sf2$precovdw <- ifelse(is.na(sf2$precovdw) == TRUE, 0, 1)
  #Creating the shift share instrument
    #share - total PPP loans lent out by state and day, shift - daily change in PPP lending size total
    sbydate <- aggregate(cbind(n,InitialApprovalAmount) ~ OriginatingLenderState + DateApproved, subset(pppm, DateApproved <= '2020-08-08'), sum)
# Regressions 
  sf3 <- subset(sf2, Date >= as.Date('2020-04-03') & Date <= as.Date('2020-08-08'))
  sf3 <- sf3[!duplicated(sf3[c("RSSD", "Date")]),]
  dict1 <- c('dw_bin' = 'DW Prob', 'demand_so_reserves' = 'Demand Shock', 'reserve_asset_ratio' = 'RA Ratio',
             'size' = 'Log(Asset)', 'log(PPPLF+1)' = 'Log(PPPLF)', 'log(RCON0010)' = 'Log(Reserves)',
             'eqcaprat' = 'Equity Cap Ratio', 'ppp_so_reserves' = 'Demand Shock',
             'ppp_so_reserves x PPPLF_ind' = 'DS*PPPLF_ind', 'log(borr_total+1)' = 'Log(FF Borrowing)',
             'log(ppp_so_reserves)' = 'Log(Demand Shock)', 'log(PPP)' = 'Log(PPP)',
             'dwbin_notest' = 'DW Prob')
  
    #Table 1: Binary regression: 1) baseline 2-5) progressively more controls
      t1 <- list()
      t1[[1]] <- feglm(dwbin_notest ~ ppp_so_reserves*PPPLF_i | State + month, 
                  sf3, family = binomial(link = "logit"), panel.id = c('RSSD','Date'))
      t1[[2]] <- update(t1[[1]], .~. + reserve_asset_ratio)
      t1[[3]] <- update(t1[[2]], .~. + precovdw)
      t1[[4]] <- update(t1[[3]], .~. + size + age )
      t1[[5]] <- update(t1[[4]], .~. + eqcaprat)
      etable(t1, dict=dict1,
             se = 'white', tex=F)

    #Robustness - families
      t2 <- list()
      t2[[1]] <- update(t1[[5]], family = binomial(link = "probit"))
      t2[[2]] <- update(t1[[5]], family = gaussian())
      etable(t1[[5]],t2, dict=dict1, se='white', tex=F,
             drop = c('PPPLF_ind','RA Ratio','Asset','Equity','FF Borrowing'))
    
    #Robustness - clustering
      t3 <- list()
      t3[[1]] <- update(t1[[5]], se = 'white')
      t3[[2]] <- update(t1[[5]], cluster = 'RSSD')
      t3[[3]] <- update(t1[[5]], cluster = 'State')
      t3[[4]] <- update(t1[[5]], cluster = 'FED')
      t3[[5]] <- update(t1[[5]], cluster = 'month')
      etable(t3, dict=dict1, tex=F,
             drop = c('PPPLF_ind','RA Ratio','Asset','Equity','FF Borrowing'))
      
      
    #Robustness - sample sizes
      t4 <- list()
      t4[[1]] <- update(t1[[5]], data = subset(sf3, ppp_so_reserves >= .01))
      t4[[2]] <- update(t1[[5]], data = subset(sf3, ppp_so_reserves >= .05))
      t4[[3]] <- update(t1[[5]], data = subset(sf3, ppp_so_reserves >= .1))
      t4[[4]] <- update(t1[[5]], data = subset(sf3, ppp_so_reserves >= .2))
      etable(t1[[5]], t4, dict=dict1, se='white', tex=F,
             drop = c('PPPLF_ind','RA Ratio','Asset','Equity','FF Borrowing'))
      
    #Robustness - different shock series creation
      t5 <- list()
      t5[[1]] <- t1[[5]]
      t5[[2]] <- update(t5[[1]], .~. - ppp_so_reserves*PPPLF_ind + log(ppp_so_reserves)*PPPLF_ind)
      t5[[3]] <- update(t5[[1]], .~. - ppp_so_reserves*PPPLF_ind + log(PPP)*PPPLF_ind )
      etable(t5, dict=dict1, se='white', tex=F,
             drop = c('PPPLF_ind','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age'))
      
    #Robustness - Fixed Effects
      t6 <- list()
      t6[[1]] <- update(t1[[5]], .~. |. - State - month)
      t6[[2]] <- update(t1[[5]], .~. |. - State )
      t6[[3]] <- update(t1[[5]], .~. |. - State + FED)
      t6[[4]] <- update(t1[[5]], .~. |. - State + RSSD)
      etable(t1[[5]], t6, dict=dict1, se='white', tex=F,
             drop = c('PPPLF_ind','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age'))
      
      
    #Marginal effects - baseline sample
      me1 <- marginaleffects(t1[[5]]); summary(me1)
      plot_cme(t1[[5]], effect = 'ppp_so_reserves', condition = 'reserve_asset_ratio')
      