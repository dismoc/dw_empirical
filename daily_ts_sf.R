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

# Import ----
ppb <- read_csv("D:/Research/DW lending empirical/Data/ppp_daily.csv")
df <- read_csv("D:/Research/DW lending empirical/Data/merged_cov.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
pplf <- read_csv("D:/Research/DW lending empirical/Data/ppplf_full.csv")
pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")
att <- read_csv("D:/Research/DW lending empirical/Data/ffiec/Atrributes_merged.csv")[,c('ID_ABA_PRIM','#ID_RSSD','STATE_ABBR_NM')]
binstr <- read_csv("D:/Research/DW lending empirical/Data/binstr.csv")

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
  
# Creating daily proxy series for assets and non-ppp CI loans.
  totci <- read_csv("D:/Research/DW lending empirical/Data/TOTCI.csv")
  totci <- totci %>% complete(Date = seq.Date(min(Date), max(Date), by='day'))
  totci$TOTCI <- approxfun(1:nrow(totci), totci$TOTCI)(1:nrow(totci))*1000000000
  aggregate(InitialApprovalAmount ~ DateApproved, pppm, sum)
  
# Daily Bank Level Data Creation: -----
  ls <- subset(df, Date == '2020-03-31')
  ls <- unique(data.frame(RSSD = ls$IDRSSD, ABA = ls$Primary.ABA.Routing.Number, STATE = ls$Financial.Institution.State))
  sf2 <- data.frame(ls, Date=rep(full_seq(c(as.Date('2020-04-03'),as.Date('2020-08-08')), period=1),5165))
  sf2 <- sf2 %>% group_by(RSSD) %>% mutate(Date = full_seq(c(as.Date('2020-04-03'),as.Date('2020-08-08')), period=1))
  ls <- unique(smartbind(ls,data.frame(att)))
  sf2 <- left_join(sf2,aggregate(cbind(n,InitialApprovalAmount) ~ rssd + DateApproved, pppm, sum), by=c('RSSD'='rssd','Date'='DateApproved'))
  sf2$n <- ifelse(is.na(sf2$n) == TRUE, 0, sf2$n)
  sf2$InitialApprovalAmount <- ifelse(is.na(sf2$InitialApprovalAmount) == TRUE, 0, sf2$InitialApprovalAmount)
  sf2 <- full_join(sf2, 
                   left_join(data.frame(aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + Date.Of.Advance, pplf, sum)), 
                             pplf[,c('Institution.RSSD', 'Date.Of.Advance', 'origin_date')]),
                  by=c('RSSD' = 'Institution.RSSD', 'Date' = 'Date.Of.Advance')) 
  #changing terms to overnights
  
  dwborrow1 <- subset(dwborrow, Loan.date >= as.Date('2020-03-01') & Type.of.credit == 'Primary Credit')
  dwborrow1 <- dwborrow1[order(dwborrow1$Borrower.ABA.number,dwborrow1$Loan.date),]
  dwborrow1$Borrower.ABA.number <- as.numeric(dwborrow1$Borrower.ABA.number)
  dwborrow1$term <- as.numeric(dwborrow1$Repayment.date - dwborrow1$Loan.date)
  
    # Changing everything to 1 term loan
    dwborrow1 <- dwborrow1[rep(1:nrow(dwborrow1), times=dwborrow1$term), ]
    dwborrow1 <- dwborrow1[order(dwborrow1$Borrower.ABA.number,dwborrow1$Loan.date),]
    for (i in 2:nrow(dwborrow1)){
      if (dwborrow1[i,'term'] > 1 & dwborrow1[i,'Borrower.ABA.number'] == dwborrow1[i-1,'Borrower.ABA.number']) {
        dwborrow1[i,'Loan.date'] <- dwborrow1[i-1,'Loan.date']+1
      }
    }
    
  dwborrow1 <- left_join(aggregate(Loan.amount ~ Borrower.ABA.number + Loan.date, dwborrow1, sum), dwborrow1[,c('Loan.date','Borrower.ABA.number','Loan.amount','Borrower.state')])
  
  
  sf2 <- full_join(sf2,dwborrow1[,c('Loan.date','Borrower.ABA.number','Loan.amount','Borrower.state')], by=c('ABA' = 'Borrower.ABA.number', 'Date' = 'Loan.date'))
  
  sf2[is.na(sf2$Original.Outstanding.Advance.Amount),c('Original.Outstanding.Advance.Amount')] <- 0
  sf2[is.na(sf2$Loan.amount),c('Loan.amount')] <- 0

  keep <- c('Date','size','IDRSSD','reserve_asset_ratio','reserve_loan_ratio','RCON2170','RCON0071','RCON0081','RCON2200','eqcaprat','age','borr_total','FED','exposure','OFFICES', 'aq', 'roe','RCONB529',
            'RCFA7204','nonppp_loans')
  temp <- subset(df, as.Date(Date) >= as.Date('2020-03-31'))[,keep]
  temp$Date <- as.Date(temp$Date)
  sf2$Quarter <- as.Date(quarter(sf2$Date, type='date_first')-1)
  sf2 <- left_join(sf2, temp, by=c('RSSD' = 'IDRSSD', 'Quarter' = 'Date'))
  setnames(sf2, old=c('InitialApprovalAmount','Original.Outstanding.Advance.Amount', 'Loan.amount'),
           new = c('PPP','PPPLF','DW'))

  sf2$Reserves <- (sf2$RCON0081 + sf2$RCON0071)*1000
  sf2$Assets <- sf2$RCON2170*1000
  
  sf2$dwsores <- sf2$DW/sf2$Reserves
  sf2$pppsores <- sf2$PPP/sf2$Reserves
  sf2$pppsores <- ifelse(is.na(sf2$pppsores) == TRUE, 0, sf2$pppsores)
  sf2$lfsores <- sf2$PPPLF/sf2$Reserves
  sf2$npplsores <- sf2$nonppp_loans/sf2$Reserves
  
  sf2$dwsoa <- sf2$DW/sf2$RCON2170
  sf2$pppsoa <- sf2$PPP/sf2$RCON2170
  sf2$pppsoa <- ifelse(is.na(sf2$pppsoa) == TRUE, 0, sf2$pppsoa)
  sf2$lfsoa <- sf2$PPPLF/sf2$RCON2170
  
  sf2$dwbin_notest <- ifelse(sf2$dwsores > 0.01, 1,0)
  sf2$dwbin_notest <- ifelse(is.na(sf2$dwbin_notest) == TRUE, 0, sf2$dwbin_notest)
  sf2$dwsores <- ifelse(sf2$dwbin_notest == 1, sf2$dwsores, 0)
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  sf2$levratio <- sf2$RCFA7204
  sf2$bigsmall <- ifelse(sf2$RCON2170 > 600000, 1, 0)
  #sf2$bigsmall <- ifelse(sf2$RCON2170 > 10000000, 2, sf2$bigsmall)
  
  #Creating the PPP active indicator
  sf2$PPPLF_ind <- ifelse(sf2$PPPLF > 0, 1, 0)
  sf2$r_delt <- -sf2$PPPLF + sf2$PPP
  sf2 <- sf2[order(sf2$RSSD,sf2$Date),]
  sf2 <- sf2 %>% group_by(RSSD) %>% mutate(cs_ppp = cumsum(PPP), cs_delt = cumsum(r_delt))
  sf2$month <- month(sf2$Date)
  
  sf2$rsa <- sf2$Reserves/sf2$Assets
  sf2$lsa <- sf2$RCONB529/sf2$RCON2170
  sf2$dsa <- sf2$RCON2200/sf2$RCON2170
  
  temp <- aggregate(cbind(size,reserve_asset_ratio) ~ RSSD, sf2, mean) %>% mutate(size_quint = ntile(size,4), shock_quint = ntile(reserve_asset_ratio,5))
  sf2 <- left_join(sf2, temp[,c('RSSD','size_quint','shock_quint')], by = 'RSSD')
  
  temp <-aggregate(cbind(PPP,DW,PPPLF) ~ RSSD, sf2, sum)
  temp$PPPLF_i <- ifelse(temp$PPPLF > 0, 1, 0)
  temp$DW_i <- ifelse(temp$DW > 0, 1, 0)
  temp$PPP_i <- ifelse(temp$PPP > 0, 1, 0)
  sf2 <- left_join(sf2,temp[,c('RSSD','PPPLF_i','DW_i','PPP_i')])
  sf2$State <- coalesce(sf2$STATE, sf2$Borrower.state)
  
  #Create series on whether bank has accessed the LF within the past month
  t <- aggregate(cbind(PPPLF_ind,PPPLF) ~ RSSD + Date, sf2, sum) %>% group_by(RSSD) %>% arrange(RSSD, Date)
  t <- t %>% group_by(RSSD) %>%
    mutate(LF_30i = rollapplyr(PPPLF_ind, width = 30, FUN = sum, partial = TRUE),
           LF_30 = rollapplyr(PPPLF, width = 30, FUN = sum, partial = TRUE)) %>%
    drop_na()
  t$LF_30i <- ifelse(t$LF_30 > 0, 1, 0)
  sf2 <- left_join(sf2,t[,c('RSSD','Date','LF_30')],by=c('RSSD','Date'))
  sf2$LF_30 <- sf2$LF_30/sf2$Reserves
  sf2$LF_30 <- ifelse(is.na(sf2$LF_30) == TRUE, 0, sf2$LF_30)
  sf2 <- left_join(sf2,binstr,by=c('RSSD'='IDRSSD','Date'))
  sf2$covexpo <- ifelse(sf2$covexpo < 0, 0, sf2$covexpo)
  
  # create series on whether bank has used DW 5 years pre-covid
  dwborrow1 <- subset(dwborrow, Loan.date >= '2015-04-03' & Loan.date < '2020-04-03')
  ls <- intersect(unique(sf2$ABA),unique(dwborrow1$Borrower.ABA.number))
  sf2[sf2$ABA %in% ls, 'precovdw'] <- 1
  sf2$precovdw <- ifelse(is.na(sf2$precovdw) == TRUE, 0, 1)
  
  #merging with the covid data
  # Making the regression dataset
  sf3 <- subset(sf2, Date >= as.Date('2020-04-03') & Date <= as.Date('2020-08-08'))
  sf3 <- sf3[!duplicated(sf3[c("RSSD", "Date")]),]
  sf3 <- sf3 %>% drop_na(RSSD, Date, State, size_quint)
  sf3 <- sf3[!sf3$Reserves == 0,]
  sf3 <- sf3[rowSums(is.na(sf3)) != ncol(sf3), ]
  
  #stargazer(data.frame(sf3[,c('PPP','PPPLF','PPPLF_i','DW','DW_i','reserve_asset_ratio','OFFICES','eqcaprat','precovdw')])) #summary statistic
  dict1 <- c('dw_bin' = 'DW Prob', 'demand_so_reserves' = 'Demand Shock', 'reserve_asset_ratio' = 'RA Ratio',
             'size' = 'Log Assets', 'log(PPPLF+1)' = 'Log(PPPLF)', 'log(RCON0010)' = 'Log(Reserves)',
             'eqcaprat' = 'Equity Cap Ratio', 'pppsores' = 'Demand Shock',
             'pppsores x PPPLF_ind' = 'DS*PPPLF_ind', 'log(borr_total+1)' = 'Log(FF Borrowing)',
             'log(pppsores)' = 'Log(Demand Shock)', 'log(PPP)' = 'Log(PPP)',
             'dwbin_notest' = 'DW Prob', 'log(reserve_asset_ratio)' = 'Log RA Ratio',
             'LF_30' = 'LF_{30}', 'log(eqcaprat)' = 'Log Equity Cap Ratio', 
             'log(dwsores+1)' = 'Log DW Borrowing', 'log(LF_30+1)' ='PPPLF Borr 30D',
             'log(covexpo+1)' = 'COVID Exposure',  'bigsmall' = 'Size', 'log(pppsores+1)' = 'Log PPP Lending',
             'bigsmall::0:log(pppsores+1)' = 'Demand*Small Banks', 'bigsmall::1:log(pppsores+1)' = 'Demand*Medium Banks',
             'bigsmall::2:log(pppsores+1)' = 'Demand*Large Banks', 'log(DW_count+1)' = 'Log DW Borrowing Count')
  

# Regressions ----
    #Table 2: Linear model
      t1 <- list()
      t1[[1]] <- feols(log(dwsores+1) ~ log(pppsores+1)  + i(bigsmall,log(LF_30+1)) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + econexpo + log(covexpo+1) + asinh(eci)| RSSD + Date  , 
                       sf3, panel.id = c('RSSD','Date'))
      t1[[8]] <- update(t1[[1]], . ~. - log(pppsores+1) |.| log(pppsores+1) ~ log(n+1) )
      t1[[3]] <- update(t1[[1]], dwbin_notest ~ . )
      t1[[5]] <- update(t1[[3]], .~. - log(pppsores+1) |.|  log(pppsores+1) ~ log(n+1))
      
      t1[[2]] <- update(t1[[1]], . ~. - log(pppsores+1) + i(bigsmall,log(pppsores+1)))
      t1[[7]] <- update(t1[[2]], . ~. - i(bigsmall,log(pppsores+1)) |.| i(bigsmall,log(pppsores+1)) ~ i(bigsmall,log(n+1)))
      t1[[4]] <- update(t1[[3]], . ~ . - log(pppsores+1) + i(bigsmall,log(pppsores+1)))
      t1[[6]] <- update(t1[[4]], .~. - i(bigsmall,log(pppsores+1)) |.|  i(bigsmall,log(pppsores+1)) ~ i(bigsmall,log(n+1)))
      
      etable(t1, dict=dict1,
             cluster = 'RSSD', tex = F)
      
      # Non-interacted
      etable(t1[[1]],t1[[8]],t1[[3]],t1[[5]], dict= dict1, fitstat = ~n + ar2,
             drop = c('Intercept', 'eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age','OFFICES','rsa','lsa','dsa','npplsores','econexpo','eci'),
             tex = T)
      
      #interacted by large/small, 600m cutoff
      etable(t1[[2]],t1[[7]],t1[[4]],t1[[6]], dict= dict1, fitstat = ~n + ar2,
             drop = c('Intercept', 'eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age','OFFICES','rsa','lsa','dsa','npplsores','econexpo','eci'),
             tex = T)
      
      #t1[[3]] <- update(t1[[2]], .~. - log(ppp_so_reserves) - log(ppp_so_reserves)*log(LF_30+1) |.| log(ppp_so_reserves) ~ BInstr)
      
    #Robustness - Fixed Effects
      t6 <- list()
      t6[[1]] <- update(t1[[length(t1)]], .~. |. - RSSD - Date)
      t6[[2]] <- update(t1[[length(t1)]], .~. |. - Date + month)
      t6[[3]] <- update(t1[[length(t1)]], .~. |. - RSSD + State)
      t6[[4]] <- update(t1[[length(t1)]], .~. |. - Date + month^State)
      etable(t1[[length(t1)]], t6, dict=dict1, se='cluster', tex=F, cluster= 'RSSD',
             drop = c('Intercept','Size', 'eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age','OFFICES','rsa','lsa','dsa','npplsores','econexpo','eci'))
      
    
    # clustering
      t3 <- list()
      t3[[1]] <- update(t1[[6]], se = 'white')
      t3[[2]] <- update(t3[[1]], se = 'cluster', cluster = c('RSSD','Date'))
      t3[[3]] <- update(t3[[1]], se = 'cluster', cluster = c('RSSD','month'))
      t3[[4]] <- update(t3[[1]], se = 'cluster', cluster = 'FED')
      t3[[5]] <- update(t3[[1]], se = 'cluster', cluster = 'State')
      etable(t3, dict=dict1, tex=F,
             drop = c('Intercept','Size', 'eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age','OFFICES','rsa','lsa','dsa','npplsores','econexpo','eci','covexpo'))
      
      
      
# Table 1 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing quantity ----
      ta <- list()
      ta[[1]] <- feols(log(dwsores+1) ~ log(pppsores+1)| RSSD + Date  , 
                       sf3, panel.id = c('RSSD','Date'))
      ta[[2]] <- update(ta[[1]], . ~. + log(LF_30+1) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + asinh(econexpo) + asinh(covexpo) + asinh(eci) )
      ta[[3]] <- update(ta[[1]], subset = sf3$bigsmall == 1)
      ta[[4]] <- update(ta[[2]], subset = sf3$bigsmall == 1)
      ta[[5]] <- update(ta[[1]], subset = sf3$bigsmall == 0)
      ta[[6]] <- update(ta[[2]], subset = sf3$bigsmall == 0)
      etable(ta, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = NA,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = FALSE)
      
      
# Table 2 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing probability ----
      ta2 <- list()
      ta2[[1]] <- feols(dwbin_notest ~ log(pppsores+1)| RSSD + Date  , 
                       sf3, panel.id = c('RSSD','Date'))
      ta2[[2]] <- update(ta2[[1]], . ~. + log(LF_30+1) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + asinh(econexpo) + asinh(covexpo) + asinh(eci) )
      ta2[[3]] <- update(ta2[[1]], subset = sf3$bigsmall == 1)
      ta2[[4]] <- update(ta2[[2]], subset = sf3$bigsmall == 1)
      ta2[[5]] <- update(ta2[[1]], subset = sf3$bigsmall == 0)
      ta2[[6]] <- update(ta2[[2]], subset = sf3$bigsmall == 0)
      etable(ta2, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = NA,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = FALSE)
      
      
      
# Table 3 - IV Results (Pooled, Large, small) with controls using the daily data ----
      ta6 <- list()
      ta6[[1]] <- update(ta[[2]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      ta6[[2]] <- update(ta[[4]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      ta6[[3]] <- update(ta[[6]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      ta6[[4]] <- update(ta2[[2]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      ta6[[5]] <- update(ta2[[4]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      ta6[[6]] <- update(ta2[[6]], .~. - log(pppsores+1) | . | log(pppsores+1) ~ log(n+1))
      etable(ta6, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = NA,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = FALSE)
      
      
# Cross Sectional Data Creation - Aggregating all daily info during covid to a cross section ----
sf4 <- aggregate(cbind(PPP, PPPLF, DW, eci, econexpo, covexpo, dwbin_notest, PPPLF_i) ~ RSSD, sf3, sum)
setnames(sf4, old = c('dwbin_notest','PPPLF_i'), new = c('DW_count','LF_count'))
sf4 <- left_join(sf4, aggregate(cbind(PPPLF_i,DW_i,OFFICES,Reserves, rsa, lsa, dsa, precovdw) ~ RSSD, sf3, mean), by='RSSD')
temp <- subset(df, Date == as.Date('2019-12-31'))
temp$levrat <- temp$RCOA8274/temp$RCOAA224
sf4 <- left_join(sf4, data.frame(temp[,c('IDRSSD','RCON2170','RCON5563','RCON5571','RCON5573','RCON5575','RCON1766','FED','levrat')]), by=c('RSSD' = 'IDRSSD'))
sf4$sb_cis <- (sf4$RCON5571+sf4$RCON5573+sf4$RCON5575)/sf4$RCON1766
sf4$bigsmall <- ifelse(sf4$RCON2170 > 600000,1,0)
sf4$pppsoa <- sf4$PPP/sf4$RCON2170
sf4$dwsoa <- sf4$DW/sf4$RCON2170
sf4$lfsoa <- sf4$PPPLF/sf4$RCON2170
list <- 
[,c('pppsores','dwsores','lfsores','eci','econexpo','covexpo','rsa','lsa','dsa','OFFICES','sb_cis')] <- lapply(sf4[,c('pppsores','dwsores','lfsores','eci','econexpo','covexpo','rsa','lsa','dsa','OFFICES','sb_cis')], Winsorize, probs=c(.01,.99), na.rm = TRUE)

# Table 4 - OLS results (Pooled, Large, small) for DW borrowing sizes (aggregate) ----
ta3 <- list()  
ta3[[1]] <- feols(log(dwsores+1) ~ log(pppsores+1) | FED , sf4)
ta3[[2]] <- update(ta3[[1]], .~. + log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + levrat + rsa + lsa + dsa + precovdw )
ta3[[3]] <- update(ta3[[1]], subset = sf4$bigsmall == 1)
ta3[[4]] <- update(ta3[[2]], subset = sf4$bigsmall == 1)
ta3[[5]] <- update(ta3[[1]], subset = sf4$bigsmall == 0)
ta3[[6]] <- update(ta3[[2]], subset = sf4$bigsmall == 0)

etable(ta3, dict = dict1,
       title = 'OLS Estimation of Log DW Borrowing',
       se = 'white',
       label = 'aggregate_quant_reg',
       headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
       group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
       fitstat = ~n + r2,
       tex = FALSE)

# Table 5 - OLS results (Pooled, Large, small) for DW borrowing quantity (aggregate) ----
ta4 <- list()  
ta4[[1]] <- feols(log(DW_count+1) ~ log(pppsores+1) | FED, sf4)
ta4[[2]] <- update(ta4[[1]], .~. + log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + levrat + rsa + lsa + dsa + precovdw)
ta4[[3]] <- update(ta4[[1]], subset = sf4$bigsmall == 1)
ta4[[4]] <- update(ta4[[2]], subset = sf4$bigsmall == 1)
ta4[[5]] <- update(ta4[[1]], subset = sf4$bigsmall == 0)
ta4[[6]] <- update(ta4[[2]], subset = sf4$bigsmall == 0)

etable(ta4, dict = dict1,
       title = 'OLS Estimation of Log DW Borrowing',
       se = 'white',
       label = 'aggregate_quant_reg',
       headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
       group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
       fitstat = ~n + r2,
       tex = FALSE)

# Table 6 - IV Result for DW Borrowing Quantity ----
ta5 <- list() 
sf4$instr <- sf4$sb_cis*log(sf4$RCON2170)

ta5[[1]] <- feols(log(dwsoa+1) ~ log(lfsoa+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + levrat + rsa + lsa + dsa + precovdw | FED| log(pppsoa+1) ~ sb_cis, sf4)
ta5[[2]] <- update(ta5[[1]], subset = sf4$bigsmall == 1)
ta5[[3]] <- update(ta5[[1]], subset = sf4$bigsmall == 0)
ta5[[4]] <- update(ta5[[1]], .~.|.|  i(bigsmall,log(pppsoa+1)) ~ i(bigsmall,sb_cis))

ta5[[4]] <- feols(log(DW_count+1) ~ log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + levrat + rsa + lsa + dsa + precovdw |  FED|log(pppsores+1) ~ sb_cis, sf4)
ta5[[5]] <- update(ta5[[4]], subset = sf4$bigsmall == 1)
ta5[[6]] <- update(ta5[[4]], subset = sf4$bigsmall == 0)

etable(ta5, dict = dict1,
       title = 'OLS Estimation of Log DW Borrowing',
       se = 'white',
       label = 'aggregate_quant_reg',
       headers = c('Pooled','Large Banks','Small Banks'),
       group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','precovdw','covexpo','OFFICES','levrat')),
       fitstat = ~ n + f,
       tex = FALSE)
