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
            'RCFA7204','nonppp_loans','RCOA8274','RCOAA224','RCONJ457','RCON5571','RCON5573','RCON5575','RCON2215','RCON2210','RCON1766','RCONJJ34','RCON1773','RCONJA22')
  temp <- subset(df, as.Date(Date) >= as.Date('2020-03-31'))[,keep]
  temp$Date <- as.Date(temp$Date)
  sf2$Quarter <- as.Date(quarter(sf2$Date, type='date_first')-1)
  sf2 <- left_join(sf2, temp, by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
  setnames(sf2, old=c('InitialApprovalAmount','Original.Outstanding.Advance.Amount', 'Loan.amount'),
           new = c('PPP','PPPLF','DW'))

  sf2$Reserves <- (sf2$RCON0081 + sf2$RCON0071)*1000
  sf2$Assets <- sf2$RCON2170*1000
  
  # From the  Li Strahan 2020 paper 
    # Relationship measures
    sf2$ci_com <- sf2$RCONJ457/sf2$RCON2170
    sf2$scisoa  <- (sf2$RCON5571+sf2$RCON5573+sf2$RCON5575)/sf2$RCON2170
    sf2$cdep   <- (sf2$RCON2215+sf2$RCON2210)/sf2$RCON2170
    
    #Balance sheet ratio
    sf2$cisoa <- sf2$RCON1766/sf2$RCON2170
    sf2$levrat <- sf2$RCOA8274/sf2$RCOAA224
    sf2$liqass <- rowSums(cbind(sf2$RCON0071,sf2$RCON0081,sf2$RCONJJ34,sf2$RCON1773,sf2$RCONJA22),na.rm=TRUE)/sf2$RCON2170
    
    sf2$scisoa <- ifelse(is.na(sf2$scisoa) == TRUE, 0, sf2$scisoa)
    
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
  sf2$ppp_ind <- ifelse(sf2$PPP > 0, 1, 0)
  sf2$bigsmall <- ifelse(sf2$RCON2170 > 600000, 1, 0)

  sf2$dwsores <- sf2$dwsores*100
  sf2$pppsores <- sf2$pppsores*100
  #sf2$bigsmall <- ifelse(sf2$RCON2170 > 10000000, 2, sf2$bigsmall)
  
  #Creating the PPP active indicator
  sf2$PPPLF_ind <- ifelse(sf2$PPPLF > 0, 1, 0)
  sf2$r_delt <- -sf2$PPPLF + sf2$PPP
  sf2 <- sf2[order(sf2$RSSD,sf2$Date),]
  sf2 <- sf2 %>% group_by(RSSD) %>% mutate(cs_ppp = cumsum(PPP), cs_delt = cumsum(r_delt))
  sf2$month <- month(sf2$Date)
  sf2$week <- as.numeric(floor(1 + (sf2$Date - as.Date('2020-04-03'))/7))
  
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
  t <- aggregate(PPPLF ~ RSSD + Date, sf2, sum) %>% group_by(RSSD) %>% arrange(RSSD, Date)
  t <- t %>% group_by(RSSD) %>% mutate(LF_30 = rollapplyr(PPPLF, width = 30, FUN = sum, partial = TRUE)) 
  sf2 <- left_join(sf2,t[,c('RSSD','Date','LF_30')],by=c('RSSD','Date'))
  sf2$LF_30 <- sf2$LF_30*100/sf2$Reserves
  sf2$LF_30 <- ifelse(is.na(sf2$LF_30) == TRUE, 0, sf2$LF_30)
  
  # Joining all the deposit weighted data
  sf2 <- left_join(sf2,binstr,by=c('RSSD'='IDRSSD','Date'))
  sf2$covexpo <- ifelse(sf2$covexpo < 0, 0, sf2$covexpo)
  sf2$eci <- abs(sf2$eci)
  
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
  sf3 <- subset(sf3, PPP_i == 1)
  
  list <- c('pppsores','dwsores','LF_30','eci','eqcaprat','covexpo','rsa','lsa','dsa','reserve_asset_ratio','ci_com','scisoa','cdep','cisoa','liqass','levrat','size','dwage')
  sf3[,list] <- lapply(sf3[,list], Winsorize, probs=c(.01,.99), na.rm = TRUE)
  
  list <- c('LF_30','log(eci)','eqcaprat','covexpo','rsa','lsa','dsa','ci_com','scisoa','cdep','cisoa','liqass','levrat','size','dwage')
    #stargazer(data.frame(sf3[,c('PPP','PPPLF','PPPLF_i','DW','DW_i','reserve_asset_ratio','OFFICES','eqcaprat','precovdw')])) #summary statistic
  dict1 <- c('dw_bin' = 'DW Prob', 'dwsores' = 'DW Borrowing/Reserves', 'reserve_asset_ratio' = 'RA Ratio',
             'size' = 'Log Assets', 'log(PPPLF+1)' = 'Log(PPPLF)', 'log(RCON0010)' = 'Log(Reserves)',
             'eqcaprat' = 'Equity Cap Ratio', 'pppsores' = 'PPP Lending/Reserves',
             'pppsores x PPPLF_ind' = 'DS*PPPLF_ind', 'log(borr_total+1)' = 'Log(FF Borrowing)',
             'log(pppsores)' = 'Log PPP share of Reserves', 'log(PPP)' = 'Log(PPP)',
             'dwbin_notest' = 'DW Prob', 'log(reserve_asset_ratio)' = 'Log RA Ratio',
             'log(LF_30)' = 'Log Borrowing from LF in last 30 Days', 'log(eqcaprat)' = 'Log Equity Cap Ratio',
             'log(LF_30+1)' = 'Log Borrowing from LF in last 30 Days',
             'log(dwsores+1)' = 'Log DW Borrowing', 
             'log(covexpo)' = 'COVID Exposure',  'bigsmall' = 'Size', 'log(pppsores+1)' = 'Log PPP Lending',
             'bigsmall::0:log(pppsores+1)' = 'Demand*Small Banks', 'bigsmall::1:log(pppsores+1)' = 'Demand*Medium Banks',
             'bigsmall::2:log(pppsores+1)' = 'Demand*Large Banks', 'log(DW_count+1)' = 'Log DW Borrowing Count', 'RSSD' = 'Bank',
             'week' = 'Week', 'log(lsa)' = 'Log loan share of assets', 'log(dsa)' = 'Log deposit share of assets', 'log(rsa)' = 'Log reserve share of assets',
             'log(eci)' = 'Economic exposure from branches', 'ci_com' = 'Unused CI comm/Assets', 'scisoa' = 'Small C&I loans/Assets',
             'cdep' = 'Core deposits/Assets', 'cisoa' = 'C&I loans/Assets', 'liqass' = 'Liquid assets/Assets', 'levrat' = 'Tier 1 leverage ratio',
             'log(dwage)' = 'log(Deposit-weighted average branch age)', 'covexpo' = 'Deposit-weighted new COVID rate',
             'LF_30' = 'Borrowing from LF in last 30 Days', 'precovdw' = "Indicator if bank has used DW within 5 years pre-COVID")
  

# Table 1 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing quantity  SD - (log(7.871206)/log(1.01))---- 
      ta <- list()
      ta[[1]] <- feols(dwsores ~ pppsores|FED + Date, 
                       sf3, 
                       #subset = sf3$PPP>0 & sf3$DW > 0
                       panel.id = c('RSSD','Date'))
      ta[[2]] <- update(ta[[1]], . ~ .
                        + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
      ta[[3]] <- update(ta[[1]], subset = sf3$bigsmall == 1)
      ta[[4]] <- update(ta[[2]], subset =  sf3$bigsmall == 1)
      ta[[5]] <- update(ta[[1]], subset =  sf3$bigsmall == 0)
      ta[[6]] <- update(ta[[2]], subset =  sf3$bigsmall == 0)
      etable(ta, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing. Columns correspond to the same shares as Table 1. ',
             label = 'main_reg',
             headers = NA,
             digits = 5,
             fitstat = ~n + r2,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             #extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = T)
      
      
# Table 2 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing probability - LPM ----
      ta2 <- list()
      ta2[[1]] <- feols(dwbin_notest ~ pppsores| FED + Date,
                       #family = binomial(link = "logit"),
                       data = sf3, panel.id = c('RSSD','Date'))
      ta2[[2]] <- update(ta2[[1]], . ~. 
                         + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
      ta2[[3]] <- update(ta2[[1]], subset = sf3$bigsmall == 1)
      ta2[[4]] <- update(ta2[[2]], subset = sf3$bigsmall == 1)
      ta2[[5]] <- update(ta2[[1]], subset = sf3$bigsmall == 0)
      ta2[[6]] <- update(ta2[[2]], subset = sf3$bigsmall == 0)
      etable(ta2, dict = dict1,
             title = 'Linear probability model estimation of DW borrowing probability. Column (1-2) is the pooled sample with and without controls, column (3-4) is for large banks that have assets greater than $600M, and columns (5-6) is for small community banks. The results from column (2) implies that a 10 percentage point increase in the PPP quantity loaned as a share of the reserves increases probability to borrow from the discount window by 1.6 percentage points. There seems to be a greater effect for large banks when shocked as compared to small banks.',
             label = 'main_reg',
             headers = NA,
             digits = 4,
             fitstat = ~n + r2,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = F)
      
# Table 7 - DW borrowing probability - Logistic Model ----
      ta7 <- list()
      ta7[[1]] <- feglm(dwbin_notest ~ pppsores| FED + Date,
                        family = binomial(link = "logit"),
                        data = sf3, panel.id = c('RSSD','Date'))
      ta7[[2]] <- update(ta7[[1]], . ~. 
                         + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
      ta7[[3]] <- update(ta7[[1]], subset = sf3$bigsmall == 1)
      ta7[[4]] <- update(ta7[[2]], subset = sf3$bigsmall == 1)
      ta7[[5]] <- update(ta7[[1]], subset = sf3$bigsmall == 0)
      ta7[[6]] <- update(ta7[[2]], subset = sf3$bigsmall == 0)
      etable(ta7, dict = dict1,
             title = 'Logistic Estimation of borrowing probability from the discount window. Column (1-2) is the pooled sample with and without controls, column (3-4) is for large banks that have assets greater than $600M, and columns (5-6) is for small community banks. The results from column (2) implies that a 10 percentage point increase in the PPP quantity loaned increases probability to borrow from the discount window by 28.1%. Since the predicted mean is 1.9% to borrow from the window, a 28.1% increase imples an increase of .53 percentage points.',
             label = 'main_reg',
             headers = NA,
             fitstat = ~n + pr2,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = T)
      
      
      
      
# Table 3 - IV Results (Pooled, Large, small) with controls using the daily data ----
      ta6 <- list()
      ta6[[1]] <- update(ta[[2]], .~. - pppsores | . | pppsores ~ l(pppsores,11:14))
      ta6[[2]] <- update(ta6[[1]], subset = sf3$bigsmall == 1)
      ta6[[3]] <- update(ta6[[1]], subset = sf3$bigsmall == 0)
      ta6[[4]] <- update(ta2[[2]], .~. - pppsores | . | pppsores ~ l(pppsores,1))
      ta6[[5]] <- update(ta6[[4]], subset = sf3$bigsmall == 1)
      ta6[[6]] <- update(ta6[[4]], subset = sf3$bigsmall == 0)
      etable(ta6, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = c('Pooled', 'Large Banks','Small Banks'),
             fitstat = ~ n + ivf,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores')),
             tex = FALSE)
      
# Robustness ----
      # Table 1 Robustness - Discount Window borrowing quantity conditional on there being an instance of DW borrowing.
      tar <- list()
      tar[[1]] <- feols(dwbin_notest ~ pppsores| RSSD + Date, 
                       sf3, panel.id = c('RSSD','Date'))
      tar[[2]] <- update(tar[[1]], . ~. 
                         + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
      tar[[3]] <- update(tar[[2]], .~. |. - Date + week)
      tar[[4]] <- update(tar[[2]], .~. |. - Date + month)
      tar[[6]] <- update(tar[[2]], .~. |. - RSSD + FED)
      etable(tar, dict = dict1,
             title = 'Robustness using different versions of fixed effect on the pooled sample.',
             label = 'main_reg',
             headers = NA,
             digits = 5,
             fitstat = ~n + r2 + ivf ,
             group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             #extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = T)
      
      
      
# Result Interpretation: ----
      temp <- colMeans(sf3[,c('pppsores','LF_30','reserve_asset_ratio','size','eqcaprat','rsa','dsa','lsa','covexpo','eci','dwage')],na.rm=TRUE)
      temp <- c(temp, 'RSSD' = 208655, 'FED' = 10, 'week' = 2)
      temp <- data.frame(as.list(temp))
      temp$Date <- as.Date('2020-04-05')
      tn <- temp; tn$pppsores = tn$pppsores + sd(sf3$pppsores)
      
      as.numeric(ta6[[1]]$coefficients[1])*log((mean(sf3$pppsores)+sd(sf3$pppsores))/mean(sf3$pppsores))/log(1.01)
      as.numeric(ta6[[4]]$coefficients[1])*log((mean(sf3$pppsores)+sd(sf3$pppsores))/mean(sf3$pppsores))/log(1.01)
      
      exp(predict(ta6[[1]], tn))/exp(predict(ta6[[1]],temp))
      
      temp <- sf3
      temp$pppsores <- temp$pppsores + sd(temp$pppsores)
      predict(ta7[[4]], tn)/median(predict(ta7[[4]]))

# Cross Sectional Data Creation - Aggregating all daily info during covid to a cross section ----
sf4 <- aggregate(cbind(PPP, PPPLF, DW, eci, covexpo, dwbin_notest, PPPLF_i, ppp_ind) ~ RSSD, sf3, sum)
setnames(sf4, old = c('dwbin_notest','PPPLF_i'), new = c('DW_count','LF_count'))
sf4 <- left_join(sf4, aggregate(cbind(PPPLF_i,DW_i,OFFICES,Reserves, rsa, lsa, dsa, precovdw, dwage, eqcaprat, epppn) ~ RSSD, sf3, mean), by='RSSD')

temp <- subset(df, Date == as.Date('2019-12-31'))
temp$levrat <- temp$RCOA8274/temp$RCOAA224
temp$ci_com <- temp$RCONJ457/temp$RCON2170
temp$scisoa  <- (temp$RCON5571+temp$RCON5573+temp$RCON5575)/temp$RCON2170
temp$cdep   <- (temp$RCON2215+temp$RCON2210)/temp$RCON2170
temp$cisoa <- temp$RCON1766/temp$RCON2170
temp$levrat <- temp$RCOA8274/temp$RCOAA224
temp$liqass <- rowSums(cbind(temp$RCON0071,temp$RCON0081,temp$RCONJJ34,temp$RCON1773,temp$RCONJA22),na.rm=TRUE)/temp$RCON2170
attach(temp, warn.conflicts = F); temp$instr2 <- rowSums(cbind(RCON5571,RCON5573,RCON5579,RCON5581,RCON5585,RCON5587,RCON5575,RCON5583,RCON5589),na.rm = TRUE)/(RCONB529+RCON5369)
list <- c('IDRSSD','RCON2170','RCON5563','RCON5571','RCON5573','RCON5575','RCON1766','FED','levrat','Primary.ABA.Routing.Number','RCON0081','RCON0071','instr2',
          'ci_com','scisoa','cdep','cisoa','liqass')
sf4 <- left_join(sf4, data.frame(temp[,list]), by=c('RSSD' = 'IDRSSD'))

temp <- subset(dwborrow, Loan.date <= as.Date('2020-03-01')) %>% group_by(Borrower.ABA.number) %>% count()
temp$Borrower.ABA.number <- as.numeric(temp$Borrower.ABA.number)
sf4 <- left_join(sf4, temp, by=c('Primary.ABA.Routing.Number' = 'Borrower.ABA.number'))
sf4$n <- ifelse(is.na(sf4$n) == TRUE, 0, sf4$n)
sf4$sb_cis <- (sf4$RCON5571+sf4$RCON5573+sf4$RCON5575)/sf4$RCON1766
sf4$bigsmall <- ifelse(sf4$RCON2170 > 600000,1,0)
sf4$Reserves <- 1000*(sf4$RCON0071 + sf4$RCON0081)
sf4$size <- log(sf4$RCON2170)
sf4$pppsores <- sf4$PPP*100/sf4$Reserves
sf4$dwsores <- sf4$DW*100/sf4$Reserves
sf4$lfsores <- sf4$PPPLF*100/sf4$Reserves
list <- c('pppsores','dwsores','lfsores','eci','covexpo','rsa','lsa','dsa','OFFICES','sb_cis','n','levrat','instr2',
          'ci_com','scisoa','cdep','cisoa','liqass')
sf4[,list] <- lapply(sf4[,list], Winsorize, probs=c(.01,.99), na.rm = TRUE)

# Table 4 - OLS results (Pooled, Large, small) for DW borrowing sizes (aggregate) ----
ta3 <- list()  
ta3[[1]] <- feols(dwsores ~ pppsores +
                    + lfsores + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo| FED , sf4)
ta3[[2]] <- update(ta3[[1]], subset = sf4$bigsmall == 1)
ta3[[3]] <- update(ta3[[1]], subset = sf4$bigsmall == 0)
ta3[[4]] <- update(ta3[[1]], log(DW_count+.01) ~.)
ta3[[5]] <- update(ta3[[4]], subset = sf4$bigsmall == 1)
ta3[[6]] <- update(ta3[[4]], subset = sf4$bigsmall == 0)

etable(ta3, dict = dict1,
       title = 'OLS Estimation of Log DW Borrowing',
       se = 'white',
       label = 'aggregate_quant_reg',
       headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
       group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
       fitstat = ~n + r2,
       tex = FALSE)

# Table 6 - IV Result for DW Borrowing Quantity ----
  ta5 <- list() 
  ta5[[1]] <- feols(dwsores ~ lfsores + precovdw + log(OFFICES) + rsa + eqcaprat + eci +  ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo | FED| 
                      pppsores ~ insrt2*size , sf4)
  ta5[[2]] <- update(ta5[[1]], subset = sf4$bigsmall == 1)
  ta5[[3]] <- update(ta5[[1]], subset = sf4$bigsmall == 0)
  ta5[[4]] <- update(ta5[[1]], log(DW_count+1) ~.)
  ta5[[5]] <- update(ta5[[4]], subset = sf4$bigsmall == 1)
  ta5[[6]] <- update(ta5[[4]], subset = sf4$bigsmall == 0)
  
  etable(ta5, dict = dict1,
         title = 'OLS Estimation of Log DW Borrowing',
         se = 'white',
         label = 'aggregate_quant_reg',
         headers = c('Pooled','Large Banks','Small Banks'),
         group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
         fitstat = ~ n + ivf + sargan,
         tex = FALSE)
