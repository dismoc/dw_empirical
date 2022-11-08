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

# Import ----
ppb <- read_csv("D:/Research/DW lending empirical/Data/ppp_daily.csv")
df <- read_csv("D:/Research/DW lending empirical/Data/merged_cov.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
pplf <- read_csv("D:/Research/DW lending empirical/Data/ppplf_full.csv")
pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")
att <- read_csv("D:/Research/DW lending empirical/Data/ffiec/Atrributes_merged.csv")[,c('ID_ABA_PRIM','#ID_RSSD','STATE_ABBR_NM')]
nf1 <- read_csv("D:/Research/DW lending empirical/Data/binstr.csv")

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
  temp <- pppm %>% group_by(rssd, DateApproved) %>% summarise(n = sum(n, na.rm=T), InitialApprovalAmount = sum(InitialApprovalAmount, na.rm=T))
  sf2 <- left_join(sf2, temp, by=c('RSSD'='rssd','Date'='DateApproved'))
  sf2$n <- ifelse(is.na(sf2$n) == TRUE, 0, sf2$n)
  sf2$InitialApprovalAmount <- ifelse(is.na(sf2$InitialApprovalAmount) == TRUE, 0, sf2$InitialApprovalAmount)
  sf2 <- left_join(sf2, aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + Date.Of.Advance, pplf, sum),
                  by=c('RSSD' = 'Institution.RSSD', 'Date' = 'Date.Of.Advance'))
  setnames(sf2, old='Original.Outstanding.Advance.Amount','LF_received')
  
  temp <- aggregate(Original.Outstanding.Advance.Amount ~ Institution.RSSD + origin_date, pplf, sum)
  temp <- left_join(temp, aggregate(processing_time ~ Institution.RSSD + origin_date, pplf, mean))
  temp <- subset(temp, origin_date >= as.Date('2020-04-03'))
  sf2 <- left_join(sf2, temp, by=c('RSSD' = 'Institution.RSSD', 'Date' = 'origin_date'))
  setnames(sf2, old='Original.Outstanding.Advance.Amount','LF_requested')
  
  #changing terms to overnights
  dwborrow1 <- subset(dwborrow, Loan.date >= as.Date('2020-04-03') & Type.of.credit == 'Primary Credit')
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
    
  dwborrow1 <- left_join(aggregate(Loan.amount ~ Borrower.ABA.number + Loan.date, dwborrow1, sum), dwborrow1[,c('Loan.date','Borrower.ABA.number','Loan.amount','Borrower.state','term')])
  sf2 <- full_join(sf2,dwborrow1[,c('Loan.date','Borrower.ABA.number','Loan.amount','Borrower.state','term')], by=c('ABA' = 'Borrower.ABA.number', 'Date' = 'Loan.date'))
  
  # making DW larger as term*DW borrowing
  dwborrow1 <- subset(dwborrow, Loan.date >= as.Date('2020-04-03') & Type.of.credit == 'Primary Credit')
  dwborrow1 <- dwborrow1[order(dwborrow1$Borrower.ABA.number,dwborrow1$Loan.date),]
  dwborrow1$Borrower.ABA.number <- as.numeric(dwborrow1$Borrower.ABA.number)
  dwborrow1$term <- as.numeric(dwborrow1$Repayment.date - dwborrow1$Loan.date)
  dwborrow1$DW_m1 <- dwborrow1$Loan.amount
  dwborrow1$DW_m2 <- dwborrow1$term*dwborrow1$Loan.amount
  sf2 <- left_join(sf2, dwborrow1[,c('Borrower.ABA.number','Loan.date','DW_m1','DW_m2')], by=c('ABA' = 'Borrower.ABA.number', 'Date' = 'Loan.date'))
  
  
  sf2[is.na(sf2$LF_received),c('LF_received')] <- 0
  sf2[is.na(sf2$LF_requested),c('LF_requested')] <- 0
  sf2[is.na(sf2$Loan.amount),c('Loan.amount')] <- 0
  sf2[is.na(sf2$DW_m2),c('DW_m2')] <- 0
  
  
  keep <- c('Date','size','IDRSSD','reserve_loan_ratio','RCON2170','RCON0071','RCON0081','RCON2200','eqcaprat','age','borr_total','FED','exposure','OFFICES', 'aq', 'roe','RCONB529',
            'RCFA7204','nonppp_loans','RCOA8274','RCOAA224','RCONJ457','RCON5571','RCON5573','RCON5575','RCON2215','RCON2210','RCON1766','RCONJJ34','RCON1773','RCONJA22','RIAD4135','RIAD4093','RCON2122')
  temp <- subset(df, as.Date(Date) >= as.Date('2020-03-31'))[,keep]
  temp$Date <- as.Date(temp$Date)
  sf2$Quarter <- as.Date(quarter(sf2$Date, type='date_first')-1)
  sf2 <- left_join(sf2, temp, by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
  setnames(sf2, old=c('InitialApprovalAmount', 'Loan.amount'),
           new = c('PPP','DW'))

  sf2$Reserves <- (sf2$RCON0081 + sf2$RCON0071)*1000
  sf2$Assets <- sf2$RCON2170*1000
  sf2$OFFICES <- ifelse(sf2$OFFICES == 0,1,sf2$OFFICES)
  sf2$empps <- sf2$RIAD4135/sf2$RIAD4093
  
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
  sf2$dw2sores <- sf2$DW_m2/sf2$Reserves
  sf2$pppsores <- sf2$PPP/sf2$Reserves
  sf2$pppsores <- ifelse(is.na(sf2$pppsores) == TRUE, 0, sf2$pppsores)
  sf2$lfsores <- sf2$LF_received/sf2$Reserves
  sf2$lf_req_sores <- sf2$LF_requested/sf2$Reserves
  sf2$npplsores <- sf2$nonppp_loans/sf2$Reserves
  
  sf2$dwsoa <- sf2$DW/sf2$Assets
  sf2$pppsoa <- sf2$PPP/sf2$Assets
  sf2$pppsoa <- ifelse(is.na(sf2$pppsoa) == TRUE, 0, sf2$pppsoa)
  sf2$lfsoa <- sf2$LF_received/sf2$Assets
  
  sf2$dwbin_notest <- ifelse(sf2$DW > 100000, 1,0)
  sf2$dwbin_notest <- ifelse(is.na(sf2$dwbin_notest) == TRUE, 0, sf2$dwbin_notest)
  sf2$dwsores <- ifelse(sf2$dwbin_notest == 1, sf2$dwsores, 0)
  sf2$dwbin_notest2 <- ifelse(sf2$DW_m2 > 100000, 1,0)
  sf2$dwbin_notest2 <- ifelse(is.na(sf2$dwbin_notest2) == TRUE, 0, sf2$dwbin_notest2)
  sf2$dw2sores <- ifelse(sf2$dwbin_notest2 == 1, sf2$dw2sores, 0)
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  
  sf2$ppp_ind <- ifelse(sf2$PPP > 0, 1, 0)
  sf2$bigsmall <- ifelse(sf2$RCON2170 > 600000, 1, 0)

  #Creating the PPP active indicator
  sf2$LF_rec_ind <- ifelse(sf2$LF_received > 0, 1, 0)
  sf2$LF_req_ind <- ifelse(sf2$LF_requested > 0, 1, 0)
  
  sf2$r_delt <- -sf2$LF_received + sf2$PPP
  sf2 <- sf2[order(sf2$RSSD,sf2$Date),]
  sf2 <- sf2 %>% group_by(RSSD) %>% mutate(cs_ppp = cumsum(PPP), cs_delt = cumsum(r_delt))
  
  #Date FE stuff
  sf2$month <- month(sf2$Date)
  sf2$week <- as.numeric(floor(1 + (sf2$Date - as.Date('2020-04-03'))/7))
  sf2$phase <- ifelse(sf2$Date > as.Date('2020-04-16') & sf2$Date < as.Date('2020-04-27'), 0, 1)
  sf2$phase <- ifelse(sf2$Date <= as.Date('2020-04-16'), 1, sf2$phase)
  sf2$phase <- ifelse(sf2$Date >= as.Date('2020-04-27'), 2, sf2$phase)
  
  sf2$rsa <- sf2$Reserves/sf2$Assets
  sf2$lsa <- sf2$RCON2122/sf2$RCON2170
  sf2$dsa <- sf2$RCON2200/sf2$RCON2170

  sf2$State <- coalesce(sf2$STATE, sf2$Borrower.state)
  
  #Create series on whether bank has accessed the LF within the past month
  t <- sf2[,c('RSSD','Date','LF_received','LF_requested')] %>%
    group_by(RSSD) %>% mutate(LF_30 = rollapplyr(LF_received, width = 30, FUN = sum, partial = TRUE),
                              LF_pre = rollapplyr(LF_requested, width = 130, FUN = sum, partial = TRUE)) 
  t$LF_pre <- ifelse(t$LF_pre > 0, 1, 0)
  sf2 <- left_join(sf2,t[,c('RSSD','Date','LF_30','LF_pre')],by=c('RSSD','Date'))
  sf2$LF_30 <- sf2$LF_30/sf2$Reserves
  sf2$LF_30 <- ifelse(is.na(sf2$LF_30) == TRUE, 0, sf2$LF_30)
  
  # Joining all the deposit weighted data
  sf2 <- left_join(sf2,nf1,by=c('RSSD'='IDRSSD','Date'))
  sf2$covexpo <- ifelse(sf2$covexpo < 0, 0, sf2$covexpo)
  sf2$eci <- sf2$eci
  sf2$c_instr <- ifelse(is.na(sf2$c_instr) == TRUE, 0, sf2$c_instr)
  
  # create series on whether bank has used DW 5 years pre-covid
  dwborrow1 <- subset(dwborrow, Loan.date >= '2015-01-01' & Loan.date < '2020-04-03')
  ls <- intersect(unique(sf2$ABA),unique(dwborrow1$Borrower.ABA.number))
  sf2[sf2$ABA %in% ls, 'precovdw'] <- 1
  sf2$precovdw <- ifelse(is.na(sf2$precovdw) == TRUE, 0, 1)
  
# Making the regression dataset ----
  sf3 <- subset(sf2, Date >= as.Date('2020-04-03') & Date < as.Date('2020-06-01'))
  sf3 <- subset(sf3, Date <= as.Date('2020-04-16') | Date >= as.Date('2020-04-27'))
  sf3 <- sf3[!duplicated(sf3[c("RSSD", "Date")]),]
  sf3 <- sf3 %>% drop_na(RSSD, Date, State, size_quint, FED)
  sf3 <- sf3[!sf3$Reserves == 0,]
  sf3 <- sf3[rowSums(is.na(sf3)) != ncol(sf3), ]
  sf3 <- sf3 %>% group_by(RSSD) %>% arrange(RSSD, Date) %>% mutate(c_instrl = dplyr::lag(c_instr), na.rm = TRUE)
  sf3 <- subset(sf3, PPP_i == 1)
  
  temp <-aggregate(cbind(pppsores,dwsores,lfsores) ~ RSSD, sf3, sum)
  temp$LF_i <- ifelse(temp$lfsores > 0, 1, 0)
  temp$DW_i <- ifelse(temp$dwsores > 0, 1, 0)
  temp$PPP_i <- ifelse(temp$pppsores > 0, 1, 0)
  sf3 <- left_join(sf3,temp[,c('RSSD','LF_i','DW_i','PPP_i')])
  
  list <- c('pppsores','dwsores','LF_30','eci','eqcaprat','covexpo','rsa','lsa','dsa','reserve_asset_ratio','empps','ci_com','scisoa','cdep','cisoa','liqass','levrat','dwage')
  sf3[,list] <- lapply(sf3[,list], Winsorize, probs=c(.001,.999), na.rm = TRUE)
  sft <- subset(sf3, DW > 0.0)
  
# Summary Statistics and Dictionary ----
  list <- c('dwsores','pppsores','LF_30','precovdw','OFFICES','rsa','eqcaprat','eci','ci_com','scisoa','cdep','cisoa','liqass','levrat','size','dwage','covexpo')
  star <- stargazer(data.frame(sf3[,list]),
              title = 'Summary Statistics Table',
              covariate.labels = c('DW/Reserves','PPP/Reserves','Borrowing from LF in last 30 Days/Reserves','Whether bank has borrowed from DW in last 5 years', 'Number of Offices','Reserve/Asset Ratio',
                                   'Equity Capital Ratio','Branch Economic Exposure','Unused CI Commitments','Small Business share of CI','Core Deposits','CI/Assets','Liquid Assets','T1 Leverage Ratio',
                                   'Log Assets','Branch weighted bank age','COVID Exposure'),
              style = 'qje',
              type = 'latex')
               #summary statistic

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
             'log(dwage)' = 'Log Deposit-weighted average branch age', 'covexpo' = 'Deposit-weighted new COVID rate',
             'LF_30' = 'Borrowing from LF in last 30 Days', 'precovdw' = "DW since 2015 Indicator", 'phase' = 'PPP Phase',
             'rsa' = 'Reserve to Asset', 'log(OFFICES)' = 'Log # of Offices', 'eci' = 'Economic Impact','empps' = 'Employee Pay Share',
             'log(n_ppp_loans)' = 'Log Number of PPP Loans', 'avg_length' = 'Avg. Length of Loan Receipt', 'size_10' = 'Size Deciles',
             'lsa' = 'Loan to Asset','dsa' = 'Deposit to Asset', 'dep_growth' = 'Deposit Growth', 'asinh(ffsores)' = 'Asinh Fed Funds Borrowing/Reserves')
  

# Table 1 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing probability - LPM ----
    ta2 <- list()
    ta2[[1]] <- feols(dwbin_notest ~ pppsores | FED + Date,
                     #family = binomial(link = "logit"),
                     data = sf3, panel.id = c('RSSD','Date'))
    ta2[[2]] <- update(ta2[[1]], . ~. +
                       LF_pre + LF_30 + precovdw + log(OFFICES) + rsa + dsa+ lsa + eqcaprat + eci + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
    ta2[[3]] <- update(ta2[[1]], subset = sf3$bigsmall == 1)
    ta2[[4]] <- update(ta2[[2]], subset = sf3$bigsmall == 1)
    ta2[[5]] <- update(ta2[[1]], subset = sf3$bigsmall == 0)
    ta2[[6]] <- update(ta2[[2]], subset = sf3$bigsmall == 0)
    etable(ta2, dict = dict1,
       title = 'Linear probability model estimation of DW borrowing probability. Column (1-2) is the pooled sample with and without controls, column (3-4) is for large banks that have assets greater than $600M, and columns (5-6) is for small community banks. The results from column (2) implies that a 10 percentage point increase in the PPP quantity loaned as a share of the reserves increases probability to borrow from the discount window by 1.6 percentage points. There seems to be a greater effect for large banks when shocked as compared to small banks.',
       label = 'main_reg',
       digits = 4,
       fitstat = ~n + r2,
       headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
       group = list('LS2020 Controls:'=c('Ratio','Impact',
                                         'Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
       tex = F)
      
      #o_beta_rsq_viz(y='dwbin_notest',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                     #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sf3,bigsmall>=0))
      #o_beta_rsq_viz(y='dwbin_notest',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                     #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sf3,bigsmall==1))
      #o_beta_rsq_viz(y='dwbin_notest',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                     #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sf3,bigsmall==0))
      
      
# Table 2 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing quantity  ---- 
  ta <- list()
  ta[[1]] <- feols(dwsores ~ pppsores|FED + Date, 
                   sft, 
                   #subset = sf3$PPP>0 & sf3$DW > 0
                   panel.id = c('RSSD','Date'))
  ta[[2]] <- update(ta[[1]], . ~ .
                    + LF_30 + precovdw + log(OFFICES) + rsa + dsa+ lsa + eqcaprat + eci + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
  ta[[3]] <- update(ta[[1]], subset =  sft$bigsmall == 1)
  ta[[4]] <- update(ta[[2]], subset =  sft$bigsmall == 1)
  ta[[5]] <- update(ta[[1]], subset =  sft$bigsmall == 0)
  ta[[6]] <- update(ta[[2]], subset =  sft$bigsmall == 0)
  etable(ta, dict = dict1,
         title = 'OLS Estimation of Log DW Borrowing. Columns correspond to the same shares as Table 1. ',
         label = 'main_reg',
         headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
         #digits = 5,
         fitstat = ~n + r2,
         group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
         #extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
         tex = F)
  
  #o_beta_rsq_viz(y='dwsores',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                 #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sft,bigsmall>=0))
  #o_beta_rsq_viz(y='dwsores',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                 #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sft,bigsmall==1))
  #o_beta_rsq_viz(y='dwsores',x='pppsores',con="LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo",
                 #delta = 1, type='lm', id = 'RSSD',time='Date',data=subset(sft,bigsmall==0))
  
  
# Table A.2 Robustness - DW borrowing probability - Logistic Model ----
      ta7 <- list()
      ta7[[1]] <- feglm(dwbin_notest ~ pppsores| FED + Date,
                        family = binomial(link = "logit"),
                        data = sf3, panel.id = c('RSSD','Date'))
      ta7[[2]] <- update(ta7[[1]], . ~ . 
                         + LF_30 + precovdw + log(OFFICES) + rsa + dsa+ lsa + eqcaprat + eci + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
      ta7[[3]] <- update(ta7[[1]], subset = sf3$bigsmall == 1)
      ta7[[4]] <- update(ta7[[2]], subset = sf3$bigsmall == 1)
      ta7[[5]] <- update(ta7[[1]], subset = sf3$bigsmall == 0)
      ta7[[6]] <- update(ta7[[2]], subset = sf3$bigsmall == 0)
      etable(ta7, dict = dict1,
             title = 'Logistic Estimation of borrowing probability from the discount window. Column (1-2) is the pooled sample with and without controls, column (3-4) is for large banks that have assets greater than $600M, and columns (5-6) is for small community banks. The results from column (2) implies that a 10 percentage point increase in the PPP quantity loaned increases probability to borrow from the discount window by 28.1%. Since the predicted mean is 1.9% to borrow from the window, a 28.1% increase imples an increase of .53 percentage points.',
             label = 'main_reg',
             headers = NA,
             fitstat = ~n + pr2,
             group = list('LS2020 Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
             tex = F)
      
      
# Table - IV Results (Pooled, Large, small) with controls using the daily data ----
      ta6 <- list()
      ta6[[1]] <- update(ta2[[2]], .~. - pppsores  | FED + Date|pppsores ~ asinh(c_instrl))
      ta6[[2]] <- update(ta6[[1]], .~. |.| . ~.  , subset = sf3$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta6[[3]] <- update(ta6[[1]], .~. |.| . ~ ., subset = sf3$bigsmall == 0, panel.id =c('RSSD','Date'))
      ta6[[5]] <- update(ta[[2]], .~. - pppsores | FED + Date |  pppsores ~  asinh(c_instrl), data=sft )
      ta6[[6]] <- update(ta6[[5]], .~. |.| . ~. , subset = sft$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta6[[7]] <- update(ta6[[5]], .~. |.| . ~ ., subset = sft$bigsmall == 0, panel.id =c('RSSD','Date'))

      etable(ta6, dict = dict1, stage=2,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = c('Pooled', 'Large Banks','Small Banks'),
             fitstat = ~ n + ivf + wh + sargan,
             group = list('LS2020 Controls:'=c('Offices','Ratio','Impact',
                                        'lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             tex = F)
      
# Table: interacting with PPPLF_i ----
      ta7 <- list()
      ta7[[1]] <- update(ta2[[2]], .~. - pppsores | FED + Date |  i(LF_pre,pppsores) ~  i(LF_pre, asinh(c_instrl)))
      ta7[[2]] <- update(ta7[[1]], subset = sf3$bigsmall == 1 )
      ta7[[3]] <- update(ta7[[1]], subset = sf3$bigsmall == 0)
      ta7[[4]] <- update(ta[[2]],.~. - pppsores |FED + Date|  i(LF_pre,pppsores) ~  i(LF_pre, asinh(c_instrl)), data=sft)
      ta7[[5]] <- update(ta7[[4]], subset = sft$bigsmall == 1)
      ta7[[6]] <- update(ta7[[4]], subset = sft$bigsmall == 0)
      etable(ta7, dict = dict1,
             title = 'OLS Estimation of Log DW Borrowing',
             label = 'main_reg',
             headers = c('Pooled', 'Large Banks','Small Banks'),
             fitstat = ~ n,
             group = list('LS2020 Controls:'=c('Offices','Ratio','Impact',
                                        'lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             tex = F)
      

      
# Table - time series (OLS) ----
      ta8 <- list()
      ta8[[1]] <- update(ta2[[2]], .~i(week,pppsores) + . - pppsores | FED + Date, cluster='RSSD')
      ta8[[2]] <- update(ta8[[1]], .~. , subset = sf3$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta8[[3]] <- update(ta8[[1]], .~. , subset = sf3$bigsmall == 0, panel.id =c('RSSD','Date'))
      ta8[[4]] <- update(ta[[2]], .~ i(week,pppsores) + . - pppsores | FED + Date, data = sft, cluster='RSSD')
      ta8[[5]] <- update(ta8[[4]], .~., subset = sft$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta8[[6]] <- update(ta8[[4]], .~. , subset = sft$bigsmall == 0, panel.id =c('RSSD','Date'))
      etable(ta8, dict=dict1,
             headers = c('Pooled', 'Large Banks','Small Banks'),
             group = list('LS2020 Controls:'=c('Offices','Ratio','Impact','lsa','rsa','dsa','Equity Cap Ratio','Log Assets',
                                               'Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
             fitstat = ~ n+r2, tex = T)
      plot1 <- data.table(Weeks = 1:6, prob_pooled = ta8[[1]]$coefficients[1:6], prob_large = ta8[[2]]$coefficients[1:6], prob_small = ta8[[3]]$coefficients[1:6],
                 quant_pooled = ta8[[4]]$coefficients[1:6], quant_large = ta8[[5]]$coefficients[1:6], quant_small = ta8[[6]]$coefficients[1:6],
                 pp_se = ta8[[1]]$se[1:6], pl_se = ta8[[2]]$se[1:6], ps_se = ta8[[3]]$se[1:6],
                 qp_se = ta8[[4]]$se[1:6], ql_se = ta8[[5]]$se[1:6], qs_se = ta8[[6]]$se[1:6])
      ggplot(plot1, aes(x=Weeks)) + 
        geom_errorbar(aes(y = prob_pooled, ymin=prob_pooled-1.96*pp_se, ymax=prob_pooled+1.96*pp_se, color='Pooled Banks'), width=.1) + geom_point(aes(y=prob_pooled, color='Pooled Banks')) +
        geom_errorbar(aes(y = prob_large, ymin=prob_large-1.96*pl_se, ymax=prob_large+1.96*pl_se, color='Large Banks'), position = position_nudge(x=.2), width=.1) + 
        geom_point(aes(y=prob_large, color='Large Banks'), position = position_nudge(x=.2)) +
        geom_errorbar(aes(y = prob_small, ymin=prob_small-1.96*ps_se, ymax=prob_small+1.96*ps_se, color='Small Banks'), position = position_nudge(x=-.2), width=.1) + 
        geom_point(aes(y=prob_small, color='Small Banks'), position = position_nudge(x=-.2)) +
        ylab('Betas Of Probability')
      ggplot(plot1, aes(x=Weeks)) + 
        geom_errorbar(aes(y = quant_pooled, ymin=quant_pooled-1.96*qp_se, ymax=quant_pooled+1.96*qp_se, color='Pooled Banks'), width=.1) + geom_point(aes(y=quant_pooled, color='Pooled Banks')) +
        geom_errorbar(aes(y = quant_large, ymin=quant_large-1.96*ql_se, ymax=quant_large+1.96*ql_se, color='Large Banks'), position = position_nudge(x=.2), width=.1) + 
        geom_point(aes(y=quant_large, color='Large Banks'), position = position_nudge(x=.2)) +
        geom_errorbar(aes(y = quant_small, ymin=quant_small-1.96*qs_se, ymax=quant_small+1.96*qs_se, color='Small Banks'), position = position_nudge(x=-.2), width=.1) + 
        geom_point(aes(y=quant_small, color='Small Banks'), position = position_nudge(x=-.2)) +
        ylab('Betas Of Quantity')
      
      
# Table - time series (IV) ----
      ta8 <- list()
      ta8[[1]] <- update(ta2[[2]], .~. - pppsores | FED + Date | i(week,pppsores) ~ i(week,c_instrl), cluster='RSSD')
      ta8[[2]] <- update(ta8[[1]], .~. , subset = sf3$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta8[[3]] <- update(ta8[[1]], .~. , subset = sf3$bigsmall == 0, panel.id =c('RSSD','Date'))
      ta8[[4]] <- update(ta[[2]], .~ . - pppsores | FED + Date | i(week,pppsores) ~ i(week,c_instrl), data = sft, cluster='RSSD')
      ta8[[5]] <- update(ta8[[4]], .~., subset = sft$bigsmall == 1, panel.id =c('RSSD','Date'))
      ta8[[6]] <- update(ta8[[4]], .~. , subset = sft$bigsmall == 0, panel.id =c('RSSD','Date'))
      etable(ta8, dict=dict1,
             group = list('LS2020 Controls:'=c('Offices','Ratio','Impact','lsa','rsa','dsa','Equity Cap Ratio','Log Assets',
                                               'Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')))
      plot1 <- data.table(Weeks = 1:6, prob_pooled = ta8[[1]]$coefficients[1:6], prob_large = ta8[[2]]$coefficients[1:6], prob_small = ta8[[3]]$coefficients[1:6],
                          quant_pooled = ta8[[4]]$coefficients[1:6], quant_large = ta8[[5]]$coefficients[1:6], quant_small = ta8[[6]]$coefficients[1:6],
                          pp_se = ta8[[1]]$se[1:6], pl_se = ta8[[2]]$se[1:6], ps_se = ta8[[3]]$se[1:6],
                          qp_se = ta8[[4]]$se[1:6], ql_se = ta8[[5]]$se[1:6], qs_se = ta8[[6]]$se[1:6])
      ggplot(plot1, aes(x=Weeks)) + 
        geom_errorbar(aes(y = prob_pooled, ymin=prob_pooled-1.96*pp_se, ymax=prob_pooled+1.96*pp_se, color='Pooled Banks'), width=.1) + geom_point(aes(y=prob_pooled, color='Pooled Banks')) +
        geom_errorbar(aes(y = prob_large, ymin=prob_large-1.96*pl_se, ymax=prob_large+1.96*pl_se, color='Large Banks'), position = position_nudge(x=.2), width=.1) + 
        geom_point(aes(y=prob_large, color='Large Banks'), position = position_nudge(x=.2)) +
        geom_errorbar(aes(y = prob_small, ymin=prob_small-1.96*ps_se, ymax=prob_small+1.96*ps_se, color='Small Banks'), position = position_nudge(x=-.2), width=.1) + 
        geom_point(aes(y=prob_small, color='Small Banks'), position = position_nudge(x=-.2)) +
        ylab('Betas Of Probability')
      ggplot(plot1, aes(x=Weeks)) + 
        geom_errorbar(aes(y = quant_pooled, ymin=quant_pooled-1.96*qp_se, ymax=quant_pooled+1.96*qp_se, color='Pooled Banks'), width=.1) + geom_point(aes(y=quant_pooled, color='Pooled Banks')) +
        geom_errorbar(aes(y = quant_large, ymin=quant_large-1.96*ql_se, ymax=quant_large+1.96*ql_se, color='Large Banks'), position = position_nudge(x=.2), width=.1) + 
        geom_point(aes(y=quant_large, color='Large Banks'), position = position_nudge(x=.2)) +
        #geom_errorbar(aes(y = quant_small, ymin=quant_small-1.96*qs_se, ymax=quant_small+1.96*qs_se, color='Small Banks'), position = position_nudge(x=-.2), width=.1) + 
        #geom_point(aes(y=quant_small, color='Small Banks'), position = position_nudge(x=-.2)) +
        ylab('Betas Of Quantity')
      
      
      
# Cross Sectional Data Creation - Aggregating all daily info during COVID to a cross section ----
sf3 %>% group_by(RSSD) %>% summarise(across(.cols = c(PPP, LF_received, DW, eci, covexpo, dwbin_notest, LF_rec_ind, ppp_ind, n), sum),
                                     across(.cols = c(Reserves, rsa, lsa, dsa, precovdw, dwage, eqcaprat,DW_i,npplsores,processing_time), mean))
setnames(sf4, old = c('dwbin_notest','LF_rec_ind','n','ppp_ind'), new = c('DW_count','LF_count','n_ppp_loans','PPP_days'))

temp <- subset(df, Date == as.Date('2020-03-31'))
temp$levrat <- temp$RCOA8274/temp$RCOAA224
temp$ci_com <- temp$RCONJ457/temp$RCON2170
temp$scisoa  <- (temp$RCON5571+temp$RCON5573+temp$RCON5575)/temp$RCON2170
temp$cdep   <- (temp$RCON2215+temp$RCON2210)/temp$RCON2170
temp$cisoa <- temp$RCON1766/temp$RCON2170
temp$levrat <- temp$RCOA8274/temp$RCOAA224
temp$liqass <- rowSums(cbind(temp$RCON0071,temp$RCON0081,temp$RCONJJ34,temp$RCON1773,temp$RCONJA22),na.rm=TRUE)/temp$RCON2170
attach(temp, warn.conflicts = F); temp$ltosb <- rowSums(cbind(RCON5570,RCON5572,RCON5574))
temp$lab_share <- temp$RIAD4135/(temp$RIAD4217 + temp$RIAD4135)
list <- c('IDRSSD','RCON2170','RCON5563','RCON5571','RCON5573','RCON5575','RCON1766','FED','levrat','Primary.ABA.Routing.Number','RCON0081','RCON0071','ltosb',
          'ci_com','scisoa','cdep','cisoa','liqass','lab_share','OFFICES')
sf4 <- left_join(sf4, data.frame(temp[,list]), by=c('RSSD' = 'IDRSSD'))

temp <- subset(df, Date >= as.Date('2020-03-31') & Date <= as.Date('2020-06-30'))
temp <- temp %>% group_by(IDRSSD) %>% arrange(Date) %>% mutate(dep_growth = dplyr::lead(RCON2200)/RCON2200, eqcap_growth = dplyr::lead(RCONG105)/RCONG105)
temp$dep_growth <- (temp$dep_growth - 1)*100
temp$eqcap_growth <- (temp$eqcap_growth - 1)*100
temp <- subset(temp, Date == as.Date('2020-03-31'))
sf4 <- left_join(sf4, temp[,c('IDRSSD','dep_growth','eqcap_growth')], by=c('RSSD' = 'IDRSSD'))

temp <- subset(df, Date == as.Date('2020-06-30'))
temp$ff_borrow <- (temp$RCONB993 + temp$RCONB995)
temp$ffsores <- temp$ff_borrow/(temp$RCON0071 + temp$RCON0081)
sf4 <- left_join(sf4, temp[,c('IDRSSD','ff_borrow','ffsores')], by=c('RSSD' = 'IDRSSD'))

#Creating the PPPE from Granja
  sf4$ltosb <- ifelse(is.na(sf4$ltosb) == TRUE, 0, sf4$ltosb)
  sf4$sb_share_tot <- sf4$ltosb/sum(sf4$ltosb)
  sf4$ppp_share_tot <- sf4$n_ppp_loans/sum(sf4$n_ppp_loans)
  sf4$pppe <- .5*(sf4$ppp_share_tot-sf4$sb_share_tot)/(sf4$ppp_share_tot+sf4$sb_share_tot)
  
# Creating the average length to get a PPP loan  
  temp <- sf3 %>% group_by(RSSD) %>% mutate(tot_ppp = sum(n))
  temp$length <- as.numeric(temp$Date - as.Date('2020-04-03'))
  temp$ppp_share_n <- temp$n/temp$tot_ppp
  temp$avg_length <- temp$ppp_share_n*temp$length
  temp <- aggregate(avg_length ~ RSSD, temp, sum)
  sf4 <- left_join(sf4, temp)

  
sf4 <- left_join(sf4, unique(binstr[,c('IDRSSD','dep_share')]),by=c('RSSD'='IDRSSD'))

temp <- subset(dwborrow, Loan.date <= as.Date('2020-03-01')) %>% group_by(Borrower.ABA.number) %>% count()
temp$Borrower.ABA.number <- as.numeric(temp$Borrower.ABA.number)
sf4 <- left_join(sf4, temp, by=c('Primary.ABA.Routing.Number' = 'Borrower.ABA.number'))
sf4$n_DW_precov <- ifelse(is.na(sf4$n) == TRUE, 0, sf4$n)
sf4$precovdw <- ifelse(sf4$n_DW_precov >0, 1, 0)
sf4$sb_cis <- (sf4$RCON5571+sf4$RCON5573+sf4$RCON5575)/sf4$RCON1766
sf4$bigsmall <- ifelse(sf4$RCON2170 > 600000,1,0)
sf4$Reserves <- 1000*(sf4$RCON0071 + sf4$RCON0081)
sf4$size <- log(sf4$RCON2170)
sf4$pppsores <- sf4$PPP/sf4$Reserves
sf4$dwsores <- sf4$DW/sf4$Reserves
sf4$lfsores <- sf4$LF_received/sf4$Reserves

sf4$scisoa <- ifelse(is.na(sf4$scisoa)==TRUE, 0, sf4$scisoa)
sf4$intensity <- sf4$pppsores/sf4$PPP_days
sf4$LF_i <- ifelse(sf4$LF_received>0,1,0)
sf4$DW_i <- ifelse(sf4$DW_count>0,1,0)
sf4$FF_i <- ifelse(sf4$ffsores>0,1,0)

sf4 <- subset(sf4, PPP > 0)

sf4 <- sf4 %>% mutate(size_10 = ntile(RCON2170, 10))

list <- c('pppsores','dwsores','lfsores','eci','covexpo','rsa','lsa','dsa','OFFICES','sb_cis','n_ppp_loans','levrat',
          'ci_com','scisoa','cdep','cisoa','liqass')
sf4[,list] <- lapply(sf4[,list], Winsorize, probs=c(.001,.999), na.rm = TRUE)
sf4$lab_share <- Winsorize(sf4$lab_share, minval = 0, maxval = 1)

# Table x - Did DW borrowing expand the amount of PPP lending? Did DW Borrowing increase in the speed of loan grants? (1) no, (2) yes----
ta <- list()
ta[[1]] <- feols( log(n_ppp_loans) ~ DW_i + LF_i + dep_growth + FF_i | FED, sf4)
ta[[2]] <- update(ta[[1]], .~.  + precovdw + log(OFFICES) + rsa + lsa + dsa + eqcaprat + eci +  ci_com + scisoa + cdep + npplsores + cisoa + liqass + levrat + size + log(dwage) + covexpo )
ta[[3]] <- update(ta[[2]], subset = sf4$bigsmall == 1)
ta[[4]] <- update(ta[[2]], subset = sf4$bigsmall == 0)
ta[[5]] <- update(ta[[1]], avg_length ~ .)
ta[[6]] <- update(ta[[2]], avg_length ~. + log(n_ppp_loans))
ta[[7]] <- update(ta[[6]], subset = sf4$bigsmall == 1)
ta[[8]] <- update(ta[[6]], subset = sf4$bigsmall == 0)
etable(ta[1:4],dict= dict1, fitstat = ~n+r2 + sargan, se='white',
       headers = c('No Controls','Pooled','Large Banks', 'Small Banks'),
       group = list('LS2020 Controls:'=c('Ratio','Equity Cap Ratio','Log Assets',
       'Log RA Ratio','econexpo','eci','npplsores','CI','C&I','age','Tier','Asset')),
       tex=F)  

etable(ta[5:8],dict= dict1, fitstat = ~n+r2 + sargan, se='white',
       headers = c('No Controls','Pooled','Large Banks', 'Small Banks'),
       group = list('LS2020 Controls:'=c('Ratio','Equity Cap Ratio','Log Assets',
                                         'Log RA Ratio','econexpo','eci','npplsores','CI','C&I','age','Tier','Asset')),
       tex=F)  


ta[[5]] <- update(ta[[1]], avg_length ~ DW_i + LF_i + dep_growth + asinh(ffsores) )
ta[[6]] <- update(ta[[2]], avg_length ~. + DW_i+ log(n_ppp_loans))
ta[[7]] <- update(ta[[6]], subset = sf4$bigsmall == 1)
ta[[8]] <- update(ta[[6]], subset = sf4$bigsmall == 0)
etable(ta,dict= dict1, fitstat = ~n+r2 + sargan, se='white',
       headers = c('No Controls','Pooled','Large Banks', 'Small Banks'),
       #group = list('LS2020 Controls:'=c('Ratio','Asset','Equity Cap Ratio','Log Assets',
                                         #'Log RA Ratio','econexpo','eci','npplsores','CI','C&I','Assets','age','Tier')),
       tex=F)  

# Table 4 - OLS results (Pooled, Large, small) for DW borrowing sizes (aggregate) ----
ta3 <- list()  
ta3[[1]] <- feols(log(DW_count+1) ~log(intensity) + asinh(lfsores) + precovdw + log(OFFICES) +
                    rsa + lsa + dsa + eqcaprat + eci +  ci_com + scisoa + cdep + npplsores + cisoa + liqass + levrat + size + log(dwage) + covexpo| FED +size_10 , data=sf4)
ta3[[2]] <- update(ta3[[1]], subset = sf4$bigsmall == 1)
ta3[[3]] <- update(ta3[[1]], subset = sf4$bigsmall == 0)
ta3[[4]] <- update(ta3[[1]], log(dwsores) ~., data=sf4)
ta3[[5]] <- update(ta3[[4]], subset = sf4$bigsmall == 1)
ta3[[6]] <- update(ta3[[4]], subset = sf4$bigsmall == 0)

etable(ta3, dict = dict1,
       title = 'OLS Estimation of Log DW Borrowing',
       se = 'cluster', cluster='RSSD',
       label = 'aggregate_quant_reg',
       headers = c('Pooled','Large Banks','Small Banks'),
       #group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
       fitstat = ~n + r2,
       tex = FALSE)

# Table 6 - IV Result for DW Borrowing Quantity ----
  ta5 <- list(); 
  ta5[[1]] <- feols(log(DW_count+1)  ~ asinh(lfsores) + precovdw +log(OFFICES) +
                      rsa + lsa + dsa + eqcaprat + eci +  ci_com + scisoa + cdep + npplsores + cisoa + liqass + levrat + size + log(dwage) + covexpo
                    | FED + size_10|
                      log(intensity) ~ sb_cis , sf4)
  ta5[[2]] <- update(ta5[[1]], subset = sf4$bigsmall == 1)
  ta5[[3]] <- update(ta5[[1]], subset = sf4$bigsmall == 0)
  ta5[[4]] <- update(ta5[[1]], log(dwsores) ~., data=sf4, subset = sf4$dwsores>0)
  ta5[[5]] <- update(ta5[[4]], subset = sf4$bigsmall == 1 & sf4$dwsores>0)
  ta5[[6]] <- update(ta5[[4]], subset = sf4$bigsmall == 0 & sf4$dwsores>0)
  
  etable(ta5, dict = dict1,
         title = 'OLS Estimation of Log DW Borrowing',
         se = 'white',
         label = 'aggregate_quant_reg',
         headers = c('Pooled','Large Banks','Small Banks'),
         group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
         fitstat = ~ n + ivf + sargan,
         tex = FALSE)

  
