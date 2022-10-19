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
cov <- read_csv("D:/Research/DW lending empirical/Data/cov.csv")

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
  
# Bank Level Data: -----
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

  sf2$Reserves <- (sf2$RCON0081+ sf2$RCON0071)*1000
  sf2$Assets <- sf2$RCON2170*1000
  
  sf2$dwsores <- sf2$DW/sf2$Reserves
  sf2$pppsores <- sf2$PPP/sf2$Reserves
  sf2$pppsores <- ifelse(is.na(sf2$pppsores) == TRUE, 0, sf2$pppsores)
  sf2$lfsores <- sf2$PPPLF/sf2$Reserves
  sf2$npplsores <- sf2$nonppp_loans/sf2$Reserves
  #sf2$delt_so_reserves <- sf2$r_delt*100/sf2$RCON2170
  
  sf2$dwbin_notest <- ifelse(sf2$dwsores > 0.01, 1,0)
  sf2$dwbin_notest <- ifelse(is.na(sf2$dwbin_notest) == TRUE, 0, sf2$dwbin_notest)
  sf2$dwsores <- ifelse(sf2$dwbin_notest == 1, sf2$dwsores, 0)
  sf2$dw_bin <- ifelse(sf2$DW > 0, 1, 0)
  sf2$levratio <- sf2$RCFA7204
  sf2$bigsmall <- ifelse(sf2$RCON2170 > 1284000, 1, 0)
  
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
  
  # create series on whether bank has used DW 5 years pre-covid
  dwborrow1 <- subset(dwborrow, Loan.date >= '2015-04-03' & Loan.date < '2020-04-03')
  ls <- intersect(unique(sf2$ABA),unique(dwborrow1$Borrower.ABA.number))
  sf2[sf2$ABA %in% ls, 'precovdw'] <- 1
  sf2$precovdw <- ifelse(is.na(sf2$precovdw) == TRUE, 0, 1)
  
  #merging with the covid data
  cov$Date <- as.Date(cov$submission_date, '%m/%d/%Y')
  sf2 <- left_join(sf2, cov[,c('Date','state','new_case')], by=c('Date','State'='state'))
  sf2$new_case <- ifelse(sf2$new_case < 0, 0, sf2$new_case)
  # Making the regression dataset
  sf3 <- subset(sf2, Date >= as.Date('2020-04-03') & Date <= as.Date('2020-08-08'))
  sf3 <- sf3[!duplicated(sf3[c("RSSD", "Date")]),]
  sf3 <- sf3 %>% drop_na(RSSD, Date, State, size_quint)
  sf3 <- sf3[!sf3$Reserves ==0,]
  #sf3 <- sf3[!sf3$delt_so_reserves <= 0,]
  sf3 <- sf3[rowSums(is.na(sf3)) != ncol(sf3), ]
  
  #stargazer(data.frame(sf3[,c('PPP','PPPLF','PPPLF_i','DW','DW_i','reserve_asset_ratio','OFFICES','eqcaprat','precovdw')])) #summary statistic
  dict1 <- c('dw_bin' = 'DW Prob', 'demand_so_reserves' = 'Demand Shock', 'reserve_asset_ratio' = 'RA Ratio',
             'size' = 'Size', 'log(PPPLF+1)' = 'Log(PPPLF)', 'log(RCON0010)' = 'Log(Reserves)',
             'eqcaprat' = 'Equity Cap Ratio', 'pppsores' = 'Demand Shock',
             'pppsores x PPPLF_ind' = 'DS*PPPLF_ind', 'log(borr_total+1)' = 'Log(FF Borrowing)',
             'log(pppsores)' = 'Log(Demand Shock)', 'log(PPP)' = 'Log(PPP)',
             'dwbin_notest' = 'DW Prob', 'log(reserve_asset_ratio)' = 'Log RA Ratio',
             'LF_30' = 'LF_{30}', 'log(eqcaprat)' = 'Log Equity Cap Ratio')
  

        
    #Table 2: Linear model
      t1 <- list()
      t1[[1]] <- feols(log(dwsores+1) ~ log(pppsores+1)  + log(LF_30+1) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + log(new_case+1)| RSSD + Date  , 
                       sf3, panel.id = c('RSSD','Date'))
      t1[[2]] <- update(t1[[1]], . ~. - log(pppsores+1) + i(bigsmall,log(pppsores+1)))
      t1[[3]] <- update(t1[[1]], dwbin_notest ~ . )
      t1[[4]] <- update(t1[[3]], . ~ . - log(pppsores+1) + i(bigsmall,log(pppsores+1)))
      t1[[5]] <- update(t1[[2]], . ~. - i(bigsmall,log(pppsores+1)) |.| i(bigsmall,log(pppsores+1)) ~ i(bigsmall,log(n+1)))
      t1[[6]] <- update(t1[[1]], . ~. - log(pppsores+1) |.| log(pppsores+1) ~ log(n+1))
      etable(t1, dict=dict1,
             cluster = 'RSSD', tex = F)
      
      #t1[[3]] <- update(t1[[2]], .~. - log(ppp_so_reserves) - log(ppp_so_reserves)*log(LF_30+1) |.| log(ppp_so_reserves) ~ BInstr)
      
    #Robustness - Fixed Effects
      t6 <- list()
      t6[[1]] <- update(t1[[length(t1)]], .~. |. - RSSD - Date)
      t6[[2]] <- update(t1[[length(t1)]], .~. |. - Date + month)
      t6[[3]] <- update(t1[[length(t1)]], .~. |. - RSSD + State)
      t6[[4]] <- update(t1[[length(t1)]], .~. |. - Date + month^State)
      etable(t1[[length(t1)]], t6, dict=dict1, se='cluster', tex=F, cluster= 'RSSD',
             drop = c('Intercept','Size', 'eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age','OFFICES'))
      
    
    # clustering
      t3 <- list()
      t3[[1]] <- update(t1[[6]], se = 'white')
      t3[[2]] <- update(t3[[1]], se = 'cluster', cluster = c('RSSD','Date'))
      t3[[3]] <- update(t3[[1]], se = 'cluster', cluster = c('RSSD','month'))
      t3[[4]] <- update(t3[[1]], se = 'cluster', cluster = 'FED')
      t3[[5]] <- update(t3[[1]], se = 'cluster', cluster = 'State')
      etable(t3, dict=dict1, tex=F,
             drop = c('eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','Size','exposure'))
      
      
      
    #Robustness - different shock series creation
      t5 <- list()
      t <- sf3; t$ppp_so_reserves <- ifelse(t$ppp_so_reserves < .01, 0, t$ppp_so_reserves)
      t5[[1]] <- t1[[length(t1)]]
      t5[[2]] <- update(t5[[1]], .~. - ppp_so_reserves*LF_30 + log(ppp_so_reserves)*LF_30, data= t)
      t5[[3]] <- update(t5[[1]], .~. - ppp_so_reserves*LF_30 + log(PPP)*LF_30, data= t)
      etable(t5, dict=dict1, cluster = 'State', tex=F,
             drop = c('Size','eqcaprat','RA Ratio','Asset','Equity','FF Borrowing','precovdw','age'))
      
    #Robustness - families
      t2 <- list()
      t2[[1]] <- update(t1[[length(t1)]], family = binomial(link = "probit"))
      t2[[2]] <- update(t1[[length(t1)]],.~. -ppp_so_reserves|. |ppp_so_reserves ~ log(BInstr),family = gaussian())
      etable(t1[[length(t1)]],t2, dict=dict1, cluster = 'State', tex=F,
             drop = c('eqcaprat','RA Ratio','Asset','Equity','FF Borrowing'))
      
      t <- ivprobit(dwbin_notest ~ log(reserve_asset_ratio) + size + i(Date) + i(RSSD) |ppp_so_reserves| log(BInstr), sf3)
      
    #Marginal effects - baseline sample
      me1 <- marginaleffects(t1[[4]]); summary(me1)
      plot_cme(t1[[5]], effect = 'ppp_so_reserves', condition = 'reserve_asset_ratio')
      
      #Table 1: Binary regression: 1) baseline 2-5) progressively more controls
      t1 <- list()
      t1[[1]] <- feglm(dwbin_notest ~ ppp_so_reserves + LF_30 | RSSD + Date, 
                       sf3, family = binomial(link = "probit"), panel.id = c('RSSD','Date'))
      t1[[2]] <- update(t1[[1]], .~. + log(reserve_asset_ratio) + size)
      t1[[3]] <- update(t1[[2]], .~. + age + OFFICES + log(eqcaprat) + precovdw)
      t1[[4]] <- update(t1[[3]], .~. - ppp_so_reserves - LF_30 + ppp_so_reserves*LF_30)
      etable(t1[[1]],t1[[2]], dict=dict1,
             cluster = 'State', tex = F)
      

# Robustness Tests
      # Fixed effects vs random effects - result from the hausman test tells us to use the fixed effects model 
      ran <- pglm(dwbin_notest ~ ppp_so_reserves*PPPLF_i + log(reserve_asset_ratio) + size, index=c("RSSD", "Date"),
               data=sf3[t1[[4]]$obs_selection$obsRemoved,], model='random', family = binomial())
      fix <- pglm(dwbin_notest ~ ppp_so_reserves*PPPLF_i + log(reserve_asset_ratio) + size, index=c("RSSD", "Date"),
                 data=sf3[t1[[4]]$obs_selection$obsRemoved,],model = "random", effect = "twoways", family = binomial())
      phtest(fix, ran)
      
      write.csv(sf3,"D:\\Research\\DW lending empirical\\Data\\reg_data.csv")
      
      
      t <- feols(ppp_so_reserves ~ log(ins) + l(ppp_so_reserves,1)+size + log(reserve_asset_ratio)|RSSD + Date, sf3, panel.id = c('RSSD','Date'), se='white')
      sf4 <- data.frame(sf3[t$obs_selection$obsRemoved,], fit <- t$fitted.values)
      feglm(dwbin_notest ~ fit +size + log(reserve_asset_ratio)|RSSD + Date, sf4)      
      