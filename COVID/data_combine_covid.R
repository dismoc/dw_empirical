# Libraries ----
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
library('lubridate')

# Merging the data tables ----

effr <- read_csv("D:/Research/DW lending empirical/Data/effr_range.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
call_rep <- read_csv("D:/Research/DW lending empirical/Data/call_rep_full.csv")
int_rate <- read_excel("D:/Research/DW lending empirical/Data/int_rates.xls")
sdc <- read_csv("D:/Research/DW lending empirical/Data/sdc_full.csv")
def <- read_csv("D:/Research/DW lending empirical/Data/defs.csv")
inst <-read_csv("D:/Research/DW lending empirical/Data/INSTITUTIONS2.CSV")
att <- read_csv("D:/Research/DW lending empirical/Data/ffiec/Atrributes_merged.csv")
int_rate$Date <- as.Date(int_rate$Date)


q <- c("0331","0630","0930","1231")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")


dwbsub <- subset(dwborrow, Type.of.credit == "Primary Credit")
dwbsub <- subset(dwbsub, dwbsub$Loan.amount >= 1000000)
dwbsub$Date <- data.frame(timeLastDayInQuarter(dwbsub$Loan.date, format = "%Y-%m-%d", zone = "", FinCenter = ""))$GMT.x..i..
dwbsub$repay <- (dwbsub$Interest.rate*dwbsub$Term/36000)*dwbsub$Loan.amount


dcomb <- merge(merge(aggregate(Term ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum), 
                     aggregate(Loan.amount*Term ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum)), 
               aggregate(repay ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum))

rm(dwbsub, dwborrow)
dcomb$avg.int <- 36000*dcomb$repay/dcomb$`Loan.amount * Term`

setnames(dcomb, old = c('Borrower.ABA.number','Date','Term','Loan.amount * Term','repay','avg.int'),
         new = c('ABA_routing','Date','dw_freq','dw_quant','repay','avg_int'))
dcomb$ABA_routing <- as.numeric(dcomb$ABA_routing)
att$ABA_routing <- att$ID_ABA_PRIM; att$IDRSSD <- att$`#ID_RSSD`
dc <- left_join(dcomb, att[,c('ABA_routing','IDRSSD','D_DT_START','ID_FDIC_CERT','ID_OCC')], by = c('ABA_routing'))
inst$IDRSSD <- inst$FED_RSSD
df <- call_rep %>% left_join(inst[,c('FED','FDICREGN','IDRSSD','OFFICES','BKCLASS','OFFDOM','OFFFOR','STMULT','CHRTAGNT','ESTYMD')], by=('IDRSSD'),
                             suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("..."))

df <- df %>% full_join(sdc, by=c('IDRSSD','Date'), suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("..."))
rm(sdc)

df <- df %>% left_join(dc, by=c('IDRSSD','Date'), suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("..."))

rm(dc, dcomb, att)
rm(inst, call_rep)

# Adding new columns ----

# Two types of access definition, dwaccess_2 is access after first loan, dwaccess_1 is access for all sample time period
#dwaccess_1
ag <- aggregate(dw_freq ~ IDRSSD, data=df, FUN = sum, na.rm = TRUE)
setnames(ag, old=c('dw_freq'), new=c('freq'))
df1 <- left_join(df, ag)
df1$dwaccess_1 <- ifelse(df1$freq > 0, 1, 0)
df1$dwaccess_1[is.na(df1$dwaccess_1)] = 0
df1 <- df1[ , -which(names(df1) %in% c("freq"))]
df <- df1
rm(df1,ag)

#dwaccess_2
df1 = df %>%
  arrange(IDRSSD,Date) %>%
  group_by(IDRSSD) %>%
  mutate(agg_access =rollapplyr(dw_freq, 60, sum, partial = TRUE, na.rm = TRUE) )
df1$dwaccess_2 <- ifelse(df1$agg_access > 0, 1, 0)
df1 <- df1[ , -which(names(df1) %in% c("agg_access"))]
df <- df1  
rm(df1)  


#Adding additional series - reserve asset ratio and reserve deposit ratio, 
df$dw_freq[is.na(df$dw_freq)] <- 0
df$dwborrow_bin <- ifelse(df$dw_freq > 0, 1, 0)


df$reserve_asset_ratio <- df$RCON0010/df$RCON2170
df$reserve_deposit_ratio <- df$RCON0010/df$RCON2200
df[is.infinite(df$reserve_deposit_ratio),c('reserve_deposit_ratio')] <- NA
df$reserve_deposit_ratio <- Winsorize(df$reserve_deposit_ratio, probs = c(.01,.99), na.rm = TRUE)

df$dwborrow_cov <- ifelse(df$dwborrow_bin == 1 & as.Date(df$Date) >= as.Date('2020-03-31'), 1, 0)


rm(effr, int_rate)
#Save File ----
write.csv(df,"D:\\Research\\DW lending empirical\\Data\\merged_cov.csv")
