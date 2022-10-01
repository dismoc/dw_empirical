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

# Reading in data ----

dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
call_rep <- read_csv("D:/Research/DW lending empirical/Data/call_rep.csv")
int_rate <- read_excel("D:/Research/DW lending empirical/Data/int_rates.xls")
inst <-read_csv("D:/Research/DW lending empirical/Data/INSTITUTIONS2.CSV")
int_rate$Date <- as.factor(int_rate$Date)

q <- c("0331","0630","0930","1231")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")


# Transform DW data into uniques

dwbsub <- subset(dwborrow, Type.of.credit == "Primary Credit")
dwbsub$Date <- data.frame(timeLastDayInQuarter(dwbsub$Loan.date, format = "%Y-%m-%d", zone = "", FinCenter = ""))$GMT.x..i..
dwbsub$int <- dwbsub$Interest.rate*dwbsub$Term/36000
dwbsub$repay <- dwbsub$int*dwbsub$Loan.amount


dcomb <- merge(merge(aggregate(Term ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum), 
               aggregate(Loan.amount*Term ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum)), 
               aggregate(repay ~ Borrower.ABA.number + Date, data = dwbsub, FUN = sum))

dcomb$avg.int <- 36000*dcomb$repay/dcomb$`Loan.amount * Term`

setnames(dcomb, old = c('Borrower.ABA.number','Date','Term','Loan.amount * Term','repay','avg.int'),
         new = c('ABA_routing','Date','dw_freq','dw_quant','repay','avg_int'))
dcomb$ABA_routing <- as.numeric(dcomb$ABA_routing)
df <- pdata.frame(left_join(call_rep,dcomb),index=c('IDRSSD','Date'))
df <- df[,-1]


# Two types of access definition, dwaccess_2 is access after first loan, dwaccess_1 is access for all sample time period
  #dwaccess_1
  ag <- aggregate(dw_freq ~ IDRSSD, data=df, FUN = sum, na.rm = TRUE)
  setnames(ag, old=c('dw_freq'), new=c('freq'))
  df1 <- left_join(df, ag)
  df1$dwaccess_1 <- ifelse(df1$freq > 0, 1, 0)
  df1$dwaccess_1[is.na(df1$dwaccess_1)] = 0
  df1 <- df1[ , -which(names(df1) %in% c("freq"))]
  df <- df1
  rm(df1,dcomb,dwborrow,dwbsub,ag,call_rep)

  #dwaccess_2
  df1 = df %>%
    arrange(IDRSSD,Date) %>%
    group_by(IDRSSD) %>%
    mutate(agg_access =rollapplyr(dw_freq, 60, sum, partial = TRUE, na.rm = TRUE) )
  df1$dwaccess_2 <- ifelse(df1$agg_access > 0, 1, 0)
  df1 <- df1[ , -which(names(df1) %in% c("agg_access"))]
  df <- df1  
  rm(df1)  

  #remove values outside of range for interest faced/loaned.
  df1 <- left_join(df, int_rate, by='Date')
  df1$i_lent <- ifelse(df1$i_lent <= 0, NA, df1$i_lent)
  df1$i_lent <- ifelse(df1$i_lent > (df1$DPCREDIT+1), NA, df1$i_lent)
  df1$i_borrow <- ifelse(df1$i_borrow <= 0, NA, df1$i_borrow)
  df1$i_borrow <- ifelse(df1$i_borrow > (df1$DPCREDIT+1), NA, df1$i_borrow)
  df <- df1; rm(df1)
  df$FED <- replace_na(df$FED, 13)
  
  #Creating # of banks exiting and entering
  dd <- unique(df$Date)
  df1 <- df
  for (i in 2:length(dd)) {
    df1$agg_exit[df1$Date == dd[i]]  <- length(setdiff(subset(df1, df1$Date == dd[i-1])$IDRSSD,subset(df, df$Date == dd[i])$IDRSSD))
    df1$agg_entry[df1$Date == dd[i]] <- length(setdiff(subset(df1, df1$Date == dd[i])$IDRSSD,subset(df, df$Date == dd[i-1])$IDRSSD))
    
    centry <- table(df[match(setdiff(subset(df, df$Date == dd[i])$IDRSSD,subset(df, df$Date == dd[i-1])$IDRSSD), df$IDRSSD),]$FED) #table of entry by  fed region
    if (length(centry) > 0 ) {
      for (j in length(centry)){
        df1$fed_entry[df1$Date == dd[i] & df1$FED == as.numeric(names(centry)[j])] <- centry[[j]]
      }
    }
    
    cexit <- table(df[match(setdiff(subset(df, df$Date == dd[i-1])$IDRSSD,subset(df, df$Date == dd[i])$IDRSSD), df$IDRSSD),]$FED) #table of entry by  fed region
    if (length(cexit) > 0 ) {
      for (j in length(cexit)){
      df1$fed_exit[df1$Date == dd[i] & df1$FED == as.numeric(names(cexit)[j])] <- cexit[[j]]
      }
    }
  }
  
  df1$fed_exit[is.na(df1$fed_exit)] <- 0
  df1$fed_entry[is.na(df1$fed_entry)] <- 0
  df <- df1
  rm(df1)
  
  # Creating # of periods before treatment
  lab_prod <- read_excel("D:/Research/DW lending empirical/Data/labor_prod.xls")
  lab_prod$Date <- as.factor(lab_prod$Date)
  df1 <- left_join(df,lab_prod)
  df <- df1; rm(df1)
  df$dw_freq[is.na(df$dw_freq)] <- 0
  
  #Merge the FDIC institution data with the main table (adds # of offices)
  df1 <- left_join(df, data.frame(IDRSSD = inst$FED_RSSD, num_off = inst$OFFICES, bkclass = inst$BKCLASS, f31 = inst$FORM31, offdom = inst$OFFDOM, offfor = inst$OFFFOR, interstate = inst$STMULT))
  
  #Save File ----
  write.csv(df,"D:\\Research\\DW lending empirical\\Data\\merged.csv")
