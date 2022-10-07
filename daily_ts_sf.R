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

# Import ----
ppb <- read_csv("D:/Research/DW lending empirical/Data/ppp_daily.csv")
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")

# Transformation ----
dwborrow1 <- dwborrow %>% count(Loan.date)
dwborrow <- aggregate(Loan.amount ~ Loan.date, dwborrow, FUN = sum)
dwborrow <- subset(full_join(dwborrow,dwborrow1), as.Date(Loan.date) >= as.Date('2020-01-01'))

sf <- full_join(ppb,dwborrow, by=c('DateApproved' = 'Loan.date'))
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

# Creation ----
sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, sf$InitialApprovalAmount)
sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) >= as.Date('2020-08-10'), 0, sf$InitialApprovalAmount)
sf$quant_week_avg <- rollapply(sf$InitialApprovalAmount, 7, mean, na.rm=TRUE, fill = NA, partial=4)
sf$dw_quant_avg <- rollapply(sf$Loan.amount, 7, mean, na.rm=TRUE, fill = NA, partial=4)
sf$id <- 1
sf$signal <- ifelse(as.Date(sf$DateApproved) >= as.Date('2020-03-16') & as.Date(sf$DateApproved) <= as.Date('2020-03-21'), 1, 0)
sf$preppp <- ifelse(as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, 1)


# Figures ----
ggplot(sf) +
  geom_line(aes(x = DateApproved, y = quant_week_avg/3, colour ='PPP')) +
  geom_line(aes(x = DateApproved, y = dw_quant_avg, colour ='DW')) +
  scale_y_continuous(name = "Moving Avg of DW Loan", sec.axis = sec_axis(~.*3, name="Moving Avg of PPP Loan")) +
  labs(x="Date") + theme(legend.position = c(.9, .9))

ggplot(sf) +
  geom_line(aes(x = DateApproved, y = log(quant_week_avg+1), colour ='PPP')) +
  geom_line(aes(x = DateApproved, y = log(dw_quant_avg+1), colour ='DW')) +
  labs(x="Date") + theme(legend.position = c(.9, .9))


#Regressions ----
dict1 <- c('log(InitialApprovalAmount)' = 'PPP', 'log(quant_week_avg)' = 'Avg Weekly PPP', 'log(Loan.amount)' = 'DW Quant',
           'log(dw_quant_avg)' = 'Avg Weekly DW Quant')

r1 <- feols(log(Loan.amount) ~ log(l(InitialApprovalAmount)) + log(l(Loan.amount)), sf, panel.id = ~id + DateApproved, se='iid')
r3 <- feols(log(dw_quant_avg) ~ log(quant_week_avg), sf, panel.id = ~id + DateApproved, se='iid')
etable(r1,r3, dict= dict1,
       drop = '(Intercept)',
       se = 'white',
       tex = F)
