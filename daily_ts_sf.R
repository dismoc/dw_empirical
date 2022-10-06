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

ppb <- read_delim("D:/Research/DW lending empirical/Data/ppp_borrower/query_20211221_082058.csv", 
                  delim = "|", escape_double = FALSE, trim_ws = TRUE)
ppb <- aggregate(InitialApprovalAmount ~ dateapproved, ppb, FUN = sum)

dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
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


sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, sf$InitialApprovalAmount)
sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) >= as.Date('2020-08-10'), 0, sf$InitialApprovalAmount)
sf$quant_week_avg <- rollapply(sf$InitialApprovalAmount, 5, mean, na.rm=TRUE, fill = NA, partial=4)
sf$dw_quant_avg <- rollapply(sf$Loan.amount, 5, mean, na.rm=TRUE, fill = NA, partial=4)
sf$id <- 1
sf$signal <- ifelse(as.Date(sf$DateApproved) >= as.Date('2020-03-16') & as.Date(sf$DateApproved) <= as.Date('2020-03-21'), 1, 0)
sf$preppp <- ifelse(as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, 1)

tikz(file = "plot_test.tex", width = 5, height = 5)
ggplot(sf) +
  geom_line(aes(x = DateApproved, y = quant_week_avg/10, colour ='PPP')) +
  geom_line(aes(x = DateApproved, y = dw_quant_avg, colour ='DW')) +
  scale_y_continuous(name = "Weekly Avg of DW Loan", sec.axis = sec_axis(~.*10, name="Weekly Avg of PPP Loan")) +
  labs(x="Date") + theme(legend.position = c(.9, .9))
dev.off()


dict1 <- c('log(InitialApprovalAmount)' = 'PPP', 'log(quant_week_avg)' = 'Avg Weekly PPP', 'log(Loan.amount)' = 'DW Quant',
           'log(dw_quant_avg)' = 'Avg Weekly DW Quant')

r1 <- feols(log(Loan.amount) ~ log(InitialApprovalAmount), sf, panel.id = ~id + DateApproved, se='iid')
r2 <- update(r1, Loan.amount ~ InitialApprovalAmount)
r3 <- feols(log(dw_quant_avg) ~ log(quant_week_avg), sf, panel.id = ~id + DateApproved, se='iid')
r4 <- update(r3, dw_quant_avg ~  quant_week_avg)
etable(r1,r2,r3,r4, dict= dict1,
       drop = '(Intercept)',
       se = 'white',
       tex = F)
