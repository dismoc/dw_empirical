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
library('vars')

# Import ----
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
df <- read_csv("D:/Research/DW lending empirical/Data/merged_cov.csv")

# Convert DW to quarterly
dd <- subset(dwborrow, Type.of.credit == 'Primary Credit' & Date <= as.Date('2020-06-30')); dd$term <- as.numeric(dd$Repayment.date - dd$Loan.date)
dd$tot_borrow <- dd$Loan.amount*dd$term
dd <- aggregate(tot_borrow ~ Borrower.ABA.number + Date,dd,sum)
dd$Borrower.ABA.number <- as.numeric(dd$Borrower.ABA.number)
dd <- dd %>% group_by(Borrower.ABA.number) %>% complete(Date = seq.Date(from = as.Date('2010-09-30'), to = as.Date('2020-03-31'), by='quarter'))

temp <- df[,c('IDRSSD','Date','Primary.ABA.Routing.Number','RCONB993','RCONB995')]

var <- left_join(temp, dd, by=c('Primary.ABA.Routing.Number' = 'Borrower.ABA.number', 'Date'))
var[is.na(var$tot_borrow)==T,'tot_borrow'] <- 0
var$tot_borrow <- var$tot_borrow/1000

var[4:6] <- log(var[4:6]+1)
var <- subset(var, Date <= as.Date('2020-06-30'))


p1 <- list()
p1[[1]] <- feols(tot_borrow ~ l(tot_borrow,1:4) + l(RCONB993,1:4) + l(RCONB995,1:4) | IDRSSD + Date, var, panel.id=c('IDRSSD','Date'))
p1[[2]] <- feols(RCONB993 ~ l(tot_borrow,1:4) + l(RCONB993,1:4) + l(RCONB995,1:4) | IDRSSD + Date, var, panel.id=c('IDRSSD','Date'))
p1[[3]] <- feols(RCONB995 ~ l(tot_borrow,1:4) + l(RCONB993,1:4) + l(RCONB995,1:4) | IDRSSD + Date, var, panel.id=c('IDRSSD','Date'))

pred <- data.frame(var[p1[[1]]$obs_selection$obsRemoved,], dw_fit = p1[[1]]$fitted.values, ff_fit = p1[[2]]$fitted.values, rrp_fit = p1[[3]]$fitted.values)
pred <- subset(pred, Date == as.Date('2020-06-30'))
pred$dw_err <- pred$tot_borrow - pred$dw_fit
pred$ff_err <- pred$RCONB993 - pred$ff_fit
pred$rrp_err <- pred$RCONB995 - pred$rrp_fit
pred <- pred[,c('IDRSSD','dw_fit','ff_fit','rrp_fit','dw_err','ff_err','rrp_err')]

write.csv(pred,"D:\\Research\\DW lending empirical\\Data\\var_instr.csv")
