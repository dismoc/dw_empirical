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

# Import

pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")

sod <- smartbind(data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/SOD/ALL_2020.csv"),show_col_types = FALSE)),
                 data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/SOD/ALL_2021.csv"),show_col_types = FALSE)))


base <- aggregate(DEPSUMBR ~ STALPBR + YEAR + RSSDID, sod, FUN = sum)
base <- subset(base, RSSDID > 0)
base <- base %>% group_by(STALPBR) %>% mutate(total_state_dep =  sum(DEPSUMBR))
base$s_expo <- base$DEPSUMBR/base$total_state_dep
bb1 <- reshape(data.frame(base[base$YEAR == 2020,c('STALPBR','RSSDID','s_expo')]), v.names = 's_expo', idvar = "RSSDID", timevar = "STALPBR", direction = "wide"); bb1$YEAR <- 2020
bb2 <- reshape(data.frame(base[base$YEAR == 2021,c('STALPBR','RSSDID','s_expo')]), v.names = 's_expo', idvar = "RSSDID", timevar = "STALPBR", direction = "wide"); bb2$YEAR <- 2021
colnames(bb1) <- sub('s_expo.',"",colnames(bb1)); colnames(bb2) <- sub('s_expo.',"",colnames(bb2)); 
base <- smartbind(bb1,bb2); rm(bb1, bb2)

b2 <- aggregate(InitialApprovalAmount ~ DateApproved + OriginatingLenderState,dfp, sum)
b2 <- reshape(b2, idvar = "DateApproved", timevar = "OriginatingLenderState", direction = "wide")
colnames(b2) <- sub('InitialApprovalAmount.',"",colnames(b2))
b2$YEAR <- year(b2$DateApproved)

nf <- data.frame(IDRSSD = rep(unique(base$RSSDID), length(b2$DateApproved)), Date = b2$DateApproved)
nf2 <- data.frame(IDRSSD = rep(unique(base$RSSDID), length(b2$DateApproved)), Date = b2$DateApproved)
nf <- nf[order(nf$IDRSSD,nf$Date),]; nf2 <- nf2[order(nf2$IDRSSD,nf2$Date),]
nf <- left_join(nf, b2, by=c('Date'='DateApproved')); nf2 <- left_join(nf2, base, by=c('IDRSSD' = 'RSSDID'))
nf2 <- subset(nf2, year(Date) == YEAR)
nf <- nf[(paste0(nf$IDRSSD,nf$Date) %in% paste0(nf2$IDRSSD,nf2$Date)),]

nf1 <- data.frame(nf[1:2] ,mapply("*", nf[intersect(names(nf), names(nf2[,3:(ncol(nf2)-1)]))],
        nf2[intersect(names(nf2), names(nf[,3:(ncol(nf)-1)]))]))
nf1$BInstr <- rowSums(nf1[,3:ncol(nf1)], na.rm=TRUE)
nf1 <- nf1[,c('IDRSSD','Date','BInstr')]
nf1 <- left_join(nf1, unique(sod[,c('RSSDID','CERT')]), by=c('IDRSSD' = 'RSSDID'))

write.csv(nf1,"D:\\Research\\DW lending empirical\\Data\\binstr.csv")
rm(base,b2,nf,nf2,nf1)
