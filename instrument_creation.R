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

# Import ----

pppm <- read_csv("D:/Research/DW lending empirical/Data/ppp_bankmatched.csv")

sod <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/SOD/ALL_2019.csv"),show_col_types = FALSE))
dfp <- read_delim(paste0("D:/Research/DW lending empirical/Data/ppp_sba/",list[1]), show_col_types = FALSE) 


base <- aggregate(DEPSUMBR ~ STALPBR + YEAR + RSSDID, sod, FUN = sum)
base <- subset(base, RSSDID > 0)
base <- base %>% group_by(STALPBR) %>% mutate(total_state_dep =  sum(DEPSUMBR))
base$s_expo <- base$DEPSUMBR/base$total_state_dep
base <- reshape(data.frame(base[base$YEAR == 2019,c('STALPBR','RSSDID','s_expo')]), v.names = 's_expo', idvar = "RSSDID", timevar = "STALPBR", direction = "wide"); base$YEAR <- 2020
colnames(base) <- sub('s_expo.',"",colnames(base))


b2 <- data.frame(OriginatingLenderState = rep(unique(dfp$OriginatingLenderState),148) , DateApproved = rep(full_seq(c(as.Date('2020-03-31'),as.Date('2020-08-25')), period=1))) %>% 
  arrange(OriginatingLenderState, DateApproved)
temp <- data.frame(dfp)
temp$DateApproved <- as.Date(temp$DateApproved, '%m/%d/%Y')
b2 <- left_join(b2,aggregate(InitialApprovalAmount ~ DateApproved + OriginatingLenderState,temp, sum))
b2$InitialApprovalAmount <- b2$InitialApprovalAmount
b2 <- reshape(b2, idvar = "DateApproved", timevar = "OriginatingLenderState", direction = "wide")
colnames(b2) <- sub('InitialApprovalAmount.',"",colnames(b2))
b2$YEAR <- year(b2$DateApproved)

temp <- rep(b2$DateApproved, length(unique(base$RSSDID)))
temp <- temp[order(temp)]
nf <- data.frame(IDRSSD = rep(unique(base$RSSDID), length(b2$DateApproved)), Date = temp) %>% arrange(IDRSSD, Date)
nf2 <- nf
nf <- nf[order(nf$IDRSSD,nf$Date),]; nf2 <- nf2[order(nf2$IDRSSD,nf2$Date),]
nf <- left_join(nf, b2, by=c('Date'='DateApproved')); nf2 <- left_join(nf2, base, by=c('IDRSSD' = 'RSSDID'))
nf2 <- subset(nf2, year(Date) == YEAR)
nf <- nf[(paste0(nf$IDRSSD,nf$Date) %in% paste0(nf2$IDRSSD,nf2$Date)),]

nf1 <- data.frame(nf[1:2] ,mapply("*", nf[intersect(names(nf), names(nf2[,3:(ncol(nf2)-1)]))],
        nf2[intersect(names(nf2), names(nf[,3:(ncol(nf)-1)]))]))
nf1$BInstr <- rowSums(nf1[,3:ncol(nf1)], na.rm=TRUE)
nf1 <- nf1[,c('IDRSSD','Date','BInstr')]
nf1 <- left_join(nf1, unique(sod[,c('RSSDID','CERT')]), by=c('IDRSSD' = 'RSSDID'))
nf1 <- nf1[!duplicated(nf1[c("IDRSSD", "Date")]),]

write.csv(nf1,"D:\\Research\\DW lending empirical\\Data\\binstr.csv")
rm(base,b2,nf,nf2,nf1)
