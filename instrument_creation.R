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
sci <- data.frame(read_delim("D:/Research/DW lending empirical/Data/SOD/CI/coincident-revised.csv",show_col_types = FALSE))
cov <- data.frame(read_delim("D:/Research/DW lending empirical/Data/cimpact/county_level_covid.csv",show_col_types = FALSE))
citmatch <- data.frame(read_delim("D:/Research/DW lending empirical/Data/cimpact/uscities.csv",show_col_types = FALSE))
windex <- data.frame(read_delim("D:/Research/DW lending empirical/Data/BLS/BLS_weekly_economic_conditions_indicators.csv",show_col_types = FALSE))

# Instrument ----
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
b2$InitialApprovalAmount <- log(b2$InitialApprovalAmount)
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

rm(base,b2,nf,nf2)
# Monthly State Coincident Index ----
sc <- sci; sc$Date <- as.Date(sc$Date, '%m/%d/%Y')
sc <- subset(sc, Date >= '2020-01-01')
for (i in 2:(ncol(sc))) {
  sc[,i] <- c(diff(sc[,i]),NA)
}

base <- aggregate(DEPSUMBR ~ STALPBR + RSSDID, sod, FUN = sum) %>% group_by(RSSDID)
base <- base %>% group_by(RSSDID) %>% mutate(totdep = sum(DEPSUMBR))
base$depshare <- base$DEPSUMBR/base$totdep; btemp <- base
base$Date <- as.Date('2020-03-01')
base <- base %>% group_by(RSSDID) %>% complete(Date = seq.Date(as.Date('2020-03-01'), as.Date('2020-10-01'), by='month'))
base <- left_join(base[,c('RSSDID','Date')], btemp, by='RSSDID'); rm(btemp)
base <- reshape(data.frame(base[,c('RSSDID','Date','STALPBR','depshare')]), idvar = c("RSSDID",'Date'), timevar = "STALPBR", direction = "wide", v.names ="depshare")
colnames(base) <- sub('depshare.',"",colnames(base))
base <- data.frame(base[1:2] , mapply("*", base[intersect(names(base), names(sc[,2:(ncol(sc))]))],
                           sc[intersect(names(sc), names(base[,3:(ncol(base))]))]))
base$econexpo <- rowSums(base[,3:ncol(base)], na.rm = TRUE)
base <- base[,c('RSSDID','Date','econexpo')]
base <- base %>% group_by(RSSDID) %>% complete(Date = seq.Date(as.Date('2020-03-01'), as.Date('2020-10-01'), by='day')) %>% fill(econexpo)
nf1 <- left_join(nf1, base, by=c('IDRSSD' = 'RSSDID','Date'))
rm(base, sc)

# COVID Exposure ----
temp <- data.table(citmatch); 
temp <-temp[,list(wdens = weighted.mean(density,population)),by=combined_key]
temp <- left_join(aggregate(population ~ combined_key, citmatch, sum), temp)
cov1 <- left_join(cov,temp, c('Combined_Key' = 'combined_key'))
cov1 <- cov1[ , -which(names(cov1) %in% c('UID', 'iso2', 'iso3', 'code3','Admin2', 'Province_State', 'Country_Region','Lat','Long_'))]
colnames(cov1) <- sub('X',"",colnames(cov1))
cov1 <- melt(cov1, id.vars = c("Combined_Key","FIPS",'population','wdens'), variable.name = "Date")
cov1$Date <- as.Date(cov1$Date,'%m.%d.%Y')
cov1 <- subset(cov1, Date >= as.Date('2020-03-01') & Date <= as.Date('2020-10-01'))
cov1 <- data.frame(cov1) %>% group_by(Combined_Key) %>% mutate(new_cases = value - dplyr::lag(value,7, default=0)) %>% arrange(Combined_Key, Date)
cov1$ncasepdens <- cov1$wdens*cov1$new_cases/cov1$population

sodn <- sod; sodn$Combined_Key <- paste0(sod$CNTYNAMB,", ",sod$STNAMEBR, ", US")
base <- aggregate(DEPSUMBR ~ Combined_Key + RSSDID, sodn, FUN = sum) %>% group_by(RSSDID)
base <- base %>% group_by(RSSDID) %>% mutate(totdep = sum(DEPSUMBR))
base$depshare <- base$DEPSUMBR/base$totdep
base$Date <- as.Date('2020-03-01')
base <- base %>% group_by(RSSDID) %>% complete(Date = seq.Date(as.Date('2020-03-01'), as.Date('2020-10-01'), by='day')) %>% fill(depshare, Combined_Key)
base <- base[,c('RSSDID','Date','Combined_Key','depshare')]

base <- left_join(base, cov1[,c('Combined_Key','Date','ncasepdens')], by=c('Combined_Key','Date'))
base$covexpo <- base$depshare*base$ncasepdens
base <- aggregate(covexpo ~ RSSDID + Date, base, sum) %>% arrange(RSSDID, Date)
nf1 <- left_join(nf1, base, by=c('IDRSSD'= 'RSSDID','Date'))
rm(base, cov1, temp)

# Weekly Economic State level indicators
temp <- unique(data.frame(State = citmatch$state_id, sname = citmatch$state_name))
temp$sname <- sub(" ",".",temp$sname)
temp[nrow(temp) + 1,] <- c('Date','Week.ending')
wi <- windex; names(wi) <- temp$State[match(names(windex), temp$sname)]
wi$Date <- as.Date(wi$Date, format='%Y-%m-%d')
wi <- subset(wi, Date >= as.Date('2020-03-01'))
wi <- melt(wi, id.vars = c('Date'), variable.name = "State")

base <- aggregate(DEPSUMBR ~ STALPBR + RSSDID, sod, FUN = sum) %>% group_by(RSSDID)
base <- base %>% group_by(RSSDID) %>% mutate(totdep = sum(DEPSUMBR))
base$dshare <- base$DEPSUMBR/base$totdep
base$Date <- as.Date('2020-03-01')
base <- base[,c('STALPBR','RSSDID','dshare','Date')] %>% group_by(RSSDID) %>% complete(Date = seq.Date(as.Date('2020-03-01'), as.Date('2020-10-01'), by='day')) %>% fill(dshare, STALPBR)

base <- left_join(base, wi, by=c('Date','STALPBR' = 'State')) %>% fill(value)
base$eci <- base$value; base <- base[,c('RSSDID','Date','eci')]
nf1 <- left_join(nf1, base, by=c('IDRSSD' = 'RSSDID', 'Date'))
rm(wi, temp, base)
#Write File ----
write.csv(nf1,"D:\\Research\\DW lending empirical\\Data\\binstr.csv")
rm(nf1)
