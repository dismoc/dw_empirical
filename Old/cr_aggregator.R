# Libraries ----
library(tidyverse)
library(mlr3verse)
library(plm)
library(readxl)
library(dplyr)
library('DataCombine')
library('stargazer')
library('simpleboot')
library('Rpdb')
library('data.table')
library(ggplot2)
library(gtools)
library(lubridate)

# Reading in data ----

q <- c("0331","0630","0930","1231")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")


crdat_a <- data.frame(read.table("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RCA 12312009.txt",sep="\t",header=TRUE))[-1,]
crdat_a <- data.frame(sapply(crdat_a,as.numeric)); crdat_a$Date <- ymd(paste0(2009,q[4]))
crdat_k <- data.frame(read_delim("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RCK 12312009.txt")[-1,])
crdat_k <- data.frame(sapply(crdat_k,as.numeric)); crdat_k$Date <- ymd(paste0(2009,q[4]))
crdat_e <- data.frame(read.table("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RCE 12312009.txt",sep="\t",header=TRUE))[-1,]
crdat_e <- data.frame(sapply(crdat_e,as.numeric)); crdat_e$Date <- ymd(paste0(2009,q[4]))
crdat_r <- data.frame(read_delim("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RCR 12312009(1 of 2).txt")[-1,])
crdat_r <- data.frame(sapply(crdat_r,as.numeric)); crdat_r$Date <- ymd(paste0(2009,q[4]))
crdat_0 <- data.frame(read.table("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RC 12312009.txt",sep="\t",header=TRUE))[-1,]
crdat_0 <- data.frame(sapply(crdat_0,as.numeric)); crdat_0$Date <- ymd(paste0(2009,q[4]))
crdat_i <- data.frame(read_delim("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Schedule RI 12312009.txt")[-1,])
crdat_i <- data.frame(sapply(crdat_i,as.numeric)); crdat_i$Date <- ymd(paste0(2009,q[4]))


crdat_p <- data.frame(read_delim("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules 12312009/FFIEC CDR Call Bulk POR 12312009.txt"))


for (i in 1:11) { 
  for(j in 1:4) {
    cr_a <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCA ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    cr_a <- data.frame(sapply(cr_a,as.numeric)); cr_a$Date <- ymd(paste0(y[i],q[j]))
    cr_k <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCK ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    cr_k <- data.frame(sapply(cr_k,as.numeric)); cr_k$Date <- ymd(paste0(y[i],q[j]))
    cr_e <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCE ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    cr_e <- data.frame(sapply(cr_e,as.numeric)); cr_e$Date <- ymd(paste0(y[i],q[j]))
    cr_0 <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RC ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    cr_0 <- data.frame(sapply(cr_0,as.numeric)); cr_0$Date <- ymd(paste0(y[i],q[j]))
    cr_p <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Bulk POR ",q[j],y[i],".txt"),show_col_types = FALSE))
    cr_i <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RI ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    cr_i <- data.frame(sapply(cr_i,as.numeric)); cr_i$Date <- ymd(paste0(y[i],q[j]))
    
    if (i < 5) {
      cr_r <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCR ",q[j],y[i],"(1 of 2).txt"),show_col_types = FALSE)[-1,])
      cr_r <- data.frame(sapply(cr_r,as.numeric)); cr_r$Date <- ymd(paste0(y[i],q[j]))
    } else if (i == 5) {
      cr_r <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCRIA ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
      cr_r <- data.frame(sapply(cr_r,as.numeric)); cr_r$Date <- ymd(paste0(y[i],q[j]))
    } else {
      cr_r <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule RCRI ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
      cr_r <- data.frame(sapply(cr_r,as.numeric)); cr_r$Date <- ymd(paste0(y[i],q[j]))
    }
    
    #colnames(cr_a) <- colnames(crdat_a); 
    
    crdat_a <-smartbind(crdat_a,cr_a); rm(cr_a)
    crdat_k <-smartbind(crdat_k,cr_k); rm(cr_k)
    crdat_e <-smartbind(crdat_e,cr_e); rm(cr_e)
    crdat_r <-smartbind(crdat_r,cr_r); rm(cr_r)
    crdat_0 <-smartbind(crdat_0,cr_0); rm(cr_0)
    crdat_i <-smartbind(crdat_i,cr_i); rm(cr_i)
    
    crdat_p <- smartbind(crdat_p,cr_p); rm(cr_p)
  }
}


inst <- data.frame(read.csv("D:/Research/DW lending empirical/Data/institutions.csv"))
inst1 <- inst[,c('FED_RSSD','FED','FDICREGN')]
setnames(inst1, old = c('FED_RSSD'), new = c('IDRSSD'))


# Cleaning Data ----

crdat_0$Assets <- pmax(crdat_0$RCON2170,crdat_0$RCFD2170, na.rm = TRUE)
crdat_0$Liab <- pmax(crdat_0$RCON2948,crdat_0$RCFD2948, na.rm = TRUE)
crdat_0$rrp_sold <- pmax(crdat_0$RCFDB995,crdat_0$RCONB995, na.rm = TRUE)
crdat_0$rrp_bought <- pmax(crdat_0$RCFDB989,crdat_0$RCONB989, na.rm = TRUE)

crdat_a$Reserves <- pmax(crdat_a$RCFD0010,crdat_a$RCON0010,na.rm = TRUE)

crdat_r$t1_lev <- pmax(crdat_r$RCFA7204,crdat_r$RCFD7204,crdat_r$RCOA7204,crdat_r$RCON7204,na.rm = TRUE)
crdat_r$rw_cap_rat <- pmax(crdat_r$RCFA7205,crdat_r$RCFD7205,crdat_r$RCOA7205,crdat_r$RCON7205,crdat_r$RCFW7205,crdat_r$RCOW7205,na.rm = TRUE)

crp <- crdat_p[!duplicated(crdat_p$IDRSSD), ]


cr <- data.frame(crdat_0$IDRSSD,crdat_0$Date)
setnames(cr, old = c('crdat_0.IDRSSD','crdat_0.Date'), new=c('IDRSSD','Date'))
cr$Date <- as.factor(cr$Date)
crdat_a$Date <- as.factor(crdat_a$Date)
crdat_i$Date <- as.factor(crdat_i$Date)

cr <- left_join(cr, crdat_0[, c('IDRSSD', 'Date', 'Assets', 'Liab', 'RCONB528', 'RCONB987', 'RCONB993','rrp_sold','rrp_bought')], all.y = TRUE)
cr <- left_join(cr, crdat_i[, c('IDRSSD', 'Date', 'RIAD4180','RIAD4020')], all.y = TRUE)
cr <- left_join(cr, crdat_a[, c('IDRSSD', 'Date', 'Reserves')], all.y = TRUE)
cr <- left_join(cr, crdat_r[, c('IDRSSD', 'Date', 't1_lev', 'rw_cap_rat')], all.y = TRUE)
cr <- left_join(cr, crdat_e[, c('IDRSSD', 'Date', 'RCON2215')], all.y = TRUE)
cr <- left_join(cr, crp[, c('IDRSSD', 'Primary.ABA.Routing.Number','Financial.Institution.Name','Financial.Institution.State')], all.y = TRUE)
cr <- left_join(cr, inst1, all.y = TRUE)

setnames(cr, old = c('RCONB528','RCONB987','RCONB993','RCON2215','RIAD4180','RIAD4020'), new = c('total_loans','ff_sold','ff_bought','total_deposit','int_expense','int_income'))
setnames(cr, old = c('Primary.ABA.Routing.Number','Financial.Institution.Name','Financial.Institution.State'), new=c('ABA_routing','name','state'))

cr <- subset(cr, is.na(cr$name) == FALSE)

#Adding columns ----
cr$asset_sold <- cr$ff_sold + cr$rrp_bought
cr$asset_bought <- cr$ff_bought + cr$rrp_sold
cr$i_borrow <- 360*cr$int_expense/cr$asset_bought
cr$i_lent <- 360*cr$int_income/cr$asset_sold



write.csv(cr,"D:\\Research\\DW lending empirical\\Data\\call_rep.csv")
