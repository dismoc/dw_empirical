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
library("R.utils")

# Merging data ----

q <- c("0331","0630","0930","1231")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
n <- c("RC", "RCA", "RCB", "RCCI","RCCII","RCE","RCEI","RCF","RCG","RCH","RCK","RCL","RCM","RCN","RCP", "RCR", "RCRI", "RCRIA", "RCRIB","RCRII","RI","RIA","RIBI","RIBII", "RIC", "RICI", "RICII")
m <- c("","(1 of 2)", "(2 of 2)", "(1 of 3)", "(2 of 3)", "(3 of 3)", "(1 of 4)", "(2 of 4)", "(3 of 4)", "(4 of 4)")

crdat <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[4],"2009","/FFIEC CDR Call Schedule ", n[1]," ", q[4], "2009",".txt"),show_col_types = FALSE)[-1,])
for (k in 2:length(n)){
  for (v in 1:length(m)){
    try(
      crdat <- crdat %>% full_join(
      data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[4],"2009","/FFIEC CDR Call Schedule ",n[k]," ",q[4],"2009",m[v],".txt"),show_col_types = FALSE)[-1,]),
                       by = "IDRSSD", suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("...")),
    silent = TRUE)
  }
}
crdat <- data.frame(sapply(crdat,as.numeric)); crdat$Date <- ymd(paste0(2009,q[4]))


for (i in 1:length(y)) {
  for(j in 1:length(q)) {
    crdat_n <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule ", n[1]," ",q[j],y[i],".txt"),show_col_types = FALSE)[-1,])
    for (k in 2:length(n)){
      for (v in 1:length(m)){
        try(crdat_n <- crdat_n %>% full_join(
            data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/FFIEC CDR Call Schedule ",n[k]," ",q[j],y[i],m[v],".txt"),show_col_types = FALSE)[-1,]),
            by = "IDRSSD", suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("...")), silent = TRUE)
      }
    }
    crdat_n <- data.frame(sapply(crdat_n,as.numeric))
    crdat_n$Date <- ymd(paste0(y[i],q[j]))
    
    crdat <- smartbind(crdat,crdat_n)
    rm(crdat_n)
  }
}
rm(i,j,k,v)

# Cleaning ----
crdat$Date <- as.Date(crdat$Date)
crdat <- crdat[,colSums(is.na(crdat))<nrow(crdat)]
crdat1 <- crdat

defs <- data.frame(read.csv("D:/Research/DW lending empirical/Data/call_report/cr_defs.csv"))[,1:9]
defs$Code <- defs$ï..Code
defs <- defs[defs$Code %in% colnames(crdat1),]

# deleting columns that mean the same thing, just duplicated reports
for (i in 1:length(unique(defs$Item.Name))) {
  n <- unique(defs[defs$Item.Name %in% unique(defs$Item.Name)[i],]$Code)
  if (length(n) > 1) {
    #List sort
    for (j in 1:length(n)) {
      k[j] = sum(is.na(eval(parse(text=paste0("crdat1$",n[j])))))
    }
    
    list <- data.frame(n,k)
    list <- list[order(list$k),]
    
    for (k in 2:length(n)) {
      crdat1[, colnames(crdat1) %in% list[1,1]] <- coalesce(eval(parse(text=paste0("crdat1$",list[1,1]))), eval(parse(text=paste0("crdat1$",list[k,1]))))
      crdat1 <- crdat1 %>% select(-contains(list[k,1]))
    }
  }
}

crdat <- crdat1
rm(crdat1, i, j, n, k, list)


inst <- data.frame(read.csv("D:/Research/DW lending empirical/Data/institutions.csv"))
inst1 <- inst[,c('FED_RSSD','FED','FDICREGN')]
setnames(inst1, old = c('FED_RSSD'), new = c('IDRSSD'))
crdat <- left_join(crdat, inst1, by= 'IDRSSD')
rm(inst, inst1)


write.csv(crdat,"D:\\Research\\DW lending empirical\\Data\\call_rep_full.csv")
