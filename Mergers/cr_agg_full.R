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

list <- dir(path=paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[4],"2009"), pattern=".txt")
df <- map(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[4],"2009/",list),
          read.delim, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, header=TRUE, skip=0)

for (i in 2:length(df)) {
  df[[i]] <- df[[i]][-1,]
  df[[i]] <- type.convert(df[[i]])
  df[[i]]$IDRSSD <- as.numeric(df[[i]]$IDRSSD)
}

crdat <- df %>% reduce(left_join, suffix = c("",".y"))
crdat$Date <- as.Date(ymd(paste0(2009,q[4])))
crdat <- crdat[,colSums(is.na(crdat))<nrow(crdat)]

for (i in 1:length(y)) {
  for(j in 1:length(q)) {
    print(paste0('Starting Loop: ',q[j],y[i]))
    list <- dir(path=paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/"), pattern=".txt")
    
    df <- map(paste0("D:/Research/DW lending empirical/Data/call_report/FFIEC CDR Call Bulk All Schedules ",q[j],y[i],"/",list),
              read.delim, stringsAsFactors = FALSE, check.names = FALSE, row.names = NULL, header=TRUE, skip=0)
    for (k in 2:length(df)){
      df[[k]] <- df[[k]][-c(1),]
      df[[k]] <- type.convert(df[[k]], as.is=TRUE)
      df[[k]]$IDRSSD <- as.numeric(df[[k]]$IDRSSD)
    }
    crdatn <- df %>% reduce(left_join, suffix = c("",".y"))
    crdatn$Date <- as.Date(ymd(paste0(y[i],q[j])))
    crdatn <- crdatn[,colSums(is.na(crdatn))<nrow(crdatn)]
    crdat <- smartbind(crdat,crdatn)
  }
}

rm(i,j,k, df, crdatn)


# Cleaning ----
crdat$Date <- as.Date(crdat$Date)

crdat1 <- crdat
defs <- data.frame(read.csv("D:/Research/DW lending empirical/Data/call_report/cr_defs.csv"))[,1:9]
defs$Code <- defs$ï..Code
defs <- defs[defs$Code %in% colnames(crdat1),]

# deleting columns that mean the same thing, just duplicated reports
ls <- unique(defs$Item.Name)
for (i in 1:length(ls)) {
  print(paste0('Begin Loop: ',i))
  n <- unique(defs[defs$Item.Name %in% ls[i],]$Code)
  if (length(n) > 1) {
    #List sort
    k <- vector(length = length(n))
    for (j in 1:length(n)) {
      k[j] = sum(is.na(eval(parse(text=paste0('crdat$',n[j])))))
    }
    
    list <- data.frame(n,k)
    list <- list[order(list$k),]
    
    for (m in 2:length(n)) {
      crdat1[, colnames(crdat1) %in% list[1,1]] <- coalesce(as.numeric(eval(parse(text=paste0("crdat1$",list[1,1])))), as.numeric(eval(parse(text=paste0("crdat1$",list[m,1])))))
    }
    crdat1 <- crdat1 %>% select(-contains(list[2:length(n),1]))
    rm(k, list, j, m)
  }
  print(paste0('End Loop: ',i))
}
crdat1 <- crdat1 %>% select(-contains('text'))
crdat <- crdat1
rm(crdat1, i, j, n, k, list, ls)


inst <- data.frame(read.csv("D:/Research/DW lending empirical/Data/institutions.csv"))
inst1 <- inst[,c('FED_RSSD','FED','FDICREGN')]
setnames(inst1, old = c('FED_RSSD'), new = c('IDRSSD'))
crdat <- left_join(crdat, inst1, by= 'IDRSSD')
rm(inst, inst1, defs)


write.csv(crdat,"D:\\Research\\DW lending empirical\\Data\\call_rep_full.csv")
