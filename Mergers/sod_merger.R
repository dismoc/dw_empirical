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

# For Q4 2009
list <- list.files(path="D:/Research/DW lending empirical/Data/SOD/", pattern=".csv")

sod <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/SOD/",list[1]),show_col_types = FALSE))

for (i in 2:length(list)) {
  sodn <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/SOD/",list[i]),show_col_types = FALSE))
  sod <- smartbind(sod,sodn)
}

base <- left_join(aggregate(DEPSUMBR ~ STALPBR + YEAR + RSSDHCR, sod, FUN = sum),
  aggregate(DEPSUMBR ~ YEAR + RSSDHCR, sod, FUN = sum), by=c('RSSDHCR', 'YEAR'))
base$dep_share <- base$DEPSUMBR.x/base$DEPSUMBR.y
base <- subset(base, base$RSSDHCR >0)[,c('STALPBR','YEAR','RSSDHCR','dep_share')]
base <- spread(base, STALPBR, dep_share)
base <- left_join(base,sod[,c('RSSDHCR','CERT','YEAR')],by=c('RSSDHCR','YEAR'))
base <- base[!duplicated(base[c("RSSDHCR","YEAR")]),]

data.frame(base,rep(c('03-31','06-30','09-30','12-31')))
base <- smartbind(base,base); base <- smartbind(base,base)

base <- pdata.frame(base,index = c('RSSDHCR','YEAR'))

q <- c("/03/31","/06/30","/09/30","/12/31")
base <- data.frame(base, rep(q,nrow(base)/4))
base$Date <- as.Date(paste0(base$YEAR,base$rep.q..nrow.base..4.))
base <- pdata.frame(data.frame(base),index = c('RSSDHCR','Date'))
base <- base[ , -which(names(base) %in% c("rep.q..nrow.base..4."))]

for (i in 3:ncol(base)-1){
  base[,i] <- lead(base[,i],2)
}

base$Date <- as.Date(base$Date)
base$RSSDHCR <- as.numeric(as.character(base$RSSDHCR))


sci <- data.frame(read_delim("D:/Research/DW lending empirical/Data/SOD/CI/coincident-revised.csv",show_col_types = FALSE))
sci$time <- as.Date(timeLastDayInQuarter(as.Date(sci$Date,'%m/%d/%Y')))
sci <- sci[seq(1, nrow(sci), 3), ][2:ncol(sci)]
list <- names(sci)

for (i in 1:(ncol(sci)-1)) {
  sci[,i] <- c(diff(sci[,i]),NA)
}

join <- data.frame(base[,c(1:2,62:63)], 
                   mapply("*", base[intersect(names(base), names(sci))],
                          sci[intersect(names(base), names(sci))]))
join <- data.frame(join[,1:4], exposure = rowSums(join[5:ncol(join)], na.rm = TRUE))
join <- join[!is.na(join$RSSDHCR),]



rm(sod, sodn, sci, base)


write.csv(join,"D:\\Research\\DW lending empirical\\Data\\sod_merged.csv")
