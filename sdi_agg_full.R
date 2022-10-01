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
q <- c("0331","0630","0930","1231")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")
list <- list.files(path="D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_20091231", pattern=".csv")

sdc <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_20091231/",list[1]),show_col_types = FALSE))

for (i in 2:length(list)) {
  sdc <- sdc %>% full_join(
    data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_20091231/",list[i]),show_col_types = FALSE)[-1,]),
    by = "fed_rssd", suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("..."))
}
sdc <- data.frame(sapply(sdc,as.numeric)); sdc$Date <- ymd(paste0(2009,q[4]))


# For all other years


for (i in 1:length(y)) {
  for(j in 1:length(q)) {
    list <- list.files(path=paste0("D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_",y[i],q[j]), pattern=".csv")
    sdcn <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_",y[i],q[j],"/",list[1]),show_col_types = FALSE))
    for (k in 2:length(list)) {
      sdcn <- sdcn %>% full_join(
        data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/All_Reports_",y[i],q[j],"/",list[k]),show_col_types = FALSE)[-1,]),
        by = "fed_rssd", suffix = c("",".y")) %>% select(-ends_with(".y"), -contains("..."))
    }
    sdcn <- data.frame(sapply(sdcn,as.numeric))
    sdcn$Date <- ymd(paste0(y[i],q[j]))
    
    sdc <- smartbind(sdc,sdcn)
    rm(sdcn)
  }
}
rm(i,j,k,list)

setnames(sdc, old = c('fed_rssd'), new = c('IDRSSD'))
sdc <- sdc[,colSums(is.na(sdc))<nrow(sdc)]


write.csv(sdc,"D:\\Research\\DW lending empirical\\Data\\sdc_full.csv")
