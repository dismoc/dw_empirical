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


# Creating Defs table

defs <- data.frame(read.csv("D:/Research/DW lending empirical/Data/call_report/cr_defs.csv"))
defs$Code <- defs$ï..Code
defs <- defs[, c('Code','Item.Name','Description')]


list <- list.files(path="D:/Research/DW lending empirical/Data/fdic_sdi/Defs", pattern=".csv")

d1 <- data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/Defs/",list[1]),show_col_types = FALSE, skip = 1))

for (i in 2:length(list)) {
  d1 <- smartbind(d1,
            data.frame(read_delim(paste0("D:/Research/DW lending empirical/Data/fdic_sdi/Defs/",list[i]),show_col_types = FALSE, skip = 1)))
}
d1 <- d1[,2:4]
setnames(d1, old = colnames(d1), new = c('Item.Name', 'Code', 'Description'))

def <- data.frame(Code = c(defs$Code, d1$Code), full_var = c(defs$Item.Name, d1$Item.Name), desc = c(defs$Description, d1$Description))
rm(d1,defs)

write.csv(def, "D:\\Research\\DW lending empirical\\Data\\defs.csv")
