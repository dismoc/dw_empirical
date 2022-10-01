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

# Reading in data ----
dwdat <- data.frame(read_excel("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_2010_q3.xls"))[c(1:15)]
dwdat_2 <- data.frame(read_excel("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_2010_q4.xls",skip=3))[c(1:15)]
colnames(dwdat_2) <- colnames(dwdat)
dwborrow <- rbind(dwdat,dwdat_2); rm(dwdat_2)

q <- c("q1","q2","q3","q4")
y <- c("2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")

for (i in 2:8) {
  dwdatq1 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[1],".xls"),skip=3)[c(1:15)]; colnames(dwdatq1) <- colnames(dwdat)
  dwdatq2 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[2],".xls"),skip=3)[c(1:15)]; colnames(dwdatq2) <- colnames(dwdat)
  dwdatq3 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[3],".xls"),skip=3)[c(1:15)]; colnames(dwdatq3) <- colnames(dwdat)
  dwdatq4 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[4],".xls"),skip=3)[c(1:15)]; colnames(dwdatq4) <- colnames(dwdat)

  dwborrow <-rbind(dwborrow,dwdatq1); rm(dwdatq1)
  dwborrow <-rbind(dwborrow,dwdatq2); rm(dwdatq2)
  dwborrow <-rbind(dwborrow,dwdatq3); rm(dwdatq3)
  dwborrow <-rbind(dwborrow,dwdatq4); rm(dwdatq4)
  }


for (i in 9:10) {
  dwdatq1 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[1],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq1) <- colnames(dwdat)
  dwdatq2 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[2],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq2) <- colnames(dwdat)
  dwdatq3 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[3],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq3) <- colnames(dwdat)
  dwdatq4 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[i],"_",q[4],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq4) <- colnames(dwdat)
  
  dwborrow <-rbind(dwborrow,dwdatq1); rm(dwdatq1)
  dwborrow <-rbind(dwborrow,dwdatq2); rm(dwdatq2)
  dwborrow <-rbind(dwborrow,dwdatq3); rm(dwdatq3)
  dwborrow <-rbind(dwborrow,dwdatq4); rm(dwdatq4)
}

dwdatq1 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[11],"_",q[1],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq1) <- colnames(dwdat)
dwdatq2 <- read_excel(paste0("D:/Research/DW lending empirical/Data/dw_borrow/dw_data_",y[11],"_",q[2],".xlsx"),skip=3)[c(1:15)]; colnames(dwdatq2) <- colnames(dwdat)
dwborrow <-rbind(dwborrow,dwdatq1); 
dwborrow <-rbind(dwborrow,dwdatq2);

rm(dwdatq1); rm(dwdat)


# Cleaning Data ----

dwborrow$Loan.date <- as.Date(dwborrow$Loan.date)
dwborrow$Maturity.date <- as.Date(dwborrow$Maturity.date)
dwborrow$Repayment.date <- as.Date(dwborrow$Repayment.date)

dwborrow$Year <- year(dwborrow$Loan.date)

dwborrow$Type.of.credit <- ifelse(dwborrow$Type.of.credit == "Primary Credit*", "Primary Credit", dwborrow$Type.of.credit)
dwborrow$Type.of.credit <- ifelse(dwborrow$Type.of.credit == "Secondary Credit*", "Secondary Credit", dwborrow$Type.of.credit)
dwborrow$Type.of.credit <- ifelse(dwborrow$Type.of.credit == "Seasonal Credit*", "Seasonal Credit", dwborrow$Type.of.credit)

#Stylized Facts ----
  
  # Number of borrowings in each quarter that is listed as primary credit
  dwborrow$Date <- data.frame(timeLastDayInQuarter(dwborrow$Loan.date, format = "%Y-%m-%d", zone = "", FinCenter = ""))$GMT.x..i..
  ggplot(aggregate(Term ~ Type.of.credit + Year, dwborrow, FUN = sum), aes(x = Year, y=Term, color=Type.of.credit)) + geom_line()

  # Plotting the frequency of borrowing.
  p1 <- dwborrow %>% count(Borrower.ABA.number)
  p1 <- subset(p1, n >0)
  p1 <- data.frame(table(p1$n))
  p1$Var1 <- as.numeric(p1$Var1)
  p1 <-data.frame(Frequency = 1:31, Count = c(p1$Freq[1:30],  colSums(p1[p1$Var1 > 30,])[2] ))
  ggplot(p1, aes(x=Frequency, y = Count)) + geom_point()

write.csv(dwborrow,"D:\\Research\\DW lending empirical\\Data\\dwborrow1.csv")
