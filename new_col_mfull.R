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
library('fuzzyjoin')
library('lubridate')

# Important Info
df$bigsmall <- ifelse(df$RCON2170 >= 1000000, 'Large', 'Small')


# Dependent variables from Acharya Mora (2015) ----
df$int_ltd_ann <- Winsorize((df$RIADA517/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, large time deposits (implicit)
df$int_cd_ann <- Winsorize(((df$RIAD4508 + df$RIAD0093)/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, core deposits (implicit)
df$qdep_growth <- Winsorize((df$RCON2200-lag(df$RCON2200))/lag(df$RCON2200),probs = c(0.01, 0.99), na.rm = TRUE) #deposit growth rates
df$qcordep_growth <- Winsorize((df$RCON2215 + df$RCON6810 + df$RCON0352 + df$RCON6648 - (lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)))/
                                               1664312391961:(lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)),probs = c(0.01, 0.99), na.rm = TRUE) #core deposit growth rates
df$ci_loans <- ifelse(is.na(df$RCON1766) == TRUE, df$RCON1763+df$RCON1764, df$RCON1766)
df$ci_loans_growth <- Winsorize((ifelse(is.na(df$RCON1766) == TRUE, df$RCON1763+df$RCON1764, df$RCON1766) -
                                                 1664312394379:ifelse(is.na(lag(df$RCON1766)) == TRUE, lag(df$RCON1763)+lag(df$RCON1764), lag(df$RCON1766)))/
                                                1664312394381:ifelse(is.na(lag(df$RCON1766)) == TRUE, lag(df$RCON1763)+lag(df$RCON1764), lag(df$RCON1766)),probs = c(0.01, 0.99), na.rm = TRUE)
df$loans <- df$RCON2122
df$loans_growth <- Winsorize((df$RCON2122 - lag(df$RCON2122))/lag(df$RCON2122),probs = c(0.01, 0.99), na.rm = TRUE)

# CAMELS - Duchin Sosyura (2014)----
# Capital Adequacy
  df$ca <- Winsorize(df$RCON8274/df$RCONA223, probs=c(.01,.99),na.rm = TRUE)
#Asset quality
  df$aq <- -(df$RCON1407 + df$RCON1403)/df$RCON2122


# Capital ratios - Duchin Sosyura (2014) ----
df$t1riskrat <- df$RCON7206
df$totriskrat <- df$RCON7205
df$eqcaprat <- df$RCONG105/df$RCON2170

# Bank fundamentals - Duchin Sosyura (2014) ----
df$size <- Winsorize(log(df$RCON2170),probs=c(.01,.99), na.rm = TRUE)
df$age <- year(df$Date) - year(df$ESTYMD)
join <- read_csv("D:/Research/DW lending empirical/Data/sod_merged.csv")
join$CERT <- as.numeric(join$CERT)
join$Date <- as.Date(join$Date)
df$Date <- as.Date(df$Date)
df <- left_join(data.frame(df), data.frame(join), by= c('cert' = 'CERT', 'Date'))
  
df$lent_total <- (df$RCONB987 + df$RCONB989)
df$lent_total[IsZero(df$lent_total)] = NA
df$int_paid_ib <- Winsorize(df$RIAD4020*100/(df$lent_total),probs = c(0.05, 0.95), na.rm = TRUE)

df$borr_total <- (df$RCONB993 + df$RCONB995)
df$borr_total[IsZero(df$borr_total)] = NA
df$int_got_ib <- Winsorize(df$RIAD4180*100/(df$borr_total),probs = c(0.05, 0.95), na.rm = TRUE)
