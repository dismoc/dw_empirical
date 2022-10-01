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

# Dependent variables from Acharya Mora (2015) ----
1664312390406:df$int_ltd_ann <- Winsorize((df$RIADA517/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, large time deposits (implicit)
1664312390864:df$int_cd_ann <- Winsorize(((df$RIAD4508 + df$RIAD0093)/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, core deposits (implicit)
1664312391291:df$qdep_growth <- Winsorize((df$RCON2200-lag(df$RCON2200))/lag(df$RCON2200),probs = c(0.01, 0.99), na.rm = TRUE) #deposit growth rates
1664312391957:df$qcordep_growth <- Winsorize((df$RCON2215 + df$RCON6810 + df$RCON0352 + df$RCON6648 - (lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)))/
                                               1664312391961:(lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)),probs = c(0.01, 0.99), na.rm = TRUE) #core deposit growth rates
1664312393706:df$ci_loans <- ifelse(is.na(df$RCON1766) == TRUE, df$RCON1763+df$RCON1764, df$RCON1766)
1664312394377:df$ci_loans_growth <- Winsorize((ifelse(is.na(df$RCON1766) == TRUE, df$RCON1763+df$RCON1764, df$RCON1766) -
                                                 1664312394379:ifelse(is.na(lag(df$RCON1766)) == TRUE, lag(df$RCON1763)+lag(df$RCON1764), lag(df$RCON1766)))/
                                                1664312394381:ifelse(is.na(lag(df$RCON1766)) == TRUE, lag(df$RCON1763)+lag(df$RCON1764), lag(df$RCON1766)),probs = c(0.01, 0.99), na.rm = TRUE)
1664312395338:df$loans <- df$RCON2122
1664312395340:df$loans_growth <- Winsorize((df$RCON2122 - lag(df$RCON2122))/lag(df$RCON2122),probs = c(0.01, 0.99), na.rm = TRUE)

1664482713719:# CAMELS - Duchin Sosyura (2014)----
1664482713721:# Capital Adequacy
  1664482713722:df$ca <- Winsorize(df$RCON8274/df$RCONA223, probs=c(.01,.99),na.rm = TRUE)
1664482714931:#Asset quality
  1664482714933:df$aq <- -(df$RCON1407 + df$RCON1403)/df$RCON2122


1664312395920:# Capital ratios - Duchin Sosyura (2014) ----
1664310004918:df$t1riskrat <- df$RCON7206
1664310004920:df$totriskrat <- df$RCON7205
1664310004921:df$eqcaprat <- df$RCONG105/df$RCON2170

1664312402351:# Bank fundamentals - Duchin Sosyura (2014) ----
1664482543424:df$size <- Winsorize(log(df$RCON2170),probs=c(.01,.99), na.rm = TRUE)
1664482543446:df$age <- year(df$Date) - year(df$ESTYMD)
1664482543491:join <- read_csv("D:/Research/DW lending empirical/Data/sod_merged.csv")
1664482543882:join$CERT <- as.numeric(join$CERT)
1664482543922:df <- df %>% left_join(join, by = c('cert' = 'CERT', 'Date')) %>%
  1664482543924:select(-contains("..."))

1664482457311:df$lent_total <- (df$RCONB987 + df$RCONB989)
1664482457315:df$lent_total[IsZero(df$lent_total)] = NA
1664482457320:df$int_paid_ib <- Winsorize(df$RIAD4020*100/(df$lent_total),probs = c(0.05, 0.95), na.rm = TRUE)

1664482457336:df$borr_total <- (df$RCONB993 + df$RCONB995)
1664482457339:df$borr_total[IsZero(df$borr_total)] = NA
1664482457344:df$int_got_ib <- Winsorize(df$RIAD4180*100/(df$borr_total),probs = c(0.05, 0.95), na.rm = TRUE)
