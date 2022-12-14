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
df$bigsmall2 <- ifelse(df$RCON2170 >= 300000, 'Large', 'Small')

df <- df %>% group_by(IDRSSD) %>% arrange(Date) %>% mutate(t1loan = dplyr::lead(RCON2122))
df$new_loans <- ifelse(df$t1loan - (df$RCON2122 - pmax(df$RCONA570,(df$RCONA247 - df$RCONA571))) < 0, 0, df$t1loan - (df$RCON2122 - pmax(df$RCONA570,(df$RCONA247 - df$RCONA571))))

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
join <- join[!is.na(join$CERT),][2:6]; join <- join[!is.na(join$exposure),]
df$Date <- as.Date(as.character(df$Date))
df <- left_join(df, join, by= c('cert' = 'CERT', 'Date'))
rm(join)


df$lent_total <- (df$RCONB987 + df$RCONB989)
df$lent_total[IsZero(df$lent_total)] = NA
df$int_paid_ib <- Winsorize(df$RIAD4020*100/(df$lent_total),probs = c(0.05, 0.95), na.rm = TRUE)

df$borr_total <- (df$RCONB993 + df$RCONB995)
df$borr_total[IsZero(df$borr_total)] = NA
df$int_got_ib <- Winsorize(df$RIAD4180*100/(df$borr_total),probs = c(0.05, 0.95), na.rm = TRUE)

# Dependent variables from Acharya Mora (2015) ----
df <- pdata.frame(df, c('IDRSSD','Date'))
df$int_ltd_ann <- Winsorize((df$RIADA517/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, large time deposits (implicit)
df$int_cd_ann <- Winsorize(((df$RIAD4508 + df$RIAD0093)/df$RCONA514)*400,probs = c(0.01, 0.99), na.rm = TRUE) #interest rate, core deposits (implicit)
df$qdep_growth <- Winsorize((df$RCON2200-lag(df$RCON2200))/lag(df$RCON2200),probs = c(0.01, 0.99), na.rm = TRUE) #deposit growth rates
df$qcordep_growth <- Winsorize((df$RCON2215 + df$RCON6810 + df$RCON0352 + df$RCON6648 - (lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)))/
                                 (lag(df$RCON2215) + lag(df$RCON6810) + lag(df$RCON0352) + lag(df$RCON6648)),probs = c(0.01, 0.99), na.rm = TRUE) #core deposit growth rates

df$ci_loans <- ifelse(is.na(df$RCON1766) == TRUE, df$RCON1763+df$RCON1764, df$RCON1766)
df$ci_loans_growth <- Winsorize((df$ci_loans - lag(df$ci_loans))/lag(df$ci_loans), probs = c(.01,.99), na.rm = TRUE)
df$loans <- df$RCON2122
df$loans_growth <- Winsorize((df$RCON2122 - lag(df$RCON2122))/lag(df$RCON2122),probs = c(0.01, 0.99), na.rm = TRUE)

df$ppp_bin <- ifelse(df$RCONLG26 > 0, 1, 0)
df$ppp_advance <- df$RCONLL59 + df$RCONLL60
df$ppp_uncovered <- df$RCONLG27 - df$RCONLL59 - df$RCONLL60
df$nonppp_loans <- ifelse(is.na(df$RCONLG27) == TRUE, df$RCON2122, df$RCON2122 - df$RCONLG27)
df$new_nppp_loans <- ifelse(is.na(df$RCONLG27) == TRUE, df$new_loans, df$new_loans - df$RCONLG27)
df$nonppp_loans_growth <- Winsorize((df$nonppp_loans - lag(df$nonppp_loans))/lag(df$nonppp_loans), probs=c(.01,.99), na.rm = TRUE)

df$loan_reserve_ratio <- df$RCON2122/df$RCON0010
df$nppp_loan_reserve_ratio <- df$nonppp_loans/df$RCON0010

df <- df %>%
  group_by(IDRSSD, Date) %>%
  slice(n())
df <- data.frame(df)
df$IDRSSD <- as.numeric(as.character(df$IDRSSD))

#Save File ----
write.csv(df,"D:\\Research\\DW lending empirical\\Data\\merged_cov.csv")

