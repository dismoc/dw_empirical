# Libraries ----
library(tidyverse)
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
library(glmnet)
library(directlabels)
library(vtable)
library(reshape)
library(ivmodel)
library(did)

# Importing and subsetting the data ----
df <- read_csv("D:/Research/DW lending empirical/Data/merged_full.csv")


db <- pdata.frame(subset(df, RCON2170 > 0 & RCON0010 > 0), index=c('IDRSSD','Date'))
db$dwborrow_bin <- ifelse(is.na(db$dwborrow_bin) == TRUE, 0, db$dwborrow_bin)
db$dwborrow_bin_tp1 <- lead(db$dwborrow_bin)

db <- db %>%
  select(IDRSSD, Date, dwaccess_1, dwborrow_bin_tp1, dwaccess_2, dwborrow_bin, everything())


#Remove duplicates
db <- db[!duplicated(db[c("IDRSSD","Date")]),]

db <- subset(db, as.Date(db$Date) > as.Date("2009-12-31") & as.Date(db$Date) < as.Date("2020-03-31"))
db$dwborrow_bin_tp1 <- ifelse(is.na(db$dwborrow_bin_tp1) == TRUE, 0, db$dwborrow_bin_tp1)



# Running the LASSO regression for variable selection ----

db$loanshareofassets <- db$RCONB528/db$RCON2170
db$res_ratio <- db$RCON0010/db$RCON2170
db$logassets <- log(db$RCON2170)

x <- data.matrix(db[,c(5:ncol(db))])
b_lamb <- cv.glmnet(x,db$dwborrow_bin_tp1,alpha=1, family = 'binomial')
model <- glmnet(x,db$dwborrow_bin_tp1,alpha=1, lambda = b_lamb$lambda.min, family = 'binomial')
coef(model)

result <- data.frame(Code = rownames(coef(model, s = 'lambda.min'))[coef(model, s = 'lambda.min')[,1]!= 0], 
           coef = coef(model, s = 'lambda.min')[coef(model, s = 'lambda.min')[,1]!= 0])
result <- left_join(result, def[!duplicated(def$Code),][,c('Code','full_var')], by='Code')
result <- result[order(result$coef),]

#use fitted best model to make predictions
y_predicted <- predict(model, s = b_lamb$lambda.min, newx = x)

#find SST and SSE
sst <- sum((db$dwborrow_bin - mean(db$dwborrow_bin))^2)
sse <- sum((y_predicted - db$dwborrow_bin)^2)

#find R-Squared
rsq <- 1 - sse/sst
rsq


# Running the second stage regression with fixed effects after choosing covariates given by LASSO
lsts <- match(result$Code[2:length(result$Code)],names(db))


reg1 <- feols(dwborrow_bin_tp1 ~ dwaccess_2 + .[result$Code[2:length(result$Code)]], data = db)

etable(feols(reserve_asset_ratio ~ dwborrow_bin | IDRSSD + Date, data=db),
  feols(f(reserve_asset_ratio) ~ 1|IDRSSD + Date|dwborrow_bin_tp1 ~ dwaccess_2 + .[result$Code[2:length(result$Code)]], data = db, panel.id = c('IDRSSD','Date')),
       fitstat= ~ivf + r2, cluster = 'FED.x')
