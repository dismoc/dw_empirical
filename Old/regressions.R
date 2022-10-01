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

# Reading in data ----
df <- read_csv("D:/Research/DW lending empirical/Data/merged.csv")

effr <- read_csv("D:/Research/DW lending empirical/Data/effr_range.csv")

  #Adding additional series - reserve asset ratio and reserve deposit ratio, 
  df$reserve_asset_ratio <- df$Reserves/df$Assets
  df$reserve_deposit_ratio <- df$Reserves/df$total_deposit
  
  df$lent_q <- ifelse(df$ff_sold > 0, 1, 0)
  df$dw_freq[is.na(df$dw_freq)] <- 0
  df$borrow_bin <- ifelse(df$dw_freq >0, 1, 0)
  df$size <- ifelse(df$Assets > 1000000, "Large", "Small")
  df$lent_q <- ifelse(df$ff_sold > 0, 1, 0)
  df <- df[,c(2:ncol(df))]
  
#Finalized  data for regressions
db <- subset(df, total_deposit > 0 & Assets > 0), index=c('IDRSSD','Date')


# EFFR Range
effr$Date = as.Date(effr$`Effective Date`)
ggplot(effr, aes(x = `Effective Date`, y=Range)) + geom_line() + geom_line(aes(x=`Effective Date`,y='Rate (%)'))+
  labs(x = "Year", y = "Ratio") +
  theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
        axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))


# Stylized Facts Using dwaccess_1 ----

  df1 <- left_join(left_join(aggregate(ff_sold ~ dwaccess_1 + Date, db, FUN = sum), aggregate(Reserves ~ dwaccess_1 + Date, db, FUN = sum)), aggregate(ff_bought ~dwaccess_1 + Date, db, FUN = sum))
  df1$lend_share <- df1$ff_sold/df1$Reserves
  df1$buy_share <- df1$ff_bought/df1$Reserves
  
  ggplot(df1, aes(x = Date, y=lend_share,  group = as.factor(dwaccess_1), colour=as.factor(dwaccess_1))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
     geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
     labs(x = "Year", y = "Ratio") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks with access to the discount window lend a lot less on the fed funds market 

  
  df2 <- left_join(aggregate(IDRSSD ~ Date + dwaccess_1, db, FUN = length), aggregate(state ~ Date, db, FUN = length))
  df2$dw_share <- df2$IDRSSD/df2$state
  ggplot(df2, aes(x = Date, y=IDRSSD,  colour=as.factor(dwaccess_1))) + geom_point() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks without access is shutting down faster than banks with access, this has huge welfare implications.
  
  df3 <- aggregate(Assets ~ Date, db, FUN = sum)
  ggplot(df3, aes(x = Date, y=Assets)) + geom_line() +
    labs(x = "Year", y = "Assets (in Thousands)") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #assets held by bank have been increasing 70% from 2010 to 2020, while CPI have gone up by 30%
  
  df4 <- aggregate(Assets ~ Date + dwaccess_1, db, FUN = sum)
  ggplot(df4, aes(x = Date, y= Assets,  colour=as.factor(dwaccess_1))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Assets (in Thousands)") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #All of that growth have been driven by banks with access to the discount window (87%) and not banks w/o access (-4.1%)
  
  df5 <- left_join(aggregate(Reserves/Assets ~ Date + dwaccess_1, db, FUN = mean),aggregate(Reserves/total_deposit ~ Date + dwaccess_1, db, FUN = mean))
  setnames(df5, old = 'Reserves/Assets', new = 'ra_ratio')
  ggplot(df5, aes(x = Date, y=ra_ratio,  colour=as.factor(dwaccess_1))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Reserve to Asset Ratio") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks who cannot access the discount window hold a higher level of reserves on average than banks that can.
  
  
  df6 <- left_join(aggregate(IDRSSD ~ Date + FED, db, FUN = length),aggregate(dwaccess_1 ~ Date + FED, db, FUN = sum))
  df6$dw_share <- df6$dwaccess_1/df6$IDRSSD
  df6 <- left_join(df6, aggregate(i_borrow ~ Date + FED, db, FUN = mean))
  ggplot(df6, aes(x = Date, y=dw_share,  colour=as.factor(FED))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(FED)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Share of Banks w/ Access") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Share of banks that can borrow at each fed reserve district.

  df7 <- left_join(aggregate(lent_q ~ dwaccess_1 + Date, db, FUN = sum), aggregate(ff_sold ~ dwaccess_1 + Date, db, FUN = length))
  df7$sold_share <- df7$lent_q/df7$ff_sold
  ggplot(df7, aes(x = Date, y=sold_share,  colour=as.factor(dwaccess_1))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Lending Share of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #The share of banks that lend on the FFM is mostly banks without access to DW
  
  df8 <- subset(df2, dwaccess_1 == 1)
  ggplot(df8, aes(x = Date, y=dw_share,  colour=as.factor(dwaccess_1))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Share w/ Access") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #share of banks that have access to DW increasing from 28% in 2010 to 33% in 2020

  df9 <- left_join(aggregate(i_borrow ~ dwaccess_1 + Date, db, FUN = mean), aggregate(i_lent ~ dwaccess_1 + Date, db, FUN = mean))
  ggplot(df9, aes(x = Date, y=i_lent,  colour=as.factor(dwaccess_1))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  
  df10 <- left_join(left_join(aggregate(i_borrow*ff_bought ~ dwaccess_1 + Date, db, FUN = sum), aggregate(ff_bought ~ dwaccess_1 + Date, db, FUN = sum)),aggregate(FEDFUNDS ~ Date, db, FUN = mean))
  df10$weighted_borrow <- df10$`i_borrow * ff_bought`/df10$ff_bought
  ggplot(df10, aes(x = Date, y=weighted_borrow,  colour=as.factor(dwaccess_1))) + geom_line() + scale_colour_discrete(guide = 'none') + geom_line(aes(x=Date, y=FEDFUNDS), colour = 'black', alpha=4) +
    geom_dl(aes(label = as.factor(dwaccess_1)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  

  
# Stylized Facts Using dwaccess_2 ----
  
  df1 <- left_join(left_join(aggregate(ff_sold ~ dwaccess_2 + Date, db, FUN = sum), aggregate(Reserves ~ dwaccess_2 + Date, db, FUN = sum)), aggregate(ff_bought ~dwaccess_2 + Date, db, FUN = sum))
  df1$lend_share <- df1$ff_sold/df1$Reserves
  df1$buy_share <- df1$ff_bought/df1$Reserves
  
  ggplot(df1, aes(x = Date, y=lend_share,  group = as.factor(dwaccess_2), colour=as.factor(dwaccess_2))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Ratio") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks with access to the discount window lend a lot less on the fed funds market 
  
  
  df2 <- left_join(aggregate(IDRSSD ~ Date + dwaccess_2, db, FUN = length), aggregate(state ~ Date, db, FUN = length))
  df2$dw_share <- df2$IDRSSD/df2$state
  ggplot(df2, aes(x = Date, y=IDRSSD,  colour=as.factor(dwaccess_2))) + geom_point() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks without access is shutting down faster than banks with access, this has huge welfare implications.
  
  df3 <- aggregate(Assets ~ Date, db, FUN = sum)
  ggplot(df3, aes(x = Date, y=Assets)) + geom_line() +
    labs(x = "Year", y = "Assets (in Thousands)") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #assets held by bank have been increasing 70% from 2010 to 2020, while CPI have gone up by 30%
  
  df4 <- aggregate(Assets ~ Date + dwaccess_2, db, FUN = sum)
  ggplot(df4, aes(x = Date, y= Assets,  colour=as.factor(dwaccess_2))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Assets (in Thousands)") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #All of that growth have been driven by banks with access to the discount window (87%) and not banks w/o access (-4.1%)
  
  df5 <- left_join(aggregate(Reserves/Assets ~ Date + dwaccess_2, db, FUN = mean),aggregate(Reserves/total_deposit ~ Date + dwaccess_2, db, FUN = mean))
  setnames(df5, old = 'Reserves/Assets', new = 'ra_ratio')
  ggplot(df5, aes(x = Date, y=ra_ratio,  colour=as.factor(dwaccess_2))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Reserve to Asset Ratio") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Banks who cannot access the discount window hold a higher level of reserves on average than banks that can.
  
  
  df6 <- left_join(aggregate(IDRSSD ~ Date + FED, db, FUN = length),aggregate(dwaccess_2 ~ Date + FED, db, FUN = sum))
  df6$dw_share <- df6$dwaccess_2/df6$IDRSSD
  df6 <- left_join(df6, aggregate(i_borrow ~ Date + FED, db, FUN = mean))
  ggplot(subset(df6, df6$FED != 13), aes(x = Date, y=dw_share,  colour=as.factor(FED))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(FED)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Share of Banks w/ Access") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #Share of banks that can borrow at each fed reserve district.
  
  df7 <- left_join(aggregate(lent_q ~ dwaccess_2 + Date, db, FUN = sum), aggregate(ff_sold ~ dwaccess_2 + Date, db, FUN = length))
  df7$sold_share <- df7$lent_q/df7$ff_sold
  ggplot(df7, aes(x = Date, y=sold_share,  colour=as.factor(dwaccess_2))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Lending Share of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #The share of banks that lend on the FFM is mostly banks without access to DW
  
  df8 <- subset(df2, dwaccess_2 == 1)
  ggplot(df8, aes(x = Date, y=dw_share,  colour=as.factor(dwaccess_2))) + geom_line() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "Share w/ Access") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #share of banks that have access to DW increasing from 28% in 2010 to 33% in 2020
  
  df9 <- left_join(aggregate(i_borrow ~ dwaccess_2 + Date, db, FUN = mean), aggregate(i_lent ~ dwaccess_2 + Date, db, FUN = mean))
  ggplot(df9, aes(x = Date, y=i_lent,  colour=as.factor(dwaccess_2))) + geom_smooth() + scale_colour_discrete(guide = 'none') +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  
  df10 <- left_join(left_join(aggregate(i_borrow*ff_bought ~ dwaccess_2 + Date, db, FUN = sum), aggregate(ff_bought ~ dwaccess_2 + Date, db, FUN = sum)),aggregate(FEDFUNDS ~ Date, db, FUN = mean))
  df10$weighted_borrow <- df10$`i_borrow * ff_bought`/df10$ff_bought
  ggplot(df10, aes(x = Date, y=weighted_borrow,  colour=as.factor(dwaccess_2))) + geom_line() + scale_colour_discrete(guide = 'none') + geom_line(aes(x=Date, y=FEDFUNDS), colour = 'black', alpha=4) +
    geom_dl(aes(label = as.factor(dwaccess_2)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  
  df11 <- aggregate(cbind(agg_exit, agg_entry) ~ Date, db3, FUN = mean)
  df11 <- melt(df11, id = c("Date"), variable.names = c("Exit", "Entry"))
  ggplot(df11, aes(x = Date, y=value,  colour=as.factor(variable))) + geom_line() + scale_colour_discrete(guide = 'none')+
    geom_dl(aes(label = as.factor(variable)), method = list(dl.combine("last.points")), cex = 1) +
    labs(x = "Year", y = "# of Banks") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  #'*Huge spike of new bank entries in 2012, exits are pretty standard. This plots aggregate entries and exit by quarter*
  
  
# Stylized facts about interest rates across districts, evidence of market segmentation?  ----
  ag1 <- aggregate(i_borrow ~ FED + Date, db3, FUN = mean)
  
  ggplot(subset(ag1, FED == 1) - subset(ag1,FED == 11), aes(x = Date, y=i_borrow)) + geom_point() + 
    labs(x = "Year", y = "Ratio") +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16))
  
# AR(1) models of different federal reserve district to test whether int rates are sig diff
  plot2 <- data.frame("FED" = 1:12, 'coef' = 1, 'lower' = 1, 'upper' = 1)
  for (i in 1:12) {
    AR <- arima(data.frame(subset(ag1, FED == i)[3]),order = c(1,0,0))
    plot2$coef[i] <- coef(AR)[1]
    plot2$lower[i] <- coef(AR)[1] - 1.96*sqrt(AR$var.coef)[1,1]
    plot2$upper[i] <- coef(AR)[1] + 1.96*sqrt(AR$var.coef)[1,1]
  }
  
  plot(plot2[1:2])
  arrows(x0=plot2[1], y0=plot2[3], x1=plot2[1], y1=plot2[4], code=3, angle=90, length=0.1)
# Running Regressions ----
dict1 <- c("log(ff_sold)"="FF Lent", "log(ff_bought)"="FF Borrow", "log(Reserves)"="Reserves",
           "log(Assets)" = "Assets", "log(Liab)"="Liabilities","dwaccess_1"="Access",
           "log(i_borrow)" = "FF Borrowed Interest", 'log(dw_share)' = "DW Share", "Date" = "Quarter",
           "IDRSSD" = "Individual", "FED" = "District", "log(reserve_asset_ratio)" = "Reserve Asset Ratio",
           "log(rw_cap_rat)" = "Risk-Weighted Cap. Ratio", "log(t1_lev)" = "T1 Leverage Ratio",
           "dwaccess_2:dwaccess_1" = "Access")


  #Robustness check 1 - dwacess_2 as x
  reg1 <- feols(log(ff_sold) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat + log(Reserves) +  dwaccess_2 | Date , db,
                cluster='FED') #allowing access to the DW decreases the amount of fed funds the bank lends on market. This is probably due to less reserve holdings.
  reg3 <- feols(log(Reserves) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat + dwaccess_2 | Date, db,
                cluster='FED') #allowing access to the DW decrease the amount of reserves held by the banks.
  reg2 <- feols(log(ff_sold) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat +  dwaccess_2 | Date + IDRSSD , db,
                cluster='FED') #allowing access to the DW decreases the amount of fed funds the bank lends on market. This is probably due to less reserve holdings.
  reg4 <- feols(log(Reserves) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat + dwaccess_2 | Date + IDRSSD, db,
                cluster='FED') #allowing access to the DW decrease the amount of reserves held by the banks.
  
  etable(reg1, reg2, reg3, reg4,
         dict = dict1, tex = F)
  
  

  reg4 <- feols(log(i_borrow) ~ log(Assets) + log(Assets/Liab) +  log(Reserves/Assets) + log(ff_bought/Assets) + dw_freq | Date + IDRSSD, db,
             index = c('IDRSSD', 'Date'), model = 'within', cluster = 'IDRSSD') # Allowing access to the discount window allow banks to borrow at a lower rate on the federal funds market.
  
  reg5 <- feols(log(i_borrow) ~ log(Assets) + log(Assets/Liab) +  log(Reserves/Assets) + ff_bought/Assets + dw_freq| Date + FED, db,
                index = c('IDRSSD', 'Date'), cluster = 'IDRSSD') #
  etable(reg4,reg5)
  
  
  db2 <- pdata.frame(df6, index=c('FED','Date'))
  reg6 <- feols(log(i_borrow) ~ dw_share | FED + Date, db2, cluster='FED'); etable(reg6, dict=dict1, tex=F)
  
  
# Running Regressions - LASSO ----
  db1 <- df[,c(4:16,22:23)]
  db1$lshareofassets <- db1$total_loans/db1$Assets
  db1$res_ratio <- db1$Reserves/db1$Assets
  db1$sold_share <- db1$asset_sold/db1$Assets
  db1$buy_share <- db1$asset_bought/db1$Assets
  db1$logassets <- log(db1$Assets)
  x <- data.matrix(db1[,c(11:12,16:20)])
  b_lamb <- cv.glmnet(x,df$dwaccess_1,alpha=1)$lambda.min
  model <- glmnet(x,df$dwaccess_1,alpha=1, lambda = b_lamb)
  coef(model)
  
# Running Regressions - DID: bank moral hazard using fixest package (good to go)----
  temp <- aggregate(dwaccess_2 ~ IDRSSD, db, FUN = sum)
  temp$time_to_treat <- length(unique(db$Date)) - temp$dwaccess_2
  db_did <- left_join(db,temp[,c(1,3)]); rm(temp)
  db_did$numq <- as.numeric(as.yearqtr(db_did$Date))*4-min(as.numeric(as.yearqtr(db_did$Date))*4)
  
  
  # reserve asset ratio
  reg1 <- feols(reserve_asset_ratio ~  log(Assets) + log(Liab) + t1_lev + rw_cap_rat + dwaccess_2:dwaccess_1  | IDRSSD + Date, data = db_did)
  # fed funds volume lent
  reg2 <- feols(log(ff_sold) ~  log(Assets) + log(Liab) + t1_lev + rw_cap_rat + dwaccess_2:dwaccess_1 | IDRSSD + Date, data = db_did)
  #'*This regression shows that there exist a moral hazard effect when allowing banks to draw from the DW. banks who use DW hold less assets (1 percentage point).*
  
  # FOR ROBUSTNESS, USE THE SUN AND ABRAHAM (2020) TO ACCOUNT FOR STAGGERED DID BIAS FOUND BY GOODMAN-BACON.
  
  reg3 <- feols(reserve_asset_ratio ~ sunab(time_to_treat,numq) + log(Assets) + log(Liab) + t1_lev + rw_cap_rat | IDRSSD + Date, data = db_did)
  reg3 <- summary(reg3, agg = "att")
  
  reg4 <- feols(log(ff_sold) ~ sunab(time_to_treat,numq) + log(Assets) + log(Liab) + t1_lev + rw_cap_rat| IDRSSD + Date, data = db_did)
  reg4 <- summary(reg4, agg = "att")
  etable(reg1,reg3,reg2, reg4, dict = dict1, tex=FALSE)
  
   
# Running Regressions - ra_share on district DW access using IV, plotted as subsets ----
  db3 <- left_join(db,df6[c(1,2,5)])
  plot1 <- data.frame("Year" = ylist[1:11], "No Access" = 1,"Access" = 2)
  ylist <- unique(year(db3$Date))
  #plotting coefficients as a function of time
  for (i in 1:(length(ylist)-1)) {
    temp1 <- subset(db3, year(db3$Date) >= ylist[i] )
    reg <- feols(reserve_asset_ratio ~ log(Assets) + log(dw_share) + dwaccess_2:log(dw_share) | IDRSSD + Date , temp1,
                  cluster='FED');  #allowing access to the DW decreases the amount of fed funds the bank lends on market. This is probably due to less reserve holdings.
    plot1[i,2] <- coef(reg)[2]
    plot1[i,3] <- coef(reg)[2] + coef(reg)[3]
  }
  
  
  ggplot(melt(plot1, id.vars = 'Year'), aes(x = Year, y=value,  group = as.factor(variable), colour=as.factor(variable))) + geom_point()  +
    labs(x = "Year", y = "Coef") + 
    scale_x_continuous(breaks = seq(2009, 2019, by = 2)) +
    theme(axis.text.x = element_text(size = 14), axis.title.x = element_text(size = 16),
          axis.text.y = element_text(size = 14), axis.title.y = element_text(size = 16),
          legend.position = c(.8,.9), title=element_blank())
  rm(reg)
  #Banks with access to the discount window lend a lot less on the fed funds market 
  #elasticitiy of banks iwth access to DW to a change in the DW share is greater than those without access.
  
  reg_int <- feols(reserve_asset_ratio ~ log(Assets) + log(dw_share) + log(dw_share):dwaccess_2 | IDRSSD + Date , db3,
               cluster='FED')
  
  etable(reg_int, dict=dict1, tex = T) 
  
# Running Regressions - ra_share on district DW access using IV ----
  
  reg1 <- feols(log(reserve_asset_ratio) ~ log(Assets) + log(dw_share)  | IDRSSD + Date, db3,
                cluster='FED')
  
  reg2 <- feols(log(reserve_asset_ratio) ~ log(Assets) | Date + IDRSSD| log(dw_share) ~ fed_exit  , db3,
               cluster='FED')
  
  reg3 <- feols(log(reserve_asset_ratio) ~ log(Assets) + log(rw_cap_rat) | Date + IDRSSD| log(dw_share) ~ fed_exit  , db3,
                cluster='FED')
  reg4 <- feols(log(reserve_asset_ratio) ~ log(Assets) + log(rw_cap_rat) + log(t1_lev)| Date + IDRSSD| log(dw_share) ~ fed_exit  , db3,
                cluster='FED')
  
  
  etable(reg1, reg2, reg3, reg4, dict=dict1, tex = F) 
  #These two sets of regressions mean that when you control for individual fixed effect, as more banks are allowed access to DW, the average reserve holdings of each bank increases.
  #When taking away the individual fixed effect, the reserve/asset holding of each bank decreases due to the 'moral hazard' problem.
  #allowing access to the DW increases the reserve to asset ratio of banks.
  
  
  #Running another IV to confirm elasticity from 2010-2015 is different than 2015-2020
  reg1 <- feols(log(reserve_asset_ratio) ~ log(Assets) | Date + IDRSSD| log(dw_share) ~ fed_delt  , db3,
                cluster='FED', subset = year(db3$Date) < 2015)
  reg2 <- feols(log(reserve_asset_ratio) ~ log(Assets) | Date + IDRSSD| log(dw_share) ~ fed_delt   , db3,
                cluster='FED', subset = year(db3$Date) >= 2015)
  etable(reg1, reg2, dict=dict1, tex = T)
  
  
# Running Regression - Evidence of market segmentation
  reg1 <- feols(log(i_borrow) ~ log10(Assets) + i(FED) + log(t1_lev)  + log(rw_cap_rat) |  Date, db3,
                cluster='FED')
  etable(reg1)  #'*large banks borrow at a lower rate, All FED coefs are compared to NY FED. It seems NY district banks borrow at a higher rate than other banks (just a few basis points). *
  
# Running Regressions - Show that simultaneity is not an issue by using a first stage probit model to predict dw_borrow
  
  #This tests whether changes in reserve to asset ratio is a good predictor 
  
  reg_logit1 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ borrow_bin + log(Assets) + log(Liab) + t1_lev + rw_cap_rat  | Date + IDRSSD,
                      data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED")
  reg_logit2 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat  | Date + IDRSSD 
                     | borrow_bin ~ size + l(log(Reserves),1) + l(t1_lev,1) + l(rw_cap_rat,1)  + l(reserve_asset_ratio,1) + ff_bought + l(ff_bought), 
        data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED"); 
  etable(reg_logit1, reg_logit2, fitstat= ~ivf + r2)

  
  reg_logit1 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ dw_freq + log(Assets) + log(Liab) + t1_lev + rw_cap_rat | Date + IDRSSD,
                      data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED")
  reg_logit2 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat| Date + IDRSSD 
                      | dw_freq ~ size + l(log(Reserves),1) + l(t1_lev,1) + l(rw_cap_rat,1)  + l(reserve_asset_ratio,1), 
                      data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED"); 
  etable(reg_logit1, reg_logit2, fitstat= ~ivf + r2)
  
  reg_logit1 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ dwaccess_2 + log(Assets) + log(Liab) + t1_lev + rw_cap_rat + l(reserve_asset_ratio) + l(ff_sold) | Date + IDRSSD,
                      data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED")
  reg_logit2 <- feols(c(reserve_asset_ratio,log(ff_sold)) ~ log(Assets) + log(Liab) + t1_lev + rw_cap_rat + l(reserve_asset_ratio) + l(ff_sold) | Date + IDRSSD 
                      | dwaccess_2 ~ size + l(log(Reserves),1) + l(t1_lev,1) + l(rw_cap_rat,1)  + l(reserve_asset_ratio,1), 
                      data=panel(db, panel.id = c('IDRSSD','Date'), duplicate.method="first"), cluster = "FED"); 
  etable(reg_logit1, reg_logit2, fitstat= ~ivf + r2)
  