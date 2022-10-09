# Libraries ----
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
library('xtable')
library('scales')

# Purpose is to see reseve / asset to fed lending in each district during covid quarters
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
conv <- data.frame(Name = unique(dwborrow$Lending.Federal.Reserve.district), FED = as.numeric(str_extract(unique(dwborrow$Lending.Federal.Reserve.district), "[[:digit:]]+")))

dfpost <- subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') )



# Some graphs about size of loans ----
agg <- left_join(left_join(left_join(left_join(aggregate(RCON0010 ~ Date + FED, cov, FUN = sum), 
                                               aggregate(RCON2170 ~ Date + FED, cov, FUN = sum)),
                                     aggregate(dw_quant ~ Date + FED, cov, FUN = sum)),
                           setnames(aggregate(RCON0010 ~ Date, cov, FUN = sum), old='RCON0010',new='total_asset')),
                 setnames(aggregate(dw_quant ~ Date, cov, FUN = sum), old='dw_quant',new='total_borrow'))

agg$total_borrow <- agg$total_borrow/1000; agg$dw_quant <- agg$dw_quant/1000
agg$'Asset Share' <- agg$RCON0010/agg$total_asset
agg$'Borrow Share' <- agg$dw_quant/agg$total_borrow

# Q4 2019 just total asset share
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2019-12-31'))[,c(2,8)],share,val,2:2); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1, aes(x=factor(Name, levels= Name), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")


# Q4 2019 comparing total asset share to borrowing share at the DW
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2019-12-31'))[,c(2,8,9)],share,val,2:3); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1[2:4], aes(x=factor(Name, levels=unique(Name)), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")

# Q1 2020 just total asset share
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2020-03-31'))[,c(2,8)],share,val,2:2); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1, aes(x=factor(Name, levels= Name), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")


# Q1 2020 comparing total asset share to borrowing share at the DW
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2020-03-31'))[,c(2,8,9)],share,val,2:3); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1[2:4], aes(x=factor(Name, levels=unique(Name)), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")
  

# Q2 2020 just total asset share
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2020-06-30'))[,c(2,8)],share,val,2:2); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1, aes(x=factor(Name, levels= Name), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")


# Q2 2020 comparing total asset share to borrowing share at the DW
plot1 <- gather(subset(agg, as.Date(agg$Date) == as.Date('2020-06-30'))[,c(2,8,9)],share,val,2:3); plot1 <- left_join(plot1, conv, by='FED')

ggplot(plot1[2:4], aes(x=factor(Name, levels=unique(Name)), y=val, fill=share)) + 
  geom_bar(stat = "identity", position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab("District") + ylab("Share")



# Some graphs about quantity of loans ----

agg <- left_join(left_join(left_join(left_join(aggregate(RCON0010 ~ Date + FED, cov, FUN = sum), 
                                               aggregate(RCON2170 ~ Date + FED, cov, FUN = sum)),
                                     aggregate(dw_freq ~ Date + FED, cov, FUN = sum)),
                           setnames(aggregate(RCON0010 ~ Date, cov, FUN = sum), old='RCON0010',new='total_asset')),
                 setnames(aggregate(dw_freq ~ Date, cov, FUN = sum), old='dw_freq',new='total_borrow'))

agg$'Asset Share' <- agg$RCON0010/agg$total_asset
agg$'Borrow Share' <- agg$dw_freq/agg$total_borrow


  # total size frequency borrowed over the three quarters
  ggplot(agg[1:3,c(1,7)], aes(x=as.Date(Date), y=total_borrow)) + 
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    xlab("District") + ylab("Quantity")
  
  
# Some information about changes in borrowing share from q4 2019 to q1 2020
  agg <- left_join(left_join(left_join(left_join(aggregate(RCON0010 ~ Date + FED, cov, FUN = sum), 
                                                 aggregate(RCON2170 ~ Date + FED, cov, FUN = sum)),
                                       aggregate(dw_quant ~ Date + FED, cov, FUN = sum)),
                             setnames(aggregate(RCON0010 ~ Date, cov, FUN = sum), old='RCON0010',new='total_asset')),
                   setnames(aggregate(dw_quant ~ Date, cov, FUN = sum), old='dw_quant',new='total_borrow'))
  
  agg$total_borrow <- agg$total_borrow/1000; agg$dw_quant <- agg$dw_quant/1000
  agg$'Asset Share' <- agg$RCON0010/agg$total_asset
  agg$'Borrow Share' <- agg$dw_quant/agg$total_borrow
  
  agg <- pdata.frame(agg, c('FED', 'Date'))
  agg$bs_fd <- agg$Borrow.Share - lag(agg$Borrow.Share)
  agg$abs_diff <- agg$dw_quant/lag(agg$dw_quant)
  agg <- subset(agg, as.Date(agg$Date) == as.Date('2020-03-31'))
  agg$FED <- as.numeric(agg$FED)
  agg <- left_join(agg, conv)
  agg <- data.frame(agg[,c('Name','bs_fd','abs_diff')])
  # Print the table displaying the changes in shares.
  print(xtable(data.frame(agg), type = "latex", include.rownames=FALSE))
  
# Share of banks that borrow (2019-2020) ----
plot1 <- data.frame(dfpost) %>% count(Date, bigsmall)
ggplot(data.frame(plot1), aes(fill=dwborrow_bin, y=n, x=Date)) + 
  geom_bar(position="stack", stat="identity")


# Share of banks that borrow from PPP loans as total bank. From the data, for q2 and q3 of 2020, 91% of large banks borrowed from the PPPLF and only 80% of small banks borrowed ----
plot1 <- data.frame(dfpost) %>% count(Date, bigsmall)
plot1 <- na.omit(left_join(plot1, aggregate(ppp_bin ~ Date + bigsmall, df, FUN = sum), by=c('Date','bigsmall')))
plot1$frac <- plot1$ppp_bin/plot1$n


# Share of banks that borrow went from .9% in Q42019 to 4.5% in Q1 2020 and 4% in Q2 2020, huge spike initially then dies down.
plot1 <- reshape(subset(df, as.Date(Date) >= as.Date('2019-03-31')) %>% count(Date, dwborrow_bin), idvar = 'Date', timevar = 'dwborrow_bin', direction = 'wide')
plot1$borrow_share <- plot1$n.1/(plot1$n.1 + plot1$n.0)
ggplot(plot1, aes(x=Date, y=borrow_share)) + geom_line()

# Share of banks large vs small that borrow during covid and 2019 (MEAN OF 2010-2019: .41 LARGE BANK SHARE, MEAN OF 2020: .61)
plot1 <- subset(df, as.Date(Date) >= as.Date('2010-01-30') & dwborrow_bin == 1) %>% count(Date, bigsmall)
plot1 <- data.frame(reshape(plot1, idvar = 'Date', timevar = 'bigsmall', direction = 'wide')[,1:3])
plot1$l_share <- plot1$n.Large/(plot1$n.Large + plot1$n.Small)
mean(plot1[1:38,4])
mean(plot1[39:41,4])

# Comparing exposure to local economic condition (measured by Philly coincident index) between large and small banks
aggregate(exposure ~ Date + dwborrow_bin, subset(df, as.Date(Date) >= as.Date('2019-09-30')), FUN = mean)

# Exposure is highly volatile in the the three periods of COVID (SD: 11.03 vs .95 in the 3 periods before COVID)
descr(subset(df, as.Date(Date) >= as.Date('2020-01-01') & as.Date(Date) <= as.Date('2020-12-01') )$exposure)
descr(subset(df, as.Date(Date) <= as.Date('2020-01-01') & as.Date(Date) >= as.Date('2019-04-01'))$exposure)


# Reserve to deposit ratio for 2018-2019 is 7.9% for all banks, post-COVID, non-borrower has 11.6% and borrowers had 7.8%
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, subset(dfpre, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, dfpost, FUN = mean)[1:3,3]) #nonborrower
mean(aggregate(reserve_deposit_ratio ~ Date + dwborrow_cov, dfpost, FUN = mean)[4:6,3]) #borrower

# Looking at total deposits in the banking system, there was an average of 1B precovid, then 1.6B for nonborrowers and 5.8B for borrowers
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01') ), FUN = mean)[,3]) #pre total
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre nonborrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, subset(df, as.Date(Date) >= as.Date('2018-01-01')), FUN = mean)[,3]) #pre borrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, dfpost, FUN = mean)[1:3,3]) #nonborrower
mean(aggregate(RCON2200 ~ Date + dwborrow_cov, dfpost, FUN = mean)[4:6,3]) #borrower

#In dense urban areas, there tend to be small number of large banks. In rural areas, there tend to be large number of small banks ----
plot1 <- left_join(aggregate(size ~ FED + Date, subset(df, as.Date(Date) == as.Date('2020-06-30')), FUN = mean), data.frame(subset(df, as.Date(Date) == as.Date('2020-06-30'))) %>% count(Date, FED))

# Graph about share of banks (count) that borrow in each district as a fraction of total banks in that districtin Q2 2020 ====
plot1 <- subset(data.frame(df), as.Date(Date) >= as.Date('2020-06-30')) %>% count(FED, dwborrow_bin) 
plot1 <- data.frame(reshape(plot1, idvar='FED', timevar = 'dwborrow_bin', direction='wide'))
plot1$borrow_share <- plot1$n.1/(plot1$n.0+plot1$n.1)
ggplot(data.frame(plot1[1:12,c(1,4)]), aes(x=as.factor(FED), y = borrow_share)) + geom_col()

# Graph about share of borrowing (quantity) that borrow in each district as a share of total borrowing from DW in Q2 2020 ====
plot1 <- aggregate(dw_quant ~ FED, subset(data.frame(df), as.Date(Date) == as.Date('2020-06-30')), FUN = sum)
plot1$borrow_share <- plot1$dw_quant/sum(plot1[,2])
ggplot(data.frame(plot1[1:12,c(1,3)]), aes(x=as.factor(FED), y = borrow_share)) + geom_col()

# difference in uptake between DW loans and PPPLF loans
plot1 <- aggregate(RCONLG27 ~ FED, subset(data.frame(df), as.Date(Date) == as.Date('2020-06-30')), FUN = sum)
plot1$borrow_share <- plot1$RCONLG27/sum(plot1[,2])
ggplot(data.frame(plot1[1:12,c(1,3)]), aes(x=as.factor(FED), y = borrow_share)) + geom_col()

# Show the share of loans that are non-ppp and ppp since 2019
plot1 <- subset(df, as.Date(Date) >= as.Date('2019-01-01'))
plot1[is.na(plot1$RCONLG27) == TRUE,'RCONLG27'] <- 0
plot1 <- aggregate(cbind(nonppp_loans,RCONLG27) ~ Date, plot1, sum)
plot1 <- plot1 %>% gather('Loan Type', val,-Date)
plot1$Date <- as.Date(plot1$Date)
plot1$`Loan Type` <- ifelse(plot1$`Loan Type` == 'RCONLG27', 'PPP Loans', 'Non-PPP Loans')
ggplot(plot1, aes(x=Date,y=val,fill=`Loan Type`)) + geom_area() +
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-9)) +
  ylab('Quantity') + theme(legend.position = c(.2, .5)) +
  scale_color_hue(labels = c("T999", "T888")) +
  scale_x_date(breaks = date_breaks("3 months"))

# Show the number of discount window borrowing in each quarter
plot1 <- left_join(dwborrow[as.Date(dwborrow$Date) > as.Date('2019-06-01'),] %>% count(Date),
          aggregate(Loan.amount ~ Date, dwborrow[as.Date(dwborrow$Date) > as.Date('2019-06-01'),], sum))
plot1 <- left_join(plot1,
                   aggregate(Loan.amount ~ Date, dwborrow[as.Date(dwborrow$Date) > as.Date('2019-06-01'),], median), by='Date')
setnames(plot1, old=c('n','Loan.amount.x','Loan.amount.y'), new=c('Count','Amount','Mean'))
plot1$Date <- as.character(plot1$Date)
plot1$Amount <- paste0(round(plot1$Amount/1e9,2),' B')
plot1$Mean <- paste0(round(plot1$Mean/1e6,2),' M')
print(xtable(plot1), include.rownames=FALSE)

# Plot quantile of PPP loans.