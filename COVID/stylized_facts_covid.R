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
library('usmap')

# Data Import ----
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
plot1 <- aggregate(cbind(RCON2122,RCONLG27) ~ Date, plot1, sum)
plot1 <- plot1 %>% gather('Loan Type', val,-Date)
plot1$Date <- as.Date(plot1$Date)
plot1$`Loan Type` <- ifelse(plot1$`Loan Type` == 'RCONLG27', 'PPP Loans', 'Non-PPP Loans')
ggplot(plot1, aes(x=Date,y=val,fill=`Loan Type`)) + geom_area() +
  scale_y_continuous(labels = unit_format(unit = "T", scale = 1e-9)) +
  ylab('Quantity') + theme(legend.position = c(.2, .5)) +
  scale_color_hue(labels = c("T999", "T888")) +
  scale_x_date(breaks = date_breaks("3 months")) +
  theme(text = element_text(size = 16))

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

# Stylized Facts - Quantile graph of demand shock to reserves
  plot1 <- data.frame(Percentile = 0:100, `Share of Reserves` = quantile(sf2$demand_so_reserves, seq(0,1,by=.01), na.rm = TRUE))
  ggplot(plot1[2:nrow(plot1),], aes(x=Percentile, y=Share.of.Reserves)) + geom_point() +
    scale_y_log10() + scale_x_continuous(breaks=seq(0,100,by=5)) +
    ylab('Demand Share of Reserves') +
    theme(text = element_text(size = 18))
  
# Average loan size by state
  sbd <- aggregate(cbind(n,InitialApprovalAmount) ~ OriginatingLenderState + DateApproved, subset(pppm, DateApproved <= '2020-08-08'), sum)
  sbd$avg_loan <- sbd$InitialApprovalAmount/sbd$n
  plot1 <- aggregate(avg_loan ~ OriginatingLenderState, sbd, mean)
  plot1 <- data.frame(state=plot1$OriginatingLenderState, values = plot1$avg_loan)
  plot_usmap(regegions = 'states', data = plot1, values = 'values') + 
    labs(title = "US States",
         subtitle = "This is a blank map of the counties of the United States.") + 
    theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# Showing that shock is conditional on size and # of offices
  plot1 <- aggregate(cbind(size,log(PPP+1)) ~ RSSD, sf3, mean)
    ggplot(subset(plot1, V2 >0), aes(x=size,y=V2)) + geom_point()
  plot(aggregate(log(PPP+1) ~ ra_quint, sf3, mean))
  
  
# Aggregate Data 1: Correlation between DW borrowing (aggregate) and aggregate PPP loans-------
  # Transformation
  ppb2<- aggregate(Original.Outstanding.Advance.Amount ~ origin_date, pplf, sum)
  sf <- full_join(ppb, ppb2, by=c('DateApproved' = 'origin_date'))
  setnames(sf, 'Original.Outstanding.Advance.Amount','LF_request')
  ppb2 <- aggregate(Original.Outstanding.Advance.Amount ~ Date.Of.Advance, pplf, sum)
  sf <- left_join(sf, ppb2, by=c('DateApproved' = 'Date.Of.Advance'))
  setnames(sf, 'Original.Outstanding.Advance.Amount','LF_received')
  sf <- sf[order(sf$DateApproved),]
  sf[is.na(sf)] <- 0

  dwborrow1 <- dwborrow %>% count(Loan.date)
  dwborrow2 <- aggregate(Loan.amount ~ Loan.date, dwborrow, FUN = sum)
  dwborrow1 <- subset(full_join(dwborrow2,dwborrow1), as.Date(Loan.date) >= as.Date('2020-01-01'))
  
  sf <- full_join(sf,dwborrow1, by=c('DateApproved' = 'Loan.date'))
  sf <- subset(sf, as.Date(DateApproved) <= as.Date('2020-10-01'))
  sf <- sf[order(sf$DateApproved),]
  
  
  ind <- which(is.na(sf$n) == TRUE)
  for (i in 2:length(ind)) {
    if (ind[i] == ind[i-1]+1) {
      ind <- ind[-i]
    }
    print(length(ind))
  }
  
  sf[ind+1,'InitialApprovalAmount'] <- sf[ind+1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  ind <- which(is.na(sf$n) == TRUE)
  ind <- ind[1:length(ind)-1]
  sf[ind+1,'InitialApprovalAmount'] <- sf[ind+1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  ind <- which(is.na(sf$n) == TRUE)
  sf[ind-1,'InitialApprovalAmount'] <- sf[ind-1,'InitialApprovalAmount'] + sf[ind,'InitialApprovalAmount']
  sf <- sf[-ind,]
  
  rm(ind, dwborrow1, dwborrow2)
  # Creation
  sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, sf$InitialApprovalAmount)
  sf$InitialApprovalAmount <- ifelse(is.na(sf$InitialApprovalAmount) == TRUE & as.Date(sf$DateApproved) >= as.Date('2020-08-09'), 0, sf$InitialApprovalAmount)
  
  #sf <- sf[-c(75:80),]; sf <- sf[-c(121:123),];  #uncut
  
  sf$ppp_week_avg <- rollapply(sf$InitialApprovalAmount, 3, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$dw_quant_avg <- rollapply(sf$Loan.amount, 3, mean, na.rm=TRUE, fill = NA, partial=3)
  sf$id <- 1
  sf$signal <- ifelse(as.Date(sf$DateApproved) >= as.Date('2020-03-16') & as.Date(sf$DateApproved) <= as.Date('2020-03-21'), 1, 0)
  sf$preppp <- ifelse(as.Date(sf$DateApproved) <= as.Date('2020-04-02'), 0, 1)
  setnames(sf, old = c('DateApproved','InitialApprovalAmount','Loan.amount'),
           new = c('Date','PPP','DW'))
  sfc <- subset(sf, Date >= '2020-04-03' & Date <= '2020-07-01') 
  
# Time series figure about the PPP shock and the DW borrowing sizes ----
  # PPP, DW, PPPLF, from 04-07
  ggplot(sfc) +
    geom_line(aes(x = Date, y = PPP/3, colour ='PPP Lending'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_request , colour ='LF Requested'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_received , colour ='LF Received'), size=1.5) +
    geom_line(aes(x = Date, y = DW, colour ='DW Borrowing'), size=1.5) +
    scale_y_continuous(name = "Daily DW Quantity", 
                       sec.axis = sec_axis(~.*3, name="Daily PPP Loans", labels = label_number(suffix = "B", scale = 1e-9)),
                       labels = label_number(suffix = "B", scale = 1e-9)) +
    labs(x="Date") + scale_x_date(date_breaks = '2 weeks') + 
    theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-07-01'),ymin = -Inf, ymax = Inf, alpha = .35)
    #annotate('rect',fill='gray',xmin=as.Date('2020-07-06'),xmax = as.Date('2020-08-08'),ymin = -Inf, ymax = Inf, alpha = .35) 
    #annotate('rect',fill='red',xmin=as.Date('2020-03-16'),xmax = as.Date('2020-03-27'),ymin = -Inf, ymax = Inf, alpha = .2)

  # PPP, DW, PPPLF, from 01-12
  ggplot(sf) +
    geom_line(aes(x = Date, y = PPP/3, colour ='PPP Lending'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_request , colour ='LF Requested'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_received , colour ='LF Received'), size=1.5) +
    geom_line(aes(x = Date, y = DW, colour ='DW Borrowing'), size=1.5) +
    scale_y_continuous(name = "Daily DW Quantity", 
                       sec.axis = sec_axis(~.*3, name="Daily PPP Loans", labels = label_number(suffix = "B", scale = 1e-9)),
                       labels = label_number(suffix = "B", scale = 1e-9)) +
    labs(x="Date", y= 'Log Quantity') + scale_x_date(date_breaks = '1 month') + 
    theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-07-01'),ymin = -Inf, ymax = Inf, alpha = .35)+
   annotate('rect',fill='gray',xmin=as.Date('2020-07-06'),xmax = as.Date('2020-08-08'),ymin = -Inf, ymax = Inf, alpha = .35) +
   annotate('rect',fill='red',xmin=as.Date('2020-03-16'),xmax = as.Date('2020-03-27'),ymin = -Inf, ymax = Inf, alpha = .35)
  
  #Now with the log values
  ggplot(sfc) +
    geom_line(aes(x = Date, y = log10(PPP), colour ='PPP'), size=1.5) +
    geom_line(aes(x = Date, y = log10(PPPLF), colour ='PPPLF'), size=1.5) +
    scale_y_continuous(name = "Log Value") +
    labs(x="Date") + ylim(0,11)+
    theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
  annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-07-01'),ymin = -Inf, ymax = Inf, alpha = .35) 
    #annotate('rect',fill='gray',xmin=as.Date('2020-07-06'),xmax = as.Date('2020-08-10'),ymin = -Inf, ymax = Inf, alpha = .35) +
    #annotate('rect',fill='red',xmin=as.Date('2020-03-16'),xmax = as.Date('2020-03-27'),ymin = -Inf, ymax = Inf, alpha = .2)
  
  # using moving averages
  ggplot(sf) +
    geom_line(aes(x = DateApproved, y = ppp_week_avg/5, colour ='PPP'), size=1.5) +
    geom_line(aes(x = DateApproved, y = dw_quant_avg, colour ='DW'), size=1.5) +
    scale_y_continuous(name = "Mov. Avg. DW Loans", 
                       sec.axis = sec_axis(~.*5, name="Mov. Avg. PPP Loans", labels = unit_format(unit = "B", scale = 1e-9)),
                       labels = unit_format(unit = "B", scale = 1e-9)) +
    labs(x="Date") + 
    theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-08-08'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='red',xmin=as.Date('2020-03-16'),xmax = as.Date('2020-03-27'),ymin = -Inf, ymax = Inf, alpha = .2)
  
  #Now with the log values
  ggplot(sf) +
    geom_line(aes(x = DateApproved, y = log(ppp_week_avg), colour ='PPP'), size=1.5) +
    geom_line(aes(x = DateApproved, y = log(dw_quant_avg), colour ='DW'), size=1.5) +
    scale_y_continuous(name = "MA Log Value") +
    labs(x="Date") + ylim(16,25)+
    theme(legend.position = c(.9, .9), legend.title=element_blank(), text = element_text(18)) +
    annotate('text', label = paste0('Correlation = ',round(cor(log(sf$ppp_week_avg+1),log(sf$dw_quant_avg+1),'complete'),2)), 
             x=max(sf$DateApproved, na.rm=TRUE) - 21, y=22) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-07-01'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='gray',xmin=as.Date('2020-07-06'),xmax = as.Date('2020-08-10'),ymin = -Inf, ymax = Inf, alpha = .35) +
    annotate('rect',fill='red',xmin=as.Date('2020-03-16'),xmax = as.Date('2020-03-27'),ymin = -Inf, ymax = Inf, alpha = .2)
  
  
# Info about share of banks that made actions ----
#Banks that did not lend PPP loans?
  length(na.omit(setdiff(unique(temp$IDRSSD),unique(subset(matchlb, dateapproved < '2020-08-09')$rssd))))
  #and they accessed the DW
    ls <- setdiff(unique(temp$IDRSSD),unique(subset(matchlb, dateapproved < '2020-08-09')$rssd))
    db <- subset(dwborrow, Loan.date >= '2020-04-03' & Loan.date <= '2020-08-08')
    length(na.omit(match(unique(df[df$IDRSSD %in% ls,c('IDRSSD','Primary.ABA.Routing.Number')]$Primary.ABA.Routing.Number),db$Borrower.ABA.number)))
  
# Banks that lend PPP loans?
  length(na.omit(match(unique(df$IDRSSD),unique(subset(matchlb, dateapproved < '2020-08-09')$rssd))))
  
  #Banks that lent out PPP loans and accessed only the DW?
    ls <-intersect(unique(df$IDRSSD),unique(subset(matchlb, dateapproved < '2020-08-09')$rssd))
    ls <- unique(df[df$IDRSSD %in% ls,c('IDRSSD','Primary.ABA.Routing.Number')])
    length(setdiff(intersect(ls$Primary.ABA.Routing.Number,db$Borrower.ABA.number),pplf$Institution.ABA))
  
  #Banks that lent out PPP loans and accessed only the PPPLF
    length(setdiff(pplf$Institution.ABA, intersect(ls$Primary.ABA.Routing.Number,db$Borrower.ABA.number)))
    
  #Banks that borrowed from both PPPLF and DW
    length(intersect(pplf$Institution.ABA, intersect(ls$Primary.ABA.Routing.Number,db$Borrower.ABA.number)))

  
# Residual on residual plot
    plot1 <- list()
    plot1[[1]] <- feols(log(dwsores+1) ~ bigsmall + log(LF_30+1) + reserve_asset_ratio + size + eqcaprat + rsa + lsa + dsa| RSSD + Date  , 
                                         sf3, panel.id = c('RSSD','Date'))
    plot1[[2]] <- update(t1[[1]], log(ppp_so_reserves+1) ~.)
    ggplot(data.frame(x=plot1[[1]]$residuals, y=plot1[[2]]$residuals), aes(x=x,y=y)) + geom_point()
    
    
# Scatter plot of DW borrowing and PPP lending ----
    # With the aggregated data
    plot1 <- subset(sf4, pppsores > 0 & dwsores>0); 
    plot1 <- data.frame(LogDW = log(plot1$dwsores), LogPPP = log(plot1$pppsores), size = log(plot1$Assets))
    plot1 <- plot1[!is.infinite(plot1$LogDW),]
    plot1 <- plot1 %>% mutate(bin = ntile(LogDW, n=20))
    plot1 <- plot1 %>% group_by(bin) %>% summarise(LogPPP = mean(LogPPP), LogDW = mean(LogDW), size=mean(size))
    ggplot(plot1, aes(x=LogDW,y=LogPPP)) + geom_point(shape = 1, aes(size=size)) + scale_shape_manual(values = c(21,19)) + 
      geom_smooth(method=lm, alpha=0, color='red')+ theme(legend.position = "none") +
      ylab('Log10 PPP') + xlab('Log10 DW')
    
    # With the time series data
    plot1 <- subset(sf3, pppsores > 0 & dwsores>0); 
    plot1 <- data.frame(LogDW = log(plot1$dwsores), LogPPP = log(plot1$pppsores), size = log(plot1$Assets))
    plot1 <- plot1[!is.infinite(plot1$LogDW),]
    plot1 <- plot1 %>% mutate(bin = ntile(LogDW, n=20))
    plot1 <- plot1 %>% group_by(bin) %>% summarise(LogPPP = mean(LogPPP), LogDW = mean(LogDW), size=mean(size))
    ggplot(plot1, aes(x=LogDW,y=LogPPP)) + geom_point(shape = 1, aes(size=size)) + scale_shape_manual(values = c(21,19)) + 
      geom_smooth(method=lm, alpha=0, color='red')+ theme(legend.position = "none") +
      ylab('Log10 PPP') + xlab('Log10 DW')
    
    
# Scattered plot of residual values
    #With the time series data
    plot1 <- subset(sf3, pppsores >= 0 & dwsores>=0)
    p2 <- feols(log(pppsores+1) ~ log(LF_30+1) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + econexpo + log(covexpo+1) + asinh(eci)| RSSD + Date, plot1)
    p2 <- data.frame(plot1[p2$obs_selection$obsRemoved,],ppr = p2$residuals)
    p2$dwr <- feols(log(dwsores+1) ~ log(LF_30+1) + log(reserve_asset_ratio) + size + eqcaprat + rsa + lsa + dsa + log(npplsores+1) + econexpo + log(covexpo+1) + asinh(eci)| RSSD + Date, p2)$residuals
    p2 <- p2 %>% mutate(bin = ntile(dwr, n=20))
    p2 <- p2 %>% group_by(bin) %>% summarise(ppr = mean(ppr), dwr = mean(dwr), size=mean(size))
    ggplot(p2, aes(x=dwr, y=ppr)) + geom_smooth(method=lm, alpha=.25, se=F, level=.95, color='red') +
      #stat_summary_bin(bin = 20) +
      geom_point(shape = 1, aes(size=size)) + theme(legend.position = "none") +
      ylab('Residualized PPP') + xlab('Residualized DW')
    
    #With the aggregated data
    plot1 <- subset(sf4, pppsores > 0 & dwsores>0)
    p2 <- feols(log(pppsores) ~ log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + rsa + lsa + dsa + precovdw, plot1)
    p2 <- data.frame(plot1, ppr = p2$residuals)
    p2$dwr <- feols(log(dwsores) ~ log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + rsa + lsa + dsa + precovdw, p2)$residuals
    p2 <- p2 %>% mutate(bin = ntile(dwr, n=20))
    p2 <- p2 %>% group_by(bin) %>% summarise(ppr = mean(ppr), dwr = mean(dwr), size=mean(log(Assets)))
    ggplot(p2, aes(x=dwr, y=ppr)) + geom_smooth(method=lm, alpha=.25, se=F, level=.95, color='red') +
      #stat_summary_bin(bin = 20) +
      geom_point(shape = 1, aes(size=size)) + theme(legend.position = "none") +
      ylab('Residualized PPP') + xlab('Residualized DW')
    
# correlation between instrument and non-PPP loans
    plot1 <- aggregate(n ~ RSSD + Quarter, sf3, mean)
    plot1 <- left_join(plot1, aggregate(nonppp_loans ~ IDRSSD + Date, df, mean), by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
    plot1 <- left_join(plot1, aggregate(RCON2170 ~ IDRSSD + Date, df, mean), by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
    plot1$npshare <- plot1$nonppp_loans/plot1$RCON2170
    feols(npshare ~ n | Quarter , plot1)
    