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
  
# FIGURE 1: Time series figure about the PPP shock and the DW borrowing sizes ----
  sfc <- subset(sf, Date >= '2020-04-03' & Date <= '2020-06-01') 
  
  # PPP, DW, PPPLF, from 04-06
  ggplot(sfc) +
    geom_line(aes(x = Date, y = PPP/8, colour ='PPP Lending'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_request , colour ='LF Requested'), size=1.5) +
    #geom_line(aes(x = Date, y = LF_received , colour ='LF Received'), size=1.5) +
    geom_line(aes(x = Date, y = DW, colour ='DW Borrowing'), size=1.5) +
    scale_y_continuous(name = "Daily DW Quantity", 
                       sec.axis = sec_axis(~.*8, name="Daily PPP Loan Quantity", labels = label_number(suffix = "B", scale = 1e-9)),
                       labels = label_number(suffix = "B", scale = 1e-9)) +
    labs(x="Date") + scale_x_date(date_breaks = '1 week') + 
    theme(legend.position = c(.85, .85), legend.title=element_blank(), axis.title = element_text(size=18)) +
    geom_vline(xintercept = as.Date('2020-04-03'), linetype='dashed', size = 1.5)+
    geom_vline(xintercept = as.Date('2020-04-27'), linetype='dashed', size = 1.5)+
    geom_vline(xintercept = as.Date('2020-04-16'), linetype='dashed', size = 1, color ='red')+
    annotate('rect',fill='gray',xmin=as.Date('2020-04-03'),xmax = as.Date('2020-04-16'),ymin = -Inf, ymax = Inf, alpha = .5) +
    annotate('rect',fill='gray',xmin=as.Date('2020-04-27'),xmax = as.Date('2020-06-01'),ymin = -Inf, ymax = Inf, alpha = .4) +
    annotate("text", x=as.Date('2020-04-02'), y=1e9, label="PPP Phase 1", angle=90) +
    annotate("text", x=as.Date('2020-04-26'), y=1e9, label="PPP Phase 2", angle=90) +
    annotate("text", x=as.Date('2020-04-17'), y=6e9, label="First PPPLF Disbursed", angle=270)
  
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
    
    # With the time series data
    plot1 <- subset(sf3, pppsores > 0 & dwsores>0); 
    plot1 <- data.frame(LogDW = plot1$dwsores, LogPPP = plot1$pppsores, size = log(plot1$Assets))
    plot1 <- plot1[!is.infinite(plot1$LogDW),]
    plot1 <- plot1 %>% mutate(bin = ntile(LogDW, n=20))
    plot1 <- plot1 %>% group_by(bin) %>% summarise(LogPPP = mean(LogPPP), LogDW = mean(LogDW), size=mean(size))
    preg <- feols(LogPPP ~ LogDW, plot1); preg
    ggplot(plot1, aes(x=LogPPP,y=LogDW)) + geom_point(shape = 1, aes(size=size)) + 
      geom_smooth(method=lm, alpha=0, color='red')+ 
      theme(legend.position = "none", axis.title = element_text(size=21)) +
      ylab('DW/Reserves') + xlab('PPP/Reserves') 
      
    # With the aggregated data
    plot1 <- subset(sf4, pppsores > 0 & dwsores>0); 
    plot1 <- data.frame(LogDW = log(plot1$dwsores), LogPPP = log(plot1$pppsores), size = log(plot1$RCON2170))
    plot1 <- plot1[!is.infinite(plot1$LogDW),]
    plot1 <- plot1 %>% mutate(bin = ntile(LogDW, n=20))
    plot1 <- plot1 %>% group_by(bin) %>% summarise(LogPPP = mean(LogPPP), LogDW = mean(LogDW), size=mean(size))
    ggplot(plot1, aes(x=LogPPP,y=LogDW)) + geom_point(shape = 1, aes(size=size)) + scale_shape_manual(values = c(21,19)) + 
      geom_smooth(method=lm, alpha=0, color='red')+ theme(legend.position = "none") +
      ylab('PPP') + xlab('DW')
    
    
# Scattered plot of residual values ----
    #With the time series data
    plot1 <- subset(sf3, pppsores > 0 & dwsores > 0)
    p2 <- feols(pppsores ~ LF_30 + precovdw + log(OFFICES) + rsa + dsa+ lsa + eqcaprat + eci + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo| FED + Date, plot1)
    p2 <- data.frame(plot1[p2$obs_selection$obsRemoved,],ppr = p2$residuals)
    p2$dwr <- feols(dwsores ~ LF_30 + precovdw + log(OFFICES) + rsa + dsa+ lsa + eqcaprat + eci + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo| FED + Date, p2)$residuals
    treg <- feols(dwr ~ ppr , p2);treg
    p2 <- p2 %>% mutate(bin = ntile(ppr, n=20))
    p2 <- p2 %>% group_by(bin) %>% summarise(ppr = weighted.mean(ppr, size), dwr = weighted.mean(dwr, size), size=weighted.mean(size, size))
    ggplot(p2, aes(x=ppr, y=dwr)) + geom_smooth(method=lm, alpha=.25, se=F, level=.95, color='red') +
      #stat_summary_bin(bins = 20, geom = 'point') +
      geom_point(shape = 1, aes(size=size)) + theme(legend.position = "none", axis.title = element_text(size=21)) +
      xlab('Residualized PPP/Reserves') + ylab('Residualized DW/Reserves')
    
    #With the aggregated data
    plot1 <- subset(sf4, pppsores > 0)
    p2 <- feols(pppsores ~ log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + rsa + lsa + dsa + precovdw| FED + Date, plot1)
    p2 <- data.frame(plot1, ppr = p2$residuals)
    p2$dwr <- feols(dwsores ~ log(lfsores+1) + asinh(eci) + asinh(econexpo) + asinh(covexpo) + asinh(OFFICES) + rsa + lsa + dsa + precovdw| FED + Date, p2)$residuals
    p2 <- p2 %>% mutate(bin = ntile(dwr, n=20))
    p2 <- p2 %>% group_by(bin) %>% summarise(ppr = mean(ppr), dwr = mean(dwr), size=mean(log(Assets)))
    ggplot(p2, aes(x=dwr, y=ppr)) + geom_smooth(method=lm, alpha=.25, se=F, level=.95, color='red') +
      #stat_summary_bin(bin = 20) +
      geom_point(shape = 1, aes(size=size)) + theme(legend.position = "none") +
      xlab('Residualized PPP/Reserves') + ylab('Residualized DW/Reserves')
    
# correlation between instrument and non-PPP loans
    temp <- subset(df, Date >= as.Date('2020-01-01'))
    temp$RCONLG27 <- ifelse(is.na(temp$RCONLG27) == TRUE, 0, temp$RCONLG27)
    plot1 <- aggregate(c_instr ~ RSSD + Quarter, sf3, sum)
    plot1 <- left_join(plot1, aggregate(cbind(RCON1766, RCONLG27) ~ IDRSSD + Date, temp, mean), by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
    plot1 <- left_join(plot1, aggregate(RCON2170 ~ IDRSSD + Date, df, mean), by=c('RSSD' = 'IDRSSD','Quarter'='Date'))
    plot1$npshare <- (plot1$RCON1766-plot1$RCONLG27)/plot1$RCON2170
    feols(log(npshare) ~ c_instr | Quarter , plot1)
    
# Bins of bank shock to reserves and the share of banks that borrow from DW in that bin ----
    plot1 <- subset(sf3, pppsores > 0.0)
    plot1 <- plot1[,c('pppsores','dwbin_notest','size')]
    plot1 <- plot1 %>% arrange(pppsores) %>% mutate(bin = ntile(pppsores, n=10), bin2 = ntile(size, n=10))
    temp <- plot1 %>% group_by(bin) %>% count()
    plot1 <- left_join(plot1, temp)    
    plot1 <- left_join(aggregate(dwbin_notest ~ bin, plot1, sum), temp)    
    plot1$dwshare <- plot1$dwbin_notest*100/plot1$n
    ggplot(plot1, aes(x=as.factor(bin), y=dwshare)) + geom_col() + 
      xlab('Shock Size Binned') + ylab('Share of Observations Accessing DW') +
      scale_y_continuous(labels = label_number(suffix = "%")) +
      theme(axis.title = element_text(size=18))
      #scale_x_binned()
    
    # bins of shock size and dw quantity borrowed
    plot1 <- subset(sf3, dwsores > 0.01)
    plot1 <- plot1[,c('pppsores','dwsores','size')]
    plot1 <- plot1 %>% arrange(pppsores) %>% mutate(bin = ntile(pppsores, n=10), bin2 = ntile(size, n=10))
    temp <- plot1 %>% group_by(bin) %>% count()
    plot1 <- left_join(plot1, temp)    
    plot1 <- left_join(aggregate(dwsores ~ bin, plot1, mean), temp)    
    plot1$dwshare <- plot1$dwbin_notest*100/plot1$n
    ggplot(plot1, aes(x=as.factor(bin), y=dwsores)) + geom_col() + 
      xlab('Bank Size Binned') + ylab('Share of Banks accesing DW')
      #scale_y_continuous(labels = label_number(suffix = "%"))
    
    
    
    
    plot1 <- subset(sf4, PPP > 0)
    plot1 <- plot1[,c('pppsores','RCON2170')]
    plot1 <- plot1 %>% arrange(pppsores) %>% mutate(bin = ntile(RCON2170, n=10))
    plot1 <- aggregate(pppsores ~ bin, plot1, mean)
    ggplot(plot1, aes(x=as.factor(bin), y=pppsores)) + geom_col() + 
      xlab('Bank Size Binned') + ylab('Share of Reserves Lent through program')
    #scale_x_binned()
    
# Decile comparison of banks that borrow from the DW and how much PPP they lent out. ----
    plot1 <- subset(sf4, DW > 0) %>% mutate(dec = ntile(dwsores, 5))
    plot1$dec <- plot1$dec+1
    temp <- subset(sf4, DW == 0) %>% mutate(dec = ntile(dwsores, 1))
    plot1 <- rbind(plot1, temp)
    plot1 <- aggregate(cbind(pppsores,dwsores) ~ dec, plot1, median)
    plot1$col <- ifelse(plot1$dec == 1, 'No DW','DW Bins')
    plot1$dwsores <- plot1$dwsores/10
    ggplot(plot1, aes(x=dec)) + geom_col(aes(y=pppsores,fill=col)) +
      ylab('Median PPP Lending as Share of Reserves') + xlab('DW Borrowing as Share of Reserves Quintiles') +
      theme(legend.title = element_blank(), legend.position = c(.2,.8), legend.text =element_text(size=24), text= element_text(size=16))

    # by big vs small banks
    plot1 <- subset(sf4, dwsores > 0) %>% group_by(bigsmall) %>% mutate(dec = ntile(dwsores,4))
    plot1$dec <- plot1$dec+1
    temp <- subset(sf4, DW == 0) %>% group_by(bigsmall) %>% mutate(dec = ntile(dwsores, 1))
    plot1 <- rbind(plot1, temp)
    plot1 <- aggregate(cbind(pppsores,dwsores) ~ dec + bigsmall, plot1, median)
    plot1$col <- ifelse(plot1$dec == 1, 'No DW','DW Bins')
    plot1$bigsmall <- ifelse(plot1$bigsmall ==0, 'Small Banks', 'Large Banks')
    plot1$f = paste0(plot1$bigsmall, " - ",plot1$col)
    ggplot(plot1, aes(x=dec)) + geom_bar(aes(y=pppsores,fill=as.factor(f)), stat="identity", width=.5, position = "dodge2") +
      ylab('Median PPP Lending as Share of Reserves') + xlab('DW Borrowing as Share of Reserves Quintiles') +
      theme(legend.title = element_blank(), legend.position = c(.3,.8), legend.text =element_text(size=20), text= element_text(size=16))+
      scale_fill_manual(values=c("cyan4", "cyan3", "darkolivegreen3",'darkolivegreen2'))
    
    
# CDF of PPPE on small business loans and PPP loans (like figure 1 granja) ----
    plot1 <- sf4; plot1$pppe <- round(plot1$pppe,2)
    plot1 <- aggregate(cbind(ppp_share_tot, sb_share_tot) ~ pppe, plot1, sum)
    plot1 <- plot1 %>% arrange(pppe) %>% mutate(cs_ppp = cumsum(ppp_share_tot), cs_sb = cumsum(sb_share_tot))
    ggplot(plot1, aes(x=pppe)) + geom_point(aes(y = cs_sb, colour='Small Business Loans'),shape=1, size=4) +
      geom_point(aes(y = cs_ppp, colour='PPP Loans'),shape=2, size=4) +
      xlab('Bank PPPE') + ylab('Cumulative Share of Loans') +
      theme(legend.position = c(.2, .9), legend.title=element_blank()) +
      geom_vline(xintercept = 0, linetype='dashed', size = 1, color = 'red')
    
# time series chart (like granja Figure 11) ----
    # Table 1 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing probability - LPM 
    ta2 <- list()
    ta2[[1]] <- feols(dwbin_notest ~ i(week,pppsores) | FED,
                      #family = binomial(link = "logit"),
                      data = sf3, panel.id = c('RSSD','Date'))
    ta2[[2]] <- update(ta2[[1]], . ~.
                       + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
    ta2[[3]] <- update(ta2[[1]], subset = sf3$bigsmall == 1)
    ta2[[4]] <- update(ta2[[2]], subset = sf3$bigsmall == 1)
    ta2[[5]] <- update(ta2[[1]], subset = sf3$bigsmall == 0)
    ta2[[6]] <- update(ta2[[2]], subset = sf3$bigsmall == 0)
    etable(ta2, dict = dict1,
           title = 'Linear probability model estimation of DW borrowing probability. Column (1-2) is the pooled sample with and without controls, column (3-4) is for large banks that have assets greater than $600M, and columns (5-6) is for small community banks. The results from column (2) implies that a 10 percentage point increase in the PPP quantity loaned as a share of the reserves increases probability to borrow from the discount window by 1.6 percentage points. There seems to be a greater effect for large banks when shocked as compared to small banks.',
           label = 'main_reg',
           headers = NA,
           digits = 4,
           fitstat = ~n + r2,
           #group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
           extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
           tex = F)
    
    # Table 2 - OLS results (Pooled, Large, small) - w/ and without controls for DW borrowing quantity
    ta <- list()
    sft <- subset(sf3, DW > 0.0)
    ta[[1]] <- feols(dwsores ~ i(week,pppsores)|FED + Date, 
                     sft, 
                     #subset = sf3$PPP>0 & sf3$DW > 0
                     panel.id = c('RSSD','Date'))
    ta[[2]] <- update(ta[[1]], . ~ .
                      + LF_30 + precovdw + log(OFFICES) + rsa + eqcaprat + eci  + ci_com + scisoa + cdep + cisoa + liqass + levrat + size +log(dwage) + covexpo)
    ta[[3]] <- update(ta[[1]], subset =  sft$bigsmall == 1)
    ta[[4]] <- update(ta[[2]], subset =  sft$bigsmall == 1)
    ta[[5]] <- update(ta[[1]], subset =  sft$bigsmall == 0)
    ta[[6]] <- update(ta[[2]], subset =  sft$bigsmall == 0)
    etable(ta, dict = dict1,
           title = 'OLS Estimation of Log DW Borrowing. Columns correspond to the same shares as Table 1. ',
           label = 'main_reg',
           headers = c('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
           #digits = 5,
           fitstat = ~n + r2,
           group = list('Controls:'=c('lsa','rsa','dsa','Equity Cap Ratio','Log Assets','Log RA Ratio','econexpo','COVID Exposure','eci','npplsores','CI','C&I','COVID','Assets','OFFICES','age','Tier')),
           #extralines = list('Pooled','Pooled','Large Banks', 'Large Banks','Small Banks','Small Banks'),
           tex = F)
    
# scatter of PPPLF Funds received vs requested ----
    plot1 <- sf; 
    plot1$LFr12  <- plot1$LF_request + dplyr::lead(plot1$LF_request,1, default=0) + dplyr::lead(plot1$LF_request,02, default=0)
    plot1$LFr1  <- dplyr::lead(plot1$LF_request,1, default=0)
    plot1 <- subset(plot1, Date >= as.Date('2020-04-03') & Date <= as.Date('2020-06-01'))
    plot1 <- subset(plot1, Date <= as.Date('2020-04-16') | Date >= as.Date('2020-04-27'))
    plot1 <- subset(plot1, PPP > 1000)
    
    plot1 <- subset(sf, Date >= as.Date('2020-04-03') & Date <= as.Date('2020-06-25'))
    plot1 <- subset(plot1, Date <= as.Date('2020-04-16') | Date >= as.Date('2020-04-27'))
    plot1$week <- week(plot1$Date)
    plot1 <- aggregate(cbind(PPP, LF_request, LF_received) ~ week, plot1, sum)    

    ggplot(plot1, aes(x=log(PPP),y=log(LF_received+1))) + geom_point() + geom_smooth(method=lm, color = 'red', alpha=0) +
  xlab('Log PPP Quantity') + ylab('Log LF Funds Received') + 
  theme(axis.title = element_text(size = 21), axis.text =  element_text(size = 16))
  #scale_y_continuous(limits =c(0,NA),labels = label_number(suffix = "B", scale = 1e-9)) +
  #scale_x_continuous(labels = label_number(suffix = "B", scale = 1e-9)) 
  
  
  
ggplot(plot1, aes(x=log(PPP),y=log(LF_request))) + geom_point() + geom_smooth(method=lm, color = 'red', alpha=0) +
  xlab('Log PPP Quantity') + ylab('Log LF Funds Requested') +
  theme(axis.title = element_text(size = 21), axis.text =  element_text(size = 16)) 
  #scale_y_continuous(labels = label_number(suffix = "B", scale = 1e-9)) +
  #scale_x_continuous(labels = label_number(suffix = "B", scale = 1e-9))

# Graph of cumulative LF demand, received, and DW borrow
plot1 <- sf2 %>% group_by(Date) %>% summarise(dwm1 = sum(DW_m1, na.rm=TRUE), dwm2 = sum(DW_m2, na.rm=TRUE), 
                                              dw = sum(DW, na.rm=TRUE),
                                              LF_received = sum(LF_received, na.rm=TRUE),
                                              LF_requested = sum(LF_requested, na.rm=TRUE),
                                              PPP = sum(PPP, na.rm=TRUE))
plot1 <- plot1 %>% mutate(LFreq_cs = cumsum(LF_requested), LFrec_cs = cumsum(LF_received))
ggplot(plot1, aes(x=Date)) + geom_line(aes(y=dw, color='DW')) + geom_line(aes(y=LF_requested, color='LF Requested')) +  geom_line(aes(y=LF_received, color='LF Received')) 
  