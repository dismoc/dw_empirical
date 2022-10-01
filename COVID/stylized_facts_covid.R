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

# Purpose is to see reseve / asset to fed lending in each district during covid quarters
dwborrow <- read_csv("D:/Research/DW lending empirical/Data/dwborrow.csv")
conv <- data.frame(Name = unique(dwborrow$Lending.Federal.Reserve.district), FED = as.numeric(str_extract(unique(dwborrow$Lending.Federal.Reserve.district), "[[:digit:]]+")))

cov <- subset(df, as.Date(df$Date) >= as.Date('2019-03-31') & as.Date(df$Date) <= as.Date('2020-06-30'))



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
  
# Share of banks that borrow (2019-2020)
plot1 <- cov %>% count(Date, dwborrow_bin)
ggplot(data.frame(plot1), aes(fill=dwborrow_bin, y=n, x=Date)) + 
  geom_bar(position="stack", stat="identity")
