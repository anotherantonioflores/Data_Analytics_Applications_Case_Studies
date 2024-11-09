library(tidyverse)
library(tseries)
library(quantmod)
data = read.delim("C:/Users/Client/OneDrive/UTSA MSDA/FALL 2024/DA Applications/case study 3/dow_jones_index.txt", sep = ",")



data_for_CAPM = data %>% 
  filter(quarter == 1) %>% 
  select(stock, date, close) %>% 
  mutate(close = substr(close,2,length(close))) %>% 
  mutate(close = as.numeric(close))





data_for_CAPM = data_for_CAPM %>% 
  pivot_wider(names_from = stock, values_from = close)

data_for_CAPM = data_for_CAPM %>% 
  select(!date)





delt_holder = data.frame(week=c(1:12))
col = 1
for (col in 1:30){
  place_holder = Delt(as.numeric(unlist(data_for_CAPM[col])))
  delt_holder = cbind(delt_holder, place_holder)
}





colnames(delt_holder) = c("Week","AA","AXP","BA","BAC","CAT","CSCO","CVX","DD","DIS","GE","HD","HPQ","IBM","INTC","JNJ","JPM", 
                     "KO","KRFT","MCD","MMM","MRK","MSFT","PFE","PG","T","TRV","UTX","VZ","WMT","XOM")




delt_holder = delt_holder %>% 
  select(!Week)

mktrate=apply(delt_holder, 1, mean, na.rm = TRUE)

delt_holder = cbind(delt_holder, mktrate)



boxplot(delt_holder)

beta_holder = data.frame()

i=1
for (i in 1:30) {
  model1 = lm(as.numeric(unlist(delt_holder[i])) ~ mktrate, data = as.data.frame(delt_holder))
  beta_holder = rbind(beta_holder, summary(model1)$coefficients[2,1])
}





DataMean=apply(delt_holder, 2, mean, na.rm = TRUE)
DataSD=apply(delt_holder, 2, sd, na.rm = TRUE)
# Take a look at the means and standard deviations. 
cbind(DataMean,DataSD,pct = 100*DataMean)



tbills = read.csv("C:/Users/Client/OneDrive/UTSA MSDA/FALL 2024/DA Applications/case study 3/tbillsrate.csv")

#Using the mean of the 13 weeks Tbills rate of the first qtr of 2011 for Risk-Free-Rate
RFR = mean(tbills$X13.WEEKS.BANK.DISCOUNT)

ERM = mean(delt_holder$mktrate, na.rm = TRUE) 
#Expected Value = RFR - BETA(ERM - RFR)
# Market Risk Premium = ERM-RFR
MRP = ERM-RFR

beta_holder$EV = RFR + (beta_holder$X1.53382786611695 * MRP)

beta_holder$stock = c("AA","AXP","BA","BAC","CAT","CSCO","CVX","DD","DIS","GE","HD","HPQ","IBM","INTC","JNJ","JPM", 
                           "KO","KRFT","MCD","MMM","MRK","MSFT","PFE","PG","T","TRV","UTX","VZ","WMT","XOM")


names(beta_holder)[names(beta_holder) == 'X1.53382786611695'] = 'beta'

plot(beta_holder$beta, beta_holder$EV)

beta_holder = beta_holder %>% 
  arrange(desc(EV))
head(beta_holder)





