library(tidyverse)
library(here)
library(lubridate)
#replace path with your own
data = read.delim("C:/Users/Client/OneDrive/UTSA MSDA/FALL 2024/DA Applications/week 8/dow_jones_index.txt", sep = ",")

summary(data)

clean_data = data %>% 
  mutate(stock = as.factor(stock)) %>% 
  mutate(date = mdy(date)) %>% 
  arrange(quarter, stock, date)

summary(clean_data)  



Train_data = clean_data %>% 
  filter(quarter == 1)
Test_data = clean_data %>% 
  filter(quarter ==2)



ggplot(data = Train_data, aes(x =date, y = percent_change_next_weeks_price, group = stock, color = stock)) +
  geom_line()


############################

stock_vector = Train_data$stock

stock_list = levels(stock_vector)


Train_data$lag1 = 0
x = 1
for (i in 2:nrow(Train_data)){
  
    if(Train_data$stock[i] == stock_list[x]){
      j = i-1
      Train_data$lag1[i] = Train_data$percent_change_next_weeks_price[j]
    } else{
      Train_data$lag1[i] = 0
      x = x + 1 
    }
        
  }



Train_data$lag2 = 0
x = 1
i = 3
while (i < nrow(Train_data)){
  if(Train_data$stock[i] == stock_list[x]){
    j = i-2
    Train_data$lag2[i] = Train_data$percent_change_next_weeks_price[j]
    i = i+1
  } else{
    Train_data$lag2[i] = 0
    i = i+1
    Train_data$lag2[i] = 0
    i = i+1
    j = i-2
    Train_data$lag2[i] = Train_data$percent_change_next_weeks_price[j]
    x = x + 1
  }
}


Train_data$lag3 = 0
x = 1
i = 4
while (i < nrow(Train_data)){
  if(Train_data$stock[i] == stock_list[x]){
    j = i-3
    Train_data$lag3[i] = Train_data$percent_change_next_weeks_price[j]
    i = i+1
  } else{
    Train_data$lag3[i] = 0
    i = i+1
    Train_data$lag3[i] = 0
    i = i+1
    Train_data$lag3[i] = 0
    i = i+1
    j = i-2
    Train_data$lag3[i] = Train_data$percent_change_next_weeks_price[j]
    x = x + 1
  }
}



Train_data %>% 
  select(stock, percent_change_next_weeks_price, lag1, lag2, lag3)



test_stock = Train_data %>% 
  filter(stock == "CAT")

ggplot(data = test_stock, aes(x =date)) +
  geom_line(aes(y=percent_change_next_weeks_price))+
  geom_line(aes(y=lag1))+
  geom_line(aes(y=lag2))+
  geom_line(aes(y=lag3))




###########################
#LM + LAG

y = Train_data %>% 
  filter(stock == "AA") %>% 
  select(percent_change_next_weeks_price)

nn = 1:nrow(y)
length(nn)
length(y)
day = seq(from = 1, to = dim(y)[1], by = 1)
#df = cbind(day, df)

#make a time-series plot
par(mfrow = c(1,1))
plot(day, y$percent_change_next_weeks_price)

#build a trend model
m1 = lm(y$percent_change_next_weeks_price ~ day - 1)# forces start at 0
summary(m1)
preds = predict(m1, type = "response")

#make a plot with actual and predicted data
plot(y$percent_change_next_weeks_price, ylab = "pct change", xlab = "Days", pch = "*")
points(preds, col="red", pch="*")


lag.plot(y$percent_change_next_weeks_price, pch = "*", set.lags = 1:4)

m2 = lm(y$percent_change_next_weeks_price ~ lag(y$percent_change_next_weeks_price, n = 1) - 1)
summary(m2)

preds_ar = predict(m2, type = "response")
#make a plot with actual and predicted data
plot(y$percent_change_next_weeks_price, ylab = "pct change", xlab = "Days", pch = "*")
points(preds, col="red", pch="*")
points(preds_ar, col="green", pch="*")



#Measures
ydat = cbind(y$percent_change_next_weeks_price[3:length(y$percent_change_next_weeks_price)], preds_ar[2:length(preds_ar)]) #making two columns: actual y and predicted y. 

# Starts from day 3 (non-zero)

#compute MSE, ME, MAE, MAPE
mse = mean((ydat[,1] - ydat[,2])^2)
mae = mean(abs(ydat[,1] - ydat[,2]))
me = mean(ydat[,1] - ydat[,2])
mape = mean(abs(ydat[,1] - ydat[,2])*100/ydat[,1])


