install.packages("numbers")
install.packages("purrr")
install.packages("cryptowatchR")
install.packagers("zoo")
install.packages("lmtest")
library(cryptowatchR)
library(numbers)
library(purrr)
library(zoo)
library(tidyverse)
library(ggplot2)
library(lmtest)
get_current_price("btcusd", exchange = "coinbase")
get_assets()
get_markets("summary", exchange = "coinbase")
btc <- get_ohlc("btcusd", exchange = "coinbase")
 get_pairs()
doge_365ma



coinbase_coins = c("BTC", "ETH", "LTC", "MANA", "DAI", "OMG",
                   "ALGO", "UNI", "FIL", "SUSHI", "MATIC", "ADA", 
                   "DOT", "DOGE", "AMP", "SHIB", "AXS", "AVAX", "CRO")

 fib_ma = function(x){
  ma2 = rollmean(x[2], k = 2, fill = NA, align = "right")
  ma3 = rollmean(x[2], k = 3, fill = NA, align = "right")
  ma5 = rollmean(x[2], k = 5, fill = NA, align = "right")
  ma8 = rollmean(x[2], k = 8, fill = NA, align = "right")
  ma13 = rollmean(x[2], k = 13, fill = NA, align = "right")
  ma21 = rollmean(x[2], k = 21, fill = NA, align = "right")
  ma34 = rollmean(x[2], k = 34, fill = NA, align = "right")
  ma55 = rollmean(x[2], k = 55, fill = NA, align = "right")
  ma89 = rollmean(x[2], k = 89, fill = NA, align = "right")
  ma144 = rollmean(x[2], k = 144, fill = NA, align = "right")
  ma233 = rollmean(x[2], k = 233, fill = NA, align = "right")
  ma377 = rollmean(x[2], k = 377, fill = NA, align = "right")
  ma610 = rollmean(x[2], k = 610, fill = NA, align = "right")
  ma987 = rollmean(x[2], k = 987, fill = NA, align = "right")
  ma1597 = rollmean(x[2], k = 1597, fill = NA, align = "right")
  
  df=data.frame(ma2, ma3, ma5,ma8, ma13, ma21, ma34, ma55,
                ma89, ma144, ma233, ma377, ma610, ma987, ma1597 )
  
  coin = deparse(substitute(x))
  
  
  colnames(df) <-  paste(coin, c("MA2", "MA3","MA5", "MA8", "MA13", "MA21", "MA34", "MA55", 
                                 "MA89", "MA144", "MA233", "MA377", "MA610", "MA987", "MA1597"), 
                         sep = "_")
  cbind(x[1:2], df)      
}


btc_fibma <- fib_ma(btc) %>% 
  filter(CloseTime > "2020-07-01")

btc_fibma
names(btc_fibma)[2] <- "btc Price"
  
gather(function_test, MA, USD, OpenPrice:btc_MA1597) %>% 
  filter(CloseTime > "2014-10-01") %>% 
  ggplot(aes(CloseTime, USD, color = MA)) + geom_line() + scale_y_log10()


eth <- get_ohlc("ethusd", exchange = "coinbase")
eth_fibma <- filter(fib_ma(eth), CloseTime > "2020-07-01") %>% select(-eth_MA1597)
names(eth_fibma)[2] <- "eth Price"

gather(eth, MA, USD, OpenPrice:eth_MA1597) %>% 
  filter(CloseTime > "2014-10-01") %>% 
  ggplot(aes(CloseTime, USD, color = MA)) + geom_line() + scale_y_log10()

doge <- get_ohlc("dogeusd", exchange = "kraken")
doge_fibma <- filter(fib_ma(doge), CloseTime > "2020-07-01") %>%
  select(-doge_MA233, -doge_MA377, -doge_MA610, -doge_MA987, -doge_MA1597)
names(doge_fibma)[2] <- "doge Price"

coin_prices <- left_join(left_join(btc_fibma, 
                                   eth_fibma,
                                   by = "CloseTime"),
                         doge_fibma,
                         by = "CloseTime")

names(coin_prices)[1] <- "Date"

mediacost_wide <- pivot_wider(mediacostandMA[-4], names_from = Channel,  values_from = c(Cost, ma2, ma3, ma5, ma8, ma13, ma21, ma34, ma55) )

 mediacost_wide[is.na(mediacost_wide)] <- 0
 
left_join(mediacost_wide, coin_prices, by = "Date")

features <- gemini_data %>% 
  group_by(Date) %>% 
  summarise(Signups = sum(Signups),
            Traders = sum(Traders)) %>% 
  left_join(left_join(mediacost_wide, coin_prices, by = "Date"), by= "Date")

features[is.na(features)] <- 0

names(features) <- make.names(names(features), unique = TRUE)

set.seed(55)

gemini_data %>% f
  

## 75% of the sample size
smp_size <- floor(0.80 * nrow(features))

## set the seed to make your partition reproducible

train_ind <- sample(seq_len(nrow(features)), size = smp_size)

train <- features[train_ind, ]
test <- features[-train_ind, ]
view(features)

fit_rf_traders <- randomForest(Traders ~., 
                                 train[-c(1, 2)] , 
                                 ntree=1000, 
                                 mtry = 7, 
                                 importance=TRUE)


names(features)
# Uses Random Forest Direct/Organic Traffic model to predict Direct Traffic from Test Data set
yhat_rf_converted <- predict(fit_rf_converted, test)


plot( yhat_rf_converted, test$Traders,
      # log = "xy",
      main = "Actual vs. Predicted Converted Signups",
      ylab = "Predicted",
      xlab="Actual", log = "xy")

varImpPlot(fit_rf_converted , 
           main =  "Feature Importance Affecting Quality Signups")

round(mean(fit_rf_converted$rsq), 2)
fit_rf_converted$rsq


fit_rf_converted$importance %>% arrange(IncNodePurity)


names(website1130) <- make.names(names(website1130), unique = TRUE)

direct_sessions_features <- 
  features[-c(1, 2, 3)] %>% 
           left_join( website1130[c(1, 4)], by = "Date")

train_ind1 <- sample(seq_len(nrow(direct_sessions_features )), size = smp_size)

train <- features[train_ind, ]
test <- features[-train_ind, ]


fit_rf_converted1 <- randomForest(Traders ~., 
                                 train[-c(2)] , 
                                 ntree=1000, 
                                 mtry = 7, 
                                 importance=TRUE)

varImpPlot(fit_rf_converted1 , 
           main =  "Feature Importance Affecting Quality Signups")
view(train)

trader_importance <- as.data.frame(fit_rf_traders$importance) 
trader_importance$feature <- rownames(trader_importance)


imp <- trader_importance %>% 
  mutate(funnel = case_when(
    str_detect(feature, "Twitter") ~ "Mid Funnel - CR",
    str_detect(feature, "Search") ~ "Lower Funnel",
    str_detect(feature, "FB.Remarketing") ~ "Mid Funnel - CR",
    str_detect(feature, "Facebook") ~ "Mid Funnel - CR",
    str_detect(feature, "Discovery") ~ "Mid Funnel - Google",
    str_detect(feature, "Snap") ~ "Mid Funnel - CR",
    str_detect(feature, "Tik") ~ "Mid Funnel - CR",
    str_detect(feature, "UAC") ~ "Lower Funnel",
    str_detect(feature, "YouTube") ~ "Upper Funnel",
    str_detect(feature, "Display") ~ "Mid Funnel - Google",
    str_detect(feature, "Roku") ~ "Upper Funnel - CR",
    str_detect(feature, "Reddit") ~ "Upper Funnel - CR",
    TRUE ~ "Coins"
  ))

str(imp)

imp %>% 
  filter(funnel != "Coins") %>% 
  group_by(funnel, feature) %>% 
  summarise(IncNodePurity = sum(IncNodePurity)) %>% 
  ggplot(aes(funnel, IncNodePurity, fill = feature)) + geom_col() + ggtitle("Effect of Funnels on Traders")

  trader_importance[,1]
  
lm_trader  <-lm(Traders ~. , data = features)
par(mar=c(1,1,1,1))
plot(Traders ~ ma21_Snap.Ads, features)

features %>%
  ggplot(aes(Traders, ma55_Twitter.Ads)) + geom_point() +
   ggtitle("Traders versus Snap 21-day MA") + theme_classic()

left_join(features, OrganicDirect, by = "Date") %>% 
  mutate(combined = (ma21_Snap.Ads+ma34_Twitter.Ads + ma21_Facebook.Ads)) %>% 
  ggplot(aes(, combined)) + geom_point() +
  ggtitle("Traders versus Paid Social") + theme_classic() +ylab("Paid Social Spend")

features %>%
  ggplot(aes(Traders, (ma21_Snap.Ads+ma34_Twitter.Ads))) + geom_point() +
  ggtitle("Traders versus Snap 21-day MA") + theme_classic()

left_join(features, filter(OrganicDirect, Channel == "Organic"), by = "Date") %>% 
  mutate(combined = (ma21_Snap.Ads+ma34_Twitter.Ads )) %>% 
  ggplot(aes(Traders, ma21_Snap.Ads)) + geom_point() +
  ggtitle("Traders versus Snap 21-day MA") + theme_classic() + ylim(300, 50000)

write.csv(trader_importance, "C:\\Users\\Eddie\\Downloads\\trader_importance.csv", row.names=TRUE)

left_join(features, pod, by = "Date") %>% 
  ggplot(aes(signups, ma21_Snap.Ads)) + geom_point() +
  ggtitle("Twitter/Snap Impact on Direct, Organic, and Paid Search Signups" ) + theme_classic() + xlim(300, 10000) + ylim(500, 40000)

pod <- PaidOrganicDirect %>% group_by(Date) %>% 
  summarise(sessions = sum(sessions), signups = sum(Signup))

ggplot(features , aes(Traders, ma34_Twitter.Ads)) + geom_point()
rlang::last_error()

features_long <- features %>% 
  pivot_longer(!Date, "MAs", "Value") %>% 
  mutate(funnel = case_when(
    str_detect(MAs, "Twitter") ~ "Mid Funnel - CR",
    str_detect(MAs, "Search") ~ "Lower Funnel",
    str_detect(MAs, "FB.Remarketing") ~ "Mid Funnel - CR",
    str_detect(MAs, "Facebook") ~ "Mid Funnel - CR",
    str_detect(MAs, "Discovery") ~ "Mid Funnel - Google",
    str_detect(MAs, "GDN.Remarketing") ~ "Mid Funnel - Google",
    str_detect(MAs, "Snap") ~ "Mid Funnel - CR",
    str_detect(MAs, "Tik") ~ "Mid Funnel - CR",
    str_detect(MAs, "UAC") ~ "Lower Funnel",
    str_detect(MAs, "YouTube") ~ "Upper Funnel",
    str_detect(MAs, "Display") ~ "Mid Funnel - Google",
    str_detect(MAs, "Roku") ~ "Upper Funnel - CR",
    str_detect(MAs, "Reddit") ~ "Upper Funnel - CR",
    str_detect(MAs, "Signups") ~ "Conversions",
    str_detect(MAs, "Traders") ~ "Conversions",
    str_detect(MAs, "Signups") ~ "Conversions",
    str_detect(MAs, "Traders") ~ "Conversions",
    str_detect(MAs, "Organic") ~ "Organic/Direct",
    str_detect(MAs, "Direct") ~ "Organic/Direct",
    TRUE ~ "Coins"))  

price_ma_chart <- features_long %>% 
  filter(MAs %in% c("ma21_Twitter.Ads",
                       "ma34_Snap.Ads",
                    "btc.Price")) %>% 
  ggplot(aes(Date, value, color = MAs)) + geom_line()

coin_conversions_long <- features_long %>% 
  filter( funnel %in% c("Conversions", "Coins"),
          !MAs %in% c("Cost_Direct", "Cost_Organic"))
         
coin_conversions_wide <- spread(coin_conversions_long[1:3], MAs, value)

coin_trader_LM <- lm( data = coin_conversions_wide[-((length(coin_conversions_wide[-1]) -1))], Traders ~.)

plot(coin_conversions_wide$Date, coin_trader_LM$residuals)

date_resid <- as.data.frame(cbind(as.Date(coin_conversions_wide$Date), coin_trader_LM$residuals))
names(date_resid) <- c(date, residuals)
dwtest(coin_trader_LM)
colnames(date_resid)<- c("date", "residuals")

date_resid%>% ggplot(aes(as.Date(date), residuals)) + geom_point() + geom_smooth() + geom_line()
