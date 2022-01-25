install.packages("bigrquery")
install.packages("tidyverse")

library(bigrquery)
library(cryptowatchR)
library(tidyverse)
library(lubridate)
library(randomForest)

#### The allocation model consist of 7 different parts and are as follows 
  #1: Crypto Prices and Transformation
  #2: Paid Media Data
  #3: L2 data from Gemini
  #4: Feature Creation
  #5: Random Forest Model
  #6: Budget Allocation Scenarios
  #7: Identifying top performing Budget Allocation Scenarious

### Part 1 - Create a table with cryptocurrency prices and their moving averages

## Get coin prices 
btc <- get_ohlc("btcusd", exchange = "coinbase")
# ether
eth <- get_ohlc("ethusd", exchange = "coinbase") %>%
  select(Date = CloseTime, ETH = OpenPrice)
#litecoin
ltc <- get_ohlc("ltcusd", exchange = "coinbase") %>%
  select(date = CloseTime, ltc_Price = OpenPrice)

#dogecoin
doge <-  get_ohlc("dogeusd", exchange = "kraken") %>%
  select(Date = CloseTime, doge = OpenPrice) 

#shiba inu / Early Date is May 2021 -- leaving out
 ##shib <- get_ohlc("shibusd", exchange = "ftx" ) %>% # ftx
 ## select(Date = CloseTime, SHIB = OpenPrice) 

  ## Gets various moving averages for a column
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

## Adds moving averages and changes column names 

btc <- na.omit(fib_ma(btc))
names(btc)[2] <- "btc_price"
names(btc)[1] <- "date"

eth <- na.omit(fib_ma(eth))
names(eth)[2] <- "eth_Price"
names(eth)[1] <- "date"

ltc <- fib_ma(ltc) %>% select(-ltc_MA1597) %>% na.omit()
names(ltc)[2] <- "ltc_Price"
names(ltc)[1] <- "date"


doge <- fib_ma(doge) %>% 
  select(-doge_MA233, -doge_MA377, -doge_MA610, -doge_MA987, -doge_MA1597) %>% 
  na.omit()

names(doge)[2] <- "doge_Price"
names(doge)[1] <- "date"

## Combines coin prices and their respective moving averages into a dataframe. 
coins <- left_join(
  left_join(
    left_join(btc, eth, by = "date"),
    ltc, by="date"), doge, by = "date") %>% 
  filter(date > "2020-11-01")

### Part 2: Paid Media Channel from BigQuery extraction and transformation
## Authenticating into BigQuery
bq_auth()
# GCP Project - Billing Account
billing <- 'getbiofuel-map'

# SQL to extract paid media  data to get cost
sql <- "SELECT * FROM `getbiofuel-map.gemini.media_channel_cost_conv`" # All Media spend should be in this table 

tb <- bq_project_query(billing, sql)
query <- bq_table_download(tb)

# cast cost data to wide format and renames channels
cost_wide <- query %>% 
  filter(!(Channel %in% c("Direct", "Organic"))) %>% 
  rename(date = Date) %>% 
  mutate(channel =
           case_when(
             str_detect(Channel, "FB Remarketing") ~ "Facebook",
             str_detect(Channel, "Facebook Ads") ~ "Facebook",
             str_detect(Channel, "Bing - Search") ~ "Bing",
             str_detect(Channel, "Twitter") ~ "Twitter",
             str_detect(Channel, "Twitter Retargeting") ~ "Twitter",
             str_detect(Channel, "Discovery") ~ "Discovery",
             str_detect(Channel, "Google Display") ~ "GDN",
             str_detect(Channel, "GDN Remarketing") ~ "GDN",
             str_detect(Channel, "Snap") ~ "Snap",
             str_detect(Channel, "Brand Search") ~ "Brand",
             TRUE ~ Channel)) %>%
  ##group_by(Date, channel) %>% 
  ##summarise(cost = sum(Cost)) %>% 
  dcast(date ~ channel , value.var = "Cost", sum)

unique(cost_wide$)
### Part 3: Signups and Traders data from Gemini. 

sql_conversions <- "SELECT * FROM `getbiofuel-map.history.attribution_channels_history`" # All Media spend should be in this table 

tb_conversions <- bq_project_query(billing, sql_conversions)

query_conversions <- bq_table_download(tb_conversions)

  # We do not need channel attribution but just total signups and traders per day for the model
signups_traders <- query_conversions %>% 
  group_by(signup_date) %>% 
  summarise(signups = sum(attribution), 
            traders = sum(traders)) %>% 
  select(date = signup_date, 
         signups = signups, 
         traders = traders)

### Part 4: Feature creation

  # Joins cryptocurrency, conversion, and paid media channel cost in a wide 
  # format to be used in the model
features <- left_join(  
  na.omit(left_join(
    coins, cost_wide,
    by = "date" )),
  signups_traders,
  by = "date") %>% 
 mutate( # creates columns for seasonality
         yday = yday(date),
         mday = day(date),
         wday = wday(date), 
         week = week(date),
         month = month(date),
         year = year(date)) %>% 
  na.omit()

### Part 5: Random Forest Model

  # Creates a training and test sets from the Features table. 

start.test_set <- ymd("2021-12-01")
end.test_set <- ymd("2022-01-15")

train_set <- filter(features, date < start.test_set) %>% 
  select(-date, -signups)

test_set <- filter(features, date >= start.test_set & 
                     date <= end.test_set) %>% 
  select(-date, -signups)

  # The following will be targeting traders as the dependant variable.  Signups and traders are interchangeable
  # depending on objective

rf_model <- randomForest(traders ~., 
                               train_set, 
                               ntree=500, 
                               mtry = 7, 
                               importance=TRUE)
  # Shows chart plotting level of importance for features in models
varImpPlot(rf_model , 
           main =  "Feature Importance Affecting Quality Signups")

 # Get r-squared
round(mean(rf_model$rsq), 3)

  # uses model to predict traders on test set
predicted_traders <- predict(rf_model, test_set)

## Part 6:  Creating budget scenarios

  # Total daily budget 
dailybudget <- 500000

  # The following are percentage ranges of each channel's budget
  # The ranges are adjustable according to what the client is comfortable with. 
  # Try to limit the range as too many permutations are hard to process
twitter <- seq(.0, .10, .025)
snap <- seq(.0, .10, .025)
discovery <- seq(.0, .10, .025)
display <- seq(.00, .1, .025)
YouTube <- seq(.00, .10, .020)
Roku <- seq(.00, .05, .1)
Brand <- .02
Retargeting <- .02
Nonbrand <- .5
UAC <- seq(.08, .20, .02)
Bing <- .03
Facebook <- seq(.0, .10, .025)
Facebook <- seq(.0, .10, .025)
TikTok = seq(.0, .10, .025)
Reddit <- 0

  #creates all permutations of budget allocations and then filters permutations that equal 1 or 100%
cross_cost <- crossing(twitter, 
                       snap, 
                       discovery,
                       display, 
                       YouTube, 
                       Roku, 
                       Nonbrand,
                       UAC, 
                       Brand,
                       Bing, 
                       Facebook, 
                       Retargeting,
                       Reddit, 
                       TikTok) %>% 
  mutate(total = 
           twitter + snap + discovery + display +YouTube +
           Roku + Nonbrand + Brand + UAC + Retargeting + Facebook + Bing + Reddit + TikTok) %>% 
  filter(total == 1)


  # multiplies the budget allocation percentage time the actual budget
scenarios <- cross_cost %>% 
  mutate(BrandSearch=  dailybudget*Brand,
         Nonbrand = Nonbrand*dailybudget,
         UAC = UAC * dailybudget, 
         Retargeting_spend = Retargeting*dailybudget, 
         Snap = snap* dailybudget, 
         GDN = display*dailybudget, 
         Twitter = dailybudget* twitter,
         Facebook= dailybudget*Facebook,
         Bing = Bing*dailybudget,
         Discovery = dailybudget*discovery, 
         YouTube = YouTube*dailybudget,
         Roku = Roku*dailybudget, 
         Reddit = Reddit*dailybudget,
         total_spend = total*dailybudget,
         index =1, 
         row_number = row_number()) 

  # adds seasonality
scenerio_features <- coins %>% 
  filter(as.Date(date) == "2021-12-01") %>% 
  mutate(index = 1) %>% 
  left_join(scenarios, by = "index") %>% 
  mutate( # creates columns for seasonality
    yday = yday(date),
    mday = day(date),
    wday = wday(date), 
    week = week(date),
    month = month(date),
    year = year(date))

### Part 7: Run model on Budget Scenarios to find ideal scenario

  #
predictions <- predict(rf_model, 
                       scenerio_features) 

max(predictions)
min(predictions)
mean(predictions)

pred_df <-  as.data.frame(predictions)

qplot(predictions, 
      main = "Budget Allocation Simulations" ,
      geom="histogram", bins = 100,
      xlab = "Quality Signups", 
      ylab = "Simulations"
) +
  theme_classic()

max(pred_df)

best_performing_index <- which(pred_df == max(pred_df))



scenerio_features[best_performing_scendarios_index, 61:81]

names(scenerio_features)

max_scenarios <- scenerio_features[which(predictions == max(predictions)) , c(61:81)]
max_scenarios[4,]
format(max_scenarios$UAC, scientific = F)

