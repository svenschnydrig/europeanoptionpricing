library(data.table)
library(tidyverse)
library(caret)
library(caretEnsemble)
library(dplyr)
library(tinytex)
#linkcolor [red]
#anchorcolor [black]
#citecolor [green]
#filecolor [cyan]
#menucolor [red]
#runcolor [cyan - same as file color]
#urlcolor [magenta]
#allcolors -- use this if you want to set all links to the same color

# Load data with data.table package ---------------------------------------
library(data.table)
df=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/df.csv")
d=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/d.csv")
v=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/v.csv")
sp=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/sp.csv")

#Create copy of data
#df.raw=df
#d.raw=d
#v.raw=v
#sp.raw=sp
#r.raw=r

#Transform date columns into date format
library(lubridate)
df$date=ymd(df$date)
df$exdate=ymd(df$exdate)
d$exdate=ymd(d$exdate)
v$date=ymd(v$date)
sp$date=ymd(sp$date)
d$date=ymd(d$date)


#Divide strike price by 1000 because strike price is times 1000 in the dataset
df$strike_price=df$strike_price/1000


#Compute time to maturity by subtracting exdate from date
df$maturity=df$exdate-df$date
df$maturity=as.integer(df$maturity+1)
df$t=df$maturity/365
str(df)

df=select(df, date, exdate, cp_flag, strike_price, best_bid, best_offer, maturity, t)

# Volatility  ---------------------
str(v)
v = subset(v, days == 30)
v=select(v, date, volatility)
df=merge(df, v, by="date")
str(df)

#alternative calculation for volatility, problem is we don't have the closing prices from december 1995 and therefore can't compute volatility Jan 96'
library(TTR)
sp$sigma=volatility(sp$close, n = 21, calc = "close", N = 253)

# Stock price  ---------------------
str(sp)
sp=select(sp, date, close)
df=merge(df, sp, by="date")

# Risk Free Rate  ---------------------
r=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/r.csv")
r$Date=mdy(r$Date)

r = rename(r, date = Date)

library(dplyr) 
r = r %>% distinct(date, .keep_all= TRUE)

#merge data sets of options data and risk free rates
rf.prep = left_join(df, r, by="date")
#rf.prep = full_join(df, r, by="date")

colSums(is.na(r))

#Insert right risk free based on time to maturity 
rf.prep$rf = ifelse(rf.prep$maturity<30, rf.prep$`1 Mo`, ifelse(rf.prep$maturity<92, rf.prep$`3 Mo`, ifelse(rf.prep$maturity<183, rf.prep$`6 Mo`, ifelse(rf.prep$maturity<365, rf.prep$`1 Yr`, ifelse(rf.prep$maturity<1095, rf.prep$`3 Yr`, ifelse(rf.prep$maturity<1825, rf.prep$`5 Yr`, ifelse(rf.prep$maturity<2555, rf.prep$`7 Yr`,ifelse(rf.prep$maturity<3650, rf.prep$`10 Yr`, ifelse(rf.prep$maturity<3650, rf.prep$`20 Yr`, rf.prep$`30 Yr`)))))))))

df$rf = ifelse(rf.prep$maturity<31, rf.prep$`1 Mo`, ifelse(rf.prep$maturity<93, rf.prep$`3 Mo`, ifelse(rf.prep$maturity<184, rf.prep$`6 Mo`, ifelse(rf.prep$maturity<366, rf.prep$`1 Yr`, 
        ifelse(rf.prep$maturity<731, rf.prep$`2 Yr`, ifelse(rf.prep$maturity<1096, rf.prep$`3 Yr`, ifelse(rf.prep$maturity<1826, rf.prep$`5 Yr`, ifelse(rf.prep$maturity<2556, rf.prep$`7 Yr`,
        ifelse(rf.prep$maturity<3652, rf.prep$`10 Yr`, ifelse(rf.prep$maturity<7304, rf.prep$`20 Yr`, rf.prep$`30 Yr`))))))))))

str(rf.prep)
str(df)
tail(df)

#Add matched riskfree rate back to original dataset
#rf.prep=select(rf.prep, date, rf)
#df2=merge(df, rf.prep, by="date")

#Add matched riskfree rate back to original dataset, it's in the same order with the same amount of observations
#df$rf=rf.prep$rf
colSums(is.na(df)) #we don't have the riskfree rate for some dates when the market was open, for example 14-15.01.1996, as only a small number of cases of very old date are concerned (0.8% of df) we will remove these rows
df$rf=as.numeric(df$rf)
df=na.omit(df)
df$rf=df$rf/100
str(df)
colSums(is.na(df))

#new_DF <- df[is.na(df$rf),]

#Anzahl der vollständigen Zeilen
sum(complete.cases(df))
#Anzahl der nicht vollständigen Zeilen
sum(!complete.cases(df))
calls_full=df[complete.cases(df), ]

#fwrite(calls_bs, "calls_bs.csv")

# Dividend Yield ---------------------
str(d)
d=select(d, date, rate)
d$rate = d$rate/100
d$rate=lag(d$rate)

df=merge(df, d, by="date")

str(df)

#rename variables
df=as_tibble(df)
str(df)
df = df %>% 
  rename(
    S = close,
    K = strike_price,
    riskfree=rf,
    T=t,
    sigma=volatility,
    D=rate
  )


#Calculate mean of best bid and best ask price
df$bidaskmean = (df$best_bid+df$best_offer)/2
str(df)
#fwrite(df, file = "df.csv")

#split into call and put data set
call = subset(df, cp_flag == "C")
put = subset(df, cp_flag == "P")


# Black-Scholes Price with Dividend Yield -------------------------------------------

options( scipen=999)

#call
call$d1 = (log(call$S/call$K)+(call$riskfree-call$D+0.5*call$sigma^2) * call$T)/(call$sigma * sqrt(call$T))
call$d2 = call$d1-call$sigma * sqrt(call$T)

call$bs = call$S * exp(-call$D* call$T) * pnorm(call$d1)-call$K * exp(-call$riskfree * call$T) * pnorm(call$d2) 
head(call)

#put
put$d1 = (log(put$S/put$K)+(put$riskfree-put$D+0.5*put$sigma^2) * put$T)/(put$sigma * sqrt(put$T))
put$d2 = put$d1-put$sigma * sqrt(put$T)
put$bs = put$K * exp(-put$riskfree * put$T) * pnorm(-put$d2) - put$S * exp(-put$D* put$T) * pnorm(-put$d1)  
head(put)

summary(call)
summary(put)

summary(df)

#call$call_call_value=round(call$call_call_value, digits = 2)
head(call)

call= subset(call, select = -c(d1,d2, cp_flag))
call$d1=NULL
call$d2=NULL
call$cp_flag=NULL

put= subset(put, select = -c(d1,d2, cp_flag))
put$d1=NULL
put$d2=NULL
put$cp_flag=NULL

df2 <- subset(df, date > as.Date("2015-12-31") & date < as.Date("2020-09-01"))

call_train <- subset(call, date > as.Date("2015-12-31") & date < as.Date("2020-09-01"))
put_train <- subset(put, date > as.Date("2015-12-31") & date < as.Date("2020-09-01"))

call_valid <- subset(call, date > as.Date("2020-08-31") & date < as.Date("2020-11-01"))
put_valid <- subset(put, date > as.Date("2020-08-31") & date < as.Date("2020-11-01"))

call_test <- subset(call, date > as.Date("2020-10-31"))
put_test <- subset(put, date > as.Date("2020-10-31"))

summary(call_train)

mean(call_train$K)
mean(call_train$S)
mean(call_train$sigma)
mean(call_train$T)
mean(call_train$riskfree)
mean(call_train$D)

sd(call_train$K)
sd(call_train$S)
sd(call_train$sigma)
sd(call_train$T)
sd(call_train$riskfree)
sd(call_train$D)

devtools::install_github("rstudio/reticulate")

library(data.table)
fwrite(df, "df-1996-2020.csv")

df=fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/R/df-1996-2020.csv")

fwrite(call, "call.csv")
fwrite(put, "put.csv")

fwrite(call_test, file = "call_test.csv")
fwrite(put_test, file = "put_test.csv")

fwrite(call_valid, file = "call_valid.csv")
fwrite(put_valid, file = "put_valid.csv")

fwrite(call_train, file = "call_train.csv")
fwrite(put_train, file = "put_train.csv")



head(puts)
tail(puts)

fwrite(calls, file = "calls.csv")
fwrite(puts, file = "puts.csv")

str(call)


library(Metrics)
#call accuracy
mape(call$bidaskmean, call$bs)
mse(call$bidaskmean, call$bs)
mdae(call$bidaskmean, call$bs)
mae(call$bidaskmean, call$bs)
#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(call$bidaskmean, call$bs)

#put accuracy
mape(put$bidaskmean, put$bs)
mse(put$bidaskmean, put$bs)
mdae(put$bidaskmean, put$bs)
mae(put$bidaskmean, put$bs)
#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(put$bidaskmean, put$bs)




# Split train and test data ----------------------------------------------
#for paper beautiful

df2 <- subset(df, date > as.Date("2015-12-31") & date < as.Date("2020-09-01"))
df2 <- df2[c("date", "exdate", "S", "K", "T", "sigma", "riskfree", "D", "best_bid", "best_offer", "bidaskmean")]

library(tidyverse)
df2 %>% 
  rename(
    'Strike price' = K,
    'Security price' = S,
    'Maturity (in y)' = T,
    'Riskfree rate' = riskfree,
    'Dividend yield' = D,
    'Best bid' = best_bid,
    'Best offer' = best_offer,
    'Bid-ask mean' = bidaskmean
    )

names(df2)[names(df2) == "K"] <- "Strikeprice"
names(df2)[names(df2) == "S"] <- "Securityprice"
names(df2)[names(df2) == "T"] <- "Maturity"
names(df2)[names(df2) == "riskfree"] <- "Riskfree"
names(df2)[names(df2) == "D"] <- "Dividenyyield"
names(df2)[names(df2) == "best_bid"] <- "Bestbid"
names(df2)[names(df2) == "best_offer"] <- "Bestoffer"
names(df2)[names(df2) == "bidaskmean"] <- "Bidaskmean"
names(df2)[names(df2) == "Bid/ask_mean"] <- "Bidaskmeantest"

str(df2)

summary(df2)

library(stargazer)
stargazer(df2)


library(xtable)
print(xtable(t(summary(1:8))), type="html", file="xt.html", include.rownames=FALSE)

library(tidyverse)

train = bs[1:4125716,] 
valid = bs[4125717:4354922,] 
test = bs[4354923:4584129,] 

train_short = bs[3000000:4125716,] 

str(train)

set.seed(42)
rows <- sample(nrow(train))
train <- train[rows, ]
rows <- sample(nrow(test))
test <- test[rows, ]
rows <- sample(nrow(valid))
valid <- valid[rows, ]

rows <- sample(nrow(train_short))
train_short_sampled <- train_short[rows, ]

library(data.table)
fwrite(valid, file = "valid.csv")
fwrite(train, file = "train.csv")
fwrite(test, file = "test.csv")
fwrite(bs, file = "bs.csv")
fwrite(train_short, file = "train_short.csv")
fwrite(train_short_sampled, file = "train_short_sampled.csv")

str(bs)
