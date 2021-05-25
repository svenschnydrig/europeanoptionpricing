library(data.table)
calls2 = fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/output/test_call.csv")

options( scipen=999)

str(calls)

#create moneyness variable to filter according to moneyness
calls$moneyness = calls$S / calls$K

t=subset(calls, maturity>30)
t=subset(calls, maturity>365)
nrow(t)/nrow(calls)

calls$Moneyness = ifelse(calls$moneyness>1.15, "4) DITM", ifelse(calls$moneyness>1.05, "3) ITM", ifelse(calls$moneyness>1.01, "2) NTM", ifelse(calls$moneyness>0.99, "1) ATM", 
            ifelse(calls$moneyness>0.95, "2) NTM", ifelse(calls$moneyness>0.85, "5) OTM", "6) DOTM"))))))
calls$Moneyness=as.factor(calls$Moneyness)

levels(calls$Moneyness)

calls$Maturity = ifelse(calls$maturity<14, "1) <14d", ifelse(calls$maturity<32, "2) 14-31d", ifelse(calls$maturity<93, "3) 1m-3m", 
                 ifelse(calls$maturity<184, "4) 3-6m", ifelse(calls$maturity<366, "5) 6-12m", 
                 ifelse(calls$maturity<731, "6) 1-2yr", "7) >2yr"))))))

calls$Maturity = ifelse(calls$maturity<14, "1) <14d", ifelse(calls$maturity<32, "2) 14-31d", ifelse(calls$maturity<93, "3) 1m-3m", 
                 ifelse(calls$maturity<184, "4) 3-6m", ifelse(calls$maturity<366, "5) 6-12m", "7) >2yr")))))

calls$Maturity = ifelse(calls$maturity<14, "1) <30d", ifelse(calls$maturity<183, "2) <6m", ifelse(calls$maturity<365, "3) <1y", 
                 ifelse(calls$maturity>=365, "4) 1-3y", NA))))

calls$Maturity = ifelse(calls$maturity<14, "1) <14d", ifelse(calls$maturity<30, "2) 15-30d", ifelse(calls$maturity<92, "3) 1m-3m", ifelse(calls$maturity<183, "4) 3m-6m", 
                 ifelse(calls$maturity<366, "5) 6m-12m",  ifelse(calls$maturity>=366, "6) 1-3y", NA))))))
calls$Maturity=as.factor(calls$Maturity)

sum(is.na(calls$Maturity))

summary(calls)

# Overall -----------------------------------------------------------------
str(calls)

calls=subset(calls, bidaskmean>1)

t=subset(calls, best_bid>1)
ti=subset(calls, bidaskmean>1)
library(Metrics)
#overall accuracy
mape(calls$bidaskmean, calls$wdnn)
mape(calls$bidaskmean, calls$bs)
smape(calls$bidaskmean, calls$wdnn)
smape(calls$bidaskmean, calls$bs)
mase(calls$bidaskmean, calls$wdnn)
mase(calls$bidaskmean, calls$bs)
mse(calls$bidaskmean, calls$wdnn)
mse(calls$bidaskmean, calls$bs)
rmse(calls$bidaskmean, calls$wdnn)
rmse(calls$bidaskmean, calls$bs)
mdae(calls$bidaskmean, calls$wdnn)
mdae(calls$bidaskmean, calls$bs)
mae(calls$bidaskmean, calls$wdnn)
mae(calls$bidaskmean, calls$bs)
#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(calls$bidaskmean, calls$wdnn)
bias(calls$bidaskmean, calls$bs)


library(Metrics)

#add metrics to dataset
#simple error
calls$bs_error = calls$bs-calls$bidaskmean
calls$wdnn_error = calls$wdnn-calls$bidaskmean

#absolute error
calls$bs_ae = abs(calls$bs-calls$bidaskmean)
calls$wdnn_ae = abs(calls$wdnn-calls$bidaskmean)

#absolute percentage error
calls$bs_ape = ape(calls$bidaskmean, calls$bs)
calls$wdnn_ape = ape(calls$bidaskmean, calls$wdnn)

#squared error
calls$bs_se = se(calls$bidaskmean, calls$bs)
calls$wdnn_se = se(calls$bidaskmean, calls$wdnn)

#squared error
calls$bs_se = se(calls$bidaskmean, calls$bs)
calls$wdnn_se = se(calls$bidaskmean, calls$wdnn)

#overall again
calls$wdnn_mape=mape(calls$bidaskmean, calls$wdnn)
calls$bs_mape=mape(calls$bidaskmean, calls$bs)

calls$wdnn_smape=smape(calls$bidaskmean, calls$wdnn)
calls$bs_smape=smape(calls$bidaskmean, calls$bs)

calls$wdnn_mase=mase(calls$bidaskmean, calls$wdnn)
calls$bs_mase=mase(calls$bidaskmean, calls$bs)

calls$wdnn_rmse=rmse(calls$bidaskmean, calls$wdnn)
calls$bs_rmse=rmse(calls$bidaskmean, calls$bs)

calls$wdnn_bias=bias(calls$bidaskmean, calls$wdnn)
calls$bs_bias=bias(calls$bidaskmean, calls$bs)

#within
one_wdnn = sum(calls$wdnn_ape<0.01)/sum(nrow(calls))
one_bs = sum(calls$bs_ape<0.01)/sum(nrow(calls))

five_wdnn = sum(calls$wdnn_ape<0.05)/sum(nrow(calls))
five_bs = sum(calls$bs_ape<0.05)/sum(nrow(calls))

ten_wdnn = sum(calls$wdnn_ape<0.1)/sum(nrow(calls))
ten_bs = sum(calls$bs_ape<0.1)/sum(nrow(calls))

twenty_wdnn = sum(calls$wdnn_ape<0.2)/sum(nrow(calls))
twenty_bs = sum(calls$bs_ape<0.2)/sum(nrow(calls))

overtwenty_wdnn = sum(calls$wdnn_ape>=0.2)/sum(nrow(calls))
overtwenty_bs = sum(calls$bs_ape>=0.2)/sum(nrow(calls))

library(tidyverse)
calls_acc <- calls %>%
  select( bs_error, wdnn_error, bs_ae, wdnn_ae, bs_ape, wdnn_ape, bs_se, wdnn_se, bs_smape, wdnn_smape, bs_mase, wdnn_mase, bs_rmse, wdnn_rmse, bs_bias, wdnn_bias)
summary(calls_acc)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(calls$bidaskmean, calls$wdnn)
rsq(calls$bidaskmean, calls$bs)


# Graphics  -----------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(hrbrthemes)

# Deal with overplotting

#prediction over actual, tag maturity figure 3
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="WDNN Pricing")
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="BS Pricing")

#error over maturity, tag moneyness, figure 5
ggplot(calls, aes(x = maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Maturity in days", y="WDNN Pricing Error")+
  ylim(-360, 90)
ggplot(calls, aes(x = maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Maturity in days", y="BS Pricing Error")+
  ylim(-360, 90)

#error over bidaskmean, tag maturity, figure 7
ggplot(calls, aes(x = bidaskmean, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="WDNN Pricing Error")+
  ylim(-360, 90)
ggplot(calls, aes(x = bidaskmean, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="BS Pricing Error")+
  ylim(-360, 90)


#error over moneyness, tag maturity, figure 9
ggplot(calls, aes(x = moneyness, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Moneyness (S/K)", y="WDNN Pricing Error")+
  ylim(-360, 90)
ggplot(calls, aes(x = moneyness, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Moneyness (S/K)", y="BS Pricing Error")+
  ylim(-360, 90)

calls_acc <- calls %>%
  select(bs_error, wdnn_error, bs_ape, wdnn_ape, bs_se, wdnn_se)

stargazer(calls_acc)

#pairs(calls_sca) computationally strangely expensive


# Error distribution ------------------------------------------------------

#error distribution Moneyness
ggplot(calls, aes(x = wdnn_error)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Moneyness ~ .)+
  labs(x = "WDNN Pricing Error")
ggplot(calls, aes(x = bs_error)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Moneyness ~ .)+
  labs(x = "BS Pricing Error")

#error distribution Maturity
ggplot(calls, aes(x = wdnn_error)) +
  geom_histogram(fill = "white", colour = "black", binwidth = 5) +
  facet_grid(Maturity ~ .)+
  labs(x = "WDNN Pricing Error")
ggplot(calls, aes(x = bs_error)) +
  geom_histogram(fill = "white", colour = "black", binwidth = 5) +
  facet_grid(Maturity ~ .)+
  labs(x = "BS Pricing Error")

#box plots
#boxplot legend moneyness, x-axis maturity, error, figure 11
ggplot(calls, aes(x = Maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="WDNN Pricing Error")+
  ylim(-360, 90)
ggplot(calls, aes(x = Maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="BS Pricing Error")+
  ylim(-360, 90)+

#boxplot legend moneyness, x-axis maturity, ape, figure 12
ggplot(calls, aes(x = Maturity, y = wdnn_ape, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="WDNN Absolute Percentage Error (APE)")


  ylim(0, 2)
  
ggplot(calls, aes(x = Maturity, y = bs_ape, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="BS Absolute Percentage Error (APE)")

  ylim(0, 2)

ggplot(calls, aes(x = Moneyness, y = wdnn_ape, shape = Maturity, colour = Maturity)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="WDNN Pricing Error")+
  ylim(-360, 90)
ggplot(calls, aes(x = Moneyness, y = bs_ape, shape = Maturity, colour = Maturity)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="BS Pricing Error")+
  ylim(-360, 90)







#theme_ipsum_rc()



# 3D Plot -----------------------------------------------------------------

calls2=subset(calls, bidaskmean>1)

library(plotly)
fig <- plot_ly(calls, x = ~moneyness, y = ~maturity, z = ~wdnn_error,
               marker = list(color = ~bidaskmean, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Moneyness'),
                                   yaxis = list(title = 'Maturity'),
                                   zaxis = list(title = 'WDNN Pricing Error')),
                      title = 'Color scaling of the legend refers to the options observed market price',
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Price',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig


fig <- plot_ly(calls, x = ~moneyness, y = ~maturity, z = ~bs_error,
               marker = list(color = ~bidaskmean, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Moneyness'),
                                   yaxis = list(title = 'Maturity'),
                                   zaxis = list(title = 'BS Pricing Error')),
                      title = 'Color scaling of the legend refers to the options observed market price',
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Price',
                        xref = 'Moneyness',
                        yref = 'Maturity',
                        showarrow = FALSE
                      ))
fig


#fwrite(bs, file = "bs.csv", append = FALSE, quote = "auto")


# Maturities --------------------------------------------------------------
library(tidyverse)
calls_b <- calls %>%
  select( bs_error, wdnn_error, bs_ape, wdnn_ape, Maturity, Moneyness)
summary(calls_b)

#less than 14 days
calls_14=subset(calls_b, Maturity=="1) <14d" & Moneyness == "5) OTM")
summary(calls_14)

# Activation function  ----------------------------------------------------

titi = select(bs, date, S, K, sigma, maturity, riskfree, D, bidaskmean)
str(bs)
plot(mtcars)

library(sigmoid)
plot(relu(x))

library(tidyverse)
x=c(-5, 0, 5)
y=c(0, 0, 5)
z=c("relu: σ(x) = max(0, x)", "relu: σ(x) = max(0, x)", "relu: σ(x) = max(0, x)")
relu <- data.frame(x, y, z)

ggplot(relu, aes(x = x, y = y, colour = z)) +
  geom_line()+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank())+
  labs(colour = "Activation function")+
  theme(legend.title = element_text(
    size = 12))+
  xlab(NULL)+ylab(NULL)

ggplot(relu, aes(x = x, y = y, colour = z)) +
  geom_line()+
  labs(colour = "Activation function")+
  theme(legend.title = element_text(
    size = 13))+
  xlab(NULL)+ylab(NULL)

ggplot(relu, aes(x = x, y = y, colour = z)) +
  geom_line()+
  labs(colour = "Activation function")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15, face="bold"), 
        legend.text=element_text(size=14), legend.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(breaks = round(seq(min(relu$x), max(relu$x), by = 2),1))+
  xlab(NULL)+ylab(NULL)


ggplot(relu, aes(x = x, y = y, colour = z)) +
  geom_line()+
  labs(colour = "Activation function")+
  theme_grey(base_size = 22)

plot(calls$bidaskmean, calls$wdnn)

# At-the money  -----------------------------------------------------------------
atm=subset(calls, moneyness>0.99 & moneyness<1.01)

#Pricing Accuracy Measures
smape(atm$bidaskmean, atm$bs)
smape(atm$bidaskmean, atm$wdnn)

mase(atm$bidaskmean, atm$bs)
mase(atm$bidaskmean, atm$wdnn)

mse(atm$bidaskmean, atm$bs)
mse(atm$bidaskmean, atm$wdnn)

mdae(atm$bidaskmean, atm$bs)
mdae(atm$bidaskmean, atm$wdnn)

mae(atm$bidaskmean, atm$bs)
mae(atm$bidaskmean, atm$wdnn)

#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(atm$bidaskmean, atm$bs)
bias(atm$bidaskmean, atm$wdnn)


# Near-the-money
itm=subset(calls, moneyness>0.95 & moneyness<1.05)

#Pricing Accuracy Measures
smape(ntm$bidaskmean, ntm$bs)
smape(ntm$bidaskmean, ntm$wdnn)

mase(ntm$bidaskmean, ntm$bs)
mase(ntm$bidaskmean, ntm$wdnn)

mse(ntm$bidaskmean, ntm$bs)
mse(ntm$bidaskmean, ntm$wdnn)

mdae(ntm$bidaskmean, ntm$bs)
mdae(ntm$bidaskmean, ntm$wdnn)

mae(ntm$bidaskmean, ntm$bs)
mae(ntm$bidaskmean, ntm$wdnn)

#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(ntm$bidaskmean, ntm$bs)
bias(ntm$bidaskmean, ntm$wdnn)

# Out-of-the money  -----------------------------------------------------------------
otm=subset(calls, moneyness<0.95 & moneyness>0.85)
summary(calls)

#Pricing Accuracy Measures
smape(otm$bidaskmean, otm$bs)
smape(otm$bidaskmean, otm$wdnn)

mase(otm$bidaskmean, otm$bs)
mase(otm$bidaskmean, otm$wdnn)

mse(otm$bidaskmean, otm$bs)
mse(otm$bidaskmean, otm$wdnn)

mdae(otm$bidaskmean, otm$bs)
mdae(otm$bidaskmean, otm$wdnn)

mae(otm$bidaskmean, otm$bs)
mae(otm$bidaskmean, otm$wdnn)

#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(otm$bidaskmean, otm$bs)
bias(otm$bidaskmean, otm$wdnn)

# Deep out-of-the money  -----------------------------------------------------------------
dotm=subset(calls, moneyness<=0.85)

#Pricing Accuracy Measures
smape(dotm$bidaskmean, dotm$bs)
smape(dotm$bidaskmean, dotm$wdnn)

mase(dotm$bidaskmean, dotm$bs)
mase(dotm$bidaskmean, dotm$wdnn)

mse(dotm$bidaskmean, dotm$bs)
mse(dotm$bidaskmean, dotm$wdnn)

mdae(dotm$bidaskmean, dotm$bs)
mdae(dotm$bidaskmean, dotm$wdnn)

mae(dotm$bidaskmean, dotm$bs)
mae(dotm$bidaskmean, dotm$wdnn)

#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(dotm$bidaskmean, dotm$bs)
bias(dotm$bidaskmean, dotm$wdnn)

plot(dotm$bidaskmean, dotm$wdnn)
plot(dotm$bidaskmean, dotm$bs)


# In-the-money  -----------------------------------------------------------------
itm=subset(calls, moneyness>1.05 & moneyness<1.15)

#Pricing Accuracy Measures
mape(itm$bidaskmean, itm$bs)
mape(itm$bidaskmean, itm$wdnn)

smape(itm$bidaskmean, itm$bs)
smape(itm$bidaskmean, itm$wdnn)

mase(itm$bidaskmean, itm$bs)
mase(itm$bidaskmean, itm$wdnn)

mse(itm$bidaskmean, itm$bs)
mse(itm$bidaskmean, itm$wdnn)

mdae(itm$bidaskmean, itm$bs)
mdae(itm$bidaskmean, itm$wdnn)

mae(itm$bidaskmean, itm$bs)
mae(itm$bidaskmean, itm$wdnn)

#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(itm$bidaskmean, itm$bs)
bias(itm$bidaskmean, itm$wdnn)


# Deep in-the-money  -----------------------------------------------------------------



# Backups -----------------------------------------------------------------

#prediction over actual, tag moneyness
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="WDNN Pricing")
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="BS Pricing")

#error over bidaskmean, tag moneyness
ggplot(calls, aes(x = bidaskmean, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="WDNN Pricing Error")
ggplot(calls, aes(x = bidaskmean, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="BS Pricing Error")



#Activation function
ggplot(relu, aes(x = x, y = y, colour = z)) +
  geom_line()+
  labs(colour = "Activation function")+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=15, face="bold"), 
        legend.text=element_text(size=14), legend.title = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+ 
  scale_x_continuous(breaks = round(seq(min(relu$x), max(relu$x), by = 2),1))+
  xlab(NULL)+ylab(NULL)

#prediction over actual, tag maturity
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=21), 
        legend.text=element_text(size=25), legend.title = element_text(size = 30))+
  labs(x = "Option market price", y="WDNN Pricing")
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=21), 
        legend.text=element_text(20), legend.title = element_text(size = 23), legend.key.size = unit(5,"line"))+
  guides(color = guide_legend(override.aes = list(size = 3)))+
  labs(x = "Option market price", y="BS Pricing")

ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), 
        legend.text=element_text(size=14), legend.title = element_text(size = 16))+
  labs(x = "Option market price", y="BS Pricing")+
  theme_minimal()
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=16), 
        legend.text=element_text(size=14), legend.title = element_text(size = 16))+
  labs(x = "Option market price", y="BS Pricing")+
  theme_minimal()

#hbrtheme
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  labs(x = "Option market price", y="WDNN Pricing")+
  theme_ipsum_rc(base_size = 18, axis_title_size = 18, grid_col = "white")
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  labs(x = "Option market price", y="BS Pricing")+
  theme_ipsum_rc(base_size = 18, axis_title_size = 18, grid_col = "white")

#prediction over actual, tag moneyness
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)
ggplot(calls, aes(x = bidaskmean, y = bs, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)

ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  labs(x = "Option market price", y="WDNN Pricing")+
  theme_ipsum_rc(base_size = 18, axis_title_size = 18)
ggplot(calls, aes(x = bidaskmean, y = wdnn, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  labs(x = "Option market price", y="WDNN Pricing")+
  theme_ipsum_rc(base_size = 18, axis_title_size = 18, grid_col = "white")

ggplot(calls, aes(x = maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)
ggplot(calls, aes(x = maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)

ggplot(calls, aes(x = maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme_ipsum_rc(base_size = 20, axis_title_size = 20, grid_col = "white")
ggplot(calls, aes(x = maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.8)+
  theme_ipsum_rc(base_size = 20, axis_title_size = 20)

ggplot(calls, aes(x = bidaskmean, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)

ggplot(calls, aes(x = bidaskmean, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)

ggplot(calls, aes(x = bidaskmean, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)
ggplot(calls, aes(x = bidaskmean, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)

#error over moneyness, tag maturity
ggplot(calls, aes(x = moneyness, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)
ggplot(calls, aes(x = moneyness, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 0.8, position = "jitter", alpha=0.5)

#baloon plot
ggplot(calls, aes(x = bidaskmean, y = moneyness, size = wdnn_ape)) +
  geom_point(shape = 21, colour = "black", fill = "cornsilk")

ggplot(calls, aes(x = maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.2)+
  stat_bin2d(bins = 90) +
  scale_fill_gradient(low = "lightblue", high = "blue")

ggplot(calls, aes(x = bidaskmean, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.2)+
  stat_bin2d(bins = 90) +
  scale_fill_gradient(low = "lightblue", high = "blue")

ggplot(calls, aes(x = bidaskmean, y = wdnn_error)) +
  geom_point(size = 1.2)+
  stat_bin2d(bins = 90) +
  scale_fill_gradient(low = "lightblue", high = "blue")


