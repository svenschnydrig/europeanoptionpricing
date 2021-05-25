library(data.table)
puts = fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/output/test_put.csv")
puts2 = fread("/Users/svenschnydrig/OneDrive - Universität St.Gallen/Bachelor/Bachelor Thesis/Coding/Data/output/test_put.csv")

options( scipen=999)

str(puts)

#create moneyness variable to filter according to moneyness
puts$moneyness = puts$K / puts$S

t=subset(puts, maturity>30)
t=subset(puts, maturity<30)
nrow(t)/nrow(puts)

puts$Moneyness = ifelse(puts$moneyness>1.15, "4) DITM", ifelse(puts$moneyness>1.05, "3) ITM", ifelse(puts$moneyness>1.01, "2) NTM", ifelse(puts$moneyness>0.99, "1) ATM", 
                  ifelse(puts$moneyness>0.95, "2) NTM", ifelse(puts$moneyness>0.85, "5) OTM", "6) DOTM"))))))
puts$Moneyness=as.factor(puts$Moneyness)


puts$Maturity = ifelse(puts$maturity<14, "1) <14d", ifelse(puts$maturity<30, "2) 15-30d", ifelse(puts$maturity<92, "3) 1m-3m", ifelse(puts$maturity<183, "4) 3m-6m", 
                 ifelse(puts$maturity<366, "5) 6m-12m",  ifelse(puts$maturity>=366, "6) 1-3y", NA))))))
puts$Maturity=as.factor(puts$Maturity)

# Overall -----------------------------------------------------------------
str(puts)

puts=subset(puts, bidaskmean>1)

t=subset(puts, best_bid>1)
ti=subset(puts, bidaskmean>1)
library(Metrics)
#overall accuracy
mape(puts$bidaskmean, puts$wdnn)
mape(puts$bidaskmean, puts$bs)
smape(puts$bidaskmean, puts$wdnn)
smape(puts$bidaskmean, puts$bs)
mase(puts$bidaskmean, puts$wdnn)
mase(puts$bidaskmean, puts$bs)
mse(puts$bidaskmean, puts$wdnn)
mse(puts$bidaskmean, puts$bs)
rmse(puts$bidaskmean, puts$wdnn)
rmse(puts$bidaskmean, puts$bs)
mdae(puts$bidaskmean, puts$wdnn)
mdae(puts$bidaskmean, puts$bs)
mae(puts$bidaskmean, puts$wdnn)
mae(puts$bidaskmean, puts$bs)
#If a model is unbiased bias(actual, predicted) should be close to zero. Bias is calculated by taking the average of (actual - predicted)
bias(puts$bidaskmean, puts$wdnn)
bias(puts$bidaskmean, puts$bs)


library(Metrics)

#add metrics to dataset
#simple error
puts$bs_error = puts$bs-puts$bidaskmean
puts$wdnn_error = puts$wdnn-puts$bidaskmean

#absolute error
puts$bs_ae = abs(puts$bs-puts$bidaskmean)
puts$wdnn_ae = abs(puts$wdnn-puts$bidaskmean)

#absolute percentage error
puts$bs_ape = ape(puts$bidaskmean, puts$bs)
puts$wdnn_ape = ape(puts$bidaskmean, puts$wdnn)

#squared error
puts$bs_se = se(puts$bidaskmean, puts$bs)
puts$wdnn_se = se(puts$bidaskmean, puts$wdnn)

#overall again
puts$wdnn_mape=mape(puts$bidaskmean, puts$wdnn)
puts$bs_mape=mape(puts$bidaskmean, puts$bs)

puts$wdnn_smape=smape(puts$bidaskmean, puts$wdnn)
puts$bs_smape=smape(puts$bidaskmean, puts$bs)

puts$wdnn_mase=mase(puts$bidaskmean, puts$wdnn)
puts$bs_mase=mase(puts$bidaskmean, puts$bs)

puts$wdnn_rmse=rmse(puts$bidaskmean, puts$wdnn)
puts$bs_rmse=rmse(puts$bidaskmean, puts$bs)

puts$wdnn_bias=bias(puts$bidaskmean, puts$wdnn)
puts$bs_bias=bias(puts$bidaskmean, puts$bs)

#within
one_wdnn = sum(puts$wdnn_ape<0.01)/sum(nrow(puts))
one_bs = sum(puts$bs_ape<0.01)/sum(nrow(puts))

five_wdnn = sum(puts$wdnn_ape<0.05)/sum(nrow(puts))
five_bs = sum(puts$bs_ape<0.05)/sum(nrow(puts))

ten_wdnn = sum(puts$wdnn_ape<0.1)/sum(nrow(puts))
ten_bs = sum(puts$bs_ape<0.1)/sum(nrow(puts))

twenty_wdnn = sum(puts$wdnn_ape<0.2)/sum(nrow(puts))
twenty_bs = sum(puts$bs_ape<0.2)/sum(nrow(puts))

overtwenty_wdnn = sum(puts$wdnn_ape>=0.2)/sum(nrow(puts))
overtwenty_bs = sum(puts$bs_ape>=0.2)/sum(nrow(puts))

library(tidyverse)
puts_acc <- puts %>%
  select( bs_error, wdnn_error, bs_ae, wdnn_ae, bs_ape, wdnn_ape, bs_se, wdnn_se, bs_smape, wdnn_smape, bs_mase, wdnn_mase, bs_rmse, wdnn_rmse, bs_bias, wdnn_bias)
summary(puts_acc)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(puts$bidaskmean, puts$wdnn)
rsq(puts$bidaskmean, puts$bs)


# Graphics  -----------------------------------------------------------------
library(ggplot2)
library(ggthemes)
library(hrbrthemes)

# Deal with overplotting

#prediction over actual, tag maturity, figure 4
ggplot(puts, aes(x = bidaskmean, y = wdnn, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="WDNN Pricing")
ggplot(puts, aes(x = bidaskmean, y = bs, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option market price", y="BS Pricing")

#error over maturity, tag moneyness, figure 6
ggplot(puts, aes(x = maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Maturity in days", y="WDNN Pricing Error")+
  ylim(-325, 125)
ggplot(puts, aes(x = maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Maturity in days", y="BS Pricing Error")+
  ylim(-325, 125)

#error over bidaskmean, tag maturity, figure 8
ggplot(puts, aes(x = bidaskmean, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="WDNN Pricing Error")+
  ylim(-325, 125)
ggplot(puts, aes(x = bidaskmean, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Option premium", y="BS Pricing Error")+
  ylim(-325, 125)


 #error over moneyness, tag maturity, figure 10
ggplot(puts, aes(x = moneyness, y = wdnn_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Moneyness (K/S)", y="WDNN Pricing Error")+
  ylim(-325, 125)
ggplot(puts, aes(x = moneyness, y = bs_error, shape = Maturity, colour = Maturity)) +
  geom_point(size = 1.5, position = "jitter", alpha=0.8)+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 7)))+
  labs(x = "Moneyness (K/S)", y="BS Pricing Error")+
  ylim(-325, 125)

#box plots
#boxplot legend moneyness, x-axis maturity, error, figure 13
ggplot(puts, aes(x = Maturity, y = wdnn_error, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="WDNN Pricing Error")+
  ylim(-325, 125)
ggplot(puts, aes(x = Maturity, y = bs_error, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="BS Pricing Error")+
  ylim(-325, 125)

#boxplot legend moneyness, x-axis maturity, ape, figure 14
wdnn_ape =
  ggplot(puts, aes(x = Maturity, y = wdnn_ape, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="WDNN Absolute Percentage Error (APE)")

  ylim(0, 1.2)

ggplot(puts, aes(x = Maturity, y = bs_ape, shape = Moneyness, colour = Moneyness)) +
  geom_boxplot()+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22), 
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 1)))+
  labs(x = "Maturity", y="BS Absolute Percentage Error (APE)")

  ylim(0, 1.2)


  


# 3D Plot -----------------------------------------------------------------

puts2=subset(puts, bidaskmean>1)

library(plotly)
fig <- plot_ly(puts, x = ~moneyness, y = ~maturity, z = ~wdnn_error,
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


fig <- plot_ly(puts, x = ~moneyness, y = ~maturity, z = ~bs_error,
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

p <- ggplot(mtcars, aes(x=factor(gear), y=mpg)) + geom_boxplot() 

titi=ggplot_build(wdnn_ape)

titi=layer_data(wdnn_ape, 1)

puts_acc <- puts %>%
  select(bs_error, wdnn_error, bs_ape, wdnn_ape, bs_se, wdnn_se)
stargazer(puts_acc)



# Error distribution ------------------------------------------------------

#error distribution Moneyness
ggplot(puts, aes(x = wdnn_error)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Moneyness ~ .)+
  labs(x = "WDNN Pricing Error")
ggplot(puts, aes(x = bs_error)) +
  geom_histogram(fill = "white", colour = "black") +
  facet_grid(Moneyness ~ .)+
  labs(x = "BS Pricing Error")

#error distribution Maturity
ggplot(puts, aes(x = wdnn_error)) +
  geom_histogram(fill = "white", colour = "black", binwidth = 5) +
  facet_grid(Maturity ~ .)+
  labs(x = "WDNN Pricing Error")
ggplot(puts, aes(x = bs_error)) +
  geom_histogram(fill = "white", colour = "black", binwidth = 5) +
  facet_grid(Maturity ~ .)+
  labs(x = "BS Pricing Error")
