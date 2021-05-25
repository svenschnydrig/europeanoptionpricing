library(keras)
library(tensorflow)


# Deploy wdnn model for calls ---------------------------------------------

WDNN_call <- load_model_tf("/Users/svenschnydrig/models/WDNN_call.h5")

summary(WDNN_call)

#for single prediction use this
K=3530
S=3647.49
sigma=0.112946
T=0.263013699
riskfree=0.0008
D=0.01495974

#for pricing function use this
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.15, 6001)
T=rep.int(3, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

# 15.1
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.08, 6001)
T=rep.int(3, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

#16.1
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.23, 6001)
T=rep.int(3, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

#17.1
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.15, 6001)
T=rep.int(0.8, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

df <- data.frame(K, S, sigma, T, riskfree, D)
dfn=df
#df$IV=ifelse(df$S>df$K, df$S-df$K, 0)


#like train starting 2016
dfn$K=(df$K-2405.18)/650.5725
dfn$S=(df$S-2714.412)/355.8858
dfn$sigma=(df$sigma-0.1602985)/0.1564802
dfn$T=(df$T-0.2843604)/0.4418
dfn$riskfree=(df$riskfree-0.01334102)/0.008336618
dfn$D=(df$D-0.01699076)/0.004606778

dfm=as.matrix(dfn)

#make prediction
df$WDNN = predict(
  WDNN_call,
  dfm,
  batch_size = 1,
  verbose = 0,
  steps = NULL,
  callbacks = NULL,
)

options( scipen=999)

#compute bs price
df$d1 = (log(df$S/df$K)+(df$riskfree-df$D+0.5*df$sigma^2) * df$T)/(df$sigma * sqrt(df$T))
df$d2 = df$d1-df$sigma * sqrt(df$T)
df$BS = df$S * exp(-df$D* df$T) * pnorm(df$d1)-df$K * exp(-df$riskfree * df$T) * pnorm(df$d2)

dfn$WDNN=df$WDNN
dfn$BS=df$BS

ggplot(df, aes(S)) + 
  geom_line(aes(y = WDNN, colour = "1) WDNN")) + 
  geom_line(aes(y = BS, colour = "2) BS"))+
  xlab('Underlying security price') +
  ylab('Option premium')+
  labs(colour="Model")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22),
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 2)))


ggplot(df, aes(S)) + 
  geom_line(aes(y = WDNN, colour = "1) WDNN")) + 
  geom_line(aes(y = BS, colour = "2) BS"))+
  geom_line(aes(y = IV, colour = "3) Intrinsic Value"))+
  xlab('Underlying security price') +
  ylab('Option premium')+
  labs(colour="Legend")

# Deploy wdnn model for puts ---------------------------------------------

wdnn_put <- load_model_tf("/Users/svenschnydrig/models/wdnn_put.h5")
summary(wdnn_put)

#for single predictions use this
K=6500
S=3727.04
sigma=0.088676
T=1.4684932
riskfree=0.0012
D=0.015411717

#for pricing function use this
K=rep.int(4000, 2801)
S=seq(from=2200, to=5000, by = 1)
sigma=rep.int(0.15, 2801)
T=rep.int(0.25, 2801)
riskfree=rep.int(0.002, 2801)
D=rep.int(0.015, 2801)

# 15.2
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.08, 6001)
T=rep.int(3, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

#16.2
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.23, 6001)
T=rep.int(3, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

#17.2
K=rep.int(4000, 6001)
S=seq(from=2000, to=8000, by = 1)
sigma=rep.int(0.15, 6001)
T=rep.int(0.8, 6001)
riskfree=rep.int(0.003, 6001)
D=rep.int(0.015, 6001)

df <- data.frame(K, S, sigma, T, riskfree, D)
dfn=df

#scale data back
dfn$K=(df$K-2405.18)/650.5725
dfn$S=(df$S-2714.412)/355.8858
dfn$sigma=(df$sigma-0.1602985)/0.1564802
dfn$T=(df$T-0.2843604)/0.4418
dfn$riskfree=(df$riskfree-0.01334102)/0.008336618
dfn$D=(df$D-0.01699076)/0.004606778

dfm=as.matrix(dfn)

df$wdnn = predict(
  wdnn_put,
  dfm,
  batch_size = 1,
  verbose = 0,
  steps = NULL,
  callbacks = NULL,
)

options( scipen=999)

#compute bs price
df$d1 = (log(df$S/df$K)+(df$riskfree-df$D+0.5*df$sigma^2) * df$T)/(df$sigma * sqrt(df$T))
df$d2 = df$d1-df$sigma * sqrt(df$T)

df$bs = df$K * exp(-df$riskfree * df$T) * pnorm(-df$d2) - df$S * exp(-df$D* df$T) * pnorm(-df$d1)  

#visualize pricing function
ggplot(df, aes(S)) + 
  geom_line(aes(y = wdnn, colour = "1) WDNN")) + 
  geom_line(aes(y = bs, colour = "2) BS"))+
  xlab('Underlying security price') +
  ylab('Option premium')+
  labs(colour="Model")+
  theme(axis.text=element_text(size=18), axis.title=element_text(size=22),
        legend.text=element_text(size=21), legend.title = element_text(size = 24), legend.key.size = unit(2,"line"))+
  guides(color = guide_legend(override.aes = list(size = 2)))




