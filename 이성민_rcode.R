setwd("C:/Users/sungm/Desktop/머신러닝/ml 프로젝트/새 폴더 (3)")
dir()

install.packages("urca")
install.packages("readxl")
install.packages("openxlsx")
library(urca)
library(readxl)
library(csv)
library(openxlsx)

########종속변수########
data <- read_excel("데이터셋 .xlsx")

Y = matrix(data$exchange)
Y = Y[-1,]

mode(Y)
plot(Y, type='l')

Y_t = diff(log(Y))
plot(Y_t, type='l')

Y_df <- data.frame(Y=Y)
Yt_df <- data.frame(Y_t=Y_t)
wb <- createWorkbook()
addWorksheet(wb,"환율 수준 (Y)",)
writeData(wb,"환율 수준 (Y)",Y_df)

addWorksheet(wb,"환율 수익률 (Y_t)")
writeData(wb,"환율 수익률 (Y_t)", Yt_df)
saveWorkbook(wb,file="환율 데이터.xlsx",overwrite = TRUE)
getwd()

#ADF
Y_adf1=ur.df(Y, type="drift")
summary(Y_adf1)

Y_adf2=ur.df(Y, type="trend")
summary(Y_adf2)

Y_t_adf1=ur.df(Y_t,type="drift")
summary(Y_t_adf1)

Y_t_adf2=ur.df(Y_t, type="trend")
summary(Y_t_adf2)

#KPSS
Y_kpss1=ur.kpss(Y, type="mu", lags = "long")
summary(Y_kpss1)

Y.t=diff(log(Y))
plot(Y.t, type = 'l')

Y_t_kpss1=ur.kpss(Y_t, type="mu", lags = "long")
summary(Y_t_kpss1)


########환율 수익률 모형 예측########
data = read.csv("데이터셋 .csv" )
data = data[1:241,]
tcode = data[1,]
tcode

data = data[-1,]
tdata = data[-(1:2),]

dim(data)
dim(tdata)

ncol(data)

########데이터 전처리########

for (i in 2:ncol(data)){
  
  if(tcode[i] == 1){
    tdata[,i] <- data[-(1:2),i]
  } # no transformation  
  
  if(tcode[i] == 2){
    tdata[,i] <- diff(data[-1,i])
  } # 1차 차분
  
  if(tcode[i] == 4){
    tdata[,i] <- log(data[-(1:2),i])
  } # log
  
  if(tcode[i] == 5){
    tdata[,i] <- diff(log(data[-1,i]))
  } # log differencing
}



exchange = data[-1,14]
plot(log(exchange), type='l')
plot(diff(log(exchange)), type='l')

complete.cases(tdata)

Y = cbind(tdata[,14],tdata[,c(-1,-14)])
mode(Y)

Y= as.matrix(Y)
mode(Y)

setwd("C:/Users/sungm/Desktop/머신러닝/ml 프로젝트/새 폴더 (3)")
dir()
npred=120
install.packages("devtools")
install.packages("randomForest")

library(devtools)  
install_github("gabrielrvsc/HDeconometrics", force=TRUE)

library(HDeconometrics)

########random walk model########

source("functions/func-rw.R")
rw1=rw.rolling.window(Y,npred,1,1)
rw3=rw.rolling.window(Y,npred,1,3)
rw6=rw.rolling.window(Y,npred,1,6)
rw12=rw.rolling.window(Y,npred,1,12)


########ar model########
source("functions/func-ar.R")
ar1=ar.rolling.window(Y,npred,1,1,type="fixed")
ar3=ar.rolling.window(Y,npred,1,3,type="fixed")
ar6=ar.rolling.window(Y,npred,1,6,type="fixed")
ar12=ar.rolling.window(Y,npred,1,12,type="fixed")


########lasso model########
source("functions/func-lasso.R")
alpha=1
lasso1=lasso.rolling.window(Y,npred,1,1,alpha,type="lasso")
lasso3=lasso.rolling.window(Y,npred,1,3,alpha,type="lasso")
lasso6=lasso.rolling.window(Y,npred,1,6,alpha,type="lasso")
lasso12=lasso.rolling.window(Y,npred,1,12,alpha,type="lasso")


########random forest model########
source("functions/func-rf.R")
library(randomForest)

rf1=rf.rolling.window(Y,npred,1,1)
rf3=rf.rolling.window(Y,npred,1,3)
rf6=rf.rolling.window(Y,npred,1,6)
rf12=rf.rolling.window(Y,npred,1,12)


########xgboost model########
source('functions/func-xgb.R')
library(HDeconometrics)
install.packages("xgboost")
library(xgboost)

xgb1=xgb.rolling.window(Y,npred,1,1)
xgb3=xgb.rolling.window(Y,npred,1,3)
xgb6=xgb.rolling.window(Y,npred,1,6)
xgb12=xgb.rolling.window(Y,npred,1,12)


########중요한 설명변수########
install.packages("Boruta")
library(Boruta)
install.packages("randomForest")
library(randomForest)
comp=princomp(scale(Y,scale=FALSE))
Y2=cbind(Y,comp$scores[,1:4])

lag = 1  # lag = horizon
horizon = lag

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]

set.seed(42)
boruta_1 <- Boruta(X, y, maxRuns = 100)
plot_1 = plot(boruta_1)

attstats_1 = attStats(boruta_1)
attstats_1

lag = 3  # lag = horizon
horizon = lag

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]

set.seed(42)
boruta_3 <- Boruta(X, y, maxRuns = 100)
plot_3 = plot(boruta_3)

lag = 6  # lag = horizon
horizon = lag

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]

set.seed(42)
boruta_6 <- Boruta(X, y, maxRuns = 100)
plot_6 = plot(boruta_6)

lag = 12  # lag = horizon
horizon = lag

aux = embed(Y2,4+lag)
y=aux[,1]
X=aux[,-c(1:(ncol(Y2)*lag))]

set.seed(42)
boruta_12 <- Boruta(X, y, maxRuns = 100)
plot_12 = plot(boruta_12)

attstats_1 = attStats(boruta_1)
attstats_3 = attStats(boruta_3)
attstats_6 = attStats(boruta_6)
attstats_12 = attStats(boruta_12)

order_1 = order(attstats_1$meanImp, decreasing = T)
order_3 = order(attstats_3$meanImp, decreasing = T)
order_6 = order(attstats_6$meanImp, decreasing = T)
order_12 = order(attstats_12$meanImp, decreasing = T)
Errors_1 = rep(NA,70)
Errors_3 = rep(NA,70)
Errors_6 = rep(NA,70)
Errors_12 = rep(NA,70)


for (i in 2:70){
  
  selected_1 = order_1[1:i]
  
  model_1=randomForest(X[,selected_1], y, importance=TRUE)
  
  pred_1 = model_1$predicted     
  error_1 = mean((pred_1-y)^2)
  
  Errors_1[i] <- error_1
}
for (i in 2:70){
  
  selected_3 = order_3[1:i]
  
  model_3=randomForest(X[,selected_3], y, importance=TRUE)
  
  pred_3 = model_3$predicted     
  error_3 = mean((pred_3-y)^2)
  
  Errors_3[i] <- error_3
}
for (i in 2:70){
  
  selected_6 = order_6[1:i]
  
  model_6=randomForest(X[,selected_6], y, importance=TRUE)
  
  pred_6 = model_6$predicted     
  error_6 = mean((pred_6-y)^2)
  
  Errors_6[i] <- error_6
}
for (i in 2:70){
  
  selected_12 = order_12[1:i]
  
  model_12=randomForest(X[,selected_12], y, importance=TRUE)
  
  pred_12 = model_12$predicted     
  error_12 = mean((pred_12-y)^2)
  
  Errors_12[i] <- error_12
}

Errors1_1 = Errors_1
Errors1_3 = Errors_3
Errors1_6 = Errors_6
Errors1_12 = Errors_12


varOrder_1 = order(attstats_1$meanImp, decreasing = T)
varOrder_3 = order(attstats_3$meanImp, decreasing = T)
varOrder_6 = order(attstats_6$meanImp, decreasing = T)
varOrder_12 = order(attstats_12$meanImp, decreasing = T)
which.min(Errors1_1)
which.min(Errors1_3)
which.min(Errors1_6)
which.min(Errors1_12)

selected_1 = varOrder_1[1:which.min(Errors1_1)]
selected_3 = varOrder_3[1:which.min(Errors1_3)]
selected_6 = varOrder_6[1:which.min(Errors1_6)]
selected_12 = varOrder_12[1:which.min(Errors1_12)]

selected_1 = varOrder_1[1:16]
selected_3 = varOrder_3[1:16]
selected_6 = varOrder_6[1:16]
selected_12 = varOrder_12[1:16]

source("functions/func-rf_selected2022.R")

BS_RF1 = rf.rolling.window(Y2,npred,1,1,selected_1)
BS_RF3 = rf.rolling.window(Y2,npred,1,3,selected_3)
BS_RF6 = rf.rolling.window(Y2,npred,1,6,selected_6)
BS_RF12 = rf.rolling.window(Y2,npred,1,12,selected_12)


###BS_RF의 RMSE, MAE###

BS_RF_RMSE = cbind(BS_RF1$errors[1],BS_RF3$errors[1],BS_RF6$errors[1],BS_RF12$errors[1])
BS_RF_RMSE

BS_RF_MAE = cbind(BS_RF1$errors[2],BS_RF3$errors[2],BS_RF6$errors[2],BS_RF12$errors[2])
BS_RF_MAE


###RMSE, MAE###

rw_RMSE = cbind(rw1$errors[1],rw3$errors[1],rw6$errors[1],rw12$errors[1])
rw_RMSE

ar_RMSE = cbind(ar1$errors[1],ar3$errors[1],ar6$errors[1],ar12$errors[1])
ar_RMSE

lasso_RMSE = cbind(lasso1$errors[1],lasso3$errors[1],lasso6$errors[1],lasso12$errors[1])
lasso_RMSE

rf_RMSE = cbind(rf1$errors[1],rf3$errors[1],rf6$errors[1],rf12$errors[1])
rf_RMSE

xgb_RMSE = cbind(xgb1$errors[1],xgb3$errors[1],xgb6$errors[1],xgb12$errors[1])
xgb_RMSE

rw_MAE = cbind(rw1$errors[2],rw3$errors[2],rw6$errors[2],rw12$errors[2])
rw_MAE

ar_MAE = cbind(ar1$errors[2],ar3$errors[2],ar6$errors[2],ar12$errors[2])
ar_MAE

lasso_MAE = cbind(lasso1$errors[2],lasso3$errors[2],lasso6$errors[2],lasso12$errors[2])
lasso_MAE

xgb_RMSE = cbind(xgb1$errors[1],xgb3$errors[1],xgb6$errors[1],xgb12$errors[1])
xgb_RMSE


###GW test###

install.packages("sandwich")
install.packages("MCS")
library(sandwich)
library(MCS)

source("functions/gwtest.R")

real=tail(Y[,1],npred)  # actual value 

rw_pred = matrix(NA,npred,4)
rw_pred[,1] = rw1$pred
rw_pred[-(1:2),2] = rw3$pred
rw_pred[-(1:5),3] = rw6$pred
rw_pred[-(1:11),4] = rw12$pred


ar_pred = matrix(NA,npred,4)
ar_pred[,1] = ar1$pred
ar_pred[-(1:2),2] = ar3$pred
ar_pred[-(1:5),3] = ar6$pred
ar_pred[-(1:11),4] = ar12$pred


lasso_pred = matrix(NA,npred,4)
lasso_pred[,1] = lasso1$pred
lasso_pred[-(1:2),2] = lasso3$pred
lasso_pred[-(1:5),3] = lasso6$pred
lasso_pred[-(1:11),4] = lasso12$pred


rf_pred = matrix(NA,npred,4)
rf_pred[,1] = rf1$pred
rf_pred[-(1:2),2] = rf3$pred
rf_pred[-(1:5),3] = rf6$pred
rf_pred[-(1:11),4] = rf12$pred


xgb_pred=matrix(NA,npred,4)
xgb_pred[,1]=xgb1$pred
xgb_pred[-(1:2),2] = xgb3$pred
xgb_pred[-(1:5),3] = xgb6$pred
xgb_pred[-(1:11),4] = xgb12$pred

BS_RF_pred=matrix(NA,npred,4)
BS_RF_pred[,1]=BS_RF1$pred
BS_RF_pred[-(1:2),2] = BS_RF3$pred
BS_RF_pred[-(1:5),3] = BS_RF6$pred
BS_RF_pred[-(1:11),4] = BS_RF12$pred


wb <- loadWorkbook("환율 데이터.xlsx")

rw_df    <- as.data.frame(rw_pred)
ar_df    <- as.data.frame(ar_pred)
lasso_df <- as.data.frame(lasso_pred)
rf_df    <- as.data.frame(rf_pred)
xgb_df   <- as.data.frame(xgb_pred)
bsrf_df  <- as.data.frame(BS_RF_pred)

colnames(rw_df)    <- colnames(ar_df)    <- colnames(lasso_df) <-
  colnames(rf_df)    <- colnames(xgb_df)   <- colnames(bsrf_df)  <-
  c("1M", "3M", "6M", "12M")


addWorksheet(wb, "RW 예측")
writeData(wb, "RW 예측", rw_df)

addWorksheet(wb, "AR 예측")
writeData(wb, "AR 예측", ar_df)

addWorksheet(wb, "LASSO 예측")
writeData(wb, "LASSO 예측", lasso_df)

addWorksheet(wb, "RF 예측")
writeData(wb, "RF 예측", rf_df)

addWorksheet(wb, "XGB 예측")
writeData(wb, "XGB 예측", xgb_df)

addWorksheet(wb, "Boruta RF 예측")
writeData(wb, "Boruta RF 예측", bsrf_df)

saveWorkbook(wb, file = "환율 데이터.xlsx", overwrite = TRUE)




#1-step

#rw
gwpvalue_ar_rw = matrix(NA,1,1)
gwtest_ar_rw = matrix(NA,1,1)

for(i in 1:1){
  
  gw = gw.test(ar_pred[,i], rw_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  gwtest_ar_rw[i] <- gw$statistic
  gwpvalue_ar_rw[i] <- gw$p.value
}

#lasso
gwpvalue_ar_lasso = matrix(NA,1,1)
gwtest_ar_lasso = matrix(NA,1,1)

for(i in 1:1){
  
  gw = gw.test(ar_pred[,i], lasso_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  gwtest_ar_lasso[i] <- gw$statistic
  gwpvalue_ar_lasso[i] <- gw$p.value
}

#randomforest
gwpvalue_ar_rf = matrix(NA,1,1)
gwtest_ar_rf = matrix(NA,1,1)

for(i in 1:1){
  
  gw = gw.test(ar_pred[,i], rf_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  gwtest_ar_rf[i] <- gw$statistic
  gwpvalue_ar_rf[i] <- gw$p.value
}

#xgboost
gwpvalue_ar_xgb = matrix(NA,1,1)
gwtest_ar_xgb = matrix(NA,1,1)

for(i in 1:1){
  
  gw = gw.test(ar_pred[,i], xgb_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  gwtest_ar_xgb[i] <- gw$statistic
  gwpvalue_ar_xgb[i] <- gw$p.value
}

#BS_RF
gwpvalue_ar_BS_RF = matrix(NA,1,1)
gwtest_ar_BS_RF = matrix(NA,1,1)

for(i in 1:1){
  
  gw = gw.test(ar_pred[,i], BS_RF_pred[,i], real, tau=i, T=npred, method="NeweyWest")
  gwtest_ar_BS_RF[i] <- gw$statistic
  gwpvalue_ar_BS_RF[i] <- gw$p.value
}

#3-step

#RW
gwpvalue_ar_rw_3 = matrix(NA,1)
gwtest_ar_rw_3 = matrix(NA,1)

for(i in 3:3){
  
  gw_3 = gw.test(ar_pred[-(1:(i-1)), i-1], rw_pred[-(1:(i-1)), i-1], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rw_3[i-1] <- gw_3$statistic
  gwpvalue_ar_rw_3[i-1] <- gw_3$p.value
}

#LASSO
gwpvalue_ar_lasso_3 = matrix(NA,1)
gwtest_ar_lasso_3 = matrix(NA,1)

for(i in 3:3){
  
  gw_3 = gw.test(ar_pred[-(1:(i-1)), i-1], lasso_pred[-(1:(i-1)), i-1], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_lasso_3[i-1] <- gw_3$statistic
  gwpvalue_ar_lasso_3[i-1] <- gw_3$p.value
}

#randomforest
gwpvalue_ar_rf_3 = matrix(NA,1)
gwtest_ar_rf_3 = matrix(NA,1)

for(i in 3:3){
  
  gw_3 = gw.test(ar_pred[-(1:(i-1)), i-1], rf_pred[-(1:(i-1)), i-1], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rf_3[i-1] <- gw_3$statistic
  gwpvalue_ar_rf_3[i-1] <- gw_3$p.value
}

#xgboost
gwpvalue_ar_xgb_3 = matrix(NA,1)
gwtest_ar_xgb_3 = matrix(NA,1)

for(i in 3:3){
  
  gw_3 = gw.test(ar_pred[-(1:(i-1)), i-1], xgb_pred[-(1:(i-1)), i-1], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_xgb_3[i-1] <- gw_3$statistic
  gwpvalue_ar_xgb_3[i-1] <- gw_3$p.value
}

#BS_RF
gwpvalue_ar_BS_RF_3 = matrix(NA,1)
gwtest_ar_BS_RF_3 = matrix(NA,1)

for(i in 3:3){
  
  gw_3 = gw.test(ar_pred[-(1:(i-1)), i-1], BS_RF_pred[-(1:(i-1)), i-1], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_BS_RF_3[i-1] <- gw_3$statistic
  gwpvalue_ar_BS_RF_3[i-1] <- gw_3$p.value
}

#6-step

#RW
gwpvalue_ar_rw_6 = matrix(NA,1)
gwtest_ar_rw_6 = matrix(NA,1)
for(i in 6:6){
  
  gw_6 = gw.test(ar_pred[-(1:(i-1)), i-3], rw_pred[-(1:(i-1)), i-3], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rw_6[i-1] <- gw_6$statistic
  gwpvalue_ar_rw_6[i-1] <- gw_6$p.value
}

#lasso
gwpvalue_ar_lasso_6 = matrix(NA,1)
gwtest_ar_lasso_6 = matrix(NA,1)

for(i in 6:6){
 
  gw_6 = gw.test(ar_pred[-(1:(i-1)), i-3], lasso_pred[-(1:(i-1)), i-3], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_lasso_6[i-1] <- gw_6$statistic
  gwpvalue_ar_lasso_6[i-1] <- gw_6$p.value
}

#randomforest
gwpvalue_ar_rf_6 = matrix(NA,1)
gwtest_ar_rf_6 = matrix(NA,1)

for(i in 6:6){
  
  gw_6 = gw.test(ar_pred[-(1:(i-1)), i-3], rf_pred[-(1:(i-1)), i-3], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rf_6[i-1] <- gw_6$statistic
  gwpvalue_ar_rf_6[i-1] <- gw_6$p.value
}

#xgboost
gwpvalue_ar_xgb_6 = matrix(NA,1)
gwtest_ar_xgb_6 = matrix(NA,1)

for(i in 6:6){
  
  gw_6 = gw.test(ar_pred[-(1:(i-1)), i-3], xgb_pred[-(1:(i-1)), i-3], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_xgb_6[i-1] <- gw_6$statistic
  gwpvalue_ar_xgb_6[i-1] <- gw_6$p.value
}

#BS_RF
gwpvalue_ar_BS_RF_6 = matrix(NA,1)
gwtest_ar_BS_RF_6 = matrix(NA,1)

for(i in 6:6){
  
  gw_6 = gw.test(ar_pred[-(1:(i-1)), i-3], BS_RF_pred[-(1:(i-1)), i-3], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_BS_RF_6[i-1] <- gw_6$statistic
  gwpvalue_ar_BS_RF_6[i-1] <- gw_6$p.value
}

#12-step

#RW
gwpvalue_ar_rw_12 = matrix(NA,1)
gwtest_ar_rw_12 = matrix(NA,1)

for(i in 12:12){
  
  gw_12 = gw.test(ar_pred[-(1:(i-1)), i-8], rw_pred[-(1:(i-1)), i-8], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rw_12[i-1] <- gw_12$statistic
  gwpvalue_ar_rw_12[i-1] <- gw_12$p.value
}

#lasso
gwpvalue_ar_lasso_12 = matrix(NA,1)
gwtest_ar_lasso_12 = matrix(NA,1)

for(i in 12:12){
  
  gw_12 = gw.test(ar_pred[-(1:(i-1)), i-8], lasso_pred[-(1:(i-1)), i-8], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_lasso_12[i-1] <- gw_12$statistic
  gwpvalue_ar_lasso_12[i-1] <- gw_12$p.value
}

#RandomForest
gwpvalue_ar_rf_12 = matrix(NA,1)
gwtest_ar_rf_12 = matrix(NA,1)

for(i in 12:12){
  
  gw_12 = gw.test(ar_pred[-(1:(i-1)), i-8], rf_pred[-(1:(i-1)), i-8], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_rf_12[i-1] <- gw_12$statistic
  gwpvalue_ar_rf_12[i-1] <- gw_12$p.value
}

#xgboost
gwpvalue_ar_xgb_12 = matrix(NA,1)
gwtest_ar_xgb_12 = matrix(NA,1)

for(i in 12:12){
  
  gw_12 = gw.test(ar_pred[-(1:(i-1)), i-8], xgb_pred[-(1:(i-1)), i-8], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_xgb_12[i-1] <- gw_12$statistic
  gwpvalue_ar_xgb_12[i-1] <- gw_12$p.value
}

#BS_RF
gwpvalue_ar_BS_RF_12 = matrix(NA,1)
gwtest_ar_BS_RF_12 = matrix(NA,1)

for(i in 12:12){
  
  gw_12 = gw.test(ar_pred[-(1:(i-1)), i-8], BS_RF_pred[-(1:(i-1)), i-8], real[-(1:(i-1))], tau = i, T = npred-i+1, method = "NeweyWest")
  gwtest_ar_BS_RF_12[i-1] <- gw_12$statistic
  gwpvalue_ar_BS_RF_12[i-1] <- gw_12$p.value
}

###MCS test###

#1-step
for(i in 1:1){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,i], ar_pred[,i],lasso_pred[,i],rf_pred[,i], xgb_pred[,i],BS_RF_pred[,i])
  
  LOSS=Pred-real
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}

#3-step
for(i in 3:3){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,(i-1)], ar_pred[,(i-1)],lasso_pred[,(i-1)], rf_pred[,(i-1)], xgb_pred[,(i-1)], BS_RF_pred[,(i-1)]) 
  LOSS=Pred[-(1:(i-1)),]-real[-(1:(i-1))]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}

#6-step
for(i in 6:6){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,(i-3)], ar_pred[,(i-3)],lasso_pred[,(i-3)],rf_pred[,(i-3)], xgb_pred[,(i-3)], BS_RF_pred[,(i-3)]) 
  LOSS=Pred[-(1:(i-1)),]-real[-(1:(i-1))]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}

#12-step
for(i in 12:12){
  cat("iteration",i,"\n")
  
  Pred=cbind(rw_pred[,(i-8)], ar_pred[,(i-8)],lasso_pred[,(i-8)], rf_pred[,(i-8)], xgb_pred[,(i-8)], BS_RF_pred[,(i-8)]) 
  LOSS=Pred[-(1:(i-1)),]-real[-(1:(i-1))]
  LOSS1=LOSS^2      # squared error
  LOSS2=abs(LOSS)   # absolute error
  
  SSM_i <- MCSprocedure(LOSS1, alpha=0.5, B=5000, statistic="TR")
}

