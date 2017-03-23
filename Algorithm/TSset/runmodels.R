# TODO: Add comment
# 
# Author: lifeng
###############################################################################

library(TTR)
library(forecast)
library(markovchain)

# 加载数据
# 数据要求：（****2列数据****）
# 1、时间轴标识
# 2、案件量数值
# 3、如果需要预测不同案件类型的案件量，则需要单独加载该种类型的案件量数据，即该脚本一次只能预测一种类型的案件量

tsdata <- read.csv("加载数据")
# tsdata为数据框类型
tsvalues <- tsdata[,2] 



tsvalues <- c(572,568,804,1217,839,643,548,556,579,597,630,597,644,631,634,589,764,655,670,663,618,702,922,614,659,628,570,645,566,591,597,596,613,728,720,664,636,702,912,675,737,659,628,693,724,713,634,670,616,649,668,762,755,780,742,755,848,1256,1454,1056,902,849,776,811,808,758,759,747,727,783,682,769,815,826,933,866,793,796,795,753,776,738,710,735,753,771,791,811,855,825,890,735,737,794,772,732,778,724,672,863,806,798,683,808,804,790,727,957,1643,1235,1086,867,858,731,827,783,847,848,762,770,872,863,772,794,677,747,835,876,732,766,697,678,682,579)
tsvalues <- tsvalues[-134]



tsvalues <- ts(tsvalues)

# 提取出水平变化
tslevel <- function(n) { # n：观测样本数
	level <- StructTS(tail(tsvalues, n), type='level')
	pred_pred <- as.numeric(ceiling(predict(level, 1)$pred)) # 给出预测一期的水平预测值
	pred_se <- as.numeric(floor(predict(level, 1)$se)) # 给出预测一期的水平预测误差
	return(c(pred=pred_pred, se=pred_se))
}
tslevel_pred_se <- NULL
for(i in 2:floor(length(tsvalues)/30)) {
	tslevel_pred_se <- rbind(tslevel_pred_se, tslevel(30*i))
}
tslevel_pred_se <- colMeans(tslevel_pred_se)
tslevel_pred_se[1] <- ceiling(tslevel_pred_se[1])
tslevel_pred_se[2] <- floor(tslevel_pred_se[2])

# 提取趋势变化（波动）的状态：连续状态变化周期
truning_point <- 'null'
for(i in 2:(length(tsvalues)-1)) {
	if(tsvalues[i-1] < tsvalues[i] & tsvalues[i] > tsvalues[i+1]) truning_point <- c(truning_point, 'max_point')
	else if(tsvalues[i-1] > tsvalues[i] & tsvalues[i] < tsvalues[i+1]) truning_point <- c(truning_point, 'min_point')
	else truning_point <- c(truning_point, 'null')
} 

truning_point_freq <- truning_point[which(truning_point != tail(truning_point, 1))]
truning_point_freq <- table(truning_point_freq)
freq_sign <- names(truning_point_freq[truning_point_freq == max(truning_point_freq)])
if(length(freq_sign) > 1) {
	freq_sign <- 'null' 
}

truning_after <- truning_point[which(truning_point == tail(truning_point, 1))+1]
truning_after <- as.vector(na.omit(truning_after))
truning_after <- table(truning_after)
if(length(truning_after) > 1) {
	pred_sign <- names(truning_after[truning_after == max(truning_after)])
	if(length(pred_sign) > 1) {
		pred_sign <- freq_sign
	}
} else { pred_sign <- names(truning_after) }

if(pred_sign == 'max_point') {
	pred_value_define <- tslevel_pred_se[1] - tslevel_pred_se[2]*0.95 
}
if(pred_sign == 'min_point') {
	pred_value_define <- tslevel_pred_se[1] + tslevel_pred_se[2]*0.95
}
if(pred_sign == 'null') { # 趋势顺延**
	if(tail(truning_point[which(truning_point != 'null')], 1) == 'max_point') {
		pred_value_define <- tslevel_pred_se[1] - tslevel_pred_se[2]*0.95
	} else {
		pred_value_define <- tslevel_pred_se[1] + tslevel_pred_se[2]*0.95
	}
}	
pred_value_define <- round(pred_value_define)
rm(list=ls()[-which(ls() %in% c("tsdata", "pred_value_define", "tsvalues"))])
gc(reset=TRUE)
# pred_value_define # 自定义规则法

sma2 <- SMA(tsvalues, n=2); sma2 <- round(sma2); sma2 <- tail(sma2, 1)
sma4 <- SMA(tsvalues, n=4); sma4 <- round(sma4); sma4 <- tail(sma4, 1)
sma <- round(mean(sma2, sma4))
rm(sma2, sma4); gc(reset=TRUE)
# sma # 简单移动平均法

ema2f <- round(tail(EMA(tsvalues, n=2, wilder=FALSE), 1))
ema2t <- round(tail(EMA(tsvalues, n=2, wilder=TRUE), 1))
ema <- round(mean(ema2f, ema2t))
rm(ema2f, ema2t); gc(reset=TRUE)
# ema # 一次指数平滑法

dema2f <- round(tail(DEMA(tsvalues, n=2, wilder=FALSE), 1))
dema2t <- round(tail(DEMA(tsvalues, n=2, wilder=TRUE), 1))
dema <- round(mean(dema2f, dema2t))
rm(dema2f, dema2t); gc(reset=TRUE)
# dema # 二次指数平滑法

wma2 <- round(tail(WMA(tsvalues, n=2), 1))
wma4 <- round(tail(WMA(tsvalues, n=4), 1))
wma <- round(mean(wma2, wma4))
rm(wma2, wma4); gc(reset=TRUE)
# wma # 加权移动平均法

zlema2 <- round(tail(ZLEMA(tsvalues, n=2), 1))
zlema4 <- round(tail(ZLEMA(tsvalues, n=4), 1))
zlema <- round(mean(zlema2, zlema4))
rm(zlema2, zlema4); gc(reset=TRUE)
# zlema # 零滞后指数平滑法


# 无效语句：如下
tsdata <- cbind(1:length(tsvalues), tsvalues)

# 数据变换 
tstrans <- NULL # 计算后的length比 length(tsdata[,2])少1
for(i in 2:length(tsdata[,2])) {
	tstrans <- c(tstrans, tsdata[i,2]-tsdata[i-1,2])
}

tstrans01 <- ifelse(tstrans>0, 1, 0)
tsmcfit <- markovchainFit(data=as.character(tstrans01))
state_shift <- predict(tsmcfit$estimate, newdata=tail(tstrans01,1),n.ahead=1)
tsarima <- auto.arima(tsvalues)
# tsarima <- predict(tsarima, 1)
tsarima <- as.data.frame(forecast(tsarima, h=1))
if(state_shift == '0') { 
	pred_value_arima <- as.numeric(tsarima[2])
} else {
	pred_value_arima <- as.numeric(tsarima[3])
}
pred_value_arima <- round(pred_value_arima)
rm(tstrans01, tsmcfit, state_shift, tsarima); gc(reset=TRUE)
# pred_value_arima # ARIMA模型预测法

tstransratio <- tstrans / tsvalues[-length(tsvalues)]
dema2f_ratio <- tail(DEMA(tstransratio, n=2, wilder=FALSE), 1)
dema2t_ratio <- tail(DEMA(tstransratio, n=2, wilder=TRUE), 1)
dema_ratio <- mean(dema2f_ratio, dema2t_ratio)
dema_ratio <- round(tail(tsvalues,1) * (1 -dema_ratio))
rm(tstrans, tstransratio, dema2f_ratio, dema2t_ratio); gc(reset=TRUE)
# dema_ratio # 间接法——估计环比增长率进行预测


comb_pred <- floor(mean(pred_value_define, pred_value_arima) * 0.8) + floor(mean(sma, ema, dema, wma, zlema, dema_ratio) * 0.2) 
comb_pred


