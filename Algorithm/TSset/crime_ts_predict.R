# Model name：crime_ts_predict
# Date：2016/9/6
# Vsersion：ARIMA模型
# Details：警情趋势预测-ARIMA模型（以后再添加其他ts预测模型）
# Library：forecast:::auto.arima


argv <- commandArgs(TRUE)
workdir <- argv[1]
setwd(workdir)
library(rjson)
config <- fromJSON(file="config.json")
data_path <- config$data_matrix

library(forecast)

tsdata <- read.csv(data_path, head=F)
tsdata <- tsdata[order(tsdata[, 1]), ]

# 如下写法需要保证：tsdata的起止时间在[星期一, 星期一)
train_nums <- 90*7 # 90：训练数据的长度为90周；7：一周的天数
crime_counts_day <- tail(tsdata[, 2], train_nums)
rm(tsdata); gc()

# 按天预测：移动法-在一周的时间内，将下一天的预测数据接在训练数据之后，运行模型，该预测结果是下下一天的预测，循环以上步骤
pred_day <- NULL # pred_day是长度为7的向量
for(i in 1:7) {
    fit_day <- auto.arima(ts(c(tail(crime_counts_day, train_nums-i+1), pred_day), start=1, frequency=1))
    pred_day <- c(pred_day, as.numeric(as.data.frame(forecast(fit_day, h=1))[1])) 	
}
pred_day <- ceiling(pred_day)

# # 按周预测：处理好周数据
# week_vector <- rep(1:90, each=7)
# crime_counts_week <- data.frame(week_vector, crime_counts_day)
# crime_counts_week <- aggregate(crime_counts_day~week_vector, data=crime_counts_week, sum)
# crime_counts_week <- crime_counts_week$crime_counts_day

# fit_week <- auto.arima(ts(crime_counts_week), start=1, frequency=1))
 
# 结果显示
last_week_crimes <- sum(tail(crime_counts_day, 7))
new_week_crimes <- sum(pred_day)


last_new_ratio <- (new_week_crimes-last_week_crimes)/last_week_crimes

cat("上周警情数：", last_week_crimes, "本周预测警情数：", new_week_crimes, "预计本周警情环比上周将（上升/下降）：", last_new_ratio, sep="\n")
cat("\n")

