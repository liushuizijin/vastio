# Version: 自激励点过程模型R语言实现Version 1.0
# Model Nmae: self exciting point process model(SEPP)
# Author: sqk (Rewrite:lf)
# Date: 2016/4/22 ——

argv <- commandArgs(TRUE)

    # data_path <- argv[1] # 格子中心点数据：3列，格子编号，格子中心点经纬度
    # result_path <- argv[2]
    # if(file.exists(result_path) == F) dir.create(result_path)
    # if(file.exists(paste(result_path, "/pred_result", sep="")) == F) dir.create(paste(result_path, "/pred_result", sep=""))
    # # pd_id <- as.numeric(argv[3])
    # pd_id <- argv[3]
    # predStartDay <- argv[4]
    # predEndDay <- argv[5] # 左闭右开
workdir <- argv[1]
result_path <- argv[1]
setwd(workdir)
if(!file.exists("pred_result")) dir.create("pred_result")
library(rjson)
config <- fromJSON(file="predict_config.json")
data_path <- config$data_matrix
pd_id <- config$pd_id
predStartDay <- as.character(config$start_date)
predEndDay <- as.character(config$end_date)


# library(sp)
# library(geosphere)

# 加载nu、mu、g函数，以及相关数据
load(paste(result_path, "/train_result/", pd_id, "_SEPP.RData", sep=""))

G <- function(x) g(x[1], x[2], x[3])
NUMU <- function(x) (nu(x[1]) * mu(x[2], x[3]))
myexp <- .Primitive("exp")
mysum <- .Primitive("sum")
mysquare <- .Primitive("^")

predDateRange <- seq(as.Date(predStartDay, "%Y%m%d"), as.Date(predEndDay, "%Y%m%d")-1, 1)

predDateRange <- as.numeric(format(predDateRange, "%Y%m%d"))
predDateRange  <- paste(predDateRange, "080000", sep="") # 将日期全部转化为上午8点的时间格式
predDateRange <- strptime(predDateRange, "%Y%m%d%H%M%S")

predDateRangeDist <- as.numeric(difftime(predDateRange, originalTime, units="days"))  # 特别注意，当predDateRange是"%Y%m%d%H%M%S"格式时，predDateRange会默认变成yyyy-mm-dd 08:00:00
# print(predDateRangeDist)
# print(originalTime)
# print(predDateRange)
trainData <- data3D; rm(data3D); gc()

gridData <- read.csv(data_path, header=F)

names(gridData) <- c("grid_num", "lon", "lat")
gridData$lon <- (gridData$lon - originalLon) * 94.69697
gridData$lat <- (gridData$lat - originalLat) * 111.319
gridNums <- nrow(gridData)

lonDist <- t(outer(gridData[, 2], trainData[, 2], "-"))
lonDist <- as.vector(lonDist)
latDist <- t(outer(gridData[, 3], trainData[, 3], "-"))
latDist <- as.vector(latDist)

classCounts <- 1

if(classCounts == 1) { classValue <- 0 }
if(classCounts == 2) { classValue <- c(0, 0.3) }
if(classCounts == 3) { classValue <- c(0, 0.3, 0.6) }

for(i in 1:classCounts) {
    result <- NULL
    for(j in 1:length(predDateRangeDist)) {
        gridData$time <- predDateRangeDist[j] + classValue[i]
        gridData <- gridData[, c("grid_num", "time", "lon", "lat")]
        
        uValue <- apply(gridData[,2:4], 1, NUMU)
        uValue <- cbind(date_str=as.numeric(format(predDateRange[j], "%Y%m%d")), grid_num=gridData$grid_num, uValue=uValue)
        uValue <- as.data.frame(uValue)
        uValue$uValue <- uValue$uValue / backgroundNums
        
        timeDist <- t(outer(gridData[, 2], trainData[, 1], "-"))
        timeDist <- as.vector(timeDist)

        dist3D <- cbind(date_str=as.numeric(format(predDateRange[j], "%Y%m%d")), 
                        grid_num=rep(gridData$grid_num, each=nrow(trainData)),
                        tDist=timeDist, lonDist=lonDist, latDist=latDist)
        rm(timeDist); gc()
        dist3D <- as.data.frame(dist3D)
        dist3D$TF <- ifelse(dist3D$tDist > dateRange | sqrt(dist3D$lonDist^2 + dist3D$latDist^2) > distRange, 0, 1)
        gValue <- apply(dist3D[which(dist3D$TF==1), c("tDist", "lonDist", "latDist")], 1, G)
        dist3D$gValue <- 0
        dist3D[which(dist3D$TF==1), "gValue"] <- gValue
        gValue <- aggregate(gValue ~ date_str+grid_num, data=dist3D[which(dist3D$TF==1),], sum)
        rm(dist3D); gc()
        gValue$gValue <- gValue$gValue / samples      

        Values <- merge(uValue, gValue, by=c("date_str", "grid_num"), all=T)
        rm(uValue, gValue); gc()
        Values[is.na(Values)] <- 0
        Values$p <- Values$uValue + Values$gValue
        result <- rbind(result, Values[, c("date_str", "grid_num", "p")])
        rm(Values); gc()
    }
    # 按照日期升序，p值降序
    result <- result[order(result$date_str, -result$p), ] 
    print(max(result$p))
    result$p <- rep(seq(0.1, 0, length=gridNums), length(predDateRangeDist))
    if(classCounts > 1) {
    	write.table(result, file=paste(result_path, "/pred_result/", "class_", i, "_", pd_id, "_SEPP.csv", sep=""), sep=",", row.names=F, col.names=F)
	} else {
		write.table(result, file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_SEPP.csv", sep=""), sep=",", row.names=F, col.names=F)
	}
}





  
  
  
