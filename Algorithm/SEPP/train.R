# Version: 自激励点过程模型R语言实现Version 1.0
# Model Nmae: self exciting point process model(SEPP)
# Author: sqk (Rewrite:lf)
# Date: 2016/4/22 ——

argv <- commandArgs(TRUE)

    # data_path <- argv[1] # 案件数据：3列，案发时间（精度转换）、案发经纬度（或所在格子中心点的经纬度）
    # result_path <- argv[2]
    # if(file.exists(result_path) == F) dir.create(result_path)
    # if(file.exists(paste(result_path, "/train_result", sep="")) == F) dir.create(paste(result_path, "/train_result", sep=""))
    # # pd_id <- as.numeric(argv[3])
    # pd_id <- argv[3]
    # startDate <- as.numeric(argv[4])
    # endDate <- as.numeric(argv[5])
    # trainCenterOrNot <- argv[6]
    # distRange <- as.numeric(argv[7]) # 1公里
    # dateRange <- as.numeric(argv[8]) # 150天
    # widthNuCounts <- as.numeric(argv[9]) # 默认100
    # widthMuCounts <- as.numeric(argv[10]) # 默认15
    # widthGCounts <- as.numeric(argv[11]) # 默认15
    # maxIterations <- as.numeric(argv[12]) # 需要大于10，默认30

workdir <- argv[1]
result_path <- argv[1]
setwd(workdir)
if(!file.exists("train_result")) dir.create("train_result")
library(rjson)
config <- fromJSON(file="train_config.json")
data_path <- config$data_matrix
pd_id <- config$pd_id
startDate <- config$start_date
endDate <- config$end_date
trainCenterOrNot <- config$center
if(trainCenterOrNot == "TRUE") trainCenterOrNot <- TRUE
if(trainCenterOrNot == "FALSE") trainCenterOrNot <- FALSE  
distRange <- config$dist_range
dateRange <- config$date_range
widthNuCounts <- config$nu_counts
widthMuCounts <- config$mu_counts
widthGCounts <- config$g_counts
maxIterations <- config$iters
if(maxIterations<=10) stop("maxIterations must gt 10")

# library(sp)
# library(geosphere)
# library(parallel)
# library(snow)
# library(bigmemeory)
library(compiler)
# enableJIT(level=3)

#options(digits=5)

data3D <- read.csv(data_path, header=F)


data3D[,1] <- substr(as.character(data3D[,1]), 2, 15) 
data3D$V1 <- strptime(data3D[,1], "%Y%m%d%H%M%S")
startDate <- strptime(paste(startDate, '000000', sep=''), "%Y%m%d%H%M%S")
endDate <- strptime(paste(endDate,'000000', sep=''), "%Y%m%d%H%M%S")
classId <- 0
classCounts <- length(unique(data3D[, 7]))


data3D <- data3D[which(data3D[, 1] >= startDate & data3D[, 1] < endDate), ]
data3D <- data3D[order(data3D[, 7], data3D[, 1]), ]
# classId <- data3D[, 7]
# classCounts <- length(unique(classId))
# classLength <- aggregate(V1~V7, data=data3D, length)[, 2]
# if(classCounts == 1) { classId <- rep(0, classLength) }
# if(classCounts == 2) { classId <- rep(c(0, 0.3), classLength) }
# if(classCounts == 3) { classId <- rep(c(0, 0.3, 0.6), classLength) }

data3D <- data3D[, -7]
if(trainCenterOrNot == "FALSE") {
    data3D <- data3D[, -c(2, 5, 6)] # 用案件经纬度
} else if(trainCenterOrNot == "TRUE") {
    data3D <- data3D[, -c(2, 3, 4)] # 用格子中心点
} else {stop("Error:trainCenterOrNot")}

names(data3D) <- c('time', 'lon', 'lat')
#data3D$time <- strptime(as.character(data3D$time), "%Y%m%d%H%M%S")
# data3D$time <- as.Date(as.character(data3D$time), "%Y%m%d")
##data3D <- data3D[-which(is.na(data3D$time)),] #当读入的数据第一列是时间时，必须是字符串形式，否则出错（NA值产生）！！！！！！
originalTime <- data3D$time[1]
originalLon <- min(data3D$lon)
originalLat <- min(data3D$lat)
data3D$time <- as.numeric(difftime(data3D$time, originalTime, units="days"))
data3D$time <- data3D$time + classId
data3D$lon <- (data3D$lon - originalLon) * 94.69697
data3D$lat <- (data3D$lat - originalLat) * 111.319
id <- 1:nrow(data3D)
samples <- length(id)

# ---------------------------------
# 建立初始矩阵P0
# ---------------------------------

# logicS <- distm(data3D[, 2:3]) < distRange # 空间距离
logicS <- as.matrix(dist(data3D[, 2:3])) < distRange # 欧氏距离判断
logicT <- outer(data3D[, 1], data3D[, 1], function(x, y) y-x) < dateRange # 时间差判断
logicST <- logicS & logicT # 满足距离条件或时间条件
rm(logicS, logicT); gc()
logicST[lower.tri(logicST)] <- 0
non0ValueIndex <- which(logicST[upper.tri(logicST)] == 1) # 在后续的迭代计算中进行修正

# fill <- apply(logicST, 2, function(x) 1/sum(x == 1)) # 均匀分配策略
fill <- apply(logicST, 2, function(x) (1-0.5)/(sum(x == 1)-1)) # 主对角线0.5分策略
fill[which(fill == Inf)] <- 1                                                  
P0 <- t(t(logicST) * fill)
diag(P0)[which(diag(P0) > 0 & diag(P0) < 1)] <- 0.5
rm(fill, logicST); gc()

P0 <- apply(P0, 2, cumsum)
P0[lower.tri(P0)] <- 0

# 划分background和offspring 
randValues <- runif(samples)

logicBackground <- t(t(P0) > randValues)
rm(P0); gc()
indexTemp <- apply(logicBackground, 2, function(x) pmatch(TRUE, x))
rm(logicBackground); gc()
indexTemp[1] <- 1 # 设定第一起案件为自发（background）

background <- data3D[which(indexTemp == id), ] # 对角线上值为1表示自发案件
backgroundNums <- nrow(background)

offTemp1 <-  data3D[indexTemp[which(indexTemp != id)], ]
offTemp2 <-  data3D[which(indexTemp != id), ]
rm(indexTemp); gc()
offspring <- offTemp2 - offTemp1
rm(offTemp1, offTemp2); gc()
offspringNums <- nrow(offspring)

backgroundCounts <- NULL
error <- NULL
myexp <- .Primitive("exp")
mysum <- .Primitive("sum")
mysquare <- .Primitive("^")

for (i in 1:maxIterations) {
    if(i>10) if((sd(tail(error)) < 1e-4 | all(abs(scale(tail(backgroundCounts), scale=F))<5))) {
        break # 设定迭代判停条件
    }
    
    # 计算nu函数（v(t)）
    bandWidthNu <- background$time / sd(background$time)
    bandWidthNu <- outer(bandWidthNu, bandWidthNu, function(x, y) abs(x-y))

    # widthNuCounts # 全局参数
    widthNuCounts <- ifelse(widthNuCounts < backgroundNums, widthNuCounts, backgroundNums)
    bandWidthNu <- apply(bandWidthNu, 2, function(x) sort(x)[widthNuCounts])
    bandWidthNu <- ifelse(bandWidthNu < 0.0001, 0.0001, bandWidthNu)

    nu <- function(tValue) {
        sum(myexp((-((tValue-background$time)^2))/(2*(sd(background$time)^2)*(bandWidthNu^2)))/((sqrt(2*pi))*bandWidthNu*(sd(background$time))))
    }

    # 计算mu函数（u(x,y)）
    bandWidthMu <- apply(background[, 2:3], 2, function(x) x/sd(x))
    bandWidthMu <- as.matrix(dist(background[, 2:3]))

    # widthMuCounts # 全局参数
    widthMuCounts <- ifelse(widthMuCounts < backgroundNums, widthMuCounts, backgroundNums)
    bandWidthMu <- apply(bandWidthMu, 2, function(x) sort(x)[widthMuCounts])
    bandWidthMu <- ifelse(bandWidthMu < 0.0001, 0.0001, bandWidthMu)

    mu <- function(lon, lat) {
        sum(myexp((-((lon-background$lon)^2))/(2*(bandWidthMu^2)*(sd(background$lon)^2))-((lat-background$lat)^2)/(2*(bandWidthMu^2)*(sd(background$lat)^2)))/((sd(background$lon))*(sd(background$lat))*(2*pi)*(bandWidthMu^2)))
    }

    # 计算g函数（g(t,x,y)）
    widthGCounts <- ifelse(widthGCounts < offspringNums, widthGCounts, offspringNums)
    bandWidthG <- apply(offspring, 2, function(x) x/sd(x))
    bandWidthG <- as.matrix(dist(bandWidthG))
    bandWidthG <- apply(bandWidthG, 2, function(x) sort(x)[widthGCounts])
    bandWidthG <- ifelse(bandWidthG < 0.0001, 0.0001, bandWidthG)

    # g <- function(x, offspring, bandWidthG) {
        # tDist <- x[1]; lonDist <- x[2]; latDist <- x[3]
        # sum(exp((-((tDist-offspring[,1])^2))/(2*(sd(offspring[,1])^2)*(bandWidthG^2))-((lonDist-offspring[,2])^2)/(2*(sd(offspring[,2])^2)*(bandWidthG^2))-((latDist-offspring[,3])^2)/(2*(sd(offspring[,3])^2)*(bandWidthG^2)))/((sd(offspring[,1]))*(sd(offspring[,2]))*(sd(offspring[,3]))*(2*pi)^(3/2)*(bandWidthG^3)))
        # # rm(tDist, lonDist, latDist, offspring); gc()
    # }

    denominator1 <- 2*(sd(offspring[,1])^2)*(bandWidthG^2)
    denominator2 <- 2*(sd(offspring[,2])^2)*(bandWidthG^2)
    denominator3 <- 2*(sd(offspring[,3])^2)*(bandWidthG^2)
    denominator <- (sd(offspring[,1]))*(sd(offspring[,2]))*(sd(offspring[,3]))*(2*pi)^(3/2)*(bandWidthG^3)
    
    g <- function(tDist, lonDist, latDist) {
        mysum(myexp(
                -mysquare(tDist-offspring[,1], 2)/denominator1
                -mysquare(lonDist-offspring[,2], 2)/denominator2
                -mysquare(latDist-offspring[,3], 2)/denominator3
            )/denominator
        )
    }

    g <- cmpfun(g, options=list(optimize=3))
    
    # newg <- function(tDist, lonDist, latDist, offspring, bandWidthG) {       
        # sum(exp((-((tDist-offspring[,1])^2))/(2*(sd(offspring[,1])^2)*(bandWidthG^2))-((lonDist-offspring[,2])^2)/(2*(sd(offspring[,2])^2)*(bandWidthG^2))-((latDist-offspring[,3])^2)/(2*(sd(offspring[,3])^2)*(bandWidthG^2)))/((sd(offspring[,1]))*(sd(offspring[,2]))*(sd(offspring[,3]))*(2*pi)^(3/2)*(bandWidthG^3)))
        # # rm(tDist, lonDist, latDist, offspring); gc()
    # }
    
    # 将nu、mu、g函数带入数据集data3D重新计算
    nu1D <- unlist(lapply(data3D$time, nu)); nu1D[1] <- 1
    mu2D <- apply(data3D[, 2:3], 1, function(x) mu(x[1], x[2])); mu2D[1] <- 1
    rm(background); gc()
    
    g1 <- t(outer(data3D[, 1], data3D[, 1], "-"))
    g2 <- t(outer(data3D[, 2], data3D[, 2], "-"))
    g3 <- t(outer(data3D[, 3], data3D[, 3], "-"))

    g1 <- g1[upper.tri(g1)][non0ValueIndex]; g2 <- g2[upper.tri(g2)][non0ValueIndex]; g3 <- g3[upper.tri(g3)][non0ValueIndex]
    # g3D <- cbind(tDist=g1, lonDist=g2, latDist=g3)
    # g3D <- as.data.frame(g3D)
    # rm(g1, g2, g3); gc()
    
    # upperNums <- nrow(g3D)
    # bigG3D <- as.big.matrix(g3D)
    # bigG3DDesc <- describe(bigG3D)   
    # g3D <- parSapply(cl, 1:upperNums, gg, bigG3DDesc, offspring, bandWidthG)
    
    # 启动多核并行计算
    # cl <- makeCluster(2, type="SOCK") # cl <- makeCluster(getOption("cl.cores", 2))
    # partId <- clusterSplit(cl, 1:nrow(g3D))
    # g3D <- lapply(partId, function(x) g3D[x, ])
    # g3D <- parLapply(cl, g3D, g, offspring, bandWidthG)
     # t1 <- Sys.time()
    # # g3D <- parRapply(cl, g3D, g, offspring, bandWidthG) # 最耗时
    # g3D <- clusterMap(cl, newg, g1, g2, g3, offspring, bandWidthG)
     # t2 <- Sys.time()
     # t2 - t1
    # stopCluster(cl)
    
    # library(cpgen)
    # library(foreach)
    # foreach(i=g1, j=g2, k=g3,  .combine='c') %dopar% gg(i, j, k, offspring, bandWidthG)
    
    # 数据切分策略
    # g3D <- split(g3D, rep(1:1000, each=10000)[1:nrow(g3D)]) # 最大案件是1万 
    # g3D <- lapply(g3D, function(x) apply(x, 1, g, offspring, bandWidthG))
    # g3D <- foreach(i=1:length(g3D), .combine='c') %dopar% apply(g3D[[i]], 1, g, offspring, bandWidthG)
    
    g3D <- eval(compile(mapply(g, g1, g2, g3)))
    
    rm(g1, g2, g3, offspring); gc()
    
    # 当迭次数大于1时
    if(i>1) { P1uper <- updateP }
    
    updateP <- matrix(NA, samples, samples)
    updateP[upper.tri(updateP)][non0ValueIndex] <- g3D / samples
    updateP[upper.tri(updateP)][-non0ValueIndex] <- 0
    rm(g3D); gc()
    diag(updateP) <- nu1D * mu2D / backgroundNums
    rm(nu1D, mu2D); gc()

    updateP <- apply(updateP, 2, function(x) x/sum(x,na.rm=T))
    updateP[lower.tri(updateP)] <- 0
    
    # 划分background和offspring 
    tempP <- apply(updateP, 2, cumsum)
    tempP[lower.tri(tempP)] <- 0
    
    randValues <- runif(samples)

    logicBackground <- t(t(tempP) > randValues)
    rm(tempP); gc()
    indexTemp <- apply(logicBackground, 2, function(x) pmatch(TRUE, x))
    rm(logicBackground); gc()
    indexTemp[1] <- 1 # 设定第一起案件为自发（background）

    background <- data3D[which(indexTemp == id), ] # 对角线上值为1表示自发案件
    backgroundNums <- nrow(background)

    # 记下每次迭代background分配到的案件数
    backgroundCounts <- c(backgroundCounts, backgroundNums)

    offTemp1 <-  data3D[indexTemp[which(indexTemp != id)], ]
    offTemp2 <-  data3D[which(indexTemp != id), ]
    rm(indexTemp); gc()
    offspring <- offTemp2 - offTemp1
    rm(offTemp1, offTemp2); gc()
    offspringNums <- nrow(offspring)
    
    if(i>1) { # 迭代1次以上前后两次概率矩阵的误差平方和
        error <- c(error, sum((updateP - P1uper)^2) / samples^2) 
    }
    #print(i)
}
print(backgroundCounts)
# 计算nu函数（v(t)）
bandWidthNu <- background$time / sd(background$time)
bandWidthNu <- outer(bandWidthNu, bandWidthNu, function(x, y) abs(x-y))

# widthNuCounts # 全局参数
widthNuCounts <- ifelse(widthNuCounts < backgroundNums, widthNuCounts, backgroundNums)
bandWidthNu <- apply(bandWidthNu, 2, function(x) sort(x)[widthNuCounts])
bandWidthNu <- ifelse(bandWidthNu < 0.0001, 0.0001, bandWidthNu)

# 计算mu函数（u(x,y)）
bandWidthMu <- apply(background[, 2:3], 2, function(x) x/sd(x))
bandWidthMu <- as.matrix(dist(background[, 2:3]))

# widthMuCounts # 全局参数
widthMuCounts <- ifelse(widthMuCounts < backgroundNums, widthMuCounts, backgroundNums)
bandWidthMu <- apply(bandWidthMu, 2, function(x) sort(x)[widthMuCounts])
bandWidthMu <- ifelse(bandWidthMu < 0.0001, 0.0001, bandWidthMu)

# 计算g函数（g(t,x,y)）
bandWidthG <- apply(offspring, 2, function(x) x/sd(x))
bandWidthG <- as.matrix(dist(bandWidthG))
bandWidthG <- apply(bandWidthG, 2, function(x) sort(x)[widthMuCounts])
bandWidthG <- ifelse(bandWidthG < 0.0001, 0.0001, bandWidthG)

denominator1 <- 2*(sd(offspring[,1])^2)*(bandWidthG^2)
denominator2 <- 2*(sd(offspring[,2])^2)*(bandWidthG^2)
denominator3 <- 2*(sd(offspring[,3])^2)*(bandWidthG^2)
denominator <- (sd(offspring[,1]))*(sd(offspring[,2]))*(sd(offspring[,3]))*(2*pi)^(3/2)*(bandWidthG^3)

save(classCounts, data3D, distRange, dateRange, originalTime, originalLon, originalLat, samples, backgroundNums, 
    background, offspring, bandWidthNu, bandWidthMu, bandWidthG, denominator1, denominator2, denominator3, denominator, 
    nu, mu, g, file=paste(result_path, "/train_result/", pd_id, "_SEPP.RData", sep=""))

