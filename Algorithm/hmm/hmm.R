# Model name：hmm
# Date：2015/9/10
# Vsersion：1.0 （no POI数据）
# Details：基于初始状态修正的隐马尔科夫模型
# Library：markovchain、plyr


argv <- commandArgs(TRUE)

    # data_path <- argv[1]
    # result_path <- argv[2]
    # if(file.exists(result_path) == F) dir.create(result_path)
    # if(file.exists(paste(result_path, "/pred_result", sep="")) == F) dir.create(paste(result_path, "/pred_result", sep=""))
    # pd_id <- as.numeric(argv[3])
    # start_date <- as.numeric(argv[4])
    # end_date <- as.numeric(argv[5])
    # hide_range <- as.numeric(argv[6])
    # observe_range <- as.numeric(argv[7])
    # if(argv[8] == "NULL") {
        # topN <- NULL # 默认
    # } else {
        # topN <- as.numeric(argv[8]) 
    # }

workdir <- argv[1]
result_path <- argv[1]
setwd(workdir)
if(!file.exists("pred_result")) dir.create("pred_result")
library(rjson)
config <- fromJSON(file="config.json")
data_path <- config$data_matrix
pd_id <- config$pd_id
start_date <- config$start_date
end_date <- config$end_date
hide_range <- config$hide_range
observe_range <- config$observe_range

# topN <- NULL
topN <- 10
    
## 加载library
library(methods)
library(markovchain)
library(plyr)

#### 对读入的数据进行处理
crime_data <- read.csv(data_path, header=F)

## 从以下开始，如果有POI数据加入，必须注意修改logic_class的计算、列名、POI概率

class_count <- (ncol(crime_data)-2)/2

if(class_count == 1) {
  names(crime_data) <- c("date_str", "grid_num", "noclass_layer0", "noclass_layer0_1")
  crime_data$noclass_layer0 <- ifelse(crime_data$noclass_layer0 >= 1, 1, 0)
  crime_data$noclass_layer0_1 <- ifelse(crime_data$noclass_layer0_1 >= 1, 1, 0)
}

if(class_count == 2) {
  names(crime_data) <- c("date_str", "grid_num", "class1_layer0", "class1_layer0_1",
    "class2_layer0", "class2_layer0_1")
  crime_data$class1_layer0 <- ifelse(crime_data$class1_layer0 >= 1, 1, 0)
  crime_data$class1_layer0_1 <- ifelse(crime_data$class1_layer0_1 >= 1, 1, 0)
  crime_data$class2_layer0 <- ifelse(crime_data$class2_layer0 >= 1, 1, 0)
  crime_data$class2_layer0_1 <- ifelse(crime_data$class2_layer0_1 >= 1, 1, 0) 
}

if(class_count == 3) {
  names(crime_data) <- c("date_str", "grid_num", "class1_layer0", "class1_layer0_1",
    "class2_layer0", "class2_layer0_1", "class3_layer0", "class3_layer0_1")
  crime_data$class1_layer0 <- ifelse(crime_data$class1_layer0 >= 1, 1, 0)
  crime_data$class1_layer0_1 <- ifelse(crime_data$class1_layer0_1 >= 1, 1, 0)
  crime_data$class2_layer0 <- ifelse(crime_data$class2_layer0 >= 1, 1, 0)
  crime_data$class2_layer0_1 <- ifelse(crime_data$class2_layer0_1 >= 1, 1, 0) 
  crime_data$class3_layer0 <- ifelse(crime_data$class3_layer0 >= 1, 1, 0)
  crime_data$class3_layer0_1 <- ifelse(crime_data$class3_layer0_1 >= 1, 1, 0)  
}

crime_data <- arrange(crime_data, date_str, grid_num)
## 列出所有格子
grid_ord <- sort(unique(crime_data$grid_num))

## 本格子：计算转移概率矩阵
## “隐藏层”：选择取样时间段hide_range；不用考虑“丢掉昨天”

## 外层格子以及其他协变量（POI数据）：计算转移概率矩阵 和 混淆矩阵
## “观测层”：选择取样时间段observe_range——混淆矩阵；hide_range——转移概率矩阵
## 当有多个观测层时需要新增程序来实现

## 这里借用HMM中“隐藏层”、“观测层”的概念，在最终做预测时不同于HMM
## HMM有不动性假设，而我们在做预测时会做时序上的动态计算

#### 加法模型实现HMM
#train_range <- as.numeric(as.Date(end_date, "%Y%m%d") - as.Date(start_date, "%Y%m%d")) ## 左闭右开模式
#if(start_date == end_date) { ## 表示每天出预测
#  train_range <- start_date
#} else { ## 表示一段时间的预测：训练（验证）
train_range <- as.numeric(format(seq(as.Date(as.character(start_date), "%Y%m%d"), as.Date(as.character(end_date), "%Y%m%d") - 1, by=1), "%Y%m%d"))
#}

hot_fun <- function(x) return(sum(tail(x, observe_range))/observe_range) 

for(i in 1:class_count) {
  class_data <- crime_data[,c(1:2, (2*i+1):(2*i+2))] ## 其他POI数据需要另外添加列
  names(class_data) <- c("date_str", "grid_num", "layer0", "layer1")
  res_p <- NULL
  for(tr in train_range) {
    min_hide_date <- as.numeric(format(as.Date(as.character(tr), "%Y%m%d")-1 - hide_range, "%Y%m%d"))
    min_observe_date <- as.numeric(format(as.Date(as.character(tr), "%Y%m%d")-1 - observe_range, "%Y%m%d"))
   
    ## 抠掉昨天 
    ## yesterday <- as.numeric(format(as.Date(as.character(tr), "%Y%m%d") - 1, "%Y%m%d")) ##“抠掉昨天”
    yesterday <- tr ##不抠掉昨天
    
    class_data_obs <- class_data[which(class_data$date_str >= min_observe_date & class_data$date_str < yesterday),]
    class_data_hide <- class_data[which(class_data$date_str >= min_hide_date & class_data$date_str < yesterday),]     
    
    ## 抠掉昨天：hide_range和observe_range必须是偶数
    ## class_data_obs <- ddply(class_data_obs, .(grid_num), function(x) x[seq(2,nrow(x),2),])
    ## class_data_hide <- ddply(class_data_hide, .(grid_num), function(x) x[seq(2,nrow(x),2),])
    
    ## 过滤格子:最宽松的过滤，即从当天往以前的observe_range<的偶数>天（“抠掉昨天”的缘由）内无案件的格子将会被剔除
    filt <- ddply(class_data_obs, .(grid_num), summarise, x=sum(layer0))
    grid_filt <- sort(filt[which(filt[, 2] > 0), 1]) ##保留满足条件的格子
    rm(filt); gc()
    
    ## 隐藏层：即本格子用markovchain预测法计算状态转移概率矩阵
    class_data_hide <- class_data_hide[which(class_data_hide$grid_num %in% grid_filt),]
    func <- function(x) return(markovchainFit(x)$estimate@transitionMatrix)
    #matrix_hide_odd <- dlply(class_data_hide, .(grid_num), summarise, col2=func(layer0[seq(1,length(layer0),2)])[,2])
    #matrix_hide_even <- dlply(class_data_hide, .(grid_num), summarise, col2=func(layer0[seq(2,length(layer0),2)])[,2])
    matrix_hide <- dlply(class_data_hide, .(grid_num), summarise, col2=func(layer0)[,2])
   
    ## 估计隐藏层当前状态（有某种概率表示的当前状态）
    #date7 <- as.numeric(format(as.Date(as.character(yesterday), "%Y%m%d")-1-7, "%Y%m%d"))    
    #hide_date7 <- class_data_hide[which(class_data_hide$date_str > date7),]
    #hide_date7 <- ddply(hide_date7, .(grid_num), summarise, init_p=sum(layer0)/7)    
    
    #hide_date7 <- ddply(class_data_hide, .(grid_num), summarise, init_p=sum(tail(layer0,7))/7)    
    
    #不估计初始状态（待预测的今天的昨天的状态，考虑到PPS数据接入延迟的利弊：弊，时序数据不充分不完整；利，充分利用初始状态为0的特性）
    hide_date7 <- ddply(class_data_hide, .(grid_num), summarise, init_p=tail(layer0,1))    
    
    hide_p_1 <- ldply(matrix_hide[as.numeric(names(matrix_hide)) %in% hide_date7[hide_date7$init_p == 0, "grid_num"]], function(x) return(x[1,]))
    hide_p_2 <- ldply(matrix_hide[!(as.numeric(names(matrix_hide)) %in% hide_date7[hide_date7$init_p == 0, "grid_num"])], function(x) return(x[2,]))  
    #names(hide_p_1) <- c("grid_num", "hide_p"); names(hide_p_2) <- c("grid_num", "hide_p")
    if(nrow(hide_p_2) != 0) {
      names(hide_p_2) <- c("grid_num", "hide_p")
      hide_p_2$hide_p <- hide_p_2$hide_p * hide_date7[hide_date7$init_p != 0,"init_p"]  ## R内部执行的格子顺序是一样的
    }
    rm(hide_date7); gc()
    if(nrow(hide_p_1) != 0) {
      names(hide_p_1) <- c("grid_num", "hide_p")
      hide_p <- rbind.fill(hide_p_1, hide_p_2)
    } else {
      hide_p <- hide_p_2
    }
    
    hide_p$grid_num <- as.numeric(hide_p$grid_num)
    hide_p <- arrange(hide_p, grid_num)
    rm(hide_p_1, hide_p_2); gc()
    
    observe <- ddply(class_data_hide, .(grid_num), summarise, obs_sum=sum(layer1))
    grid_p <- hide_p[hide_p$grid_num %in% observe[observe$obs_sum==0,"grid_num"],] ## 在min_hide_date天内外层状态全为0时，观测层的影响为0
    names(grid_p)[2] <- "p"
    
    ## 观测层： 外层格子或其他POI数据
    ## 观测层：转移概率矩阵；混淆矩阵
    mk_observe <- dlply(class_data_hide[!(class_data_hide$grid_num %in% observe[observe$obs_sum==0,"grid_num"]),], .(grid_num), summarise, col2=func(layer1))
    ## 此时mk_observe是2行2列的matrix
    mix_matrix <- dlply(class_data_obs[class_data_obs$grid_num %in% observe[observe$obs_sum!=0,"grid_num"],], .(grid_num), summarise, prop.table(table(layer0, layer1)))
    ## 此时mix_matrix有可能是2行1列（外层全为0）的table
    
    ## “估计”观测层转移状态：为什么用“估计”，需细品其中的逻辑意义
    #obs_tab7 <- class_data_hide[which(class_data_hide$date_str > date7 & !(class_data_hide$grid_num %in% observe[observe$obs_sum==0,"grid_num"])),]
    #obs_tab7 <- ddply(obs_tab7, .(grid_num), summarise, init_p=sum(layer1)/7)
    
    #obs_tab7 <- ddply(class_data_hide[which(!(class_data_hide$grid_num %in% observe[observe$obs_sum==0,"grid_num"])),], .(grid_num), summarise, init_p=sum(tail(layer1,7))/7) 
    
    #不估计初始状态
    obs_tab7 <- ddply(class_data_hide[which(!(class_data_hide$grid_num %in% observe[observe$obs_sum==0,"grid_num"])),], .(grid_num), summarise, init_p=tail(layer1,1)) 
   
    rm(class_data_hide); gc()
    
    obs_tab7_0 <- obs_tab7[obs_tab7$init_p ==0,]
    obs_tab7_1 <- obs_tab7[obs_tab7$init_p !=0,]
    rm(obs_tab7); gc()
    
    mix_matrix_1 <- mix_matrix[lapply(mix_matrix, function(x) length(x[2,]))==1]
    mix_matrix_2 <- mix_matrix[lapply(mix_matrix, function(x) length(x[2,]))==2]
    rm(mix_matrix); gc()
    
    grid_0_1 <- intersect(as.character(obs_tab7_0$grid_num), names(mix_matrix_1))
    obs_p_0_1 <- unlist(lapply(mk_observe[grid_0_1], function(x) x[1,][1])) * unlist(lapply(mix_matrix_1[grid_0_1], function(x) x[2,][1])) 
    obs_p_0_1 <- cbind(grid_num=as.numeric(grid_0_1), obs_p=obs_p_0_1)
    
    grid_0_2 <- intersect(as.character(obs_tab7_0$grid_num), names(mix_matrix_2))
    rm(obs_tab7_0); gc()
    obs_p_0_2 <- unlist(lapply(mk_observe[grid_0_2], function(x) x[2,][1])) * unlist(lapply(mix_matrix_2[grid_0_2], function(x) x[2,][2]))
    obs_p_0_2 <- cbind(grid_num=as.numeric(grid_0_2), obs_p=obs_p_0_2)
    
    grid_1_1 <- intersect(as.character(obs_tab7_1$grid_num), names(mix_matrix_1))
    obs_p_1_1 <- unlist(lapply(mk_observe[grid_1_1], function(x) x[1,][2])) * obs_tab7_1[obs_tab7_1$grid_num %in% as.numeric(grid_1_1),"init_p"] * unlist(lapply(mix_matrix_1[grid_1_1], function(x) x[2,][1]))
    obs_p_1_1 <- cbind(grid_num=as.numeric(grid_1_1), obs_p=obs_p_1_1)
    
    grid_1_2 <- intersect(as.character(obs_tab7_1$grid_num), names(mix_matrix_2))
    obs_p_1_2 <- unlist(lapply(mk_observe[grid_1_2], function(x) x[2,][2])) * obs_tab7_1[obs_tab7_1$grid_num %in% as.numeric(grid_1_2),"init_p"] * unlist(lapply(mix_matrix_2[grid_1_2], function(x) x[2,][2]))
    rm(obs_tab7_1); gc()
    obs_p_1_2 <- cbind(grid_num=as.numeric(grid_1_2), obs_p=obs_p_1_2)
    rm(mk_observe); gc()
    
    obs_p <- rbind.fill(as.data.frame(obs_p_0_1), as.data.frame(obs_p_0_2), as.data.frame(obs_p_1_1), as.data.frame(obs_p_1_2))
    rm(obs_p_0_1, obs_p_0_2, obs_p_1_1, obs_p_1_2); gc()
    obs_p <- arrange(obs_p, grid_num)
    
    obs_p$obs_p <- obs_p$obs_p + hide_p[hide_p$grid_num %in% obs_p$grid_num, "hide_p"]
    names(obs_p)[2] <- "p"; rm(hide_p); gc()
    
    grid_p <- rbind.fill(grid_p, obs_p)
    rm(obs_p); gc()
    
    ## 修正排序
    ## 1.某天排在靠前的p值相同时（不为0）
    ## 2.某天p值全为0时
    hot_p <- ddply(class_data_obs[class_data_obs$grid_num %in% grid_p$grid_num,], .(grid_num), summarise, hot_p=hot_fun(layer0))
    rm(class_data_obs); gc()
    hot_p <- arrange(hot_p, grid_num) ## 再次确保按格子升序

    grid_p <- cbind(grid_p, hot_p=hot_p[,2])
    rm(hot_p); gc()
    grid_p <- arrange(grid_p, desc(p)) ## 按照p值降序

    if(sum(head(grid_p$p, 20)) == 0) {
      grid_p$p <- grid_p$hot_p
    } else if(length(unique(head(grid_p$p, 20))) < length(head(grid_p$p, 20))) {   
      grid_p$p <- grid_p$p + 0.001 + grid_p$hot_p * 0.1 ## 去0值，去重复值<这样之后也可能造成单独对p值的排序也有重复行，但格子顺序可以达到我们的需求>
      grid_p <- arrange(grid_p, desc(p), desc(hot_p))
    }
    grid_p$hot_p <- NULL

    ## 包括所有格子的概率估计
    rest_grid_p <- cbind(grid_num=grid_ord[which(!(grid_ord %in% grid_filt))], p=0)
    grid_p <- rbind.fill(grid_p, as.data.frame(rest_grid_p))
    rm(rest_grid_p); gc()
    grid_p <- cbind(date_str=tr, grid_p)
    grid_p <- arrange(grid_p, desc(p)) ## 按照p值降序

    grid_p$p[1:(sum(grid_p$p>0)+1)] <- rev(seq(min(grid_p$p), max(grid_p$p), length=sum(grid_p$p>0)+1))
    
    res_p <- rbind.fill(res_p,grid_p); rm(grid_p); gc()    
  }
  
  if(is.null(topN)) {rm(class_data); gc()} else {
    # 命中案件数
    tmp_data <- tail(class_data[,c(1:3)], length(grid_ord)*length(train_range))
    names(tmp_data)[3] <- 'layer0'
    new_data <- as.data.frame(merge(tmp_data, res_p, by=c("date_str", "grid_num")))
    rm(tmp_data); gc()
    new_data <- new_data[order(new_data$date_str, -new_data$p), ]
    myhead <- function(x) return(sum(head(x, topN)))
    case_top <- aggregate(new_data$layer0, list(date_str=new_data$date_str), myhead)
    sum_top <- sum(case_top[,2]) 
    # 日内总案件
    case_total <- aggregate(new_data$layer0, list(date_str=new_data$date_str), sum)
    sum_total <- sum(case_total[,2])     
    if(class_count==1) {
      write_content <- paste("无班次", " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
    } else {
      write_content <- paste("NoCombine班次 ", i, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
    }
    cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)            
  }
  res_p$p <- res_p$p <- 0.001 + ((res_p$p - min(res_p$p)) / (max(res_p$p) - min(res_p$p))) * (0.1 - 0.001)
  if(class_count > 1) {
    write.table(res_p, file=paste(result_path, "/pred_result/", "class_nocombine_", i, "_", pd_id, "_hmm.csv", sep=""), sep=",", row.names=F, col.names=F)
  } else {
    write.table(res_p, file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_hmm.csv", sep=""), sep=",", row.names=F, col.names=F)
  }
  rm(res_p); gc()
}
if(!is.null(topN)) {
    cat(paste("##########################以上是hmm：", min(case_total[,1]),"-", max(case_total[,1]), "################################\n", sep=""), file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
}
cat("\n",sep="\n")

