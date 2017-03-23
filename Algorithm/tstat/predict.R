# Model name：tstat
# Date：2016/5/12
# Vsersion：2.0 （no POI数据，相对于1.0，做了部分细节的优化，修改该了1个bug）
# Details：基于时序推移的统计记忆模型
# Library：plyr

argv <- commandArgs(TRUE)

    # data_path <- argv[1]
    # result_path <- argv[2]
    # if(file.exists(result_path) == F) dir.create(result_path)
    # if(file.exists(paste(result_path, "/pred_result", sep="")) == F) dir.create(paste(result_path, "/pred_result", sep=""))
    # if(file.exists(paste(result_path, "/train_result", sep="")) == F) stop("NO file path: ~/train_result")
    # pd_id <- as.numeric(argv[3])
    # start_date <- as.numeric(argv[4])
    # end_date <- as.numeric(argv[5])
    # if(argv[6] == "TRUE") combineOrnot <- TRUE 
    # if(argv[6] == "FALSE") combineOrnot <- FALSE
workdir <- argv[1]
result_path <- argv[1]
setwd(workdir)
if(!file.exists("pred_result")) dir.create("pred_result")
library(rjson)
config <- fromJSON(file="predict_config.json")
data_path <- config$data_matrix
pd_id <- config$pd_id
start_date <- config$start_date
end_date <- config$end_date
combineOrnot <- config$combine
if(combineOrnot == "TRUE") combineOrnot <- TRUE
if(combineOrnot == "FALSE") combineOrnot <- FALSE  

  
library(plyr)

move_forward <- function(in_dataframe, layer, sequence_range) {
  ####判断每sequence_range天里有警情的记录对应的sequence_range中的序号
  judge_fun <- function(in_data, data_col, args_range) {
    item_index <- list()
	  for(i in length(in_data[, data_col]):(args_range + 1)){###(i-1):(i-args_range)的意义
	    if(length(which(in_data[, data_col][(i - 1):(i - args_range)] >= 1)) == 0) item_index[[i]] <- 0 #NA
	    else item_index[[i]] <- which(in_data[, data_col][(i - 1):(i - args_range)] >= 1) 
	    ###此处用ifelse会出问题
	  }###0表示sequence_range内无案件发生
	  return(item_index)
  } 
  
  if(layer == 0) judge_layer <- as.function(alist(in_data=, data_col="is_layer0", args_range=sequence_range, judge_fun(in_data, data_col, args_range)))
  if(layer == 1) judge_layer <- as.function(alist(in_data=, data_col="is_layer0_1", args_range=sequence_range, judge_fun(in_data, data_col, args_range)))
  if(layer == 2) judge_layer <- as.function(alist(in_data=, data_col="is_layer1_2", args_range=sequence_range, judge_fun(in_data, data_col, args_range)))

  ###in_dataframe是在grid_num分组下按照date_str或date_id升序的
  list_gridn_seqr_layer <- dlply(in_dataframe, .(grid_num), judge_layer)
  
  for(i in 1:length(list_gridn_seqr_layer)) {###length(list_gridn_seqr_layer0)等于length(grid_num)即格子的个数
    list_gridn_seqr_layer[[i]] <- list_gridn_seqr_layer[[i]][which(lapply(list_gridn_seqr_layer[[i]], is.null) != 1)]
  }#去掉空值记录，相当于只取出考察期的数据
  
  for(i in names(list_gridn_seqr_layer)) {
    names(list_gridn_seqr_layer[[i]]) <- length(list_gridn_seqr_layer[[i]]):1
  }#更改list_gridn_seqr_layer[[i]]的names，相当于做倒序变换(最小值=1)
  
  return(list_gridn_seqr_layer)
}

predict_grid_layer_fun <- function(in_dataframe, in_datalist) {
  predict_grid_layer_p <- list()
  for(i in names(in_datalist)){
    predict_grid_layer_p[[i]] <- lapply(in_datalist[[i]], function(x) sum(in_dataframe[which(as.numeric(rownames(in_dataframe)) %in% x), names(in_datalist[i])]))
  }
  return(predict_grid_layer_p)
} 

predict_layer_p_fun <- function(in_dataframe, layer, in_datalist, n_days) {
  in_dataframe[, paste("p", layer, sep="")] <- NA
  for(i in as.numeric(names(in_datalist))){
    in_dataframe[which(in_dataframe$grid_num == i),paste("p", layer, sep="")][length(which(in_dataframe$grid_num == i)):(length(which(in_dataframe$grid_num == i)) - n_days + 1)] <- rev(unlist(in_datalist[[as.character(i)]]))
  } 
  return(in_dataframe)
}

predict_data <- read.csv(data_path,header=F)
logic_class <- (ncol(predict_data)-2)/2
n_days <- as.numeric(as.Date(as.character(end_date), "%Y%m%d") - as.Date(as.character(start_date), "%Y%m%d"))
  
if(logic_class == 1) {
  names(predict_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  predict_data$is_layer0 <- ifelse(predict_data$layer0 >= 1, 1, 0)
  predict_data$is_layer0_1 <- ifelse(predict_data$is_layer0_1 >= 1, 1, 0)
}

if(logic_class == 2) {
  tmp2 <- predict_data[, c(1:2,5:6)]
  names(tmp2) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp2$class_id <- 2
  predict_data <- predict_data[, c(1:4)]; names(predict_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  predict_data$class_id <- 1
  predict_data <- rbind(predict_data, tmp2); rm(tmp2); gc(reset=T)
  predict_data$is_layer0 <- ifelse(predict_data$layer0 >= 1, 1, 0)
  predict_data$is_layer0_1 <- ifelse(predict_data$is_layer0_1 >= 1, 1, 0)
  predict_data <- ddply(predict_data, .(grid_num), function(x) {x <- subset(x, select=-grid_num); x <- arrange(x, date_str, class_id); x$date_id <- 1:nrow(x); return(x)})
}

if(logic_class == 3) {
  tmp2 <- predict_data[, c(1:2,5:6)]
  names(tmp2) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp2$class_id <- 2
  tmp3 <- predict_data[, c(1:2,7:8)]
  names(tmp3) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp3$class_id <- 3    
  predict_data <- predict_data[, c(1:4)]; names(predict_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  predict_data$class_id <- 1
  predict_data <- rbind(predict_data, tmp2, tmp3); rm(tmp2, tmp3); gc(reset=T)
  predict_data$is_layer0 <- ifelse(predict_data$layer0 >= 1, 1, 0)
  predict_data$is_layer0_1 <- ifelse(predict_data$is_layer0_1 >= 1, 1, 0)
  predict_data <- ddply(predict_data, .(grid_num), function(x) {x <- subset(x, select=-grid_num); x <- arrange(x, date_str, class_id); x$date_id <- 1:nrow(x); return(x)})
}

###加载训练结果
load(paste(result_path, "/train_result/", pd_id, "_tstat.RData", sep=""))

# ###检查文件目录
# files_name <- list.files(paste(result_path, "/train_result/", sep=""))
# file_index <- grep(paste("^", pd_id, "_", sep=""), files_name)
# if(length(file_index) == 0) stop("No train data!")
# files_name <- files_name[file_index]

# ###无班次
# file_index <- grep(paste("^", pd_id, "_noclass_rate_layer\\d.csv", sep=""), files_name)

# if(length(file_index) != 0) {###该派出所无班次
if("noclass" %in% names(result)) {###该派出所无班次
  rate_layer0 <- result[["noclass"]]$rate_layer0
  rate_layer1 <- result[["noclass"]]$rate_layer1
  # rate_layer0 <- read.csv(paste(result_path, "/train_result/",pd_id,"_noclass_rate_layer0.csv",sep=""))
  # names(rate_layer0) <- substr(names(rate_layer0),2,length(names(rate_layer0)))
  # rownames(rate_layer0) <- seq(0,max(as.numeric(row.names(rate_layer0)))-1)
  
  # rate_layer1 <- read.csv(paste(result_path, "/train_result/",pd_id,"_noclass_rate_layer1.csv",sep=""))
  # names(rate_layer1) <- substr(names(rate_layer1),2,length(names(rate_layer1)))
  # rownames(rate_layer1) <- seq(0,max(as.numeric(row.names(rate_layer1)))-1)
  
  sequence_range <- nrow(rate_layer0)-1
  start_date  <- as.numeric(format(as.Date(as.character(start_date), "%Y%m%d") - sequence_range, "%Y%m%d"))
  predict_data <- predict_data[which(predict_data$date_str >= start_date & predict_data$date_str < end_date),]
  
  ###当rate_layer0是由过滤格子后计算得来时以下语句起作用——*在train_result中被强制注掉
  #predict_data_filter <- predict_data[which(predict_data$grid_num %in% as.numeric(names(rate_layer0))),]   
  
  ###按时间推移计算
  predict_list_grid_n_layer0 <- move_forward(in_dataframe=predict_data, layer=0, sequence_range)
  predict_list_grid_n_layer1 <- move_forward(in_dataframe=predict_data, layer=1, sequence_range)

  ###抠掉昨天
  # predict_list_grid_n_layer0 <- lapply(predict_list_grid_n_layer0, function(x) {x <- x[names(x) != '1']; x})
  # predict_list_grid_n_layer1 <- lapply(predict_list_grid_n_layer1, function(x) {x <- x[names(x) != '1']; x})
  
  ###预测数据比率计算
  predict_grid_layer0_p <- predict_grid_layer_fun(rate_layer0, predict_list_grid_n_layer0)
  predict_grid_layer1_p <- predict_grid_layer_fun(rate_layer1, predict_list_grid_n_layer1)

  ###预测数据概率表示
  predict_data <- predict_layer_p_fun(predict_data, 0, predict_grid_layer0_p, n_days)
  predict_data <- predict_layer_p_fun(predict_data, 1, predict_grid_layer1_p, n_days)

  ###组合p0和p1为p,根据p0与p1的计算缘由（度量方式,即同度[量]的）,可以直接相加：p=p0+p1
  predict_data$p <- predict_data$p0 + predict_data$p1
  predict_data <- predict_data[which(!is.na(predict_data$p)),]  ###相当于减掉被推移的数据，即date_str的前sequence_range行的所有格子的数据行

  predict_data <- predict_data[, c("date_str", "grid_num", "p")]
  predict_data <- predict_data[order(predict_data$date_str, -predict_data$p),]
  predict_data$p <- 0.001 + ((predict_data$p - min(predict_data$p)) / (max(predict_data$p) - min(predict_data$p))) * (0.1 - 0.001)
  write.table(predict_data, file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_tstat.csv", sep=""), sep=",", row.names=F, col.names=F)
}

# if(combineOrnot == TRUE) {#目的：训练数据同时保存了班次合并与不合并的比率矩阵，在每日出格子时可自由选择
if(combineOrnot == TRUE & "class_combine" %in% names(result)) {
  ###有班次有合并
  rate_layer0 <- result[["class_combine"]]$rate_layer0
  rate_layer1 <- result[["class_combine"]]$rate_layer1 
  # file_index <- grep(paste("^", pd_id, "_combine_rate_layer\\d.csv", sep=""), files_name)
  # if(length(file_index) != 0) {###该派出所有班次且合并了班次
    class_count <- logic_class
    # class_count <- as.numeric(max(substr(files_name,as.numeric(gregexpr("_\\d_",files_name))+1,as.numeric(gregexpr("_\\d_",files_name))+1)))  ###这条语句发生作用的前提是必须有单独班次的训练数据比率矩阵产生  ************************
  
    # rate_layer0 <- read.csv(paste(result_path, "/train_result/",pd_id,"_combine_rate_layer0.csv",sep=""))
    # names(rate_layer0) <- substr(names(rate_layer0),2,length(names(rate_layer0)))
    # rownames(rate_layer0) <- seq(0,max(as.numeric(row.names(rate_layer0)))-1)
   
    # rate_layer1 <- read.csv(paste(result_path, "/train_result/",pd_id,"_combine_rate_layer1.csv",sep=""))
    # names(rate_layer1) <- substr(names(rate_layer1),2,length(names(rate_layer1)))
    # rownames(rate_layer1) <- seq(0,max(as.numeric(row.names(rate_layer1)))-1)
    
    sequence_range <- nrow(rate_layer0)-1  ###这个值与单个班次或无班次的sequence_range值不一样
    start_date  <- as.numeric(format(as.Date(as.character(start_date), "%Y%m%d") - sequence_range/class_count, "%Y%m%d"))
    predict_data <- predict_data[which(predict_data$date_str >= start_date & predict_data$date_str < end_date),]
  
    ###当rate_layer0是由过滤格子后计算得来时以下语句起作用——*在train_result中被强制注掉
    #predict_data_filter <- predict_data[which(predict_data$grid_num %in% as.numeric(names(rate_layer0))),]  

    ###按时间推移计算
    predict_list_grid_n_layer0 <- move_forward(in_dataframe=predict_data, layer=0, sequence_range)
    predict_list_grid_n_layer1 <- move_forward(in_dataframe=predict_data, layer=1, sequence_range)

    ###抠掉昨天
    # predict_list_grid_n_layer0 <- lapply(predict_list_grid_n_layer0, function(x) {x <- x[names(x) != '1']; x})
    # predict_list_grid_n_layer1 <- lapply(predict_list_grid_n_layer1, function(x) {x <- x[names(x) != '1']; x})
    
    ###预测数据比率计算
    predict_grid_layer0_p <- predict_grid_layer_fun(rate_layer0, predict_list_grid_n_layer0)
    predict_grid_layer1_p <- predict_grid_layer_fun(rate_layer1, predict_list_grid_n_layer1)
    
    ###预测数据概率表示
    predict_data <- predict_layer_p_fun(predict_data, 0, predict_grid_layer0_p, class_count * n_days)
    predict_data <- predict_layer_p_fun(predict_data, 1, predict_grid_layer1_p, class_count * n_days)

    ###组合p0和p1为p,根据p0与p1的计算缘由（度量方式,即同度[量]的）,可以直接相加：p=p0+p1
    predict_data$p <- predict_data$p0 + predict_data$p1
    predict_data <- predict_data[which(!is.na(predict_data$p)),]  ###相当于减掉被推移的数据，即date_id的前sequence_range行的所有格子的数据行
	
	predict_data <- predict_data[order(predict_data$class_id, predict_data$date_str, -predict_data$p),]
    for(i in 1:class_count) {
      predict_data[which(predict_data$class_id == i), "p"] <- 0.001 + ((predict_data[which(predict_data$class_id == i), "p"] - min(predict_data[which(predict_data$class_id == i), "p"])) / 
        (max(predict_data[which(predict_data$class_id == i), "p"]) - min(predict_data[which(predict_data$class_id == i), "p"]))) * (0.1 - 0.001)
      write.table(predict_data[which(predict_data$class_id == i),c("date_str", "grid_num", "p")], file=paste(result_path, "/pred_result/", "class_combine_", i, "_", pd_id, "_tstat.csv", sep=""), sep=",", row.names=F, col.names=F)
    }
  # }
}
if(combineOrnot == FALSE & length(grep("class_nocombine_", names(result)))>0) {  
# } else { 
  ###有班次无合并
  # file_index <- grep(paste("^", pd_id, "_class_\\d_rate_layer\\d.csv", sep=""), files_name)
  # if(length(file_index) != 0) {######该派出所有班次，分班次单独计算
    # class_count <- as.numeric(max(substr(files_name,as.numeric(gregexpr("_\\d_",files_name))+1,as.numeric(gregexpr("_\\d_",files_name))+1)))  ###这条语句发生作用的前提是必须有单独班次的训练数据比率矩阵产生  ************************
    class_count <- length(grep("class_nocombine_", names(result)))
    for(i in 1:class_count) {
      rate_layer0 <- result[[paste("class_nocombine_", i, sep="")]]$rate_layer0
      rate_layer1 <- result[[paste("class_nocombine_", i, sep="")]]$rate_layer1
      # rate_layer0 <- read.csv(paste(result_path, "/train_result/",pd_id,"_","class_",i,"_rate_layer0.csv",sep=""))
      # names(rate_layer0) <- substr(names(rate_layer0),2,length(names(rate_layer0)))
      # rownames(rate_layer0) <- seq(0,max(as.numeric(row.names(rate_layer0)))-1)

      # rate_layer1 <- read.csv(paste(result_path, "/train_result/",pd_id,"_","class_",i,"_rate_layer1.csv",sep=""))
      # names(rate_layer1) <- substr(names(rate_layer1),2,length(names(rate_layer1)))
      # rownames(rate_layer1) <- seq(0,max(as.numeric(row.names(rate_layer1)))-1)
      
      sequence_range <- nrow(rate_layer0)-1  ###这个值与单个班次或无班次的sequence_range值不一样
      start_date  <- as.numeric(format(as.Date(as.character(start_date), "%Y%m%d") - sequence_range, "%Y%m%d"))
      predict_data <- predict_data[which(predict_data$date_str >= start_date & predict_data$date_str < end_date),]
      class_data <- predict_data[which(predict_data$class_id == i),]
  
      ###当rate_layer0是由过滤格子后计算得来时以下语句起作用——*在train_result中被强制注掉
      #predict_data_filter <- class_data[which(class_data$grid_num %in% as.numeric(names(rate_layer0))),]  

      ###按时间推移计算
      predict_list_grid_n_layer0 <- move_forward(in_dataframe=class_data, layer=0, sequence_range)
      predict_list_grid_n_layer1 <- move_forward(in_dataframe=class_data, layer=1, sequence_range)

      ###抠掉昨天
      # predict_list_grid_n_layer0 <- lapply(predict_list_grid_n_layer0, function(x) {x <- x[names(x) != '1']; x})
      # predict_list_grid_n_layer1 <- lapply(predict_list_grid_n_layer1, function(x) {x <- x[names(x) != '1']; x})
      
      ###预测数据比率计算
      predict_grid_layer0_p <- predict_grid_layer_fun(rate_layer0, predict_list_grid_n_layer0)
      predict_grid_layer1_p <- predict_grid_layer_fun(rate_layer1, predict_list_grid_n_layer1)

      ###预测数据概率表示
      class_data <- predict_layer_p_fun(class_data, 0, predict_grid_layer0_p, n_days)
      class_data <- predict_layer_p_fun(class_data, 1, predict_grid_layer1_p, n_days)

      ###组合p0和p1为p,根据p0与p1的计算缘由（度量方式,即同度[量]的）,可以直接相加：p=p0+p1
      class_data$p <- class_data$p0 + class_data$p1
      class_data <- class_data[which(!is.na(class_data$p)),]  ###相当于减掉被推移的数据，即date_id的前sequence_range行的所有格子的数据行
	  class_data <- class_data[order(class_data$date_str, -class_data$p),]
	  class_data$p <- 0.001 + ((class_data$p - min(class_data$p)) / (max(class_data$p) - min(class_data$p))) * (0.1 - 0.001)
      write.table(class_data[, c("date_str", "grid_num", "p")], file=paste(result_path, "/pred_result/", "class_nocombine_", i, "_", pd_id, "_tstat.csv", sep=""), sep=",", row.names=F, col.names=F)
    }
  # }
}  
 


