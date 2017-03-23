# Model name：tstat
# Date：2016/5/12
# Vsersion：2.0 （no POI数据，相对于1.0，做了部分细节的优化，修改该了1个bug）
# Details：基于时序推移的统计记忆模型
# Library：plyr

argv <- commandArgs(TRUE)

    # data_path <- argv[1]
    # result_path <- argv[2]
    # if(file.exists(result_path) == F) dir.create(result_path)
    # if(file.exists(paste(result_path, "/train_result", sep="")) == F) dir.create(paste(result_path, "/train_result", sep="")) 
    # pd_id <- as.numeric(argv[3])
    # start_date <- as.numeric(argv[4])
    # end_date <- as.numeric(argv[5])
    # sequence_range <- as.numeric(argv[6])
    # train_range <- as.numeric(as.Date(as.character(end_date), "%Y%m%d") - as.Date(as.character(start_date), "%Y%m%d")) +1

workdir <- argv[1]
result_path <- argv[1]
setwd(workdir)
if(!file.exists("train_result")) dir.create("train_result")
library(rjson)
config <- fromJSON(file="train_config.json")
data_path <- config$data_matrix
pd_id <- config$pd_id
start_date <- config$start_date
end_date <- config$end_date
sequence_range <- config$sequence_range
train_range <- as.numeric(as.Date(as.character(end_date), "%Y%m%d") - as.Date(as.character(start_date), "%Y%m%d")) +1


library(plyr)

move_forward <- function(in_dataframe, layer, seq_range) {
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
  
  if(layer == 0) judge_layer <- as.function(alist(in_data=, data_col="is_layer0", args_range=seq_range, judge_fun(in_data, data_col, args_range)))
  if(layer == 1) judge_layer <- as.function(alist(in_data=, data_col="is_layer0_1", args_range=seq_range, judge_fun(in_data, data_col, args_range)))
  if(layer == 2) judge_layer <- as.function(alist(in_data=, data_col="is_layer1_2", args_range=seq_range, judge_fun(in_data, data_col, args_range)))

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

train_rate <- function(in_dataframe, in_datalist, tr_range) {#in_dataframe是训练数据集，in_datalist是过滤推移后的列表数据
  ###取样本（训练）数据集，取最新的数据
  mytail <- as.function(alist(x=, n=tr_range, tail(x, n)))
  #要求 n < length(unique(train_data$date_str))-sequence_range
  in_dataframe <- ddply(in_dataframe, .(grid_num), mytail)

  # in_datalist <- lapply(in_datalist, function(x) {x <- x[names(x) != '1']; x}) ###关键：训练数据、隔天（去掉昨天）  
  for(i in names(in_datalist)){
    names(in_datalist[[i]]) <- length(in_datalist[[i]]):1  
  }#更改in_datalist[[i]]的names，相当于做倒序变换（最小值=1）
  in_datalist <- lapply(in_datalist, mytail)
   
  ###样本数据发生案情与否的行索引（样本数据同考察期）
  index1 <- dlply(in_dataframe, .(grid_num), function(x) which(x$layer0 >= 1))
  index0 <- dlply(in_dataframe, .(grid_num), function(x) which(x$layer0 == 0))
  tr_range <- length(unique(in_dataframe$date_str))
  rm(in_dataframe);gc(reset=T)

  ###考察期是否案发比例计算
  grid_yes_rate <- unlist(lapply(index1, length)) / tr_range  ###在unlist的作用下，考察期无案情的格子会被过滤掉
  ###下面的语句可以注掉，因为在后续的计算中会重新过滤掉考察期无案情的格子(*在in_dataframe数据生成的时候就已经被过滤掉了)
  grid_yes_rate[names(index1)[which(lapply(index1, length)==0)]] <- 0
    
  sequence_range <- max(unlist(in_datalist))

  ###考察期内当天有案情时:
  #list_gridn_seqr_layer_yes <- NULL
  list_gridn_seqr_layer_yes <- list()
  for(i in names(index1)) {
    list_gridn_seqr_layer_yes[[i]] <- in_datalist[[i]][which(1:tr_range %in% index1[[i]])]
    names(list_gridn_seqr_layer_yes[[i]]) <- which(1:tr_range %in% index1[[i]])
  }
  rm(index1);gc(reset=T)
  length_seq_yes <- unlist(lapply(list_gridn_seqr_layer_yes, function(x) return(length(x)-sum(x %in% 0))))
  ###以上函数返回值中0有两种情形：1.某格子在考察期全无案件：2.某格子在推移期无案件
  list_gridn_seqr_layer_yes <- list_gridn_seqr_layer_yes[which(lapply(list_gridn_seqr_layer_yes, length) != 0)]

  ###考察期内当天无案情时:
  list_gridn_seqr_layer_no <- list()
  for(i in names(index0)) {
    list_gridn_seqr_layer_no[[i]] <- in_datalist[[i]][which(1:tr_range %in% index0[[i]])]
    names(list_gridn_seqr_layer_no[[i]]) <- which(1:tr_range %in% index0[[i]])
  }
  rm(index0);gc(reset=T)
  length_seq_no <- unlist(lapply(list_gridn_seqr_layer_no, function(x) return(length(x)-sum(x %in% 0))))
  ###以上函数返回值中0有两种情形：1.某格子在考察期全有案件；2.某格子在推移期一直有案件
  list_gridn_seqr_layer_no <- list_gridn_seqr_layer_no[which(lapply(list_gridn_seqr_layer_no, length) != 0)]
 
  list_gridn_seqr_layer_yes_grid <- lapply(list_gridn_seqr_layer_yes, unlist)
  list_gridn_seqr_layer_yes_grid <- lapply(list_gridn_seqr_layer_yes_grid, table)
  rm(list_gridn_seqr_layer_yes);gc(reset=T)
  
  list_gridn_seqr_layer_no_grid <- lapply(list_gridn_seqr_layer_no, unlist)
  list_gridn_seqr_layer_no_grid <- lapply(list_gridn_seqr_layer_no_grid, table)
  rm(list_gridn_seqr_layer_no);gc(reset=T)
  
  rep_grid_name <- names(in_datalist)###取所有格子的names  
  rm(in_datalist);gc(reset=T)  
  
  seq_yes_p <- list() 
  ###在R中提前声明list可以保证后文引用中类型不变，而上文中的list_gridn_seqr_layer_yes直接声明为NULL是因为在后续调用时是以list来进行赋值运算的
  for(i in rep_grid_name) { ###以list_gridn_seqr_layer_yes_grid内的dateid为基准
    if((i %in% names(list_gridn_seqr_layer_yes_grid)) | (i %in% names(list_gridn_seqr_layer_no_grid))) {
	    same_index_yes <- which(names(list_gridn_seqr_layer_yes_grid[[i]]) %in% names(list_gridn_seqr_layer_no_grid[[i]]))
	    if(length(same_index_yes)==0){ ###yes和no两个数据list的内部list的names无交集 或 两个数据list的names无交集（即，至少有一个是空值）
	      seq_yes_p[[i]][names(list_gridn_seqr_layer_yes_grid[[i]])] <- 1 ###此处的极端情形（有两种）之一：某格子在考察期全有案情
	      seq_yes_p[[i]][names(list_gridn_seqr_layer_no_grid[[i]])] <- 0  ###此处的极端清晰（有两种）之一：某格子在考察期全无案情
	      ###*重要理解！
			} else {
	      same_index_no <- which(names(list_gridn_seqr_layer_no_grid[[i]]) %in% names(list_gridn_seqr_layer_yes_grid[[i]]))   
	      same_seq_yes <- list_gridn_seqr_layer_yes_grid[[i]][same_index_yes]
	      same_seq_no <- list_gridn_seqr_layer_no_grid[[i]][same_index_no]
	      seq_yes_p[[i]] <- same_seq_yes/(same_seq_yes+same_seq_no)
	      #加入=1的向量元素
	      seq_yes_p[[i]][names(list_gridn_seqr_layer_yes_grid[[i]])[-same_index_yes]] <- 1  ###经实验在极端可能报错情况下，实际上不会报错
	      #加入=0的向量元素
	      seq_yes_p[[i]][names(list_gridn_seqr_layer_no_grid[[i]])[-same_index_no]] <- 0
	    }
	  } else seq_yes_p[[i]][as.character(seq(1:sequence_range))] <- 0  ###表示某个格子在观察期一直无案情，这种情况对于本格子（外层就不一定了)已经不会出现了,已被过滤掉(如果过滤条件仅仅对于本格子的话--这样才显得合理）
  }
  rm(list_gridn_seqr_layer_yes_grid, list_gridn_seqr_layer_no_grid);gc(reset=T)
  
  ###强行填充 
  seq_yes_p <- lapply(seq_yes_p, function(x){x[as.character(seq(0, sequence_range))[which(!(as.character(seq(0, sequence_range)) %in% names(x)))]] <- 0; x})
  
  ###length_seq_yes与length_seq_no的names是同样长、同序的
  len_seq_yes_rate <- ifelse((length_seq_yes + length_seq_no) == 0, 0, length_seq_yes / (length_seq_yes+length_seq_no))
  ###按照逻辑：前面生成数据集的过滤条件，ifelse中的0（0表示判别（注意此处的判别环境：某格子无论在考察期是否有案情，但在推移期无案情）为yes的可能性,而不取0.5）不会出现  
  seq_yes_p <- seq_yes_p[order(as.numeric(names(seq_yes_p)))]
  seq_yes_p <- lapply(seq_yes_p, function(x) x[order(as.numeric(names(x)))])
  len_seq_yes_rate <- len_seq_yes_rate[order(as.numeric(names(len_seq_yes_rate)))]
  grid_yes_rate <- grid_yes_rate[order(as.numeric(names(grid_yes_rate)))]
  
  ####seq_yes_p,len_seq_yes_rate,grid_yes_rate的names必须一样
  
  seq_p <- list()
  for(i in names(seq_yes_p)) seq_p[[i]] <- seq_yes_p[[i]] * (len_seq_yes_rate*grid_yes_rate)[i]
  
  return(seq_p) 
}

###只扩展到外一层
###获得数据:最多8列，最少4列，列的顺序固定，无表头，无trans（即无0-1转换）
###date_str,grid_num,crime_num(班次1),当天外一层(班次1),crime_num(班次2),当天外一层(班次2）,crime_num(班次3）,当天外一层(班次3)

train_data <- read.csv(data_path,header=F)
logic_class <- (ncol(train_data)-2)/2
start_date  <- as.numeric(format(as.Date(as.character(start_date), "%Y%m%d") - sequence_range, "%Y%m%d"))
train_data <- train_data[which(train_data[,1] >= start_date & train_data[,1] < end_date),]

if(logic_class == 1) {
  names(train_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  train_data$is_layer0 <- ifelse(train_data$layer0 >= 1, 1, 0)
  train_data$is_layer0_1 <- ifelse(train_data$is_layer0_1 >= 1, 1, 0)
}

if(logic_class == 2) {
  tmp2 <- train_data[, c(1:2,5:6)]
  names(tmp2) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp2$class_id <- 2
  train_data <- train_data[, c(1:4)]; names(train_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  train_data$class_id <- 1
  train_data <- rbind(train_data, tmp2); rm(tmp2); gc(reset=T)
  train_data$is_layer0 <- ifelse(train_data$layer0 >= 1, 1, 0)
  train_data$is_layer0_1 <- ifelse(train_data$is_layer0_1 >= 1, 1, 0)
  train_data <- ddply(train_data, .(grid_num), function(x) {x <- subset(x, select=-grid_num); x <- arrange(x, date_str, class_id); x$date_id <- 1:nrow(x); return(x)})
}

if(logic_class == 3) {
  tmp2 <- train_data[, c(1:2,5:6)]
  names(tmp2) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp2$class_id <- 2
  tmp3 <- train_data[, c(1:2,7:8)]
  names(tmp3) <- c("date_str", "grid_num", "layer0", "is_layer0_1"); tmp3$class_id <- 3    
  train_data <- train_data[, c(1:4)]; names(train_data) <- c("date_str", "grid_num", "layer0", "is_layer0_1")
  train_data$class_id <- 1
  train_data <- rbind(train_data, tmp2, tmp3); rm(tmp2, tmp3); gc(reset=T)
  train_data$is_layer0 <- ifelse(train_data$layer0 >= 1, 1, 0)
  train_data$is_layer0_1 <- ifelse(train_data$is_layer0_1 >= 1, 1, 0)
  train_data <- ddply(train_data, .(grid_num), function(x) {x <- subset(x, select=-grid_num); x <- arrange(x, date_str, class_id); x$date_id <- 1:nrow(x); return(x)})
}

call_train_fun <- function(in_dataframe, k) {
  ###按条件过滤格子--可不进行过滤：暂不过滤，因为担心有出格子策略的影响（某些小片区的格子全部被过滤掉了）
  #train_data <- filter_grid(in_data=train_data, k * interval_days, k * near_days)
  
  ###按时间推移计算----注：move_forward后的list的元素的内部长度可能大于起始日期的差值，但是在训练数据集中可以忽略这个问题
  train_list_gridn_seqr_layer0 <- move_forward(in_dataframe, layer=0, k * sequence_range)
  train_list_gridn_seqr_layer1 <- move_forward(in_dataframe, layer=1, k * sequence_range)
  
  ###计算比率值
  rate_layer0 <- train_rate(in_dataframe, in_datalist=train_list_gridn_seqr_layer0, k * train_range)
  rate_layer1 <- train_rate(in_dataframe, in_datalist=train_list_gridn_seqr_layer1, k * train_range)

  rm(train_list_gridn_seqr_layer0, train_list_gridn_seqr_layer1);gc(reset=T)
  ###将list转化为data.frame以便写出到文件（因为此处list的每个元素是向量，故可以直接转化为data.frame）
  rate_layer0_names <- names(rate_layer0)
  rate_layer0 <- as.data.frame(matrix(unlist(rate_layer0), ncol=length(rate_layer0))) 
  names(rate_layer0) <- rate_layer0_names

  rate_layer1_names <- names(rate_layer1)
  rate_layer1 <- as.data.frame(matrix(unlist(rate_layer1), ncol=length(rate_layer1))) 
  names(rate_layer1) <- rate_layer0_names
  
  return(list(rate_layer0=rate_layer0, rate_layer1=rate_layer1))
}

result <- list()
if(logic_class > 1) {###有班次
  ###不合并班次的情况：
  for(i in unique(train_data$class_id)) {
    temp <- call_train_fun(train_data[which(train_data$class_id == i),], 1)
    result[[paste("class_nocombine_", i, sep="")]] <- temp
    # rate_layer0 <- temp$rate_layer0
    # rate_layer1 <- temp$rate_layer1
    # rm(temp);gc(reset=T)
    # ###将计算出的比率集合数据写入到文件
    # write.csv(rate_layer0, file=paste(result_path, "/train_result/", pd_id, "_class_", i, "_rate_layer0.csv", sep=""), row.names=F)  ###注明详细的文件名
    # write.csv(rate_layer1, file=paste(result_path, "/train_result/", pd_id, "_class_", i, "_rate_layer1.csv", sep=""), row.names=F)
    # #在此处补充其他层或影响因素的比率集合
  }
  
  ###合并班次的情况：
  train_data <- arrange(train_data, grid_num, date_id)
  temp <- call_train_fun(train_data, logic_class)  ###班次合并后推移期限是单个班次的logic_class倍
  result[["class_combine"]] <- temp
  # rate_layer0 <- temp$rate_layer0
  # rate_layer1 <- temp$rate_layer1
  # rm(temp);gc(reset=T)
  # ##将计算出的比率集合数据写入到文件
  # write.csv(rate_layer0, file=paste(result_path, "/train_result/", pd_id, "_combine_rate_layer0.csv", sep=""), row.names=F)  ###注明详细的文件名
  # write.csv(rate_layer1, file=paste(result_path, "/train_result/", pd_id, "_combine_rate_layer1.csv", sep=""), row.names=F)
  # #在此处补充其他层或影响因素的比率集合 
} else {###无班次
  temp <- call_train_fun(train_data, 1)
  result[["noclass"]] <- temp
  # rate_layer0 <- temp$rate_layer0
  # rate_layer1 <- temp$rate_layer1
  # rm(temp);gc(reset=T)
  # ###将计算出的比率集合数据写入到文件
  # write.csv(rate_layer0, file=paste(result_path, "/train_result/", pd_id, "_noclass_rate_layer0.csv", sep=""), row.names=F)  ###注明详细的文件名
  # write.csv(rate_layer1, file=paste(result_path, "/train_result/", pd_id, "_noclass_rate_layer1.csv", sep=""), row.names=F)
  # #在此处补充其他层或影响因素的比率集合
}
save(result, file=paste(result_path, "/train_result/", pd_id, "_tstat.RData", sep=""))

