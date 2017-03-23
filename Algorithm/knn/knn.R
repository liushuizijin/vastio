# Model name：knn
# Vsersion：2.0 
# Details：按时序强度衰减的最近邻距离算法
# Library：kknn

# --------------------
# 读入参数
# --------------------

argv <- commandArgs(TRUE)
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
sample_range <- config$col_nums
iter_range <- config$row_nums

topN <- 10

pred_range <- as.numeric(format(seq(as.Date(as.character(start_date), "%Y%m%d"), as.Date(as.character(end_date), "%Y%m%d")-1, by=1), "%Y%m%d"))

library(kknn)

# --------------------
# 读入数据
# --------------------

crime_data <- read.csv(data_path, header=F)

# --------------------
# 数据转换
# --------------------

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

grid_ord <- sort(unique(crime_data$grid_num))
all_data <- data.frame(date_str=rep(pred_range, each=length(grid_ord)),grid_num=rep(grid_ord, length(pred_range)))

for(cls in 1:class_count) { 
	pred_result <- NULL
	for(pred_date in pred_range) {
		model_data <- NULL
		for(i in iter_range:1) {
			down_date <- as.numeric(format(as.Date(as.character(pred_date), "%Y%m%d") - sample_range - i, "%Y%m%d"))
			up_date <- as.numeric(format(as.Date(as.character(pred_date), "%Y%m%d") - i, "%Y%m%d"))
			class_data <- crime_data[which(crime_data$date_str >= down_date & crime_data$date_str <= up_date), c(1:2, 1+cls*2, 2+cls*2)]
			## 列名
			names(class_data) <- c("date_str", "grid_num", "layer0", "layer1")
			## 按日期、格子排序（升序）
			class_data <- class_data[order(class_data$date_str, class_data$grid_num), ]
			
			## 整理成类似微阵列数据
			lt <- split(subset(class_data, select = -c(date_str, grid_num)), class_data["date_str"])
			rm(class_data);gc()
			lt <- matrix(unlist(lt), ncol = ncol(lt[[1]]) * length(lt),
					dimnames = list(NULL, paste(names(lt[[1]]), rep(paste("_",sample_range:0,sep=""), each=ncol(lt[[1]])), sep="")))
			
			## 格子（行记录）过滤：如果入样时间段内本格子持续无案情则被剔除
			filt_index <- which(rowSums(lt[, paste("layer0_", sample_range:1, sep="")]) >= 1)
			lt <- lt[filt_index,] ## 保留下来的格子
			grid_filt <- grid_ord[filt_index]
			lt <- cbind(date_str=up_date, grid_num=grid_filt, lt)
			
			model_data <- rbind(model_data, lt)
			rm(lt, grid_filt, filt_index); gc()
		}
		
		train_data <- subset(model_data, select=-layer1_0)
		test_data <- model_data[which(model_data[,1] == max(model_data[,1])), c("date_str", "grid_num", paste("layer0_", 0:(sample_range-1), sep=""), paste("layer1_", 0:(sample_range-1), sep=""))]
		# rm(model_data); gc()
		test_data[, "date_str"] <- pred_date
		colnames(test_data) <- c("date_str", "grid_num", paste("layer0_", 1:sample_range, sep=""), paste("layer1_", 1:sample_range, sep=""))
		
		## layer0与layer1交互
		train_interact <- train_data[,paste("layer0_", sample_range:1, sep="")] * train_data[,paste("layer1_", sample_range:1, sep="")]
		colnames(train_interact) <- paste(paste("layer01_", sample_range:1, sep=""))
		train_interact <- as.data.frame(cbind(layer0_0=train_data[,"layer0_0"], train_interact))
		
		test_interact <- test_data[,paste("layer0_", sample_range:1, sep="")] * test_data[,paste("layer1_", sample_range:1, sep="")]
		colnames(test_interact) <- paste(paste("layer01_", sample_range:1, sep=""))
		test_interact <- as.data.frame(test_interact)
		
		## knn_interact <- kknn(layer0_0 ~ ., train_interact, test_interact)
		
		train_data <- cbind(train_data, train_interact[, -1])
		test_data <- cbind(test_data, test_interact)
		rm(train_interact, test_interact); gc()
		
        ## 计算热点概率
        hot_p <- rowSums(test_data[,paste("layer0_", sample_range:1, sep="")]) / sample_range
       
		## 时间跨度累加（交互项不做时序累加，不符合场景意义）
		layer0 <- t(apply(train_data[,paste("layer0_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
		layer1 <- t(apply(train_data[,paste("layer1_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
		# layer01 <- t(apply(train_data[,paste("layer01_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
		train_data_cumsum <- as.data.frame(cbind(train_data[,c("date_str", "grid_num", "layer0_0")], layer0, layer1))
		
		layer0 <- t(apply(test_data[,paste("layer0_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
		layer1 <- t(apply(test_data[,paste("layer1_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))       
        # layer01 <- t(apply(test_data[,paste("layer01_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))        
		test_data_cumsum <- as.data.frame(cbind(test_data[,c("date_str", "grid_num")], layer0, layer1))        
		rm(layer0, layer1); gc()
		
		knn_cumsum <- kknn(layer0_0 ~ ., subset(train_data_cumsum, select=-c(date_str, grid_num)), subset(test_data_cumsum, select=-c(date_str, grid_num)),
			k=4, kernel = "gaussian", scale=FALSE)
		
		## 时序强度衰减（交互项不做时序衰减，不符合场景意义，只当作调整/修正）
		train_data[, c(paste("layer0_", 1:sample_range, sep=""), paste("layer1_", 1:sample_range, sep=""))] <- t(t(train_data[, c(paste("layer0_", 1:sample_range, sep=""), 
										paste("layer1_", 1:sample_range, sep=""))]) * c(sample_range:1, sample_range:1))
		test_data[, c(paste("layer0_", 1:sample_range, sep=""), paste("layer1_", 1:sample_range, sep=""))] <- t(t(test_data[, c(paste("layer0_", 1:sample_range, sep=""), 
										paste("layer1_", 1:sample_range, sep=""))]) * c(sample_range:1, sample_range:1))
		
	    knn_decay <- kknn(layer0_0 ~ ., subset(train_data, select=-c(date_str, grid_num)), subset(test_data, select=-c(date_str, grid_num)),
			k=4, kernel = "gaussian", scale=FALSE)
		
		## 先累加再衰减
	    train_data_cumsum[, c(paste("layer0_", 1:sample_range, sep=""), paste("layer1_", 1:sample_range, sep=""))] <- t(t(train_data_cumsum[, c(paste("layer0_", 1:sample_range, sep=""), 
	        paste("layer1_", 1:sample_range, sep=""))]) * c(sample_range:1, sample_range:1))
	    test_data_cumsum[, c(paste("layer0_", 1:sample_range, sep=""), paste("layer1_", 1:sample_range, sep=""))] <- t(t(test_data_cumsum[, c(paste("layer0_", 1:sample_range, sep=""), 
	        paste("layer1_", 1:sample_range, sep=""))]) * c(sample_range:1, sample_range:1))        
	    
	    knn_cumsum_decay <- kknn(layer0_0 ~ ., subset(train_data_cumsum, select=-c(date_str, grid_num)), subset(test_data_cumsum, select=-c(date_str, grid_num)), 
			k=4, kernel = "gaussian", scale=FALSE)
		rm(train_data_cumsum, test_data_cumsum); gc()
		
		## 先衰减再累加
	    layer0 <- t(apply(train_data[,paste("layer0_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
	    layer1 <- t(apply(train_data[,paste("layer1_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
	    # layer01 <- t(apply(train_data[,paste("layer01_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
	    train_data <- as.data.frame(cbind(train_data[,c("date_str", "grid_num", "layer0_0")], layer0, layer1))
	    
	    layer0 <- t(apply(test_data[,paste("layer0_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
	    layer1 <- t(apply(test_data[,paste("layer1_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))
	    # layer01 <- t(apply(test_data[,paste("layer01_", sample_range:1, sep="")],1,function(x) rev(cumsum(rev(x)))))        
	    test_data <- as.data.frame(cbind(test_data[,c("date_str", "grid_num")], layer0, layer1))        
	    rm(layer0, layer1, layer01); gc()
	    
	    knn_decay_cumsum <- kknn(layer0_0 ~ ., subset(train_data, select=-c(date_str, grid_num)), subset(test_data, select=-c(date_str, grid_num)),
			k=4, kernel = "gaussian", scale=FALSE)        
        # knn_cumsum$fit+knn_decay$fit+knn_cumsum_decay$fit+knn_decay_cumsum$fit+
		fit_knn <- data.frame(test_data[, 1:2], p=hot_p)
		fit_knn$p <- 0.001 + ((fit_knn$p - min(fit_knn$p)) / (max(fit_knn$p) - min(fit_knn$p))) * (0.1 - 0.001)      
		rm(train_data, test_data); gc()
		pred_result <- rbind(pred_result, fit_knn)
	}
	pred_result <- merge(pred_result, all_data, by=c("date_str", "grid_num"), all=T)
	pred_result[is.na(pred_result)] <- 0
	
	if(class_count > 1) {
		write.table(pred_result, file=paste(result_path, "/pred_result/", "class_nocombine_", cls, "_", pd_id, "_knn.csv", sep=""), sep=",", row.names=F, col.names=F)		
	} else {
		write.table(pred_result, file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_knn.csv", sep=""), sep=",", row.names=F, col.names=F)
	}   
	
	if(!is.null(topN)) {
		class_data <- crime_data[which(crime_data$date_str %in% pred_range), c(1:2, 1+cls*2)] 
		names(class_data)[3] <- 'layer0'
		new_data <- as.data.frame(merge(pred_result, class_data, by=c("date_str", "grid_num"))) 
		rm(class_data); gc()
		new_data <- new_data[order(new_data$date_str, -new_data$p),]
		myhead <- function(x) return(sum(head(x, topN)))
		case_top <- aggregate(new_data$layer0, list(date_str=new_data$date_str), myhead)
		sum_top <- sum(case_top[,2]) 
		case_total <- aggregate(new_data$layer0, list(date_str=new_data$date_str), sum)
		sum_total <- sum(case_total[,2]) 
		if(class_count==1) {
			write_content <- paste("无班次", " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
		} else {
			write_content <- paste("Combine班次 ", cls, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
		}                  
		cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)                  	
	}
}
if(!is.null(topN)) {
	cat(paste("#########################以上是knn：", min(case_total[,1]), "-", max(case_total[,1]), "#################################\n", sep=""), file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)                  
}
cat("\n",sep="\n")
