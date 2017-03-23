# Model name：glmST
# Date：2016/5/10
# Vsersion：1.0 （no POI数据）
# Details：基于时空数据的广义线性混合模型
# Library：CARBayesST

# --------------------
# 读入参数
# --------------------

argv <- commandArgs(TRUE)
# data_path <- argv[1]
# result_path <- argv[2]
# if(file.exists(result_path) == F) dir.create(result_path)
# if(file.exists(paste(result_path, "/pred_result", sep="")) == F) dir.create(paste(result_path, "/pred_result", sep=""))
# pd_id <- as.numeric(argv[3])
# start_date <- as.numeric(argv[4])
# end_date <- as.numeric(argv[5])
# if(argv[6] == "TRUE") combineOrnot <- TRUE 
# if(argv[6] == "FALSE") combineOrnot <- FALSE # 默认
# burnin <- as.numeric(argv[7])
# sampleN <- as.numeric(argv[8])
# if(argv[9] == "NULL") {
# topN <- NULL # 默认
# } else {
# topN <- as.numeric(argv[9]) 
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
combineOrnot <- config$combine
if(combineOrnot == "TRUE") combineOrnot <- TRUE
if(combineOrnot == "FALSE") combineOrnot <- FALSE  
burnin <- config$burnin
sampleN <- config$samples

topN <- NULL

pred_range <- as.numeric(as.Date(as.character(end_date), "%Y%m%d") - as.Date(as.character(start_date), "%Y%m%d"))

library(MASS)
library(Rcpp)
library(CARBayesST)

# --------------------
# 读入数据
# --------------------

crime_data <- read.csv(data_path, header=F)

# --------------------
# 数据转换
# --------------------
# --------------------
# 转换说明
# --------------------

# 根据crime_data的样式
# 生成邻接矩阵W
# 对时空表示的因变量Y进行向量化转化：rep(所有格子的crime_ornot, date_range)
# 难点1：如何用程序生成W，特别注意格子的crime_ornot与Y的对应顺序
# 难点2：如何上线使用进行预测，利用模型可以接受缺失值的特性

class_count <- ncol(crime_data)-3

if(class_count == 1) {
	names(crime_data) <- c("date_str", "grid_num", "noclass_layer0", "neighbor_grids")
	crime_data <- crime_data[order(crime_data$date_str, crime_data$grid_num),]
	crime_data$noclass_layer0 <- ifelse(crime_data$noclass_layer0 >= 1, 1, 0)
	comb_data <- crime_data[,1:3]
	comb_data <- comb_data[order(comb_data$date_str, comb_data$grid_num),]
	neighbor_grid <- as.character(crime_data[,4])
}

if(class_count == 2) {
	names(crime_data) <- c("date_str", "grid_num", "class1_layer0", "class2_layer0", "neighbor_grids")
	crime_data <- crime_data[order(crime_data$date_str, crime_data$grid_num),]
	crime_data$class1_layer0 <- ifelse(crime_data$class1_layer0 >= 1, 1, 0)
	crime_data$class2_layer0 <- ifelse(crime_data$class2_layer0 >= 1, 1, 0)
	comb_data <- as.data.frame(cbind(date_str=rep(crime_data$date_str, 2), grid_num=rep(crime_data$grid_num, 2), 
					crime_ornot=c(crime_data[,3], crime_data[,4]), class_id=rep(1:2,each=nrow(crime_data))))
	comb_data <- comb_data[order(comb_data$date_str, comb_data$class_id, comb_data$grid_num),]
	neighbor_grid <- as.character(crime_data[,5])
}

if(class_count == 3) {
	names(crime_data) <- c("date_str", "grid_num", "class1_layer0", "class2_layer0", "class3_layer0", "neighbor_grids")
	crime_data <- crime_data[order(crime_data$date_str, crime_data$grid_num),]
	crime_data$class1_layer0 <- ifelse(crime_data$class1_layer0 >= 1, 1, 0)
	crime_data$class2_layer0 <- ifelse(crime_data$class2_layer0 >= 1, 1, 0)
	crime_data$class3_layer0 <- ifelse(crime_data$class3_layer0 >= 1, 1, 0)
	comb_data <- as.data.frame(cbind(date_str=rep(crime_data$date_str, 3), grid_num=rep(crime_data$grid_num, 3), 
					crime_ornot=c(crime_data[,3], crime_data[,4], crime_data[,5]), class_id=rep(1:3,each=nrow(crime_data))))
	comb_data <- comb_data[order(comb_data$date_str, comb_data$class_id, comb_data$grid_num),]
	neighbor_grid <- as.character(crime_data[,6])
}
# rm(crime_data) # 只考虑班次合并的情形时

grid_num <- sort(unique(comb_data$grid_num))
diff_range <- as.numeric(as.Date(as.character(start_date), "%Y%m%d") - as.Date(as.character(max(comb_data$date_str)), "%Y%m%d")) - 1 # 在预测时防止“扣掉昨天”的情形，以及取未来任意天进行预测
# diff_range <- 0 
date_range <- diff_range + pred_range
# grid_count <- length(grid_num)

pred_range_date <- as.numeric(format(seq(as.Date(as.character(start_date), "%Y%m%d"), as.Date(as.character(end_date), "%Y%m%d")-1, 1), "%Y%m%d"))

all_date_grid <- as.data.frame(cbind(date_str=rep(pred_range_date, each=length(grid_num)*class_count),
				class_id=rep(1:class_count, pred_range*length(grid_num)),
				grid_num=rep(rep(grid_num, each=class_count), pred_range)))         

# 计算邻接矩阵

neighbor_grid <- unlist(lapply(neighbor_grid[1:length(grid_num)], function(x) substr(x, 2, nchar(x)-1)))

W <- strsplit(neighbor_grid, ":")
# W <- lapply(W, as.numeric)
# W <- lapply(W, function(x) as.numeric(grid_num %in% x))
# W <- matrix(unlist(W), nrow=grid_count)
W <- sapply(W, function(x) as.numeric(grid_num %in% as.numeric(x)))

removeIndex <- which(rowSums(W)==0)
if(length(removeIndex) > 0 ) {
	W <- W[-removeIndex,]
	W <- W[, -removeIndex]
	grid_num_rest <- grid_num[-removeIndex]
} else {grid_num_rest <- grid_num}
grid_count <- nrow(W)

if(combineOrnot == FALSE) { # 单个班次计算
	for(i in 1:class_count) { 
		Y <- crime_data[which((crime_data$grid_num %in% grid_num_rest) & (crime_data$date_str < start_date)), 2+i]
		Y <- c(Y, rep(NA, grid_count * date_range))
		
		# 时空交互效应
		model_1 <- ST.CARanova(formula=Y~1, family="binomial", W=W, interaction=TRUE, trials=rep(1, length(Y)), burnin=burnin, n.sample=sampleN)
		
		# 时空自回归
		model_2 <- ST.CARar(formula=Y~1, family="binomial", W=W, trials=rep(1, length(Y)), burnin=burnin,  n.sample=sampleN)
		
		# 时空线性趋势
		model_3 <- ST.CARlinear(formula=Y~1, family="binomial", W=W, trials=rep(1, length(Y)), burnin=burnin,  n.sample=sampleN)
		
		p_1 <- tail(model_1$fitted.values, grid_count*pred_range)
		p_2 <- tail(model_2$fitted.values, grid_count*pred_range)
		p_3 <- tail(model_3$fitted.values, grid_count*pred_range)
		
		results <- cbind(date_str=rep(pred_range_date, each=grid_count),
				grid_num=rep(grid_num_rest, pred_range), p=(p_1+p_2+p_3)/3)
		results <- merge(results, all_date_grid[all_date_grid$class_id==i, -2], by=c("date_str", "grid_num"), all=T)
		results[is.na(results)] <- 0
		results <- results[order(results$date_str, -results$p), ]
		
		if(!is.null(topN)) {
			# 命中案件数
            class_data <- tail(crime_data[,c(1:2, 2+i)], pred_range*length(grid_num))
            names(class_data)[3] <- 'layer0'
			new_data <- as.data.frame(merge(class_data, results, by=c("date_str", "grid_num")))
            rm(class_data); gc()
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
			#cat(paste("#########################glmST：", min(case_total[,1]), "-", max(case_total[,1]), "################################", sep=""), file=paste("E:/WorkSpace/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
			cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)            
		}
		results$p <- 0.001 + ((results$p - min(results$p)) / (max(results$p) - min(results$p))) * (0.1 - 0.001)
		if(class_count > 1) {
			write.table(results, file=paste(result_path, "/pred_result/", "class_nocombine_", i, "_", pd_id, "_glmST.csv", sep=""), sep=",", row.names=F, col.names=F)
		} else {
			write.table(results, file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_glmST.csv", sep=""), sep=",", row.names=F, col.names=F)          
		}
	}
} else { # 合并班次计算
	if(is.null(topN)) { rm(crime_data); gc() }
	Y <- comb_data[which((comb_data$grid_num %in% grid_num_rest) & (comb_data$date_str < start_date)), "crime_ornot"]
	Y <- c(Y, rep(NA, grid_count * date_range * class_count))
	
	# --------------------
	# 训练+预测
	# --------------------
	
	# 时空交互效应
	model_1 <- ST.CARanova(formula=Y~1, family="binomial", W=W, interaction=TRUE, trials=rep(1, length(Y)), burnin=burnin, n.sample=sampleN)
	
	# 时空自回归
	model_2 <- ST.CARar(formula=Y~1, family="binomial", W=W, trials=rep(1, length(Y)), burnin=burnin,  n.sample=sampleN)
	
	# 时空线性趋势
	model_3 <- ST.CARlinear(formula=Y~1, family="binomial", W=W, trials=rep(1, length(Y)), burnin=burnin,  n.sample=sampleN)
	
	# --------------------
	# 评估+预测
	# --------------------
	
	p_1 <- tail(model_1$fitted.values, grid_count*pred_range*class_count)
	p_2 <- tail(model_2$fitted.values, grid_count*pred_range*class_count)
	p_3 <- tail(model_3$fitted.values, grid_count*pred_range*class_count)
	
	results <- cbind(date_str=rep(pred_range_date, each=grid_count*class_count), 
        class_id=rep(1:class_count, pred_range*grid_count),
        grid_num=rep(rep(grid_num_rest, each=class_count), pred_range), p=(p_1+p_2+p_3)/3)
	results <- merge(results, all_date_grid, by=c("date_str", "class_id", "grid_num"), all=T)
	results[is.na(results)] <- 0
	results <- results[order(results$date_str, results$class_id, -results$p), ]
	
	for(i in 1:class_count) {
		if(!is.null(topN)) {
			# 命中案件数
            class_data <- tail(crime_data[,c(1:2, 2+i)], pred_range*length(grid_num))
            names(class_data)[3] <- 'layer0'
			new_data <- as.data.frame(merge(class_data, results[results$class_id==i, -2], by=c("date_str", "grid_num")))
			rm(class_data)
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
				write_content <- paste("Combine班次 ", i, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
			}
			#cat(paste("#########################glmST：", min(case_total[,1]), "-", max(case_total[,1]), "################################", sep=""), file=paste("E:/WorkSpace/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)                  
			cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)                  
		}
		results$p <- 0.001 + ((results$p - min(results$p)) / (max(results$p) - min(results$p))) * (0.1 - 0.001)
        if(class_count > 1) {
            write.table(results[results$class_id==i, -2], file=paste(result_path, "/pred_result/", "class_combine_", i, "_", pd_id, "_glmST.csv", sep=""), sep=",", row.names=F, col.names=F)		
        } else {
            write.table(results[results$class_id==i, -2], file=paste(result_path, "/pred_result/", "noclass_", pd_id, "_glmST.csv", sep=""), sep=",", row.names=F, col.names=F)
        }
	}
}
if(!is.null(topN)) {
	cat(paste("#########################以上是glmST：", min(case_total[,1]), "-", max(case_total[,1]), "################################\n", sep=""), file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)                  
}
cat("\n",sep="\n")














