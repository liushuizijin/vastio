## 命中率评估
argv <- commandArgs(TRUE)

data_path_file <- argv[1]
result_path <- argv[2]
pd_id <- as.numeric(argv[3])
topN <- as.numeric(argv[4])

data_matrix <- read.csv(paste(data_path_file, ".csv", sep=""),header=F)
class_count <- (ncol(data_matrix)-2)/2
if(class_count == 1) {
	names(data_matrix) <- c("date_str", "grid_num", "noclass_layer0", "noclass_layer1")
	data_matrix <- data_matrix[, -4]
}
if(class_count == 2) {
	names(data_matrix) <- c("date_str", "grid_num", "class1_layer0", "class1_layer1",
			"class2_layer0", "class2_layer1")
	data_matrix <- data_matrix[, -c(4, 6)]
}
if(class_count == 3) {
	names(data_matrix) <- c("date_str", "grid_num", "class1_layer0", "class1_layer1",
			"class2_layer0", "class2_layer1", "class3_layer0", "class3_layer1") 
	data_matrix <- data_matrix[, -c(4, 6, 8)]
}

files_name <- list.files(paste(result_path, "/pred_result", sep=""))
file_index <- grep(paste("_", pd_id, "_tstat.csv", sep=""), files_name)

write_content <- NULL
for(f in files_name[file_index]) {
	data_result <- read.csv(paste(result_path, "/pred_result/", f, sep=""),header=F)
	names(data_result) <- c("date_str", "grid_num", "p")
	
	data_matrix <- data_matrix[data_matrix$date_str <= max(data_result$date_str) & 
					data_matrix$date_str >= min(data_result$date_str), ]
	
	if(grepl("class_combine_", f) == TRUE) { ## 有班次且合并
		class_num0 <- as.numeric(substr(f, 15, 15))
		data_tmp <- data_matrix[,c(1:2, 2+class_num0)]
	} else if(grepl("class_nocombine_", f) == TRUE) { ## 有班次不合并
		class_num1 <- as.numeric(substr(f, 17, 17))
		data_tmp <- data_matrix[,c(1:2, 2+class_num1)]
	} else { ## 无班次
		data_tmp <- data_matrix
	}
	
	## 命中案件数
    tmp_data <- data_tmp
    names(tmp_data)[3] <- 'layer0'
	new_data <- as.data.frame(merge(tmp_data, data_result, by=c("date_str", "grid_num")))
	new_data <- new_data[order(new_data$date_str, -new_data$p), ]
	myhead <- function(x) return(sum(head(x, topN)))
	case_top <- aggregate(new_data$layer0, list(date_str=new_data$date_str), myhead)
	sum_top <- sum(case_top[,2]) 
	
	## 日内总案件
	case_total <- aggregate(new_data$layer0, list(date_str=new_data$date_str), sum)
	sum_total <- sum(case_total[,2])
	
	if(exists("class_num0")) {
		write_content <- paste("Combine班次 ", class_num0, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
		rm(class_num0); gc()
	} else if(exists("class_num1")){
		write_content <- paste("NoCombine班次 ", class_num1, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
		rm(class_num1); gc()
	} else {
		write_content <- paste("无班次", " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
	}
	#cat(paste("#########################tstat：", min(case_total[,1]), "-", max(case_total[,1]), "################################", sep=""), file=paste("E:/WorkSpace/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
	cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
}

cat(paste("#########################以上是tstat：", min(case_total[,1]), "-", max(case_total[,1]), "################################\n", sep=""), file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
cat("\n",sep="\n")