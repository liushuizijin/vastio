## 命中率评估
argv <- commandArgs(TRUE)

data_path <- argv[1]
result_path <- argv[2]
pd_id <- argv[3]
topN <- as.numeric(argv[4])

data_matrix <- read.csv(paste(data_path, ".csv", sep=""),header=F)
data_matrix <- data_matrix[,c(1:3,7)]
names(data_matrix) <- c("date_str", "grid_num", "V", "class_id")

data_matrix$class_id <- 0

files_name <- list.files(paste(result_path, "/pred_result", sep=""))
file_index <- grep(paste("_", pd_id, "_SEPP.csv", sep=""), files_name)

for(f in files_name[file_index]) {
    if(length(file_index) == 1) { class_id <- 0 } else {
        class_id <- as.numeric(substr(f, 7, 7))
    }
	data_result <- read.csv(paste(result_path, "/pred_result/", f, sep=""),header=F)
	names(data_result) <- c("date_str", "grid_num", "p")
	
	class_matrix <- data_matrix[which(data_matrix$date_str <= max(data_result$date_str) & 
							data_matrix$date_str >= min(data_result$date_str) & data_matrix$class_id == class_id), c("date_str", "grid_num", "V")]
	crimeCounts <- aggregate(V~date_str+grid_num, data=class_matrix, length)   
	names(crimeCounts)[3] <- "crimes" 
	
	evaluateData  <- as.data.frame(merge(crimeCounts, data_result, by=c("date_str", "grid_num"), all=T))
	evaluateData[is.na(evaluateData)] <- 0
	evaluateData <- evaluateData[order(evaluateData$date_str, -evaluateData$p), ]
	
	myhead <- function(x) return(sum(head(x, topN)))
	case_top <- aggregate(evaluateData$crimes, list(date_str=evaluateData$date_str), myhead)
	sum_top <- sum(case_top[,2]) 
	sum_total <- nrow(class_matrix)    
	
	if(length(file_index) == 1) {
		write_content <- paste("无班次", " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")
	} else {
		write_content <- paste("班次", class_id, " ：总命中数（", sum_top, "），总案件数（", sum_total, "）命中率（", sum_top/sum_total, "）", sep="")    
	}
	#cat(paste("#########################SEPP：", min(case_top[,1]), "-", max(case_top[,1]), "################################", sep=""), file=paste("E:/WorkSpace/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
	cat(write_content, file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)   
}

cat(paste("#########################以上是SEPP：", min(case_top[,1]), "-", max(case_top[,1]), "################################\n", sep=""), file=paste("E:/WorkSpace/Eclipse/PPSModels/Results/evaluates/", pd_id, "_evaluates.csv", sep=""), sep="\n", append=T)
cat("\n",sep="\n")













