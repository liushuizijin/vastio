######################     read and cut data   ##############################
read_data_cut <- function(file_site,file_name,out_data_site,out_data_name,train_start_date,train_end_date,train_len_day=NULL,test_start_date,test_end_date,test_len_day=NULL){

	##数据切分,划分训练数据train_data和测试验证数据test_data

	#train_start_date=20130901
	#train_end_date=20130902
	#train_len_day=NULL
	##边界都是封闭的
	#train_data <- crime_data[as.Date(as.character(train_start_date),"%Y%m%d"):as.Date(as.character(train_end_date),"%Y%m%d"),]
        
	#生成训练数据,终止提示
	if(is.null(train_start_date)) stop("ERROR:train_start_date shoud not be null")
	if((is.null(train_end_date)==T & is.null(train_len_day)==T) | (is.null(train_end_date)==F & is.null(train_len_day)==F)) 
	    stop("ERROR:please make sure only one of the arguments(train_end_date,train_len_day) is not null")

        #生成测试验证数据,终止提示
	if(is.null(test_start_date)) stop("ERROR:test_start_date shoud not be null")
	if((is.null(test_end_date)==T & is.null(test_len_day)==T) | (is.null(test_end_date)==F & is.null(test_len_day)==F)) 
	    stop("ERROR:please make sure only one of the arguments(test_end_date,test_len_day) is not null")
	
#####################################################################
	pps1 <- read.table(paste(file_site,file_name,".csv",sep=""),header=T,sep=",")

#		names(pps1)=c('date_str', 'grid_num', 'crime_num', 'crime_bi', 'same_crime', 'human_trace', 'bank', 'temporal_spatial_0_0', 
#		     'temporal_spatial_0_1', 'temporal_spatial_1_0', 'temporal_spatial_1_1', 'temporal_spatial_2_0', 'temporal_spatial_2_1',
#		     'temporal_spatial_3_0', 'temporal_spatial_3_1', 'temporal_spatial_4_0', 'temporal_spatial_4_1', 'temporal_spatial_5_0', 
#		     'temporal_spatial_5_1', 'temporal_spatial_6_0', 'temporal_spatial_6_1', 'temporal_spatial_7_0', 'temporal_spatial_7_1',
#		     'temporal_spatial_8_0', 'temporal_spatial_8_1', 'temporal_spatial_9_0', 'temporal_spatial_9_1')
#####################################################################
	pps1$date_str <- as.Date(as.factor(pps1$date_str),"%Y%m%d")

	####对全数据集的基本认识：
	nrowpps1 <- nrow(pps1)
	ncolpps1 <- ncol(pps1)
	mindate_pps1 <- min(pps1$date_str)
	maxdate_pps1 <- max(pps1$date_str)
	grid_all <- length(pps1[!duplicated(pps1$grid_num),1])

	if((is.null(train_end_date)==F & as.Date(as.character(train_end_date),"%Y%m%d") > maxdate_pps1) | 
	    as.Date(as.character(train_start_date),"%Y%m%d")+train_len_day > maxdate_pps1) stop("ERROR:train_end_date is out of limit")
	if((is.null(test_end_date)==F & as.Date(as.character(test_end_date),"%Y%m%d") > maxdate_pps1) | 
	    as.Date(as.character(test_start_date),"%Y%m%d")+test_len_day > maxdate_pps1) stop("ERROR:test_end_date is out of limit")

	##划分训练数据集
	if(is.null(train_end_date)==F & is.null(train_len_day)==T){
	    if(train_end_date>max_date) stop("ERROR:train_end_date is out of max_date")
	    else train_data <- crime_data[crime_data$date_str>=train_start_date & crime_data$date_str<=train_end_date,]
	}
	if(is.null(train_end_date)==T & is.null(train_len_day)==F){
	    train_end_date <- as.Date(as.character(train_start_date),"%Y%m%d")+train_len_day
	    train_end_date <- paste(substr(train_end_date,1,4),substr(train_end_date,6,7),substr(train_end_date,9,10),sep="")
	    if(train_end_date>max_date) stop("ERROR:train_end_date is out of max_date")
	    else train_data <- crime_data[crime_data$date_str>=train_start_date & crime_data$date_str<=train_end_date,]
	}

	##划分测试验证数据集
	if(is.null(test_end_date)==F & is.null(train_len_day)==T){
	    if(test_end_date>max_date) stop("ERROR:test_end_date is out of max_date")
	    else test_data <- crime_data[crime_data$date_str>=test_start_date & crime_data$date_str<=test_end_date,]
	}
	if(is.null(test_end_date)==T & is.null(train_len_day)==F){
	    test_end_date <- as.Date(as.character(test_start_date),"%Y%m%d")+test_len_day
	    test_end_date <- paste(substr(test_end_date,1,4),substr(test_end_date,6,7),substr(test_end_date,9,10),sep="")
	    if(test_end_date>max_date) stop("ERROR:test_end_date is out of max_date")
	    else test_data <- crime_data[crime_data$date_str>=test_start_date & crime_data$date_str<=test_end_date,]
	}

##打印输出汇总结果
	cat(paste("#########################对数据的基本认识#######################",
	paste("每日推送格子数topN=",topN,sep=""),
	paste("全数据集的总记录数(训练数据+测试数据)=",nrowpps1,sep=""),
	paste("全数据集的总列数=",ncolpps1," ; 总(自)变量数=",ncolpps1-4,sep=""),
	paste("全数据集的起止日期:",mindate_pps1,"::",maxdate_pps1," ; 全数据集的总格子数",grid_all,sep=""),sep="\n"))

#train_data$date_str <- as.numeric(paste(substr(train_data$date_str,1,4),substr(train_data$date_str,6,7),substr(train_data$date_str,9,10),sep=""))
#test_data$date_str <- as.numeric(paste(substr(test_data$date_str,1,4),substr(test_data$date_str,6,7),substr(test_data$date_str,9,10),sep=""))
save(train_data,test_data,file=paste(out_data_site,out_data_name,".RData",sep=""))

rm(list=ls())
gc(reset=TRUE)

}

cat("#####已成功加载数据分割函数#####
#####可传入的参数说明：调用函数baseP_stat(file_site,file_name,out_data_site,train_end_date,train_len_day=NULL,
                                          test_start_date,test_end_date,test_len_day=NULL,out_data_name,train_start_date)
  file_site:             生成的矩阵数据所在的路径,如：'c:\\xx\\xxx'  或  'c:/xx/xxx' 
  file_name:             需要使用的某个派出所的矩阵文件名,如：'207_x_x_x'----##统一数据格式为.csv
  train_start_date:      划分训练数据集起始时间,如：20130101
  train_end_date:        划分训练数据集结束时间
  train_len_day：        在训练数据起始时间确定的情况下设定训练数据的时间跨度(历时)
  test_start_date:       划分测试数据集起始时间
  test_end_date:         划分测试数据集结束时间
  test_len_day:          在测试数据起始时间确定的情况下设定测试数据的时间跨度(历时)
  out_data_site:         划分数据集后保存为R文件所在的路径,格式同file_site
  out_data_name:         划分数据集后保存为R文件的文件名,格式同file_name
############################################################",sep="\n")   



read_data_cut(file_site,file_name,train_start_date,train_end_date,train_len_day=NULL,test_start_date,test_end_date,test_len_day=NULL,out_data_site,out_data_name)

 
