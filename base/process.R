######################     analysis   ##############################
process <- function(file_site,file_name,out_message_site,train_start_date,train_end_date,test_start_date,test_end_date,isnotbaseP=T,len_day=NULL,len_day_cut=NULL,topN,print_train=F){

#####################################################################
	pps1 <- read.table(paste(file_site,file_name,".csv",sep=""),header=T,sep=",")

	######################################################################################
	#  重要说明：file_site下的file_name数据必须是start_date再向前推了至少300天的数据！  ##
	######################################################################################
	
#		names(pps1)=c('date_str', 'grid_num', 'crime_num', 'crime_bi', 'same_crime', 'human_trace', 'bank', 'temporal_spatial_0_0', 
#		   `  'temporal_spatial_0_1', 'temporal_spatial_1_0', 'temporal_spatial_1_1', 'temporal_spatial_2_0', 'temporal_spatial_2_1',
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

#####################################################################
#计算不同基础概率
##计算基础概率函数
	baseP_stat <- function(stat_data_needbaseP,len_day=NULL,len_day_cut=NULL){
		if(is.null(len_day) & is.null(len_day_cut)) stop("ERROR:please make sure one of the arguments(len_day,len_day_cut) is not null")

		crime_data_p <- stat_data_needbaseP[order(stat_data_needbaseP$date_str,stat_data_needbaseP$grid_num),]
		rm(stat_data_needbaseP)
		gc(reset=TRUE)
		##数据历时,天数
		(min_date=min(crime_data_p$date_str));(max_date=max(crime_data_p$date_str))
		(day_count=length(crime_data_p[!duplicated(crime_data_p$date_str),"date_str"]))
		##格子数量
		grid_list=crime_data_p[!duplicated(crime_data_p$grid_num),"grid_num"]
			(grid_count=length(grid_list))
		    
		##计算不同天数的基础概率,包含关系的天数len_day,分段天数len_day_cut

		##向前推len天的发生的案件数/len天数
		stat_len <- function(x,len){
		    x[,(ncol(x)+1):(ncol(x)+length(len))] <-NA
		    ##包含关系
		    #if(TorF==FALSE) names(x)[(ncol(x)-length(len)+1):ncol(x)] <-paste("len_day_",len,sep="")
		    ##分段关系
		    #if(TorF==TRUE) names(x)[(ncol(x)-length(len)+1):ncol(x)] <-paste("len_day_cut_",len,sep="")
		    
		    #rownames(x) <- 1:day_count
		    for(j in len){
			for(i in (day_count:(j+1))){
			    x[i,which(len==j)+ncol(x)-length(len)] <- sum(x[((i-1):(i-j)),"crime_bi"])/j        
			}
		    }
		    return(x)
		}

		##包含关系
		if(is.null(len_day)==F & is.null(len_day_cut)==T){
		    stat_len_day <- as.function(alist(x=,len=len_day,stat_len(x,len)))
		    library(plyr)
		    crime_data_p <- ddply(crime_data_p,.(grid_num=as.factor(grid_num)),stat_len_day)
		    crime_data_p[,(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)] <- t(t(crime_data_p[,(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)])/len_day)
		    names(crime_data_p)[(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)] <-paste("len_day_p",len_day,sep="")    
		}

		##分段关系
		if(is.null(len_day)==T & is.null(len_day_cut)==F){
		    stat_len_cut <- as.function(alist(x=,len=cumsum(len_day_cut),stat_len(x,len)))
		    library(plyr)
		    crime_data_p <- ddply(crime_data_p,.(grid_num=as.factor(grid_num)),stat_len_cut)
		    if(length(len_day_cut)==1){
			crime_data_p[,ncol(crime_data_p)] <- crime_data_p[,5]/len_day_cut
			names(crime_data_p)[ncol(crime_data_p)] <-  paste("len_day_cut_p",len_day_cut,sep="")
		    }
		    if(length(len_day_cut)>1){
			for(i in (length(len_day_cut)-1)){
			    crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)+1] <- crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)+1]-crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)]            
			}
			crime_data_p[,(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)] <- t(t(crime_data_p[,(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)])/len_day_cut)
			names(crime_data_p)[(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)] <-paste("len_day_cut_p",len_day_cut,sep="")
		    }
		}

		##包含+分段关系
		if(is.null(len_day)==F & is.null(len_day_cut)==F){
		    stat_len_day <- as.function(alist(x=,len=len_day,stat_len(x,len)))
		    library(plyr)    
		    crime_data_p <- ddply(crime_data_p,.(grid_num=as.factor(grid_num)),stat_len_day)
		    crime_data_p[,(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)] <- t(t(crime_data_p[,(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)])/len_day)
		    names(crime_data_p)[(ncol(crime_data_p)-length(len_day)+1):ncol(crime_data_p)] <-paste("len_day_p",len_day,sep="") 

		    stat_len_cut <- as.function(alist(x=,len=cumsum(len_day_cut),stat_len(x,len)))
		    crime_data_p <- ddply(crime_data_p,.(grid_num=as.factor(grid_num)),stat_len_cut)
		    if(length(len_day_cut)==1){
			crime_data_p[,ncol(crime_data_p)] <- crime_data_p[,5]/len_day_cut
			names(crime_data_p)[ncol(crime_data_p)] <-  paste("len_day_cut_p",len_day_cut,sep="")
		    }
		    if(length(len_day_cut)>1){
			for(i in (length(len_day_cut)-1)){
			    crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)+1] <- crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)+1]-crime_data_p[,i+ncol(crime_data_p)-length(len_day_cut)]            
			}
			crime_data_p[,(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)] <- t(t(crime_data_p[,(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)])/len_day_cut)
			names(crime_data_p)[(ncol(crime_data_p)-length(len_day_cut)+1):ncol(crime_data_p)] <-paste("len_day_cut_p",len_day_cut,sep="")
		    }
		}
		return(crime_data_p)
	}
	
	##构造函数,提取命中率最高的前k个格子
	myhead <- as.function(alist(x=,n=topN,sum(head(x,n))))

	####包含基础概率的全数据集
	if(isnotbaseP==T){
		pps1 <- baseP_stat(pps1,len_day,len_day_cut)
	}
	####划分数据集
	train_data <- pps1[pps1$date_str >= as.Date(as.character(train_start_date),"%Y%m%d") & pps1$date_str <= as.Date(as.character(train_end_date),"%Y%m%d"),]
	test_data <- pps1[pps1$date_str >= as.Date(as.character(test_start_date),"%Y%m%d") & pps1$date_str <= as.Date(as.character(test_end_date),"%Y%m%d"),]
rm(pps1)
gc(reset=TRUE)

##打印输出汇总结果
	cat(paste("#########################对数据的基本认识#######################",
	paste("每日推送格子数topN=",topN,sep=""),
	paste("全数据集的总记录数(训练数据+测试数据)=",nrowpps1,sep=""),
	paste("全数据集的总列数=",ncolpps1," ; 总(自)变量数=",ncolpps1-4,sep=""),
	paste("全数据集的起止日期:",mindate_pps1,"::",maxdate_pps1," ; 全数据集的总格子数",grid_all,sep=""),sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)


##运行不同模型#######################################################
	####全变量模型
	logit <- glm(crime_bi~.-date_str-grid_num-crime_num,data=train_data[,-c((ncol(train_data)-(length(len_day)+length(len_day_cut))+1):ncol(train_data))],family=binomial(link="logit"))

	####step筛选变量模型
#	logit_step <- step(logit,trace=0)

	####干掉step筛选变量模型
	p_frame <- as.matrix(summary(logit)$coef)
	#drop_var <- names(which(p_frame[,4]>=alpha))
	drop_var <- names(which(p_frame[,4]>=0.1))
	drop_col_index <- which(names(train_data) %in% drop_var)
	if(length(drop_var)==0) fmla <- as.formula(paste("crime_bi ~ ",paste(names(train_data[,-c(1:4,(ncol(train_data)-(length(len_day)+length(len_day_cut))+1):ncol(train_data))]),collapse= "+")))
	if(length(drop_var)>=1) fmla <- as.formula(paste("crime_bi ~ ",paste(names(train_data[,-c(1:4,drop_col_index,(ncol(train_data)-(length(len_day)+length(len_day_cut))+1):ncol(train_data))]),collapse= "+")))
	logit_nostep <- glm(fmla,data=train_data,family=binomial(link="logit"))

	#### 其他模型 ####
	#
	# 代码块 
	#
	##################

	train_data <- train_data[,c(1:4,(ncol(train_data)-(length(len_day)+length(len_day_cut))+1):ncol(train_data))]
	gc(reset=TRUE)
#####################################################################


##评价训练数据的命中率（输出相关统计量）
	evaluate_model_train <- function(model_name,print_train){
		##对训练数据的基本描述
		nrowtrain_data <- nrow(train_data)
		mindate_train_data <- min(train_data$date_str)
		maxdate_train_data <- max(train_data$date_str)

		train_data$pred <- predict(model_name,type="response")

		##计算每日发生总案件数
		day_count_train <- aggregate(train_data$crime_num,list(train_data$date_str),sum)
		names(day_count_train)[2] <- "total_crime_day"
		day_count_train <- as.data.frame(day_count_train)

		if(isnotbaseP==T){
			train_data <- train_data[,c(1:4,ncol(train_data),5:(ncol(train_data)-1))]			
		##构造组合基础概率的函数,计算最大命中率下的组合参数
			max_score <- function(alpha){
				train_data$combin_p <- rowSums(t(t(train_data[,6:ncol(train_data)])*alpha))	
				##对提取出的前topN个格子对应的实际发生案件数进行汇总求和
				train_data <- train_data[order(train_data$date_str,-train_data$combin_p),]
				day_top_train <- aggregate(train_data$crime_num,list(train_data$date_str),myhead)
				names(day_top_train) <- c("Group.2","top_n_day")
				day_top_train <- as.data.frame(day_top_train)
			rm(train_data)
			gc(reset=TRUE)
				day_score_train <- merge(day_count_train,day_top_train,by.x="Group.1",by.y="Group.2")
				names(day_score_train)[1] <- "date_str" 
				return(sum(day_score_train$top_n_day)/sum(day_score_train$total_crime_day))
			}
			##最优化求解组合参数
			opt <- optim(rep(0,ncol(train_data)-5),max_score,control=list(fnscale=-1),method="L-BFGS-B")
			par_p <- opt$par

			train_data$combin_p <- rowSums(t(t(train_data[,6:ncol(train_data)])*par_p))
			train_data <- train_data[order(train_data$date_str,-train_data$combin_p),]
			day_top_train <- aggregate(train_data$crime_num,list(train_data$date_str),myhead)
			names(day_top_train) <- c("Group.2","top_n_day")
			day_top_train <- as.data.frame(day_top_train)
		rm(train_data)
		gc(reset=TRUE)
		}
		if(isnotbaseP==F){
			train_data <- train_data[order(train_data$date_str,-train_data$pred),]
			day_top_train <- aggregate(train_data$crime_num,list(train_data$date_str),myhead)
			names(day_top_train) <- c("Group.2","top_n_day")
			day_top_train <- as.data.frame(day_top_train)
		rm(train_data)
		gc(reset=TRUE)
		}
		day_score_train <- merge(day_count_train,day_top_train,by.x="Group.1",by.y="Group.2")
		names(day_score_train)[1] <- "date_str" 
	####每日命中率统计
		##添加每日命中率列
		day_score_train$rate_day <- day_score_train$top_n_day/day_score_train$total_crime_day

		##画时序图----略
		#ts_score_train <- ts(day_score_train$rate_day,start=1)
		#plot(ts_score_train)

		#每天的命中率,统计概要
		stat_train_day_p <- summary(day_score_train$rate_day)

		#每天平均命中数

		stat_train_day_mean <- mean(day_score_train$top_n_day,na.rm=T)

		#每天平均案件数

		crime_train_day_mean <- mean(day_score_train$total_crime_day,na.rm=T)

		#每日命中率的方差
		stat_train_day_var <- var(day_score_train$rate_day,na.rm=T)

		#每日案件数方差
		crime_train_day_var <- var(day_score_train$total_crime_day,na.rm=T)

	####每周命中率统计
		##计算每周命中数、案件数、命中率
		day_score_train$rate_day <- ifelse(is.na(day_score_train$rate_day),0,day_score_train$rate_day)
		week_score_train <- aggregate(day_score_train[,c("top_n_day","total_crime_day","rate_day")],
					   list(week_front_later=cut(day_score_train$date_str,"weeks",right=T)),sum)
		names(week_score_train) <- c("week_front_later","top_n_week","total_crime_week","rate_week")
		week_score_train <- as.data.frame(week_score_train)

		#每周的命中率,统计概要
		stat_train_week_p <- summary(week_score_train$rate_week)

		#每周平均命中数

		stat_train_week_mean <- mean(week_score_train$top_n_week,na.rm=T)

		#每周平均案件数

		crime_train_week_mean <- mean(week_score_train$total_crime_week,na.rm=T)

		#每周命中率的方差
		stat_train_week_var <- var(week_score_train$rate_week,na.rm=T)

		#每周案件数方差
		crime_train_week_var <- var(week_score_train$total_crime_week,na.rm=T)

	####每月命中率统计
		##计算每月命中数、案件数、命中率
		day_score_train$rate_day <- ifelse(is.na(day_score_train$rate_day),0,day_score_train$rate_day)
		month_score_train <- aggregate(day_score_train[,c("top_n_day","total_crime_day","rate_day")],
					   list(month_front_later=cut(day_score_train$date_str,"months",right=T)),sum)
		names(month_score_train) <- c("month_front_later","top_n_month","total_crime_month","rate_month")
		month_score_train <- as.data.frame(month_score_train)

		#每月的命中率,统计概要
		stat_train_month_p <- summary(month_score_train$rate_month)

		#每月平均命中数

		stat_train_month_mean <- mean(month_score_train$top_n_month,na.rm=T)

		#每月平均案件数

		crime_train_month_mean <- mean(month_score_train$total_crime_month,na.rm=T)

		#每月命中率的方差
		stat_train_month_var <- var(month_score_train$rate_month,na.rm=T)

		#每月案件数方差
		crime_train_month_var <- var(month_score_train$total_crime_month,na.rm=T)

	####训练期限内总命中数、案件数

		#训练数据总命中数
		stat_train_count <- sum(day_score_train$top_n_day)

		#训练数据总案件数
		stat_train_crime <- sum(day_score_train$total_crime_day)

	rm(day_count_train,day_top_train,day_score_train,week_score_train,month_score_train)
	gc(reset=TRUE)

		if(isnotbaseP==T & print_train==F){
			return(par_p)
		}
		if(isnotbaseP==T & print_train==T){
			####打印输出结果
			##每天
			cat(paste(paste("\ndatasource is",file_name,"; model is",paste(model_name$call,collapse="##"),sep=" "),
			"#########################对训练效果的评价：每天#######################",
			paste("训练数据的总记录数:",nrowtrain_data,paste("; 训练数据的起止日期:",mindate_train_data,"::",maxdate_train_data,sep=""),sep=" "),
			paste("训练数据的每日平均命中数=",stat_train_day_mean," ; 训练数据的每日平均案件数=",crime_train_day_mean,sep=""),
			paste("训练数据的每日命中率的方差=",stat_train_day_var," ; 标准差=",sqrt(stat_train_day_var),sep=""),
			paste("训练数据的每日案件数的方差=",crime_train_day_var,";标准差=",sqrt(crime_train_day_var),sep=""),
			paste("训练数据的总命中数=",stat_train_count," ; 训练数据的总案件数=",stat_train_crime," ; 训练数据的总命中率=",stat_train_count/stat_train_crime,sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每日命中率统计概要:",paste("#####",names(stat_train_day_p),"=",stat_train_day_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			#每周
			cat(paste("\n#########################对训练效果的评价：每周#######################",
			paste("训练数据的每周平均命中数=",stat_train_week_mean," ; 训练数据的每周平均案件数=",crime_train_week_mean,sep=""),
			paste("训练数据的每周命中率的方差=",stat_train_week_var," ; 标准差=",sqrt(stat_train_week_var),sep=""),
			paste("训练数据的每周案件数的方差=",crime_train_week_var,";标准差=",sqrt(crime_train_week_var),sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每周命中率统计概要:",paste("#####",names(stat_train_week_p),"=",stat_train_week_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			#每月
			cat(paste("\n#########################对训练效果的评价：每月#######################",
			paste("训练数据的每月平均命中数=",stat_train_month_mean," ; 训练数据的每月平均案件数=",crime_train_month_mean,sep=""),
			paste("训练数据的每月命中率的方差=",stat_train_month_var," ; 标准差=",sqrt(stat_train_month_var),sep=""),
			paste("训练数据的每月案件数的方差=",crime_train_month_var,";标准差=",sqrt(crime_train_month_var),sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每月命中率统计概要:",paste("#####",names(stat_train_month_p),"=",stat_train_month_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			
			return(par_p)
		}
		if(isnotbaseP==F & print_train==T){
			####打印输出结果
			##每天
			cat(paste(paste("\ndatasource is",file_name,"; model is",paste(model_name$call,collapse="##"),sep=" "),
			"#########################对训练效果的评价：每天#######################",
			paste("训练数据的总记录数:",nrowtrain_data,paste("; 训练数据的起止日期:",mindate_train_data,"::",maxdate_train_data,sep=""),sep=" "),
			paste("训练数据的每日平均命中数=",stat_train_day_mean," ; 训练数据的每日平均案件数=",crime_train_day_mean,sep=""),
			paste("训练数据的每日命中率的方差=",stat_train_day_var," ; 标准差=",sqrt(stat_train_day_var),sep=""),
			paste("训练数据的每日案件数的方差=",crime_train_day_var,";标准差=",sqrt(crime_train_day_var),sep=""),
			paste("训练数据的总命中数=",stat_train_count," ; 训练数据的总案件数=",stat_train_crime," ; 训练数据的总命中率=",stat_train_count/stat_train_crime,sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每日命中率统计概要:",paste("#####",names(stat_train_day_p),"=",stat_train_day_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			#每周
			cat(paste("\n#########################对训练效果的评价：每周#######################",
			paste("训练数据的每周平均命中数=",stat_train_week_mean," ; 训练数据的每周平均案件数=",crime_train_week_mean,sep=""),
			paste("训练数据的每周命中率的方差=",stat_train_week_var," ; 标准差=",sqrt(stat_train_week_var),sep=""),
			paste("训练数据的每周案件数的方差=",crime_train_week_var,";标准差=",sqrt(crime_train_week_var),sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每周命中率统计概要:",paste("#####",names(stat_train_week_p),"=",stat_train_week_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			#每月
			cat(paste("\n#########################对训练效果的评价：每月#######################",
			paste("训练数据的每月平均命中数=",stat_train_month_mean," ; 训练数据的每月平均案件数=",crime_train_month_mean,sep=""),
			paste("训练数据的每月命中率的方差=",stat_train_month_var," ; 标准差=",sqrt(stat_train_month_var),sep=""),
			paste("训练数据的每月案件数的方差=",crime_train_month_var,";标准差=",sqrt(crime_train_month_var),sep=""),
			sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
			cat("\n      训练数据的每月命中率统计概要:",paste("#####",names(stat_train_month_p),"=",stat_train_month_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
		}
		#if(isnotbaseP==F & print_train==F){##########}
	}

##评价测试数据的命中率（输出相关统计量）
	evaluate_model_test <- function(model_name,print_train){
		##对测试数据的基本描述
		nrowtest_data <- nrow(test_data)
		mindate_test_data <- min(test_data$date_str)
		maxdate_test_data <- max(test_data$date_str)

		test_data$pred <- predict(model_name,test_data[,-c((ncol(test_data)-(length(len_day)+length(len_day_cut))+1):ncol(test_data))],type="response")
		test_data <- test_data[,c(1:4,ncol(test_data),(ncol(test_data)-(length(len_day)+length(len_day_cut))):(ncol(test_data)-1))]
		gc(reset=TRUE)

		##计算每日发生总案件数
		day_count_test <- aggregate(test_data$crime_num,list(test_data$date_str),sum)
		names(day_count_test)[2] <- "total_crime_day"
		day_count_test <- as.data.frame(day_count_test)

		if(isnotbaseP==T){
			par_p <- evaluate_model_train(model_name,print_train)

			test_data$combin_p <- rowSums(t(t(test_data[,6:ncol(test_data)])*par_p))
			test_data <- test_data[order(test_data$date_str,-test_data$combin_p),]
			day_top_test <- aggregate(test_data$crime_num,list(test_data$date_str),myhead)
			names(day_top_test) <- c("Group.2","top_n_day")
			day_top_test <- as.data.frame(day_top_test)
		}
		if(isnotbaseP==F){
		##对提取出的前topN个格子对应的实际发生案件数进行汇总求和
			test_data <- test_data[order(test_data$date_str,-test_data$pred),]
			day_top_test <- aggregate(test_data$crime_num,list(test_data$date_str),myhead)
			names(day_top_test) <- c("Group.2","top_n_day")
			day_top_test <- as.data.frame(day_top_test)
		}

		rm(test_data)
		gc(reset=TRUE)

		day_score_test <- merge(day_count_test,day_top_test,by.x="Group.1",by.y="Group.2")
		names(day_score_test)[1] <- "date_str" 

	####每日命中率统计

		##添加每日命中率列
		day_score_test$rate_day <- day_score_test$top_n_day/day_score_test$total_crime_day

		##画时序图----略
		#ts_score_test <- ts(day_score_test$rate_day,start=1)
		#plot(ts_score_test)

		#每天的命中率,统计概要
		stat_test_day_p <- summary(day_score_test$rate_day)

		#每天平均命中数

		stat_test_day_mean <- mean(day_score_test$top_n_day,na.rm=T)

		#每天平均案件数

		crime_test_day_mean <- mean(day_score_test$total_crime_day,na.rm=T)

		#每日命中率的方差
		stat_test_day_var <- var(day_score_test$rate_day,na.rm=T)

		#每日案件数方差
		crime_test_day_var <- var(day_score_test$total_crime_day,na.rm=T)

	####每周命中率统计
		##计算每周命中数、案件数、命中率
		day_score_test$rate_day <- ifelse(is.na(day_score_test$rate_day),0,day_score_test$rate_day)
		week_score_test <- aggregate(day_score_test[,c("top_n_day","total_crime_day","rate_day")],
					   list(week_front_later=cut(day_score_test$date_str,"weeks",right=T)),sum)
		names(week_score_test) <- c("week_front_later","top_n_week","total_crime_week","rate_week")
		week_score_test <- as.data.frame(week_score_test)

		#每周的命中率,统计概要
		stat_test_week_p <- summary(week_score_test$rate_week)

		#每周平均命中数

		stat_test_week_mean <- mean(week_score_test$top_n_week,na.rm=T)

		#每周平均案件数

		crime_test_week_mean <- mean(week_score_test$total_crime_week,na.rm=T)

		#每周命中率的方差
		stat_test_week_var <- var(week_score_test$rate_week,na.rm=T)

		#每周案件数方差
		crime_test_week_var <- var(week_score_test$total_crime_week,na.rm=T)

	####每月命中率统计
		##计算每月命中数、案件数、命中率
		day_score_test$rate_day <- ifelse(is.na(day_score_test$rate_day),0,day_score_test$rate_day)
		month_score_test <- aggregate(day_score_test[,c("top_n_day","total_crime_day","rate_day")],
					   list(month_front_later=cut(day_score_test$date_str,"months",right=T)),sum)
		names(month_score_test) <- c("month_front_later","top_n_month","total_crime_month","rate_month")
		month_score_test <- as.data.frame(month_score_test)

		#每月的命中率,统计概要
		stat_test_month_p <- summary(month_score_test$rate_month)

		#每月平均命中数

		stat_test_month_mean <- mean(month_score_test$top_n_month,na.rm=T)

		#每月平均案件数

		crime_test_month_mean <- mean(month_score_test$total_crime_month,na.rm=T)

		#每月命中率的方差
		stat_test_month_var <- var(month_score_test$rate_month,na.rm=T)

		#每月案件数方差
		crime_test_month_var <- var(month_score_test$total_crime_month,na.rm=T)

	####测试期限内总命中数、案件数

		#测试数据总命中数
		stat_test_count <- sum(day_score_test$top_n_day)

		#测试数据总案件数
		stat_test_crime <- sum(day_score_test$total_crime_day)

	rm(day_count_test,day_top_test,day_score_test,week_score_test,month_score_test)
	gc(reset=TRUE)

	####打印输出结果

	##每天
		cat(paste(paste("\n\ndatasource is",file_name,"; model is",paste(model_name$call,collapse="##"),sep=" "),
		"#########################对测试效果的评价：每天#######################",
		paste("测试数据的总记录数:",nrowtest_data,paste("; 测试数据的起止日期:",mindate_test_data,"::",maxdate_test_data,sep=""),sep=" "),
		paste("测试数据的每日平均命中数=",stat_test_day_mean," ; 测试数据的每日平均案件数=",crime_test_day_mean,sep=""),
		paste("测试数据的每日命中率的方差=",stat_test_day_var," ; 标准差=",sqrt(stat_test_day_var),sep=""),
		paste("测试数据的每日案件数的方差=",crime_test_day_var,";标准差=",sqrt(crime_test_day_var),sep=""),
		paste("测试数据的总命中数=",stat_test_count," ; 测试数据的总案件数=",stat_test_crime," ; 测试数据的总命中率=",stat_test_count/stat_test_crime,sep=""),
		sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
		cat("\n      测试数据的每日命中率统计概要:",paste("#####",names(stat_test_day_p),"=",stat_test_day_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
	#每周
		cat(paste("\n#########################对测试效果的评价：每周#######################",
		paste("测试数据的每周平均命中数=",stat_test_week_mean," ; 测试数据的每周平均案件数=",crime_test_week_mean,sep=""),
		paste("测试数据的每周命中率的方差=",stat_test_week_var," ; 标准差=",sqrt(stat_test_week_var),sep=""),
		paste("测试数据的每周案件数的方差=",crime_test_week_var,";标准差=",sqrt(crime_test_week_var),sep=""),
		sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
		cat("\n      测试数据的每周命中率统计概要:",paste("#####",names(stat_test_week_p),"=",stat_test_week_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
	#每月
		cat(paste("\n#########################对测试效果的评价：每月#######################",
		paste("测试数据的每月平均命中数=",stat_test_month_mean," ; 测试数据的每月平均案件数=",crime_test_month_mean,sep=""),
		paste("测试数据的每月命中率的方差=",stat_test_month_var," ; 标准差=",sqrt(stat_test_month_var),sep=""),
		paste("测试数据的每月案件数的方差=",crime_test_month_var,";标准差=",sqrt(crime_test_month_var),sep=""),
		sep="\n"),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
		cat("\n      测试数据的每月命中率统计概要:",paste("#####",names(stat_test_month_p),"=",stat_test_month_p,sep=" "),sep="\n",file=paste(out_message_site,"analysis_message_",file_name,"_",isnotbaseP,"_",print_train,".txt",sep=""),append=T)
	}

####执行函数,打印输出分析摘要汇总

	evaluate_model_test(model_name1,print_train)

	evaluate_model_test(model_name2,print_train)

	evaluate_model_test(model_name3,print_train)
	
	##########
	#其他----->model_name
	##########
}


#####以word文件的形式保存统计摘要----目前以txt文件保存


cat("#####已成功流程化建模分析函数#####
#####可传入的参数说明：调用函数process(file_site,file_name,train_start_date,train_end_date,
                                       test_start_date,test_end_date,topN,print_train=F)
  file_site:          导入矩阵数据所在的文件路径,如：'c:\\xx\\xxx'  或  'c:/xx/xxx' 
  file_name:          导入矩阵数据的R文件名,如：'207_200_2_2'
  out_message_site:   输出分析报告的路径名
  train_start_date：  设定训练数据的起始时间,如：20130101,注:起始时间要尽可能近以便保证训练数据中剪掉推移损失的天数
  train_end_date：    设定训练数据的结束时间
  test_start_date：   设定测试数据的起始时间
  test_end_date：     设定测试数据的结束时间
  isnotbaseP:         是否加基础概率,默认加基础概率(有组合的基础概率)
  len_day:            计算基础概率时连续向前推移天数,可传入向量,如c(1,2,3)----包含关系
  len_day_cut:        计算基础概率时间断向前推移天数,可传入向量------------分段关系
                      ##len_day,len_day_cut二者不能同时为空,可同时非空
  topN:               每日推送格子数
  print_train：       是否打印输出训练数据训练效果摘要
############################################################",sep="\n")


#  process(file_site,file_name,train_start_date,train_end_date,test_start_date,test_end_date,isnotbaseP=T,len_day=NULL,len_day_cut=NULL,topN,print_train=F)

