######################     evaluate p    ##############################
evaluate_p <- function(in_model_site,in_data_model_name,in_data_p_site,in_data_p_name,topN,isnotbaseP=T,print_train=F){
##加载数据文件和模型文件
	load(paste(in_model_site,in_data_model_name,".RData",sep=""),.GlobalEnv)
	if(isnotbaseP==T){
	###加载包含基础概率的数据
		load(paste(in_data_p_site,in_data_p_name,".RData",sep=""),.GlobalEnv)
		np <- ncol(crime_data_p)-4
	}

##评价训练数据的命中率（输出相关统计量）
	evaluate_model_train <- function(k=topN,model_name,print_train){
		##对训练数据的基本描述
		nrowtrain_data <- nrow(train_data)
		mindate_train_data <- min(train_data$date_str)
		maxdate_train_data <- max(train_data$date_str)

		train_data$pred <- predict(model_name,type="response")

		##计算每日发生总案件数
		day_count_train <- aggregate(train_data$crime_num,list(train_data$date_str),sum)
		names(day_count_train)[2] <- "total_crime_day"
		day_count_train <- as.data.frame(day_count_train)

##构造组合基础概率的函数,计算最大命中率下的组合参数
		##构造函数,提取命中率最高的前k个格子
		myhead <- as.function(alist(x=,n=k,sum(head(x,n))))
		max_score <- function(alpha){
			train_data$combin_p <- rowSums(t(t(train_data[,6:ncol(train_data)])*alpha))	
			##对提取出的前k个格子对应的实际发生案件数进行汇总求和
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
		if(isnotbaseP==T){
			train_data <- merge(train_data,crime_data_p,by=c("date_str","grid_num","crime_num","crime_bi"))
		rm(crime_data_p)
		gc(reset=TRUE)
			##最优化求解组合参数
			opt <- optim(rep(0,np),max_score,control=list(fnscale=-1),method="L-BFGS-B")
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

	rm(day_count_train,day_top_train,day_score_train,week_count_train,week_top_train,week_score_train,month_count_train,month_top_train,month_score_train)
	gc(reset=TRUE)

		if(isnotbaseP==T & print_train==F){
			return(par_p)
			rm(list=ls())
			gc(reset=TRUE)
		}
		if(isnotbaseP==T & print_train==T){
			####打印输出结果
			##每天
			cat(paste(paste("datasource is",out_data_model_name,"; model_name is",model_name,sep=" "),
			"#########################对训练效果的评价：每天#######################",
			paste("训练数据的总记录数:",nrowtrain_data,paste("; 训练数据的起止日期:",mindate_train_data,"::",maxdate_train_data,sep=""),sep=" "),
			paste("训练数据的每日平均命中数=",stat_train_day_mean," ; 训练数据的每日平均案件数=",crime_train_day_mean,sep=""),
			paste("训练数据的每日命中率的方差=",stat_train_day_var," ; 标准差=",sqrt(stat_train_day_var),sep=""),
			paste("训练数据的每日案件数的方差=",crime_train_day_var,";标准差=",sqrt(crime_train_day_var),sep=""),
			paste("训练数据的总命中数=",stat_train_count," ; 训练数据的总案件数=",stat_train_crime," ; 训练数据的总命中率=",stat_train_count/stat_train_crime,sep=""),
			sep="\n"));cat("\n      训练数据的每日命中率统计概要:",paste("#####",names(stat_train_day_p),"=",stat_train_day_p,sep=" "),sep="\n")
			#每周
			cat(paste("#########################对训练效果的评价：每周#######################",
			paste("训练数据的每周平均命中数=",stat_train_week_mean," ; 训练数据的每周平均案件数=",crime_train_week_mean,sep=""),
			paste("训练数据的每周命中率的方差=",stat_train_week_var," ; 标准差=",sqrt(stat_train_week_var),sep=""),
			paste("训练数据的每周案件数的方差=",crime_train_week_var,";标准差=",sqrt(crime_train_week_var),sep=""),
			sep="\n"));cat("\n      训练数据的每周命中率统计概要:",paste("#####",names(stat_train_week_p),"=",stat_train_week_p,sep=" "),sep="\n")
			#每月
			cat(paste("#########################对训练效果的评价：每月#######################",
			paste("训练数据的每月平均命中数=",stat_train_month_mean," ; 训练数据的每月平均案件数=",crime_train_month_mean,sep=""),
			paste("训练数据的每月命中率的方差=",stat_train_month_var," ; 标准差=",sqrt(stat_train_month_var),sep=""),
			paste("训练数据的每月案件数的方差=",crime_train_month_var,";标准差=",sqrt(crime_train_month_var),sep=""),
			sep="\n"));cat("\n      训练数据的每月命中率统计概要:",paste("#####",names(stat_train_month_p),"=",stat_train_month_p,sep=" "),sep="\n")
			return(par_p)
			rm(list=ls())
			gc(reset=TRUE)
		}
		if(isnotbaseP==F & print_train==T){
			####打印输出结果
			##每天
			cat(paste(paste("datasource is",out_data_model_name,"; model_name is",model_name,sep=" "),
			"#########################对训练效果的评价：每天#######################",
			paste("训练数据的总记录数:",nrowtrain_data,paste("; 训练数据的起止日期:",mindate_train_data,"::",maxdate_train_data,sep=""),sep=" "),
			paste("训练数据的每日平均命中数=",stat_train_day_mean," ; 训练数据的每日平均案件数=",crime_train_day_mean,sep=""),
			paste("训练数据的每日命中率的方差=",stat_train_day_var," ; 标准差=",sqrt(stat_train_day_var),sep=""),
			paste("训练数据的每日案件数的方差=",crime_train_day_var,";标准差=",sqrt(crime_train_day_var),sep=""),
			paste("训练数据的总命中数=",stat_train_count," ; 训练数据的总案件数=",stat_train_crime," ; 训练数据的总命中率=",stat_train_count/stat_train_crime,sep=""),
			sep="\n"));cat("\n      训练数据的每日命中率统计概要:",paste("#####",names(stat_train_day_p),"=",stat_train_day_p,sep=" "),sep="\n")
			#每周
			cat(paste("#########################对训练效果的评价：每周#######################",
			paste("训练数据的每周平均命中数=",stat_train_week_mean," ; 训练数据的每周平均案件数=",crime_train_week_mean,sep=""),
			paste("训练数据的每周命中率的方差=",stat_train_week_var," ; 标准差=",sqrt(stat_train_week_var),sep=""),
			paste("训练数据的每周案件数的方差=",crime_train_week_var,";标准差=",sqrt(crime_train_week_var),sep=""),
			sep="\n"));cat("\n      训练数据的每周命中率统计概要:",paste("#####",names(stat_train_week_p),"=",stat_train_week_p,sep=" "),sep="\n")
			#每月
			cat(paste("#########################对训练效果的评价：每月#######################",
			paste("训练数据的每月平均命中数=",stat_train_month_mean," ; 训练数据的每月平均案件数=",crime_train_month_mean,sep=""),
			paste("训练数据的每月命中率的方差=",stat_train_month_var," ; 标准差=",sqrt(stat_train_month_var),sep=""),
			paste("训练数据的每月案件数的方差=",crime_train_month_var,";标准差=",sqrt(crime_train_month_var),sep=""),
			sep="\n"));cat("\n      训练数据的每月命中率统计概要:",paste("#####",names(stat_train_month_p),"=",stat_train_month_p,sep=" "),sep="\n")
			rm(list=ls())
			gc(reset=TRUE)
		}
		else{
			rm(list=ls())
			gc(reset=TRUE)	
		}
	}

	

##评价测试数据的命中率（输出相关统计量）
	evaluate_model_test <- function(k=topN,model_name){
		##对测试数据的基本描述
		nrowtest_data <- nrow(test_data)
		mindate_test_data <- min(test_data$date_str)
		maxdate_test_data <- max(test_data$date_str)

		test_data$pred <- predict(model_name,test_data,type="response")
		test_data <- test_data[,c("date_str","grid_num","crime_num","crime_bi","pred")]
		gc(reset=TRUE)

		##计算每日发生总案件数
		day_count_test <- aggregate(test_data$crime_num,list(test_data$date_str),sum)
		names(day_count_test)[2] <- "total_crime_day"
		day_count_test <- as.data.frame(day_count_test)
		
		##构造函数,提取命中率最高的前k个格子
		myhead <- as.function(alist(x=,n=k,sum(head(x,n))))

		if(isnotbaseP==T){
			test_data <- merge(test_data,crime_data_p,by=c("date_str","grid_num","crime_num","crime_bi"))
		rm(crime_data_p)
		gc(reset=TRUE)
			par_p <- evaluate_model_train(topN,model_name,F)
			test_data$combin_p <- rowSums(t(t(test_data[,6:ncol(test_data)])*par_p))
			test_data <- test_data[order(test_data$date_str,-test_data$combin_p),]
			day_top_test <- aggregate(test_data$crime_num,list(test_data$date_str),myhead)
			names(day_top_test) <- c("Group.2","top_n_day")
			day_top_test <- as.data.frame(day_top_test)	
		}
		if(isnotbaseP==F){
		##对提取出的前k个格子对应的实际发生案件数进行汇总求和
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

	rm(day_count_test,day_top_test,day_score_test,week_count_test,week_top_test,week_score_test,month_count_test,month_top_test,month_score_test)
	gc(reset=TRUE)

	####打印输出结果

	##每天
		cat(paste(paste("datasource is",out_data_model_name,"; model_name is",model_name,sep=" "),
		"#########################对测试效果的评价：每天#######################",
		paste("测试数据的总记录数:",nrowtest_data,paste("; 测试数据的起止日期:",mindate_test_data,"::",maxdate_test_data,sep=""),sep=" "),
		paste("测试数据的每日平均命中数=",stat_test_day_mean," ; 测试数据的每日平均案件数=",crime_test_day_mean,sep=""),
		paste("测试数据的每日命中率的方差=",stat_test_day_var," ; 标准差=",sqrt(stat_test_day_var),sep=""),
		paste("测试数据的每日案件数的方差=",crime_test_day_var,";标准差=",sqrt(crime_test_day_var),sep=""),
		paste("测试数据的总命中数=",stat_test_count," ; 测试数据的总案件数=",stat_test_crime," ; 测试数据的总命中率=",stat_test_count/stat_test_crime,sep=""),
		sep="\n"));cat("\n      测试数据的每日命中率统计概要:",paste("#####",names(stat_test_day_p),"=",stat_test_day_p,sep=" "),sep="\n")
	#每周
		cat(paste("#########################对测试效果的评价：每周#######################",
		paste("测试数据的每周平均命中数=",stat_test_week_mean," ; 测试数据的每周平均案件数=",crime_test_week_mean,sep=""),
		paste("测试数据的每周命中率的方差=",stat_test_week_var," ; 标准差=",sqrt(stat_test_week_var),sep=""),
		paste("测试数据的每周案件数的方差=",crime_test_week_var,";标准差=",sqrt(crime_test_week_var),sep=""),
		sep="\n"));cat("\n      测试数据的每周命中率统计概要:",paste("#####",names(stat_test_week_p),"=",stat_test_week_p,sep=" "),sep="\n")
	#每月
		cat(paste("#########################对测试效果的评价：每月#######################",
		paste("测试数据的每月平均命中数=",stat_test_month_mean," ; 测试数据的每月平均案件数=",crime_test_month_mean,sep=""),
		paste("测试数据的每月命中率的方差=",stat_test_month_var," ; 标准差=",sqrt(stat_test_month_var),sep=""),
		paste("测试数据的每月案件数的方差=",crime_test_month_var,";标准差=",sqrt(crime_test_month_var),sep=""),
		sep="\n"));cat("\n      测试数据的每月命中率统计概要:",paste("#####",names(stat_test_month_p),"=",stat_test_month_p,sep=" "),sep="\n")
	}

####对评价输出进行选择,是否输出训练数据的评价汇总
	if(print_train==F){
		evaluate_model_test(k=topN,model_name1)

		evaluate_model_test(k=topN,model_name2)

		evaluate_model_test(k=topN,model_name3)

		#其他model

		#其他model
	}
	if(print_train==T){
		evaluate_model_train(k=topN,model_name1)

		evaluate_model_train(k=topN,model_name2)

		evaluate_model_train(k=topN,model_name3)

		#其他model

		#其他model


		evaluate_model_test(k=topN,model_name1)

		evaluate_model_test(k=topN,model_name2)

		evaluate_model_test(k=topN,model_name3)

		#其他model

		#其他model
	}
}

cat("#####已成功加载命中率评估函数#####
#####可传入的参数说明：调用函数evaluate_p(in_model_site,in_data_model_name,topN,print_train=F)
  in_model_site:          导入数据和模型的文件的路径,如：'c:\\xx\\xxx'  或  'c:/xx/xxx'
  in_data_model_name:     导入数据和模型的R文件名,如：'207_data_model'
  in_data_p_site:         导入带基础概率的文件的路径
  in_data_p_name:         导入带基础概率的文件名
  topN:                   每日推送格子数
  isnotbaseP:             是否加基础概率,默认加基础概率(有组合的基础概率)
  print_train:            是否打印输出训练数据的评价汇总,默认是否
############################################################",sep="\n")   
	

evaluate_p(in_model_site,in_data_model_name,in_data_p_site,in_data_p_name,topN,isnotbaseP,print_train=F)


