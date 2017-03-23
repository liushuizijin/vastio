######################     run model and evaluate p  ##############################
run_model <- function(in_data_site,in_data_name,model_site,out_data_model_name){
##加载已拆分的数据文件
	load(paste(in_data_site,in_data_name,".RData",sep=""),.GlobalEnv)
##运行不同模型
	####全变量模型
	logit <- glm(crime_bi~.-date_str-grid_num-crime_num,data=train_data,family=binomial(link="logit"))

	####step筛选变量模型
	logit_step <- step(logit,trace=0)

	####干掉step筛选变量模型
	p_frame <- as.matrix(summary(logit)$coef)
	#drop_var <- names(which(p_frame[,4]>=alpha))
	drop_var <- names(which(p_frame[,4]>=0.1))
	drop_col_index <- which(names(train_data) %in% drop_var)
	if(length(drop_var)==0) fmla <- as.formula(paste("crime_bi ~ ",paste(names(train_data[,-c(1:4)]),collapse= "+")))
	if(length(drop_var)>=1) fmla <- as.formula(paste("crime_bi ~ ",paste(names(train_data[,-c(1:4,drop_col_index)]),collapse= "+")))
	logit_nostep <- glm(fmla,data=train_data,family=binomial(link="logit"))

	#### 其他模型 ####
	#
	# 代码块 
	#
	##################


	train_data <- train_data[,1:4]
	gc(reset=TRUE)

save(train_data,test_data,logit,logit_step,logit_nostep,other_model,,,,file=paste(model_site,out_data_model_name,".RData",sep=""))

rm(list=ls())
gc(reset=TRUE)
}


cat("#####已成功加载模型函数#####
#####可传入的参数说明：调用函数run_model(in_data_site,in_data_name,model_site,out_data_model_name
  in_data_site:          导入划分好的数据集所在的文件路径,如：'c:\\xx\\xxx'  或  'c:/xx/xxx' 
  in_data_name:          导入划分好的数据集的R文件名,如：'207_train_test_data'
  model_site:            保存数据和模型的文件路径,格式同in_data_site
  out_data_model_name:   保存数据和模型的文件名,如：'207_data_model' 
############################################################",sep="\n")   


run_model(in_data_site,in_data_name,model_site,out_data_model_name)



