#Base-Probility-PPS

baseP_stat <- function(file_site,file_name,len_day=NULL,len_day_cut=NULL,out_data_p_site,out_data_p_name){
	
	####报错,程序终止提示：################
        if(is.character(file_site)==F | is.character(file_name)==F | is.character(out_data_p_site)==F | is.character(out_data_p_name)==F) 
	   stop("ERROR:file_site,file_name,out_data_p_site,out_data_p_name must be a character")
	if(is.null(len_day) & is.null(len_day_cut)) stop("ERROR:please make sure one of the arguments(len_day,len_day_cut) is not null")

	#######################################

	##读入数据,数据格式：[.txt],.csv,无表头(字段、变量名)------具体使用时需修改
	crime_data_p <- read.table(paste(file_site,file_name,".csv",sep=""),header=T,sep=",")

	# names(crime_data_p)=c('date_str', 'grid_num', 'crime_num', 'crime_bi', 'same_crime', 'human_trace', 'bank', 'temporal_spatial_0_0', 
	#          'temporal_spatial_0_1', 'temporal_spatial_1_0', 'temporal_spatial_1_1', 'temporal_spatial_2_0', 'temporal_spatial_2_1',
	#          'temporal_spatial_3_0', 'temporal_spatial_3_1', 'temporal_spatial_4_0', 'temporal_spatial_4_1')
	###########	
		    
	crime_data_p <- crime_data_p[,1:4]
	gc(reset=T)

	crime_data_p$date_str <- as.Date(as.character(crime_data_p$date_str),"%Y%m%d")
	crime_data_p <- crime_data_p[order(crime_data_p$date_str,crime_data_p$grid_num),]

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
		    x[i,which(len==j)+ncol(x)-length(len)] <- sum(x[((i-1):(i-j)),"crime_bi"])       
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

save(crime_data_p,file=paste(out_data_p_site,out_data_p_name,".RData",sep=""))

	#return(crime_data_p)
rm(list=ls())
gc(reset=TRUE)
}

cat("#####已成功加载生成多列基础概率函数#####
#####可传入的参数说明：调用函数baseP_stat(file_site,file_name,len_day=NULL,len_day_cut=NULL,out_data_p_site,out_data_p_name)
  file_site:             读入生成矩阵数据的文件路径,'d:/xx/xxx' 或 'd:\\xx\\xxx'(字符串)
  file_name:             生成矩阵数据的文件名,'xxxx'(字符串)----##统一数据格式为.csv
  len_day:               计算基础概率时连续向前推移天数,可传入向量,如c(1,2,3)----包含关系
  len_day_cut:           计算基础概率时间断向前推移天数,可传入向量------------分段关系
                         ##len_day,len_day_cut二者不能同时为空,可同时非空
  out_data_p_site:       输出包含基础概率的数据文件的路径值,格式同file_site
  out_data_p_name：      输出包含基础概率的数据文件名,格式同file_name
############################################################",sep="\n")   
    
##  crime_data_p <- baseP_stat(file_site="q:/",file_name="mudu_class1",len_day=NULL,len_day_cut=c(30,60),out_data_p_site="q:/",out_data_p_name="xxxxxxxxx")
    


