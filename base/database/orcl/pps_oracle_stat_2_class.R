                        ######################################
                        ##########  不分班次,生成矩阵 ###########
                        ######################################
####构造函数:case_stat####

####参数说明###
#Arguments:【id】派出所编号[PD_ID]
#Arguments:【start_time】选取案件数据的起始时间[格式:20130101]
#Arguments:【start_hour】分班次的开始时间,以数字表示,24小时计数
#Arguments:【end_hour】分班次的结束时间,以数字表示,24小时计数
#Arguments:【gd_size】格子大小[50,100,150,200]
#Arguments:【layer】向外扩展层数,1<=layer<=3
#Arguments:【method】method=1九宫格方法取相邻案件,method=2中心距离方法取相邻案件
#Arguments:【file_path】保存数据到本地的路径,建议采用格式："c:/xx/xxx/"

#Arguments:【move】向前推移的天数,可以传入向量生成多列----------------[包含关系][格式：move=c(2,5)]
#Arguments:【move_cut】向前推移的有跨度的天数,可以传入向量生成多列------[间隔关系][格式：move_cut=c(2,3)等价于move=c(2,5)]
############默认都为NULL,但二者不能同时为空,且不能同时赋值


case_stat_class <- function(id,start_date,start_hour,end_hour,discon=F,gd_size,layer,method=1,file_path,move=NULL,move_cut=NULL
                      #,char="is not null"
                      ){
library(plyr)

#设定[全局]有效数字位数
options(digits=10)

#导入连接数据库的加载包：DBI,ROracle
library(DBI)
library(ROracle)

#打开数据库服务器驱动
drv <- dbDriver("Oracle")

#连接oracle数据库,username="szgadba",password="Peptalk123"
#host <- "10.15.100.104"
#port <- 1521
#svc <- "orcl"
#connect.string <- paste(
#"(DESCRIPTION=",
#"(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
#"(CONNECT_DATA=(SERVICE_NAME=", svc, ")))", sep = "")

con <- dbConnect(drv,"szgadba","Peptalk123"
               #    ,dbname = "P77"
                 )
        #####dr <- dbReadTable(con, "PPS_CRIME_DATA")
        #####head(dr)###直接查看数据表PPS_CRIME_DATA

    #print(ls())###R自定义函数的参数是提前传入到计算环境的

    if(is.numeric(id)==FALSE) stop("ERROR:please make sure id is numeric")
    if(is.numeric(start_date)==FALSE | nchar(start_date)!=8) stop("ERROR:please make sure start_date like 20140101")
    if(is.numeric(gd_size)==FALSE | length(gd_size)!=1 | (gd_size %in% c(50,100,150,200))==FALSE) stop("ERROR:please make sure gd_size is numeric and is in the vector(50,100,150,200)")
    if(is.character(file_path)==F) stop("ERROR:please make sure file_path is character") 
    if((layer %in% 1:3)==FALSE) stop("ERROR:please make sure 1<=layer<=3")
    if(is.null(move)==TRUE & is.null(move_cut)==TRUE) stop("ERROR:please input either of the arguments(move,move_cut) as a vector")
    if(is.null(move)==FALSE & is.null(move_cut)==FALSE) stop("ERROR:one of the arguments(move,move_cut) must equal NULL" )
 
    ###导入某个派出所的案情数据,自定义起始时,
    ###分班次
    type_id <- "(011705,011710,011712,011713,011714,011715,011716,011717,011718,011719,011790,020201,020204,
               020205,020206,020207,020210,020211,020212,020213,020290,011700,020200,011711,020208,011701,011702,
	       011703,020202,020203,020209,011707,011708,011709,011706,011704,011600,011601,011602,011603,011604,
	       011605,011606,011607,011608,011609,011610,011611,011612,011613,011614,011615,011616,011617,011618,
	       011619,011620,011621,011622,011690,011900,011901,011902,011903,011904,011905,011906,011907,011908,
	       011990,010801,011800,011801,011802,011808,011809,011812,011813,011814,011815,020300,020301,020302,
	       020304,020305,020306,020308,020309,020310,011810,011811,020800,020890)"

    if(discon==F){
        cd <- dbGetQuery(con,paste("select to_date(substr(crime_time,1,8),'yyyy-mm-dd') as crime_date,ref_id,type_id,
                              to_char(wgs84_x) wgs84_x,to_char(wgs84_y) wgs84_y from pps_crime_data where pd_id=",id,
			      "and type_id in",type_id,
			      "and to_date(substr(crime_time,1,8),'yyyy-mm-dd') >= ","to_date('",start_date,"','yyyy-mm-dd')",
			      "and",start_hour," <= substr(crime_time,9,2)","and substr(crime_time,9,2) < ",end_hour,sep=" "))
    }
    if(discon==T){
        cd <- dbGetQuery(con,paste("select to_date(substr(crime_time,1,8),'yyyy-mm-dd') as crime_date,ref_id,type_id,
                              to_char(wgs84_x) wgs84_x,to_char(wgs84_y) wgs84_y from pps_crime_data where pd_id=",id,
			      "and type_id in",type_id,
			      "and to_date(substr(crime_time,1,8),'yyyy-mm-dd') >=","to_date('",start_date,"','yyyy-mm-dd')",
			      "and","(( substr(crime_time,9,2) <= 24","and",start_hour," <= substr(crime_time,9,2))",
			      "or","( 0 <= substr(crime_time,9,2)","and substr(crime_time,9,2) <",end_hour,"))",sep=" "))    
    }

    ###将POSIX*格式的时间改为Date类型[class()查看]
    cd$CRIME_DATE <- as.Date(as.character(cd$CRIME_DATE),"%Y-%m-%d")

    ###导入某个派出所,不同格子大小的经纬度坐标数据 
    grid_data <- function(gd_size,id,char="is not null") {
         grid_size <- dbGetQuery(con, paste("select grid_num,to_char(p1_x) p1_x,to_char(p1_y) p1_y,to_char(p2_x) p2_x ,to_char(p2_y) p2_y,
                is_buffer from",paste("grid_info_",gd_size,"m_wgs84",sep=""),"where pd_id =",id,"and is_buffer",char,sep=" "))
    return(grid_size)
    }
    ###调用grid_data函数,赋予对象gd    
    gd <- grid_data(gd_size,id) 

    ###重写grid_data函数,只提取出不包含缓冲区的格子,目的：减少匹配次数
    grid_data_0 <- as.function(alist(gd_size=,id=,char="=0",grid_data(gd_size,id,char)))
    gd0 <- grid_data_0(gd_size,id)

    #######################################################################################################
    ###将案件匹配到格子中
    ###添加列GRID_NUM到数据集cd
    cd$GRID_NUM <- NA
    for(i in 1:nrow(cd)){
        index <- which(cd$WGS84_X[i] >= gd0$P1_X & cd$WGS84_X[i] <= gd0$P2_X &
                     cd$WGS84_Y[i] >= gd0$P2_Y & cd$WGS84_Y[i] <= gd0$P1_Y)
        if(length(index)==0) cd$GRID_NUM[i] <- NA        
        if(length(index)>=1) cd$GRID_NUM[i] <- gd0[index[1],"GRID_NUM"]
    }#sum(is.na(cd$GRID_NUM))#229个案件没有匹配到格子---娄葑20130101->20140929
 
    ###清理内存
    rm(list=ls()[-which(ls() %in% c("cd","gd","id","start_date","gd_size","layer","method","file_path","move","move_cut"))])
    gc(reset=TRUE)

    #######################################################################################################
    ###计算该天本格子的案件数 
    stat_grid_0 <- aggregate(cd$REF_ID,list(date_str=cd$CRIME_DATE,grid_num=cd$GRID_NUM),length)
    names(stat_grid_0)[3] <- "layer0"
    stat_grid_0$crime_bi <- ifelse(stat_grid_0$layer0>=1,1,0)
    stat_grid_0 <- stat_grid_0[,c(1,2,4,3)]

    #######################################################################################################
    ###该天本格子以及外层(k=1,2,……)案件数 
    if(method==1){
    fun <- function(k){
        ###增加本格子周围k层的经纬度坐标
        gd$lay_x1 <- as.double(gd$P1_X)-k*(as.double(gd$P2_X)-as.double(gd$P1_X))
        gd$lay_y1 <- as.double(gd$P1_Y)+k*(as.double(gd$P1_Y)-as.double(gd$P2_Y))
        gd$lay_x2 <- as.double(gd$P2_X)+k*(as.double(gd$P2_X)-as.double(gd$P1_X))
        gd$lay_y2 <- as.double(gd$P2_Y)-k*(as.double(gd$P1_Y)-as.double(gd$P2_Y))
        names(gd)[7:10] <- c(paste("lay",k,"_x1",sep=""),paste("lay",k,"_y1",sep=""),paste("lay",k,"_x2",sep=""),paste("lay",k,"_y2",sep=""))
 
        #cd <- join(cd,gd[,c(1,7:10)],by=c("GRID_NUM"),type="inner")

        g_day <- NA
        for(i in 1:nrow(gd)){
            TF <- gd[i,paste("lay",k,"_x1",sep="")] <= cd$WGS84_X & cd$WGS84_X <= gd[i,paste("lay",k,"_x2",sep="")] &
                  gd[i,paste("lay",k,"_y2",sep="")] <= cd$WGS84_Y & cd$WGS84_Y <= gd[i,paste("lay",k,"_y1",sep="")]      
            xx <- cbind(cd[,c("CRIME_DATE","GRID_NUM")],TF)
            xx_day <-aggregate(xx[,3],list(xx[,1]),FUN="sum") 
            g_day <- rbind(t(t(g_day)),t(t(xx_day[,2])))               
        }
        gn <- rep(gd$GRID_NUM,each=nrow(xx_day))
        date <- rep(xx_day[,1],length(gd$GRID_NUM))
        stat_grid_k<- data.frame(date_str=date,grid_num=gn,layer=g_day[-1,])
        names(stat_grid_k)[3] <- paste("layer",k,sep="")
        return(stat_grid_k)  
    }
    }

    if(method==2){
     fun <- function(k){
        ###增加本格子周围k层的经纬度坐标
        gd$lay_x1 <- as.double(gd$P1_X)-k*(as.double(gd$P2_X)-as.double(gd$P1_X))
        gd$lay_y1 <- as.double(gd$P1_Y)+k*(as.double(gd$P1_Y)-as.double(gd$P2_Y))
        gd$lay_x2 <- as.double(gd$P2_X)+k*(as.double(gd$P2_X)-as.double(gd$P1_X))
        gd$lay_y2 <- as.double(gd$P2_Y)-k*(as.double(gd$P1_Y)-as.double(gd$P2_Y))
        names(gd)[7:10] <- c(paste("lay",k,"_x1",sep=""),paste("lay",k,"_y1",sep=""),paste("lay",k,"_x2",sep=""),paste("lay",k,"_y2",sep=""))
 
        #cd <- join(cd,gd[,c(1,7:10)],by=c("GRID_NUM"),type="inner")

        g_day <- NA
        for(i in 1:nrow(gd)){
            TF <- (gd[i,paste("lay",k,"_x1",sep="")] <= cd$WGS84_X & cd$WGS84_X <= gd[i,paste("lay",k,"_x2",sep="")] &
                  gd[i,paste("lay",k,"_y2",sep="")] <= cd$WGS84_Y & cd$WGS84_Y <= gd[i,paste("lay",k,"_y1",sep="")]) | 
		  ((gd[i,paste("lay",k,"_x1",sep="")]-(as.double(gd[i,"P2_X"])-as.double(gd[i,"P1_X"]))) <= cd$WGS84_X & cd$WGS84_X <= gd[i,paste("lay",k,"_x1",sep="")] &
                  gd[i,"P2_Y"] <= cd$WGS84_Y & cd$WGS84_Y <= gd[i,"P1_Y"]) | 
		  (gd[i,paste("lay",k,"_x2",sep="")] <= cd$WGS84_X & cd$WGS84_X <= (gd[i,paste("lay",k,"_x2",sep="")]+(as.double(gd[i,"P2_X"])-as.double(gd[i,"P1_X"]))) &
                  gd[i,"P2_Y"] <= cd$WGS84_Y & cd$WGS84_Y <= gd[i,"P1_Y"]) |
		  (gd[i,paste("lay",k,"_y1",sep="")] <= cd$WGS84_Y & cd$WGS84_Y <= (gd[i,paste("lay",k,"_y1",sep="")]+(as.double(gd[i,"P1_Y"])-as.double(gd[i,"P2_Y"]))) &
                  gd[i,"P1_X"] <= cd$WGS84_X & cd$WGS84_X <= gd[i,"P2_X"]) | 
		  ((gd[i,paste("lay",k,"_y2",sep="")]-(as.double(gd[i,"P1_Y"])-as.double(gd[i,"P2_Y"]))) <= cd$WGS84_Y & cd$WGS84_Y <= gd[i,paste("lay",k,"_y2",sep="")] &
                  gd[i,"P1_X"] <= cd$WGS84_X & cd$WGS84_X <= gd[i,"P2_X"])	
            xx <- cbind(cd[,c("CRIME_DATE","GRID_NUM")],TF)
            xx_day <-aggregate(xx[,3],list(xx[,1]),FUN="sum") 
            g_day <- rbind(t(t(g_day)),t(t(xx_day[,2])))               
        }
        gn <- rep(gd$GRID_NUM,each=nrow(xx_day))
        date <- rep(xx_day[,1],length(gd$GRID_NUM))
        stat_grid_k<- data.frame(date_str=date,grid_num=gn,layer=g_day[-1,])
        names(stat_grid_k)[3] <- paste("layer",k,sep="")
        return(stat_grid_k)  
    }
    }

    #######################################################################################################
    ###案件数合并,输出stat表和写出文件
    #if(layer>3 | layer<0) print("ERROR:please make sure 1<=layer<=3")
    if(layer==3) {abc <- cbind(fun(1),layer2=fun(2)[,3],layer3=fun(3))
        stat <- join(stat_grid_0,abc,by=c("date_str","grid_num"),type="left")
        ###去掉包括本格的案件数:
        #1.将全部的NA值替换为0
        #stat[is.na(stat)]<-0
        #2.将外层的非0值与本格子作差
        #stat$layer1 <- ifelse(stat$layer1!=0,stat$layer1-stat$layer0,0)
        #stat$layer2 <- ifelse(stat$layer2!=0,stat$layer2-stat$layer0,0)
        #stat$layer3 <- ifelse(stat$layer3!=0,stat$layer3-stat$layer0,0)
        ###外层去除本格子
        stat$layer0_1 <- stat$layer1-stat$layer0
        stat$layer0_2 <- stat$layer2-stat$layer0
        stat$layer0_3 <- stat$layer3-stat$layer0
        ###外层环内
        stat$layer1_2 <- stat$layer2-stat$layer1
        stat$layer2_3 <- stat$layer3-stat$layer2
    }
    if(layer==2) {ab <- cbind(fun(1),layer2=fun(2)[,3])
        stat <- join(stat_grid_0,ab,by=c("date_str","grid_num"),type="left")
        #stat$layer1 <- ifelse(stat$layer1!=0,stat$layer1-stat$layer0,0)
        #stat$layer2 <- ifelse(stat$layer2!=0,stat$layer2-stat$layer0,0)  
        ###外层去除本格子
        stat$layer0_1 <- stat$layer1-stat$layer0
        stat$layer0_2 <- stat$layer2-stat$layer0
        ###外层环内
        stat$layer1_2 <- stat$layer2-stat$layer1
    }
    if(layer==1) {stat <- join(stat_grid_0,layer1=fun(1)[,3],by=c("date_str","grid_num"),type="left")       
        #stat$layer1 <- ifelse(stat$layer1!=0,stat$layer1-stat$layer0,0)
        ###外层去除本格子,外层环内
        stat$layer0_1 <- stat$layer1-stat$layer0
    }
    stat <- stat[order(stat$date_str,stat$grid_num),]
    names(stat)[4] <- "crime_num"
    write.csv(stat,file=paste(file_path,id,"_",start_date,"_",gd_size,"_",layer,"_",method,"_class",".csv",sep=""),row.names=F)
    names(stat)[4] <- "layer0"

    ###清理内存
    rm(list=ls()[-which(ls() %in% c("cd","stat","id","start_date","gd_size","layer","method","file_path","move","move_cut"))])
    gc(reset=TRUE)

    #######################################################################################################
    ###对数据(本格子、外层格子)向前推m天,##训练数据会丢失掉前m天
    move_m <- function(x,m){  
        temp_date <- data.frame(date_str=seq(min(cd$CRIME_DATE),max(cd$CRIME_DATE),by=1))
        temp <- join(x[,-2],temp_date,by="date_str",type="full")
        temp[is.na(temp)]<-0
        tp <- temp
        tp_out <- tp
        for(j in m){
            for(i in (j+1):nrow(temp_date)){
                temp[i,-c(1:2)] <-  colSums(tp[(i-j):(i-1),-c(1:2)])
#                names(temp)[3:ncol(temp)] <- paste(names(tp)[3:ncol(tp)],"_move",j,sep="")
            }
	    names(temp)[3:ncol(temp)] <- paste(names(tp)[3:ncol(tp)],"_move",j,sep="")
            tp_out <-cbind(tp_out,temp[,-c(1:2)]) 
        }
        #tp_out <- tp_out[-(1:max(m)),] 
	rm(temp,tp)
	gc()
        return(tp_out)   
    }

    #move=c(2,5)表示累积向前推移2天,推移5天,5天包括2天
    #move=cumsum(move_cut)

    #if(is.null(move)==TRUE & is.null(move_cut)==TRUE) print("ERROR:please input either of the arguments(move,move_cut) as a vector")
    #if(is.null(move)==FALSE & is.null(move_cut)==FALSE) print("ERROR:one of the arguments(move,move_cut) must equal NULL" )
    
    if(is.null(move)==FALSE & is.null(move_cut)==TRUE){
        ###推移存在包含关系   
        func <- as.function(alist(x=,m=move,move_m(x,m)))

        stat_move=ddply(stat,.(grid_num=as.factor(grid_num)),func)

	stat_move <- stat_move[which(stat_move$date_str > (min(stat_move$date_str)+max(move_cut))),]
	stat_move$date_str <- as.numeric(paste(substr(stat_move$date_str,1,4),substr(stat_move$date_str,6,7),substr(stat_move$date_str,9,10),sep=""))
        names(stat_move)[4] <- "crime_num"
	write.csv(stat_move,file=paste(file_path,id,"_",start_date,"_",gd_size,"_",layer,"_",method,"_move",".csv",sep=""),row.names=F)

        ###清理内存
        #rm(list=ls()[-which(ls() %in% c("move","move_cut","stat_move"))])
        #gc(reset=TRUE)

        #或者如下,这样做方便调用主体函数时可继续分析数据集：stat
        rm(list=ls()[-which(ls() %in% c("stat","move","move_cut","stat_move"))]) 
        gc(reset=TRUE)       
    }
    if(is.null(move)==TRUE & is.null(move_cut)==FALSE){
        ###推移存在间断关系
        func <- as.function(alist(x=,m=cumsum(move_cut),move_m(x,m)))

        stat_move=ddply(stat,.(grid_num=as.factor(grid_num)),func)
        
        for(i in (1+3+3*layer):(ncol(stat_move)-layer*3)){
            stat_move[,i+layer*3] <- stat_move[,i+layer*3]-stat_move[,i]   
        }
        names(stat_move)[(4+3*layer):ncol(stat_move)] <- paste(strsplit(names(stat_move)[(4+3*layer):ncol(stat_move)],"move\\d+"),
                                                                paste("move_cut",rep(move_cut,each=3*layer),sep=""),sep="")
        ####                                           sub("move\\d+",
        ####                                           paste("move_cut",move_cut,sep=""),
        ####                                           names(stat_move)[(3+3*layer):ncol(stat_move)])
 
	stat_move <- stat_move[which(stat_move$date_str > (min(stat_move$date_str)+max(cumsum(move_cut)))),]
	stat_move$date_str <- as.numeric(paste(substr(stat_move$date_str,1,4),substr(stat_move$date_str,6,7),substr(stat_move$date_str,9,10),sep=""))
        names(stat_move)[4] <- "crime_num"
	write.csv(stat_move,file=paste(file_path,id,"_",start_date,"_",gd_size,"_",layer,"_",method,"_move_cut",".csv",sep=""),row.names=F)
       
        ###清理内存
        #rm(list=ls()[-which(ls() %in% c("move","move_cut","stat_move"))]) 
        #gc(reset=TRUE)

        #或者如下,这样做方便调用主体函数时可继续分析数据集：stat
        rm(list=ls()[-which(ls() %in% c("stat","move","move_cut","stat_move"))])
        gc(reset=TRUE)
    }

    #######################################################################################################
    ###主体函数返回输出运算结果数据集
    #return(stat_move)
    #或者如下
##    return(list(stat=stat,stat_move=stat_move))   
rm(list=ls())
gc(reset=TRUE)
}

cat("#####已成功加载生成矩阵函数,调用函数case_stat,并赋给一个对象,这个输出对象是是一个列表类型,
     该列表内部的两个对象(数据框)分别是stat,stat_move,其中stat是某天某格子发生案情的记录数据,
     stat_move是每天每个格子向前推移一些天数后统计出的数据.
#####可传入的参数说明：函数调用case_stat(id,start_date,start_hour,end_hour,discon=F,gd_size,layer,method=1,file_path,move=NULL,move_cut=NULL)
     id:                    派出所编号[PD_ID]
     start_date:            选取案件数据的起始时间[格式如:20130101],注:起始时间要尽可能远以便剪掉推移损失天数
     start_hour:            分班次的开始时刻,以小时表示
     end_hour:              分班次的结束时刻,以小时表示
     discon:                是否是间断时间的班次,如:[start_hour=20--到--end_hour=次日6)
     gd_size:               格子大小[如:50,100,150,200]
     layer:                 向外扩展层数,1<=layer<=3
     method:                取相邻案件的方法：method=1九宫格;method=2中心距离
     file_path:             保存文件的路径,格式如,'c:/xx/xxx/'
     move:                  向前推移的天数,可以传入向量生成多列-------------[包含关系][格式：move=c(2,5)]
     move_cut:              向前推移的有跨度的天数,可以传入向量生成多列-----[间隔关系][格式：move_cut=c(2,3)等价于move=c(2,5)]
                            ##默认都为NULL,但二者不能同时为空,且不能同时赋值
#######################################################",sep="\n")     
#例如:crime_data_class_id <- case_stat_class(207,20130101,6,14,F,200,2,1,"Q:/orcl_pps/",move=c(2,5),move_cut=NULL)

#娄葑

#crime_data_class_1 <- case_stat_class(207,20140101,6,14,F,200,2,1,"Q:/project_pps_R/orcl_pps/",move=NULL,move_cut=c(3,2,3,4,5))


