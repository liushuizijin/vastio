############################################################
#   只取手机号，忽略固话号，如下规则按照顺序来计算
#   
#   如果以0开头则去掉0；
#   如果以+0开头则去掉+0；
#   如果以+00开头则去掉+00；
#   如果以+开头则去掉+
#   如果有空格则去掉空格
#   如果有-则去掉-
#
#    13位，如果以86开头去掉86；
#    16位，如果是以10193或17911开头则去掉10193或17911
#    17位，如果以125831，则去掉125831
#    11位，只保留11位长度的记录；去掉不是1开头的则去掉这些记录
#
#   结合需求场景，作支持度和置信度分析，然后结合igraph参照关系网络图给出嫌疑人推荐
#
############################################################
# library(reshape2)
# library(dplyr)
library(plyr)
library(arules)

# zw <- read.csv("E:/WorkSpace/zw2017.csv", header=T, stringsAsFactors=F)
# library(readr)
# zw <- read_csv("E:/WorkSpace/ZhuWang/from_num.csv", col_names=c("FROM_NUM", "P_NUM", "P_NAME", "TO_NUM", "CALL_TIME", "CALL_LEN", "LOCALACTION"), 
#                col_types=list(FROM_NUM = col_character(),
#                              P_NUM = col_character(),
#                               P_NAME = col_character(),
#                               TO_NUM = col_character(),
#                               CALL_TIME = col_character(),
#                               CALL_LEN = col_character(),
#                               LOCALACTION = col_character()))
# 如上还是出错或报警告，数据内部还有问题，忽略！
# zw <- zw[, c("FROM_NUM", "TO_NUM", "CALL_TIME", "LOCALACTION")]
# zw <- read.delim("E:/WorkSpace/ZhuWang/zw_dupin_txl.txt", header=F, sep="\t", stringsAsFactors=F, encoding="UTF-8")
zw <- read.delim("E:/WorkSpace/ZhuWang/zw_dubo_txl.txt", header=F, sep="\t", stringsAsFactors=F, encoding="UTF-8")
names(zw) <- c("FROM_NUM", "TO_NUM", "CALL_TIME", "CALL_LEN", "LOCALACTION")
zw <- zw[, c("FROM_NUM", "TO_NUM", "CALL_TIME", "LOCALACTION")]



zw$FROM_NUM <- sub("(^\\+?0{0,2})|(\\s)|(-)", "", zw$FROM_NUM)
zw$TO_NUM <- sub("(^\\+?0{0,2})|(\\s)|(-)", "", zw$TO_NUM)

keep_index <- which(
    (nchar(zw$FROM_NUM) %in% c(11, 13, 16, 17)) &
    (nchar(zw$TO_NUM) %in% c(11, 13, 16, 17)))
zw <- zw[keep_index, ]

filter_func <- function(col_name, n, gregex_char, start_position){
    keep_index <- which(
        nchar(zw[, col_name]) == n &
        grepl(gregex_char, zw[, col_name]))
    temp <- zw[keep_index, col_name]   
    temp <- substring(temp, start_position, n)
    zw[keep_index, col_name] <- temp
    return(zw)
}
zw <- filter_func("FROM_NUM", 13, "^86", 3)
zw <- filter_func("TO_NUM", 13, "^86", 3)

zw <- filter_func("FROM_NUM", 16, "(^10193)|(^17911)", 6)
zw <- filter_func("TO_NUM", 16, "(^10193)|(^17911)", 6)

zw <- filter_func("FROM_NUM", 17, "^125831", 7)
zw <- filter_func("TO_NUM", 17, "^125831", 7)

keep_index_11 <- which(
    (nchar(zw$FROM_NUM) == 11) &
    (nchar(zw$TO_NUM) == 11) &
    (grepl("^1", zw$FROM_NUM)) &
    (grepl("^1", zw$TO_NUM))
    )
zw <- zw[keep_index_11, ]

zw$CRIME <- ifelse(zw$LOCALACTION == "拨入", zw$TO_NUM, zw$FROM_NUM)
zw$CALL_TIME <- substring(zw$CALL_TIME, 1, 10)

# 过滤掉自己打给自己的记录
zw <- zw[-which(zw$FROM_NUM == zw$TO_NUM), ]


# 自定义观察的起止日期：start_date, end_date
# 聚合统计频率：设为k天，即对每一个CRIME而言每k天为一行记录
# 连续观察的周期数：设为m个
# 所需的数据需要多少天：m*k
# 当最后一个周期的结束日期小于或等于end_date时，也作为一个周期存在
k=5
start_date <- 20160101
end_date <- 20170301

start_date <- as.Date(as.character(start_date), "%Y%m%d")
end_date <- as.Date(as.character(end_date), "%Y%m%d")

breaks <- seq(start_date, end_date, k)
breaks <- c(breaks, end_date)

zw$DATE_GROUP <- as.character(cut(as.Date(zw$CALL_TIME, "%Y-%m-%d"), breaks))
# 添加NA值过滤（日期CALL_TIME存在异常错误）
zw <- na.omit(zw)
crime <- unique(zw$CRIME)  # 犯案人

# 用于关联规则的数据rules_list
rules_list <- dlply(zw,.(DATE_GROUP, CRIME), function(x) union(unique(x$FROM_NUM), unique(x$TO_NUM)))

# =================================支持度分析
# 关联规则：eclat
rules_df <- as(rules_list, "transactions")
itemset_eclat <- eclat(rules_df, parameter=list(support=0.0005, minlen=1, maxlen=20))
itemset_eclat <- sort(itemset_eclat, decreasing=TRUE,by="support")
freq_eclat <- as(itemset_eclat, "data.frame")
freq_eclat$items <- as.character(freq_eclat$items)
freq_eclat$items <- gsub("\\{|\\}", "", freq_eclat$items)

freq_eclat_items <- strsplit(as.character(freq_eclat$items), ",")
intems_count <- lapply(freq_eclat_items, length)
which_eq1 <- which(intems_count == 1)
center_crime_num <- intersect(unlist(freq_eclat_items[which_eq1]), crime)  # 核心犯案人 （核心，指在观察期内高频通话）
center_relation_num <- setdiff(unlist(freq_eclat_items[which_eq1]), crime)  # 核心关系人
if(length(center_crime_num) > 0) {center_crime <- freq_eclat[which_eq1, ]; center_crime <- center_crime[center_crime$items %in% center_crime_num, ]}
if(length(center_relation_num) > 0) {center_relation <- freq_eclat[which_eq1, ]; center_relation <- center_relation[center_relation$items %in% center_relation_num, ]}

which_up1 <- which(intems_count > 1)
if(length(which_up1) > 0) {
    relation_crime_index <- which(unlist(lapply(freq_eclat_items[which_up1], function(x) any(x %in% crime))))
    # relation_relation_index <- which(unlist(lapply(freq_eclat_items[which_up1], function(x) !any(x %in% crime))))
    if(length(relation_crime_index) > 0) relation_crime <- freq_eclat[which_up1[relation_crime_index], ]  # 与犯案人有较强关联的关系人    
    # if(length(relation_relation_index) > 0) relation_relation <- freq_eclat[which_up1[relation_relation_index], ]  # 联系密切的关系人群
}

# 寻找crime之间互联（至少2个crime）
crime_relation_up2_func <- function(n) { # n>=2
    relation_crime_index <- which(unlist(lapply(freq_eclat_items, function(x) sum(x %in% crime) == n)))
    if(length(relation_crime_index) > 0) return(freq_eclat[relation_crime_index, ]) else return("无记录")
}

# crime_relation_up2_func(2)  # 犯案人互联

if(nrow(center_crime) > 0) center_crime <- center_crime[order(-center_crime$support), ]  # 观察期内，产生通讯较多的犯案人
if(nrow(center_relation) > 0) center_relation <- center_relation[order(-center_relation$support), ]  # 观察期内，产生通讯较多的相关人
if(nrow(relation_crime) > 0) relation_crime <- relation_crime[order(-relation_crime$support), ]  # 观察期内，犯案人与相关人联系较多的关联记录
# if(nrow(relation_relation) > 0) relation_relation <- relation_relation[order(-relation_relation$support), ]  # 观察期内，相关人与相关人联系较多的关联记录

# ===========================================

# =================================置信度分析
rules <- ruleInduction(itemset_eclat, rules_df, confidence=0.2)
sub_rules <- subset(rules, subset=lhs %in% crime & !(rhs %in% crime))  # 理解其意义！
sub_rules <- sort(sub_rules, decreasing=T, by=c("confidence", "lift"))
# inspect(head(sub_rules))
sub_rules_df <- as(sub_rules, "data.frame")
sub_rules_df$rules <- as.character(sub_rules_df$rules)
sub_rules_df$rules <- gsub("\\{|\\}|\\s", "", sub_rules_df$rules)

lhs <- lapply(strsplit(sub_rules_df$rules, "=>"), function(x) strsplit(x[1], ",")[[1]])
rhs <- lapply(strsplit(sub_rules_df$rules, "=>"), function(x) strsplit(x[2], ",")[[1]])

crime_relation_up2_func2 <- function(n) {
    crime_n_index <- which(unlist(lapply(lhs, function(x) sum(x %in% crime) == n)))
    crime_for_plot <- intersect(unique(unlist(lhs[crime_n_index])), crime)
    recommendation <- setdiff(unique(unlist(rhs[crime_n_index])), crime)
    if(length(recommendation) > 0) {
        return(list(crime_for_plot=crime_for_plot,recommendation=recommendation))
    } else { return("暂无推荐") }
}

# crime_relation_up2_func2(2)

# ===========================================

# ==========================用igraph进行可视化
# 用于igraph的数据
library(igraph)

relations <- zw[which(as.Date(zw$CALL_TIME, "%Y-%m-%d") >= start_date &
    as.Date(zw$CALL_TIME, "%Y-%m-%d") < end_date), 
    c("FROM_NUM", "TO_NUM")]
relations <- ddply(relations, .(FROM_NUM, TO_NUM), summarise, weight=length(TO_NUM))
relations <- arrange(relations, desc(weight))
# recommendation_all <- setdiff(union(relations$FROM_NUM, relations$TO_NUM), crime)
# recommendation_all <- data.frame(num=recommendation_all)  # 数据框recommendation_all表示嫌疑人与几个犯案人发生了联系
# recommendation_all_count <- apply(recommendation_all, 1, function(x) sum(intersect(relations$FROM_NUM, crime) %in% x) + sum(intersect(relations$TO_NUM, crime) %in% x))
# recommendation_all$count <- recommendation_all_count

vertices <- union(zw$FROM_NUM, zw$TO_NUM)

plot_top <- function(top_n) {
    relations_top <- head(relations, top_n)
    top_vertices <- union(relations_top$FROM_NUM, relations_top$TO_NUM)
    g <- graph_from_data_frame(relations_top, directed=TRUE, vertices=top_vertices)
    crime_top <- intersect(crime, top_vertices)
    relation_top <- setdiff(top_vertices, crime)
    vertex_color <- ifelse(names(V(g)) %in% crime_top, "red", "orange")
    plot(g, vertex.size=degree(g), vertex.color=vertex_color, edge.arrow.size=0.3)  # 参数vertex.color=degree(g)
}

# plot_top(20)  # 查看观察期内联系频次最高的20个链接关系

plot_someones <- function(someones) {
    someones <- as.character(someones)
    g <- graph_from_data_frame(relations, directed=TRUE, vertices=vertices)
    gp <- 0
    for(someone in someones) {
        gg <- make_ego_graph(g, 1, someone)
        gp <- gp + gg[[1]]
    }
    vertex_color <- ifelse(names(V(gp)) %in% crime, "red", "orange")  
    plot(gp, vertex.color=vertex_color, edge.arrow.size=0.3)
}

# plot_someones(as.numeric(crime))  # 查看观察期内某几个联系人的链接关系

# ===========================================

# ===================================推荐结果
# center_crime
# center_relation
# inspect(head(sub_rules))
# crime_relation_up2_func(2)

# 初步推荐
result <- crime_relation_up2_func2(1)  # 2,3,4,5,...
# result$crime_for_plot
# result$recommendation

recommendations <- data.frame(num=result$recommendation)  
recommendations_count <- apply(recommendations , 1, function(x) sum(relations$TO_NUM[relations$FROM_NUM %in% intersect(relations$FROM_NUM, crime)] %in% x) + 
                                                                sum(relations$FROM_NUM[relations$TO_NUM %in% intersect(relations$TO_NUM, crime)] %in% x))
recommendations$count <- recommendations_count
recommendations$num <- as.character(recommendations$num)
recommendations <- arrange(recommendations, desc(count))

# plot_someones(result$crime_for_plot)
# plot_someones(recommendations$num)

# ===========================================

# ===================================看图甄选
# 看图+主观过滤
plot_someones2 <- function(someones) {
    someones <- as.character(someones)
    g <- graph_from_data_frame(relations, directed=TRUE, vertices=vertices)
    gp <- 0
    for(someone in someones) {
        gg <- make_ego_graph(g, 1, someone)
        gp <- gp + gg[[1]]
    }
    vertex_color <- ifelse(names(V(gp)) %in% crime, "red", ifelse(names(V(gp)) %in% result$recommendation, "orange", "green"))  
    plot(gp, vertex.color=vertex_color, edge.arrow.size=0.3, vertex.label=NA)
}

# plot_someones2(result$crime_for_plot)
# plot_someones2(recommendations$num)

# ===========================================

# ===================================图形交互，生成HTML文件
library(visNetwork)
nodes <- data.frame(id=vertices, 
                    group=ifelse(vertices %in% crime, "犯罪人", ifelse(vertices %in% result$recommendation, "嫌疑人", "相关人")),
                    title=vertices,
                    stringsAsFactors=FALSE)
edges <- relations
names(edges) <- c("from", "to", "value")
visNetwork(nodes[nodes$id %in% c(result$crime_for_plot, recommendations$num), ], edges, width = "100%", main="蛛网-手机通讯录-SNA可视化") %>% 
    visGroups(groupname="犯罪人", color="red", shape="dot", size=8) %>%
    visGroups(groupname="嫌疑人", color="orange", shape="diamond", size=6) %>%
    visGroups(groupname="相关人", color="green", shape="diamond", size=2) %>%
    visEdges(arrows = "from") %>%
    # visOptions(highlightNearestlist=list(enabled=T, degree=2, hover=T)) %>%
    # visOptions(highlightNearestlist=list(enabled=T, hover=T), nodesIdSelection=list(enabled=TRUE, values=recommendations$num), selectedBy="group") %>%
    visOptions(nodesIdSelection=list(enabled=TRUE, values=recommendations$num), selectedBy="group") %>%
    visLegend(width=0.06) 

visNetwork(nodes[nodes$id %in% c(result$crime_for_plot, recommendations$num), ], edges[(edges$from %in% c(result$crime_for_plot, recommendations$num)) | (edges$to %in% c(result$crime_for_plot, recommendations$num)), ], 
    width = "100%", main="蛛网-手机通讯录-SNA可视化") %>% 
    visGroups(groupname="犯罪人", color="red", shape="dot", size=8) %>%
    visGroups(groupname="嫌疑人", color="orange", shape="diamond", size=6) %>%
    visGroups(groupname="相关人", color="green", shape="diamond", size=2) %>%
    visEdges(arrows = "from") %>%
    # visOptions(highlightNearestlist=list(enabled=T, degree=2, hover=T)) %>%
    # visOptions(highlightNearestlist=list(enabled=T, hover=T), nodesIdSelection=list(enabled=TRUE, values=recommendations$num), selectedBy="group") %>%
    visOptions(nodesIdSelection=list(enabled=TRUE, values=recommendations$num), selectedBy="group") %>%
    visLegend(width=0.06) 
    


