
library(digest)
library(REmap)

library(xlsx)
map_data <- read.xlsx("E:/map_data.xlsx", 1, encoding="UTF-8")
map_data <- map_data[, 2:3]
map_data[,1] <- as.character(map_data[,1])
map_data[,2] <- as.character(map_data[,2])
names(geo_data)[1] <- "lon"

geo_data <- read.xlsx("E:/geo_data.xlsx", 1, encoding="UTF-8")
geo_data <- geo_data[,2:4]
geo_data[,3] <- as.character(geo_data[,3])

centers <- read.xlsx("E:/centers.xlsx", 1, encoding="UTF-8")
centers <- centers[,2:3]
names(centers)[1] <- "lon"

(map_result<-remapB(
    # center=centers,
    zoom=6, 
    color="Blue",
    title="春运人口迁徙图",
    subtitle="人口流动方向",
    markLineData=map_data,
    markPointData=map_data[, 2],                
    markLineTheme = markLineControl(
        symbol=NA,            
        symbolSize=c(0,4),   
        smooth=T,             
        smoothness=0.2,      
        effect=T,            
        lineWidth=0.5,         
        lineType="dotted",  
        color="red"),
        markPointTheme=markPointControl(
        symbol="none", 
        symbolSize="Random",  
        effect=F,              
        effectType="scale",   
        color="white"), 
        geoData=geo_data
))

