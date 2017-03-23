# TODO: Add comment
# Date: 2016/7/27
# Author: lifeng
###############################################################################

library(clogitboost)

train_data <- read.csv("H:/pps_matrix_shishan.txt", header=F)
train_data <- train_data[which(train_data$V1 < 20140901),]
grid_num_true <- aggregate(x=train_data$V4, by=list(train_data$V2), FUN='sum')
grid_num_true <- grid_num_true[grid_num_true$x > 0, 1]
train_data <- train_data[train_data$V2 %in% grid_num_true,]
fit <- clogitboost(y=train_data$V4, x=train_data[,-c(1:4)], strata=train_data$V2, iter=20, rho=0.05)

library(glmm)
fixed <- as.formula(paste("V4 ~ 0 +", paste(names(train_data)[-c(1:4)], collapse="+")))
m=nrow(train_data)/length(grid_num_true)
fit <- glmm(fixed=fixed, random=list(V4 ~ 0 + V2), varcomps.names=c("V2"), data=train_data[1:10000,], family.glmm=poisson.glmm, m=m ,doPQL=TRUE)







