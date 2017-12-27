data_good <- all_1[all_1$what==1,]                          #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<可调节参数为业绩好的                                                    #
data_bad <- data_good[data_good$sort==500,]                                                                                                                  #
data_good <- data_good[data_good$sort<500,]                                                                                                                  #

#没有随机种子，每一次运行都发生变化
data_bad <- data_bad[sample(1*nrow(data_bad),1*nrow(data_good)),]  #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<比例限制

#合成数据集最终进入模型中
data_model <- rbind(data_good,data_bad)
data_model<- data_model[data_model$dpd %in% c(4:6),]       #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<账期不同调整

#模型需要的字段选择
#----------------------------------------------进入选择的数据集为data_model数据框--------------------------------------------------------------------     
x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs')
DT_data <- data_model[,x]

#分类的变量
DT_data$days_last_p <- with(DT_data,ifelse(is.na(days_last_p)==TRUE,0,1))
DT_data$sxdays_eff_logs <- with(DT_data,ifelse(sxdays_eff_logs==0,0,1))
DT_data$if_jyd[is.na(DT_data$if_jyd)] <-0
DT_data$if_new_case[is.na(DT_data$if_new_case)] <-0
DT_data$if_seek[is.na(DT_data$if_seek)] <-0

#数值的变量
DT_data$sv_due_rate[is.na(DT_data$sv_due_rate)] <-0
DT_data$lastpay_tonow[is.na(DT_data$lastpay_tonow)] <-0
DT_data$return_to_loan_term[is.na(DT_data$return_to_loan_term)] <- 0 
DT_data$due_amount[is.na(DT_data$due_amount)] <- 0

#sort值的切割
DT_data$sort <- ifelse(DT_data$sort <18,1,0)

#变为模型的数据结构，因子
DT_data$sort <- as.factor(DT_data$sort)
DT_data$dpd <- as.factor(DT_data$dpd)
DT_data$days_last_p <- as.factor(DT_data$days_last_p)
DT_data$if_jyd <- as.factor(DT_data$if_jyd)
DT_data$if_new_case <-as.factor(DT_data$if_new_case)
DT_data$if_seek <- as.factor(DT_data$if_seek)
DT_data$sxdays_eff_logs <- as.factor(DT_data$sxdays_eff_logs)

tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$if_seek <- NULL
#DT_data$return_to_loan_term <- NULL
tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$if_new_case <-NULL
tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$sxdays_eff_logs <- NULL
tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$lastpay_tonow <- NULL
tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$sxdays_eff_logs <- NULL
tree <- rpart(sort~.,data=DT_data)
rpart.plot(tree)

DT_data$if_jyd <-NULL
tree <- rpart(sort~.,data= DT_data)
rpart.plot(tree)
