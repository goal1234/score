setwd("E:/order/data")
library(sqldf)
library(tcltk)
library(dplyr)
library(MASS)

file_name <- 'full_info.csv'
glm_sort <- read.csv(file_name,stringsAsFactors = F)
sum(glm_sort$sort != 500)/nrow(glm_sort)

age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)

all_data <- merge(glm_sort,age_sex,by.x = "customer_code",by.y = "CUSTOMER_CODE",all.x = T)
sum(all_data$sort != 500)/nrow(all_data)
rm(age_sex,glm_sort);gc()

info_model_two <- read.csv('info_model_two.csv',stringsAsFactors = F)
all_data <- merge(all_data,info_model_two,by.x = "customer_code",by.y = "customer_code",all.x = T)
maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)

rol_for_pre <- c("customer_code","contractno","sort","dpd","USER_NAME", "enter_date2","due_amount","to_rtp_days", "days_last_p","if_new_case",
                 "sv_due_rate","sx_eff_rate","return_to_loan_term","if_jyd","if_seek","age0","acdamic_type1","province" )
all_data <- all_data[,rol_for_pre]

all_data <- sqldf('select a.*,
                  b.lastpay_tonow
                  from all_data as a
                  left join maxrepay as b
                  on a.contractno = b.contractno and
                  a.enter_date2 = b.enter_date2')
rm(maxrepay);gc()
#释放内存
rm(info_model_two);gc()
seven_days_log <- read.csv('seven_days_log.csv',stringsAsFactors = T)
all_data <- sqldf('select a.*,
                  b.seven_days_logs,
                  b.seven_days_eff_logs
                  from all_data as a
                  left join seven_days_log as b
                  on a.customer_code = b.customer_code and
                  a.enter_date2 = b.enter_date2')
rm(seven_days_log,file_name,rol_for_pre);gc()
all_data <- na.omit(all_data)
#---------------------------------------------------------------------------------------
sum(all_data$sort != 500)/nrow(all_data)

#联系的客户
connect <- all_data
sum(connect$sort != 500)/nrow(connect)

result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)
user_connect_rank <- filter(result, x > 200)

#result  sort值减少了到0.089
sort_120 <- sqldf('select a.* from all_data as a 
                  inner join user_connect_rank as b on a.USER_NAME = b.name and a.enter_date2 = b.time')

connect <- sort_120
result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)
#按照user_name 和enter_date2 进行聚合
connect <- sort_120[sort_120$sort != 500,]
result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)

#联系客户的数量情况
summary(result$x);boxplot(result$x);plot(density(result$x))

#过滤一些信息
user_connect_rank <- filter(result, x %in% c(15:35))


#result 
sort_15_25 <- sqldf('select a.* from sort_120 as a 
                    inner join user_connect_rank as b on a.USER_NAME = b.name and a.enter_date2 = b.time')

#移除数据释放内存
rm(connect,result);gc()

#####################################################result的情况#############################
sum(sort_15_25$sort ==500)/nrow(sort_15_25)
1-(sum(sort_15_25$sort ==500)/nrow(sort_15_25))

how_z(sort_15_25);print(per_result)

#出现次数太少的自变量将被弃掉
#if_seek if_new_case seven_days_eff_logs
#至多识别为 5%的比例
#如果其值对应程度很高那么就设定规则
a <- sum(sort_15_25$if_seek==1) + sum(sort_15_25$if_seek ==1) + sum(sort_15_25$seven_days_eff_logs == 1) 
a
(max_known <- a/nrow(sort_15_25))

#--------------------------------------对应情况---------------------------------------
#if_seek <<<<<<<<<<<<<<<<<<<<<<<不知道为什么if_seek里面就是需要联系的
(correspond <- sum((sort_15_25$if_seek==1) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$if_seek==1) & (sort_15_25$sort != 500)))
correspond / sum(sort_15_25$if_seek ==1)

data <- sort_15_25[(sort_15_25$if_seek==1) & (sort_15_25$sort != 500),]
#下面花费约5分钟
#len <- nrow(sort_15_25)
#rate <- 0
#for(i in 1:len){
# if((sort_15_25$if_seek==1)[i] & (sort_15_25$sort != 500)[i]){
#  rate <- rate +1
#}
# }
#(rate/sum(sort_15_25$if_seek ==1))

#if_new_case--------------------0.6034304
(correspond <- sum((sort_15_25$if_new_case==1) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$if_new_case==1) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$if_new_case==1))

#seven_days_eff_logs----------------------0.2028351
(correspond <- sum((sort_15_25$seven_days_eff_logs==1) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$seven_days_eff_logs==1) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$seven_days_eff_logs==1))

#if_jyd------------------------------------0.09880124
(correspond <- sum((sort_15_25$if_jyd==1) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$if_jyd==1) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$if_jyd==1))

#sx_eff_rate---------------------------------0.132505
(correspond <- sum((sort_15_25$sx_eff_rate != 0) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$sx_eff_rate != 0) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$sx_eff_rate != 0))

#sv_due_rate-----------------------------0.09598727
(correspond <- sum((sort_15_25$sv_due_rate != 0) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$sv_due_rate != 0) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$sv_due_rate != 0))

#seven_days_logs-----------------------------0.1050024
(correspond <- sum((sort_15_25$seven_days_logs != 0) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$seven_days_logs != 0) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$seven_days_logs != 0))

#days_last_p---------------------------------0.1008559
(correspond <- sum((sort_15_25$days_last_p != 0) == (sort_15_25$sort != 500)))
(correspond <- sum((sort_15_25$days_last_p != 0) & (sort_15_25$sort != 500)))
(correspond / sum(sort_15_25$days_last_p != 0))

#变为0，1的情况;<----------------------------------如果不行就分箱看看能提高一点是一点
sort_15_25$seven_days_eff_logs <- ifelse(sort_15_25$seven_days_eff_logs != 0,1,0)
sort_15_25$sx_eff_rate <- ifelse(sort_15_25$sx_eff_rate != 0, 1, 0)
sort_15_25$sv_due_rate <- ifelse(sort_15_25$sv_due_rate != 0, 1, 0)
sort_15_25$seven_days_logs <- ifelse(sort_15_25$seven_days_logs != 0 ,1,0)
sort_15_25$days_last_p <- ifelse(sort_15_25$days_last_p != 0, 1, 0)

###your target
sort_15_25$sort <- ifelse(sort_15_25$sort < 40 ,1,0)

#需要单调性分箱的变量
#return_to_loan_term,due_amount,to_rtp_days,age0,lastpay_tonow,acdamic_type1
ks_data(sort_15_25$return_to_loan_term,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0.007  3.385

sort_woe <- sort_15_25
#分组5 10的点
breaks <- as.numeric(as.matrix(dltfun$当组最大值))
sort_15_25$return_to_loan_term <- ifelse(sort_15_25$return_to_loan_term ==0,0,
                                          ifelse(sort_15_25$return_to_loan_term < breaks[5],1,
                                         ifelse(sort_15_25$return_to_loan_term < breaks[10],2,3)))

ks_data(sort_15_25$due_amount,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0.011   3.219
#分组5 10的点
breaks <- as.numeric(as.matrix(dltfun$当组最大值))
sort_15_25$due_amount <- ifelse(sort_15_25$due_amount < breaks[5],0,
                                ifelse(sort_15_25$due_amount < breaks[10],1,2))

ks_data(sort_15_25$to_rtp_days,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0.011   4.273

ks_data(sort_15_25$age0,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0  0.602

ks_data(sort_15_25$lastpay_tonow,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0.186  18.172
#分组在4， 10点
breaks <- as.numeric(as.matrix(dltfun$当组最大值))
sort_15_25$lastpay_tonow <- ifelse(sort_15_25$lastpay_tonow < breaks[4],0,
                                ifelse(sort_15_25$lastpay_tonow < breaks[10],1,2))

ks_data(sort_15_25$acdamic_type1,sort_15_25$sort,20)
(IVdlt <- sum(dltfun$IV数据源))
(KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')    #0.001  0.945

#对于可以分箱的变量  lastpay_tonow  due_amount  return_to_loan_term
#被扔掉的变量  to_rtp_days age0 acdamic_type1


#所有变量seven_days_eff_logs sx_eff_rate sv_due_rate seven_days_logs  
#         days_last_p lastpay_tonow due_amount return_to_loan_term if_jyd

#分箱之后竟然没有分出来
md1 <- glm(sort~if_jyd +return_to_loan_term + due_amount + lastpay_tonow+days_last_p+
             seven_days_eff_logs + sx_eff_rate + sv_due_rate + seven_days_logs,data = sort_15_25,
           family = binomial(link = 'logit'))
summary(md1)

pred2 <- predict(md1,sort_15_25,type='response')
pred2 <- as.numeric(as.matrix(pred2))
plot(pred2[order(pred2)],type='l',main = "glm_2------------logistic")

#混淆矩阵
classify <- function(probability)ifelse(probability<0.5,'0','1')
classified <- classify(pred2)
sort <- sort_15_25$sort
confusion_matrix <- table(sort,classified,dnn=c('real','prediction'))
a <- as.data.frame(confusion_matrix)
a1 <-as.numeric(as.character(a$real))
a2<-as.numeric(as.character(a$prediction))
accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
real_in_prediction <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a2==1])
predict_rate <- sum(a$Freq[a2==1])/sum(a$Freq)  #这个值的比例在0.05
(call_back <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a1==1]))

accuracy;Sensitivity;Specificity;real_in_prediction;predict_rate;call_back
confusion_matrix
