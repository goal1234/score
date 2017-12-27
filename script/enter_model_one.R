#-------------------------把决策树变为logit回归--------------------
#----------------------------------------------------------读入数据-----------------------------------------
setwd("E:/order/data")
library(magrittr)
library(sqldf)
library(tcltk)
library(dplyr)
#library(readr)
#-----------------------------------------------------------------------------------------------------------
file_name <- 'glm_sort.csv'
glm_sort <- read.csv(file_name,stringsAsFactors = F)
age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)
all_data <- merge(glm_sort,age_sex,by.x = "customer_code",by.y = "CUSTOMER_CODE",all.x = T)
rm(age_sex,glm_sort);gc()
all_data <- all_data[all_data$score_category == 'GOOD',]
info_model_two <- read.csv('info_model_two.csv',stringsAsFactors = F)
all_data <- merge(all_data,info_model_two,by.x = "customer_code",by.y = "customer_code",all.x = T)
maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)
all_data <- sqldf('select a.*,
               b.lastpay_tonow
               from all_data as a
               left join maxrepay as b
               on a.contractno = b.contractno and
               a.enter_date2 = b.enter_date2')

rm(maxrepay);gc()


#释放内存
rm(info_model_two)
gc()                                             
##################################################################################################################
tail(all_data)
data_high <- all_data[all_data$score_category == 'GOOD',]                          #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<可调节参数为业绩好的                                                    
#data_high <- merge(data_high,maxrepay,by.x = "contractno",by.y = "contractno",all.x = T)
data_bad <- data_high[data_high$sort==500,]                                                                                                                  
data_good <- data_high[data_high$sort<500,]                                                                                                                  
nrow(data_good)/nrow(data_bad)

#分组排序
data_good <- data_good %>% group_by(USER_NAME) %>% arrange(enter_date2,USER_NAME)

data_bad <- data_bad[sample(1*nrow(data_bad),0.2*nrow(data_good),replace = T),]  #<<<<<<<<<<<<<<<<<<<<<<<比例限制

#合成数据集最终进入模型中
data_model <- rbind.data.frame(data_good,data_bad)
data_model<- data_model[data_model$dpd %in% c(4:6),]       #<<<<<<<<<<<<<<<<<<<<<<<<<<<<账期不同调整

#data_model <- data_good
#data_model<- data_model[data_model$dpd %in% c(4:6),] 
#模型需要的字段选择
#----------------------------------------------进入选择的数据集为data_model数据框--------------------------------------------------------------------     
x <- c("sort",'days_last_p','to_rtp_days','if_jyd','if_new_case','if_seek',
       'sv_due_rate' ,'return_to_loan_term','due_amount','lastpay_tonow','sx_eff_rate','age0','province','marry_staus1','acdamic_type1')
DT_data <- data_model[,x]                                                                      #<<<<<<<<<<<<<<检查一下开始的数据

#分类的变量
DT_data$days_last_p <- with(DT_data,ifelse(days_last_p <= 120,0,1))
DT_data$sx_eff_rate <- with(DT_data,ifelse(sx_eff_rate==0,0,1))
DT_data$if_jyd[is.na(DT_data$if_jyd)] <-0
DT_data$if_new_case[is.na(DT_data$if_new_case)] <-0
DT_data$if_seek[is.na(DT_data$if_seek)] <-0

#数值的变量
DT_data$sv_due_rate[is.na(DT_data$sv_due_rate)] <-0
DT_data$lastpay_tonow[is.na(DT_data$lastpay_tonow)] <-0
DT_data$return_to_loan_term[is.na(DT_data$return_to_loan_term)] <- 0 
DT_data$due_amount[is.na(DT_data$due_amount)] <- 0

DT_data <- na.omit(DT_data)
DT_data$return_to_loan_term <- ifelse(DT_data$return_to_loan_term <0.38,1,ifelse(DT_data$return_to_loan_term >0.65,1,0))

DT_data$sv_due_rate <- ifelse(DT_data$sv_due_rate <0.40,0,1)  #
DT_data$lastpay_tonow <- ifelse(DT_data$lastpay_tonow >= 115,1,0)
DT_data$due_amount <- ifelse(DT_data$due_amount < 10000,0,ifelse(DT_data$due_amount>25000,1,0))
DT_data$to_rtp_days <- ifelse(DT_data$to_rtp_days <7,0,
                              ifelse(DT_data$to_rtp_days <14,1,
                                     ifelse(DT_data$to_rtp_days <21,2,3)))
DT_data$age0 <- ifelse(DT_data$age0 < 30,0,ifelse(DT_data$age0 >45 ,1,0))
#sort值的切割
DT_data$sort <- ifelse(DT_data$sort <18,1,0)
DT_data <- na.omit(DT_data)

plot(DT_data$sort);plot(DT_data$days_last_p);plot(DT_data$to_rtp_days);plot(DT_data$if_jyd);plot(DT_data$if_new_case)
plot(DT_data$sv_due_rate);plot(DT_data$return_to_loan_term);plot(DT_data$due_amount);plot(DT_data$lastpay_tonow);plot(DT_data$sx_eff_rate)
plot(DT_data$age0);plot(DT_data$province);plot(DT_data$marry_staus1);plot(DT_data$acdamic_type1)

#-------------------------------------------------构建模型
model_one <- glm(sort~.-if_seek-sx_eff_rate-acdamic_type1-to_rtp_days-days_last_p,data=DT_data,family = binomial(link = "logit")) #poisson(link = "log") binomial(link = "logit")

summary(model_one)


#-------------------------------------------------预测部分
x <- c("sort",'USER_NAME','enter_date2','days_last_p','to_rtp_days','if_jyd','if_new_case','if_seek',
       'sv_due_rate' ,'return_to_loan_term','due_amount','lastpay_tonow','sx_eff_rate','age0','province','marry_staus1','acdamic_type1')
DT_data <- data_model[,x]                                                                      #<<<<<<<<<<<<<<检查一下开始的数据

#分类的变量
DT_data$days_last_p <- with(DT_data,ifelse(days_last_p <= 120,0,1))
DT_data$sx_eff_rate <- with(DT_data,ifelse(sx_eff_rate==0,0,1))
DT_data$if_jyd[is.na(DT_data$if_jyd)] <-0
DT_data$if_new_case[is.na(DT_data$if_new_case)] <-0
DT_data$if_seek[is.na(DT_data$if_seek)] <-0

#数值的变量
DT_data$sv_due_rate[is.na(DT_data$sv_due_rate)] <-0
DT_data$lastpay_tonow[is.na(DT_data$lastpay_tonow)] <-0
DT_data$return_to_loan_term[is.na(DT_data$return_to_loan_term)] <- 0 
DT_data$due_amount[is.na(DT_data$due_amount)] <- 0

DT_data <- na.omit(DT_data)
DT_data$return_to_loan_term <- ifelse(DT_data$return_to_loan_term <0.38,1,ifelse(DT_data$return_to_loan_term >0.65,1,0))

DT_data$sv_due_rate <- ifelse(DT_data$sv_due_rate <0.40,0,1)  #
DT_data$lastpay_tonow <- ifelse(DT_data$lastpay_tonow >= 115,1,0)
DT_data$due_amount <- ifelse(DT_data$due_amount < 10000,0,ifelse(DT_data$due_amount>25000,1,0))
DT_data$to_rtp_days <- ifelse(DT_data$to_rtp_days <7,0,
                              ifelse(DT_data$to_rtp_days <14,1,
                                     ifelse(DT_data$to_rtp_days <21,2,3)))
DT_data$age0 <- ifelse(DT_data$age0 < 30,0,ifelse(DT_data$age0 >45 ,1,0))
#sort值的切割
DT_data$sort <- ifelse(DT_data$sort <18,1,0)
DT_data <- na.omit(DT_data)


data_test <- DT_data

 a <- predict(model_one,data_test[,-c(1,2,3)],type='response')
 a <- as.numeric(as.matrix(a))
 data_test$pred <- a
 
 data_test <-data_test %>%
                       group_by(USER_NAME) %>% arrange(enter_date2,USER_NAME)
 
 
 #data_test %>% group_by('USER_NAME','enter_date2')
 #result <- data_test %>% group_by('USER_NAME','enter_date2') %>% mutate(rank_pred = rank(pred,ties.method = c('first')))
 #result <- data_test[,c(1:3)]
 
  data <-aggregate(data_test$pred,data_test[,c(2,3)],order)
  sort1 <-as.numeric(as.matrix(unlist(data$x)))
  data_test$sort_pred <- sort1
 
  result <- data_test[,c(1,2,3,15)]
  #预测的标准与真值的差距  
  #(MSE <- sum((result[,1]-result[,4])^2)/nrow(result))
  (RMSE <-sqrt(sum((result[,1]-result[,4])^2)/nrow(result)))
  print(RMSE)
  #(MAE <- sum(abs(result[,1]-result[,4]))/nrow(result))
  #(SD <-sqrt(sum((result[,4]-mean(result[,1]))^2)/nrow(result)))

