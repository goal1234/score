


#-------------------------------------------------------------------准确性函数----------------------------------------------
accuracy_model <- function(x){
  #二分类判断
  classify <- function(probability)ifelse(probability<0.5,'0','1')
  classified <- classify(x)
  data_test<- data_fit
  #预测的准确性,来个表
  data_test$sort <- ifelse(data_test$sort != 500,1,0)
  confusion_matrix <- table(data_test$sort,classified,dnn=c('real','prediction'))
  #计算评价指标
  a <- as.data.frame(confusion_matrix)
  a1 <-as.numeric(as.character(a$real))
  a2<-as.numeric(as.character(a$prediction))
  accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
  Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
  Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
  print(paste("准确概率",round(accuracy*100,2),"%"))
  print(paste("正向选择",round(Sensitivity*100,2),"%"))
  print(paste("负向选择",round(Specificity*100,2),"%"))
}

for(i in 1:3){
#--------------------------------------------------------------------开始第一个模型-----------------------------------------------------------------
x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs',
       'prd','age0','last_visit_days','marry_staus1','acdamic_type1','province','age0')

#---------------------------------------------------------------------数据分类处理--------------------------------------
data_new <-all_1[,x]
data_new <- data_new[data_new$dpd %in% c(4:6),]
x <- c("sort",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs')
DT_data <- data_new[,x]

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

DT_data$return_to_loan_term <- ifelse(DT_data$return_to_loan_term <0.38,0,1)
DT_data$sv_due_rate <- ifelse(DT_data$sv_due_rate <0.04,0,1)
DT_data$lastpay_tonow <- ifelse(DT_data$lastpay_tonow >= 96,0,1)
DT_data$due_amount <- ifelse(DT_data$due_amount < 10000,0,1)

data_fit <- DT_data
#------------------------------------------------------------------------------------------------------------------------

########################################################################进行分类################################################
data_new$pred <- predict(enter_model_one,data_fit[,-1],type='response')
accuracy_model(data_new$pred)
data_from_model1 <- data_new[data_new$pred >0.5,]



#-------------------------------------------------------------------开始第二个模型--------------------------------------
x <- c('sort','marry_staus1','acdamic_type1','province','age0','due_amount')
data_model_two <- data_from_model1[,x]
data_model_two$due_amount <-ifelse(data_model_two$due_amount<8000,0,1)
data_model_two$age0 <- ifelse(data_model_two$age0<35,0,
                              ifelse(data_model_two$age0<45,1,2))
data_fit <- data_model_two

###############################################################进行分类##########################################
data_from_model1$pred1 <-predict(enter_model_two,data_fit,type='response')
data_from_model2 <-data_from_model1[data_from_model1$pred1>0.5,]


sum(data_from_model2$sort==500)/nrow(data_from_model2)

model_1_num <- nrow(data_from_model1)/nrow(data_new)
model_2_num <- nrow(data_from_model2)/nrow(data_from_model1)

paste('第一个模型的筛选的概率为：',round(model_1_num*100,2),"%")

paste('第二个模型筛选出的概率为：',round(model_2_num*100,2),'%')
}
