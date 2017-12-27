  #----------------------------------------------------------读入数据-----------------------------------------
  setwd("E:/order/data")
  library(magrittr)
  library(Hmisc)
  library(ggplot2)
  library(corrplot)
  library(sqldf)
  library(tcltk)
  #-----------------------------------------------------------------------------------------------------------
  file_name_bad <- 'full_info_bad.csv'
  info_bad_0 <- read.csv(file_name_bad,stringsAsFactors = FALSE)
  info_bad <- info_bad_0
  file_name_good <- 'full_info_good.csv'
  info_good_0<- read.csv(file_name_good,stringsAsFactors = FALSE)
  info_good <- info_good_0
  info_bad$what =0
  info_good$what=1
  all_0 <- rbind(info_bad,info_good)
  age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)
  maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)
  all_1 <- sqldf('select a.*,
                  b.age0,
                  b.sex
                  from all_0 as a
                  left join age_sex as b
                  on a.customer_code = b.customer_code')

  all_1 <- sqldf('select a.*,
                  b.lastpay_tonow
                  from all_1 as a
                  left join maxrepay as b
                  on a.contractno = b.contractno and
                  a.enter_date2 = b.enter_date2')  
  #------------------------------------------------------------筛选数据--------------------------------------
  #用好的组模拟一个情况
  #好业绩中的产生联系部分数据框
  #data_model <- all_1[all_1$what==1,]
  #data_model <- data_model[data_model$sort <500,]
  #data_model<- data_model[data_model$dpd %in% c(2:3),]
  
  #好的催收业绩组中发生了联系的-最终输出为data_good
  data_good <- all_1[all_1$what==1,]                          #---------------------可调节参数
  data_bad <- data_good[data_good$sort==500,]
  data_good <- data_good[data_good$sort<500,]
  
  #bad组中抽样为没有联系的,抽样比例为1:1
  #data_bad <- all_1[all_1$what==0,]
  #data_bad[is.na(data_bad)] <-0
  #data_bad <-data_bad[data_bad$sort==500,]

  #没有随机种子，每一次运行都发生变化
  data_bad <- data_bad[sample(1*nrow(data_bad),4*nrow(data_good)),]
  
  #合成数据集最终进入模型中
  data_model <- rbind(data_good,data_bad)
  data_model<- data_model[data_model$dpd %in% c(4:6),]      #-----------------------账期不同调整
  
  #模型需要的字段选择
  #----------------------------------------------进入选择的数据集为data_model数据框--------------------------------------------------------------------     
  x <- c("sort","dpd",'days_last_p','if_jyd','if_new_case','if_seek','sv_due_rate',"lastpay_tonow" ,'return_to_loan_term','due_amount','sxdays_eff_logs')
  data_fit <- data_model[,x]
  
  #-----------------------------------------------------将变量变为分类变量----------------------------------------------------------------
  #数据集中均为分类变量，其中sort为多分类的y,其余字段为0,1分类变量
  #TRUE为1,值小为0
  data_fit$days_last_p <- ifelse(is.na(data_fit$days_last_p)==TRUE,0,1)
  data_fit$sv_due_rate <-ifelse(is.na(data_fit$sv_due_rate)==TRUE,0,1)
  data_fit$return_to_loan_term <- with(data_fit,ifelse(return_to_loan_term <0.7,0,1))
  data_fit$sxdays_eff_logs <- ifelse(is.na(data_fit$sxdays_eff_logs)==TRUE,0,1)
  #DUE_AMOUNT 由dpd不同水平来判断
  dpd_amount_level <- list()
  for(i in 2:35){
         a <- all_1$due_amount[all_1$dpd==i]
        dpd_amount_level[i-1] <-(median(a,na.rm = T))
  }
  for(i in 2:35){
    data_fit[data_fit$dpd==i,]$due_amount <- ifelse((data_fit[data_fit$dpd==i,]$due_amount <= dpd_amount_level[i-1]),0,1)
  }
  
  #lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
  dpd_tonow_level <- list()
  for(i in 2:35){
    a <- all_1$lastpay_tonow[all_1$dpd==i]
    dpd_tonow_level[i-1] <-(median(a,na.rm = T))
  }
  for(i in 2:35){
    data_fit[data_fit$dpd==i,]$lastpay_tonow <- ifelse((data_fit[data_fit$dpd==i,]$lastpay_tonow <= dpd_tonow_level[i-1]),0,1)
  }
  data_fit$lastpay_tonow[is.na(data_fit$lastpay_tonow)]=1
  
  
  #-------------------------------------------------------确定sort的值---------------------------------------
  #并没有准则决定那个sort合适，所以现在的是画图展示
  acu <- list()
  b <- list()
  c <- list()
  for(i in 1:50){
    data_sort <- data_fit
    data_0 <- subset(data_sort,sort==500)
    data_1<- subset(data_sort,sort !=500)
    data_1 <-subset(data_1,sort %in% c(1:5))                #<<<<<<<<<<<<<<<<<<<<<<<<<<
    data_1$sort  <-1
    random <- sample(1:nrow(data_0),nrow(data_1))
    data_0 <- data_0[random,]
    data_0$sort <-0
    
    data_sort <-rbind(data_1,data_0)
    random <- sample(1:nrow(data_sort),0.8*nrow(data_sort))
    data_model_use <- data_sort[random,]
    data_model_test <- data_sort[-random,]
    
    fitted_model <- data_model_use %>% glm(sort~.-dpd,family=binomial(link = "logit"),data=.)
    print(summary(fitted_model))
    print(ks.test(fitted_model$residuals,'pnorm'))
    
    classify <- function(probability) ifelse(probability<0.5,'0','1')
    classified_malignant <- classify(predict(fitted_model,data_model_test[,-1],type='response'))
    classified <- classify(predict(fitted_model,data_model_test[,-1]))
  
    #预测的准确性,来个表
    confusion_matrix <- table(data_model_test[,1],classified,dnn=c('real','prediction'))
    print(addmargins(confusion_matrix))
    
    
    a <- as.data.frame(confusion_matrix)
    a1 <-as.numeric(as.character(a$real))
    a2<-as.numeric(as.character(a$prediction))
    accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
    Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
    Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
    
    num1 <- as.numeric(data_model_test$sort)
    num2 <- predict(fitted_model,data_model_test[,-1],type='response')
    roc_intime <- roc(num1,num2)
    plot(roc_intime, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), 
         grid.col=c("green", "red"), max.auc.polygon=TRUE, 
         auc.polygon.col="skyblue", print.thres=TRUE,main="out_time ROC")
    
    
    #指标存放
    acu[i] <- accuracy
    b[i] <- Sensitivity
    c[i] <- Specificity
    print(acu[i])
  }

  layout(matrix(c(1,1,2,3),2,2))
  plot(1:length(acu),as.numeric(acu),type = 'b',main="预测的准确性",lwd=3,col=1)
  plot(1:length(b),as.numeric(b),type = 'l',main="将正样本预测为正样本",lwd=3,col=2)
  plot(1:length(c),as.numeric(c),type = 'l',main="将负样本预测为负样本",lwd=3,col=3)
  layout(1)
  
  ###########################################################################################################
  #----------------------------------------------加入prd的变量进行选择--------------------------------------#
  data_prd <- data_model
  #仅在联系的里面模拟产品的不同
  data_prd <- subset(data_prd,sort !=500)  
  
  #选择需要的变量
  #sort 为因变量，同时,'age0',last_visit_days,prd为需要分类的变量，
  x <- c('sort','prd','age0','last_visit_days')
  data_prd <- data_prd[,x]
  data_prd$prd <-with(data_prd,
                      ifelse(prd %in% c('精英贷','精英贷（银行合作）'),3,
                             ifelse(prd %in% c('新薪贷','薪薪贷（银行合作）','新薪宜楼贷'),2,
                                    ifelse(prd %in% c('助业贷','助业宜楼贷'),1,0))))
  #lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
  prd_age_level <- list()
  for(i in 0:3){
    a <- data_prd$age0[data_prd$prd==i]
    prd_age_level[i+1] <-(median(a,na.rm = T))
  }
  for(i in 0:3){
    data_prd[data_prd$prd==i,]$age0 <- ifelse((data_prd[data_prd$prd==i,]$age0 <= prd_age_level[i+1]),1,0)
  }
  data_prd$age0[is.na(data_prd$age0)]=1
  
  data_prd$last_visit_days <- ifelse(data_prd$last_visit_days<90,1,0)
  data_prd$last_visit_days[is.na(data_prd$last_visit_days)] <-0
  
  auc<-list()
  b<-list()
  c<-list()
  prd_estimate <- list()
  age_estimate <-list()
  last_visit_estimate <-list()
  for(i in 1:50){
    data_sort <- data_prd
    data_sort$sort <- with(data_sort,ifelse(sort<=i,1,0))
    
    random <- sample(1:nrow(data_sort),0.8*nrow(data_sort))
    data_model_use <- data_sort[random,]
    data_model_test <- data_sort[-random,]
    
    fitted_model <- data_model_use %>% glm(sort~.,family=binomial(link = "logit"),data=.)
    print(summary(fitted_model))
    print(ks.test(fitted_model$residuals,'pnorm'))
    
    classify <- function(probability) ifelse(probability<0.5,'0','1')
    classified_malignant <- classify(predict(fitted_model,data_model_test[,-1],type='response'))
    classified <- classify(predict(fitted_model,data_model_test[,-1]))
    
    #预测的准确性,来个表
    confusion_matrix <- table(data_model_test[,1],classified,dnn=c('order_connected','prediction'))
    a <- as.data.frame(confusion_matrix)
    a1 <-as.numeric(as.character(a$order_connected))
    a2<-as.numeric(as.character(a$prediction))
    accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
    Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
    Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
    
    #指标存放
    prd_estimate[i] <-fitted_model[[1]][2]  #prd的系数
    age_estimate[i] <-fitted_model[[1]][3]
    last_visit_estimate[i] <-fitted_model[[1]][4]
    auc[i] <- accuracy
    b[i] <- Sensitivity
    c[i] <- Specificity
  }
  layout(matrix(c(1,1,2,3),2,2))
  plot(1:length(auc),as.numeric(auc),type = 'b',main="预测的准确性",lwd=3,col=1)
  plot(1:length(b),as.numeric(b),type = 'l',main="将正样本预测为正样本",lwd=3,col=2)
  plot(1:length(c),as.numeric(c),type = 'l',main="将负样本预测为负样本",lwd=3,col=3)
  layout(1)
  
  plot(as.numeric(prd_estimate),type='h')
  plot(as.numeric(age_estimate),type='h')
  plot(as.numeric(last_visit_estimate),type='h')
  
  
  
  
  #选出正确的变量模型
  data_sort <- data_fit
  data_sort$sort <- with(data_sort,ifelse(sort<=30,1,0))
  
  random <- sample(1:nrow(data_sort),0.8*nrow(data_sort))
  data_model_use <- data_sort[random,]
  data_model_test <- data_sort[-random,]
  fitted_model <- data_model_use %>% glm(sort~.-dpd,family=binomial(link = "logit"),data=.)
  
  #-----------------------------------数据分类--------------
    data_new <-all_1[,x]
    data_fit <-data_new
    data_fit$days_last_p <- ifelse(is.na(data_fit$days_last_p)==TRUE,0,1)
    data_fit$sv_due_rate <-ifelse(is.na(data_fit$sv_due_rate)==TRUE,0,1)
    data_fit$return_to_loan_term <- with(data_fit,ifelse(return_to_loan_term <0.7,0,1))
    data_fit$sxdays_eff_logs <- ifelse(is.na(data_fit$sxdays_eff_logs)==TRUE,0,1)
    #DUE_AMOUNT 由dpd不同水平来判断
    dpd_amount_level <- list()
    data_fit$dpd[is.na(data_fit$dpd)]<-0
    for(i in 2:35){
      a <- all_1$due_amount[all_1$dpd==i]
      dpd_amount_level[i-1] <-(median(a,na.rm = T))}
    
    for(i in 2:35){
      data_fit[data_fit$dpd==i,]$due_amount <- 
        ifelse((data_fit[data_fit$dpd==i,]$due_amount <= dpd_amount_level[i-1]),0,1)}
  
    #lastpay_tonow 由dpd的不同水平决定,将其中NA值变为1
    dpd_tonow_level <- list()
    for(i in 2:35){
    a <- all_1$lastpay_tonow[all_1$dpd==i]
    dpd_tonow_level[i-1] <-(median(a,na.rm = T))}
    
    for(i in 2:35){
      data_fit[data_fit$dpd==i,]$lastpay_tonow <- ifelse((data_fit[data_fit$dpd==i,]$lastpay_tonow <= dpd_tonow_level[i-1]),0,1)
    }
    data_fit$lastpay_tonow[is.na(data_fit$lastpay_tonow)]=1
   #------------------------------------------------------------------------------------------------------------------------
  
  #--------------------------------------------------------------进行分类--------------------------------------------------
  pred <- predict(fitted_model,data_fit,type='response')
  data_fit$pred <- pred
  
  data_model_get <- data_fit[data_fit$pred >0.5,]
  