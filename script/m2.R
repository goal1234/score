#-------------------------------工作路径----------------------------#
setwd("E:/order/data")
library(magrittr)
library(Hmisc)
library(ggplot2)
library(corrplot)
library(sqldf)
library(tcltk)
library(pROC)

file_name <- 'full_info_m2m3.csv'
glm_sort <- read.csv(file_name,stringsAsFactors = F)
sum(glm_sort$sort != 500)/nrow(glm_sort)

age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)

all_data <- merge(glm_sort,age_sex,by.x = "customer_code",by.y = "CUSTOMER_CODE",all.x = T)
sum(all_data$sort != 500)/nrow(all_data)
rm(age_sex,glm_sort);gc()

info_model_two <- read.csv('info_model_two.csv',stringsAsFactors = F)
all_data <- merge(all_data,info_model_two,by.x = "customer_code",by.y = "customer_code",all.x = T)
maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)
sum(all_data$sort != 500)/nrow(all_data)

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
mob <- read.csv('mob.csv',stringsAsFactors = T)

all_data <- sqldf('select a.*,
                  b.mobdays
                  from all_data as a
                  left join mob as b 
                  on a.contractno = b.CONTRACTNO and
                  a.enter_date2 = b.enter_date2')
all_data <- na.omit(all_data)
rm(mob);gc()
all_data <- na.omit(all_data)
how_z(all_data);print(per_result)

#联系的客户
connect <- all_data
sum(connect$sort != 500)/nrow(connect)

result <- aggregate(connect$sort,list(name = connect$USER_NAME,time = connect$enter_date2),length)
user_connect_rank <- result[result$x %in% c(80:150),]

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
user_connect_rank <- result[(result$x %in% c(25:45)),]

#result 
sort_15_25 <- sqldf('select a.* from sort_120 as a 
                    inner join user_connect_rank as b on a.USER_NAME = b.name and a.enter_date2 = b.time')

#移除数据释放内存
rm(connect,result);gc()
sort_15_25$days_last_p <- NULL

##########################################################################
########################--------numerics--------##########################
##########################################################################
#将因变量放在第一个
num <- sort_15_25[,c('sort','mobdays','lastpay_tonow','age0','bal_percent','maxdue_days','due_amount','bal_prin2',
                   'return_to_loan_term','seven_days_logs')]

#------------------------------------some feature in num_money------------------------------------#
head(num)
money_bad <- summary(num)

#密度图
ggplot(num,aes(bal_prin2)) +geom_density()

#相关性
num_cor <-scale(num)
num_cor <-cor(num_cor)
num_cov <- cov(num_cor)
#---------------------------------画图----------------------#
corrplot(num_cor)
corrplot(num_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)

plot(all_data$sort,all_data$mobdays)

##########################################################################################################################
#################----------------------------观察一下logist的效用情况------------------------------------#################
##########################################################################################################################

  #target
  sort_15_25$sort <- ifelse(sort_15_25$sort != 500,1,0)

  #画图
  sort_15_25 %>% 
    ggplot(aes(x=mobdays,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #lastpay_tonow
  sort_15_25 %>% 
    ggplot(aes(x=lastpay_tonow,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #age0
  sort_15_25 %>% 
    ggplot(aes(x=age0,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  #bal_percent
  sort_15_25 %>% 
    ggplot(aes(x=bal_percent,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  #no_call_days
  sort_15_25 %>% 
    ggplot(aes(x=no_call_days,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  #maxdue_days
  sort_15_25 %>% 
    ggplot(aes(x= maxdue_days,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  #due_amount
  sort_15_25 %>% 
    ggplot(aes(x=due_amount,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  # bal_prin2
  sort_15_25 %>% 
    ggplot(aes(x= bal_prin2,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #return_to_loan_term
  sort_15_25 %>% 
    ggplot(aes(x=return_to_loan_term,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #seven_days_logs
  sort_15_25 %>% 
    ggplot(aes(x=seven_days_logs,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  #th_due_rate
  sort_15_25 %>% 
    ggplot(aes(x=th_due_rate,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #thdays_payoff_times
  sort_15_25 %>% 
    ggplot(aes(x=thdays_payoff_times,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #sv_due_rate
  sort_15_25 %>% 
    ggplot(aes(x=sv_due_rate,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #sx_eff_rate
  sort_15_25 %>% 
    ggplot(aes(x=sx_eff_rate,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #th_eff_rate
  sort_15_25 %>% 
    ggplot(aes(x=th_eff_rate,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #seven_days_eff_logs
  sort_15_25 %>% 
    ggplot(aes(x=seven_days_eff_logs,y=sort))+geom_jitter(height = 0.05,width = 0.3, alpha =0.4)+
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #####################################################################################################
  ###################------------------进行分箱子--------------------------------------################
  #####################################################################################################
  
  #通过赋予空值将其删除
  sort_15_25$customer_code <- NULL
  sort_15_25$USER_NAME<- NULL
  sort_15_25$contractno<- NULL
  sort_15_25$enter_date2<- NULL
  sort_15_25$score_category<- NULL
  sort_15_25$sort_percent <- NULL
  
  sort_15_25$dpd<- NULL
  sort_15_25$prd<- NULL
  sort_15_25$order_connected<- NULL
  sort_15_25$AGE<- NULL
  sort_15_25$sex <- NULL
  sort_15_25$if_new_case <- NULL
  
  sort_15_25$province <- NULL
  sort_15_25$acdamic_type1 <- NULL
  
  sort_15_25$if_rec <- NULL
  sort_15_25$tim <- NULL
  
  ks_data(sort_15_25$bal_prin2,sort_15_25$sort,20)
  ks_data(sort_15_25$LOANTERM,sort_15_25$sort,20)
  ks_data(sort_15_25$rtp,sort_15_25$sort,20)
  ks_data(sort_15_25$returned_term,sort_15_25$sort,20)
  ks_data(sort_15_25$due_amount,sort_15_25$sort,20)
  ks_data(sort_15_25$hisdue_times,sort_15_25$sort,20)
  ks_data(sort_15_25$maxdue_days,sort_15_25$sort,20)
  ks_data(sort_15_25$to_rtp_days,sort_15_25$sort,20)
  
  ks_data(sort_15_25$no_call_days,sort_15_25$sort,20)
  
  ks_data(sort_15_25$on_ptp,sort_15_25$sort,10)
  ks_data(sort_15_25$thdays_payoff_times,sort_15_25$sort,20)
  ks_data(sort_15_25$svdays_payoff_times,sort_15_25$sort,20)
  ks_data(sort_15_25$if_new_rtp,sort_15_25$sort,20)
  ks_data(sort_15_25$th_due_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$sv_due_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$th_due_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$th_kp_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$sx_kp_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$count_sx_kp_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$count_th_kp_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$return_to_loan_term,sort_15_25$sort,20)
  ks_data(sort_15_25$bal_percent,sort_15_25$sort,20)
  ks_data(sort_15_25$th_eff_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$sx_eff_rate,sort_15_25$sort,20)
  ks_data(sort_15_25$if_jyd,sort_15_25$sort,2)
  ks_data(sort_15_25$if_seek,sort_15_25$sort,2)
  ks_data(sort_15_25$age0,sort_15_25$sort,20)
  ks_data(sort_15_25$marry_staus1,sort_15_25$sort,20)
  ks_data(sort_15_25$lastpay_tonow,sort_15_25$sort,20)
  ks_data(sort_15_25$seven_days_logs,sort_15_25$sort,20)
  ks_data(sort_15_25$seven_days_eff_logs,sort_15_25$sort,20)
  ks_data(sort_15_25$mobdays,sort_15_25$sort,20)
  
  
  sort_15_25$marry_staus1 <- NULL
  sort_15_25$count_th_kp_rate <- NULL
  sort_15_25$to_rtp_days <- NULL
  sort_15_25$count_sx_kp_rate <- NULL
  sort_15_25$th_kp_rate <- NULL
  sort_15_25$sx_kp_rate <- NULL
  sort_15_25$return_to_loan_term <- NULL
  sort_15_25$rtp <- NULL
  sort_15_25$mobdays <- NULL
  sort_15_25$due_amount <- NULL
  sort_15_25$sv_due_rate <- NULL
  sort_15_25$returned_term <- NULL
  sort_15_25$age0 <- NULL
  
  sort_15_25$if_seek <- NULL
  sort_15_25$on_ptp <- NULL
  sort_15_25$seven_days_logs <- NULL
  sort_15_25$th_eff_rate <- NULL
  sort_15_25$svdays_payoff_times <- NULL
  sort_15_25$maxdue_days <- NULL
  sort_15_25$if_new_rtp <- NULL
  
  sort_15_25$thdays_payoff_times <- NULL
  
  sort_15_25$bal_percent <- NULL
  sort_15_25$bal_prin2 <- NULL
  sort_15_25$th_due_rate <- NULL
  
  md1 <- glm(sort~.,data = sort_15_25,family = binomial)
  summary(md1)
  
  pred2 <- predict(md1,sort_15_25,type='response')
  pred2 <- as.numeric(as.matrix(pred2))
  plot(pred2[order(pred2)],type='l',main = "glm_2------------logistic")
  
  #roc 
  modelroc <- roc(sort_15_25$sort,pred2)
  plot(modelroc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
       grid.col=c("green", "red"), max.auc.polygon=TRUE,
       auc.polygon.col="skyblue", print.thres=TRUE)
  
  ##测试数据的结果----------------5折交叉验证
  w <- sort_15_25
  n<-nrow(sort_15_25);zz1 <- 1:n  #所有观察的下标
  
  zz2 = rep(1:5,ceiling(n/5))[1:n]
  set.seed(100);zz2 <-sample(zz2,n) #为1-5的所及排序
  NMSE <-rep(0,5);NMSEO =NMSE
  
  classify <- function(probability)ifelse(probability<0.5,'0','1')
  acc <- vector()
  call_back <- vector()
  for (i in 1:5){
    m =zz1[zz2==i]
    a =glm(sort~.,data = w[-m,],family = binomial)
    y0=predict(a,w[-m,])
    y1 = predict(a,w[m,])
    
    classified <- classify(y1)
    sort <- w[m,]$sort
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
    print(accuracy)
    print(call_back)
    print(Specificity)
    print(real_in_prediction)
    print(predict_rate)
  }
  
  
  ############################################################################################################################
  #####################---------------------------------对变量进行分箱--------------------------------------##################
  ############################################################################################################################
  sort_woe <- sort_15_25
  
  #no_call_days
  ks_data(sort_15_25$no_call_days,sort_15_25$sort,20)
  plot(dltfun$联系比率,type = 'l',lwd = 2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$no_call_days <- ifelse(sort_15_25$no_call_days <= breaks[2],0,
                                ifelse(sort_15_25$no_call_days <= breaks[6],1,2))
  
  #seven_days_eff_logs
  ks_data(sort_15_25$seven_days_eff_logs,sort_15_25$sort,20)
  plot(dltfun$联系比率,type = 'l',lwd = 2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$seven_days_eff_logs <- ifelse(sort_15_25$seven_days_eff_logs == 0,0,
                                  ifelse(sort_15_25$seven_days_eff_logs <= breaks[3],1,2))
  
  #sx_eff_rate
  ks_data(sort_15_25$sx_eff_rate,sort_15_25$sort,20)
  plot(dltfun$联系比率,type = 'l', lwd =2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$sx_eff_rate <- ifelse(sort_15_25$sx_eff_rate == 0,0,
                                  ifelse(sort_15_25$sx_eff_rate <= breaks[8],1,
                                         ifelse(sort_15_25$sx_eff_rate <= breaks[12],2,3)))

  #lastpay_tonow
  ks_data(sort_15_25$lastpay_tonow,sort_15_25$sort,20)
  plot(dltfun$联系比率, type = 'l', lwd = 2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$lastpay_tonow <- ifelse(sort_15_25$lastpay_tonow <= breaks[4],0,
                                 ifelse(sort_15_25$lastpay_tonow <= breaks[13],1,2))

  #LOANTERM
  ks_data(sort_15_25$LOANTERM,sort_15_25$sort,4)
  plot(dltfun$联系比率,type = 'l',lwd =2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$LOANTERM <- ifelse(sort_15_25$LOANTERM <= breaks[1],0,1)
  
  #hisdue_times
  ks_data(sort_15_25$hisdue_times,sort_15_25$sort,20)
  plot(dltfun$联系比率,type = 'l',lwd =2)
  breaks <- as.numeric(as.matrix(dltfun$当组最大值))
  sort_woe$hisdue_times <- ifelse(sort_15_25$hisdue_times <= breaks[5],0,
                              ifelse(sort_15_25$hisdue_times <= breaks[9],1,2))
  
  ########################################################################################################
  ##############---------------------------------求取woe-------------------------------###################
  ########################################################################################################
  
  #if_jyd
  woe(sort_woe$sort,sort_woe$if_jyd);print(result)
  kniff
  sort_woe$if_jyd <- ifelse(sort_woe$if_jyd==0,result$woe[1],result$woe[2])
  
  #no_call_days
  woe(sort_woe$sort,sort_woe$no_call_days);print(result);kniff
  sort_woe$no_call_days <- ifelse(sort_woe$no_call_days==0,result$woe[1],
                                  ifelse(sort_woe$no_call_days==1,result$woe[2],result$woe[3]))
  
  #seven_days_eff_logs
  woe(sort_woe$sort,sort_woe$seven_days_eff_logs);print(result);kniff
  sort_woe$seven_days_eff_logs <- ifelse(sort_woe$seven_days_eff_logs==0,result$woe[1],
                                  ifelse(sort_woe$seven_days_eff_logs==1,result$woe[2],result$woe[3]))
  
  #sx_eff_rate
  woe(sort_woe$sort,sort_woe$sx_eff_rate);print(result);kniff
  sort_woe$sx_eff_rate <- ifelse(sort_woe$sx_eff_rate==0,result$woe[1],
                                         ifelse(sort_woe$sx_eff_rate==1,result$woe[2],
                                                ifelse(sort_woe$sx_eff_rate==2,result$woe[3],result$woe[4])))
  
  #lastpay_tonow
  woe(sort_woe$sort,sort_woe$lastpay_tonow);print(result);kniff
  sort_woe$lastpay_tonow <- ifelse(sort_woe$lastpay_tonow==0,result$woe[1],
                                 ifelse(sort_woe$lastpay_tonow==1,result$woe[2],result$woe[3]))
  
  #LOANTERM
  woe(sort_woe$sort,sort_woe$LOANTERM);print(result);kniff
  sort_woe$LOANTERM <- ifelse(sort_woe$LOANTERM==0,result$woe[1],result$woe[2])
  
  #hisdue_times
  woe(sort_woe$sort,sort_woe$hisdue_times);print(result);kniff
  sort_woe$hisdue_times <- ifelse(sort_woe$hisdue_times==0,result$woe[1],
                                  ifelse(sort_woe$hisdue_times==1,result$woe[2],result$woe[3]))

  ##################################################################################################
  #############------------------------进行打分----------------#####################################
  ##################################################################################################
  md_woe <-glm(sort~.,data=sort_woe,family = binomial(link = 'logit'))
  summary(md_woe)
  
  est <-md_woe$coefficients
  
  #1:1是好坏比决定的基础分值,增加一个胜率高50分
  p <- 50/log(2)
  
  q <- 500-50*log(1)/log(2)
  base <- q + p*est[[1]]
  base
  
  getscore<-function(i,x){
    score = round(p*est[[i]]*x,0)
    return(score)
  }
  
  odds <- est[[1]] + sort_woe$LOANTERM*est[[2]]+sort_woe$hisdue_times*est[[3]]+sort_woe$no_call_days*est[[4]]+
          sort_woe$sx_eff_rate*est[[5]]+sort_woe$if_jyd*est[[6]]+sort_woe$lastpay_tonow*est[[7]]+
          sort_woe$seven_days_eff_logs*est[[8]]
  
  score <- q + p*odds
  hist(score,breaks = c(100),xlab = "分数", ylab = '频数情况',main = '分数的频数图',col = 'lightblue')
  
  unique(sort_woe$LOANTERM)
  getscore(2,unique(sort_woe$LOANTERM))
  
  unique(sort_woe$hisdue_times)
  getscore(3,unique(sort_woe$hisdue_times))
  
  unique(sort_woe$no_call_days)
  getscore(4,unique(sort_woe$no_call_days))
  
  unique(sort_woe$sx_eff_rate)
  getscore(5,unique(sort_woe$sx_eff_rate))
  
  unique(sort_woe$if_jyd)
  getscore(6,unique(sort_woe$if_jyd))
  
  unique(sort_woe$lastpay_tonow)
  getscore(7,unique(sort_woe$lastpay_tonow))
  
  unique(sort_woe$seven_days_eff_logs)
  getscore(8,unique(sort_woe$seven_days_eff_logs))
  
  ##############################对于进入量增加观察混淆矩阵的变化*************************************
  
  sort_woe$score <- score
  maybe <- sort_woe[order(sort_woe$score,decreasing = T),]
  
  auc <- NULL
  cb <- NULL
  fr <- NULL
  rp<- NULL
  goin <- NULL
  for(i in seq(0.01,1,0.01)){
    len <- nrow(maybe)
    eggs <- rep(0,len)
    eggs[1:(len*i)] <-1
    sort <- maybe$sort
    confusion_matrix <- table(sort,eggs,dnn=c('real','prediction'))
    a <- as.data.frame(confusion_matrix)
    a1 <-as.numeric(as.character(a$real))
    a2<-as.numeric(as.character(a$prediction))
    accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
    Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
    Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])
    (real_in_prediction <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a2==1]))
    predict_rate <- sum(a$Freq[a2==1])/sum(a$Freq)  #这个值的比例在0.05
    call_back <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a1==1])
    accuracy;Sensitivity;Specificity;real_in_prediction;predict_rate;call_back
    print(confusion_matrix)
    auc <- c(auc,accuracy)
    cb <- c(cb,call_back)
    fr <- c(fr,Specificity)
    rp <- c(rp,real_in_prediction)
    goin <- c(goin,predict_rate)
  }
  
  index <- data.frame(auc = auc*100, cb = cb*100, fr = fr*100,rp = rp*100)
  matplot(index,type = 'l',lwd=3,col=c(1:4),xlab = '进入百分比',ylab = '百分比')
  title(main = '进入量对分类的影响')
  legend(83,80, inset=.05, legend=c('准确性','查全率','错误识别','查准率'), pch=10, col=c(1:4), horiz=FALSE) 
  
  ks_data(sort_woe$score,sort_woe$sort,10)
  
