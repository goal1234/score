  info_good <- read.csv('info_good.csv',stringsAsFactors = FALSE)
  info_bad <- read.csv('info_bad.csv',stringsAsFactors = FALSE)
  
  data <- info_good
  data$days_last_p[is.na(data$days_last_p)]=max(data$days_last_p)
  data$maxdue_days[is.na(data$maxdue_days)] = 0
  #将order_connected变为分类变量
  data$order_connected <- with(data,
                               ifelse(order_connected %in% c(1,2,3),0,1))
  #筛选dpd >= 4的数据
  data_high <- data[data$dpd >= 4,]
  
  #属性变量数据集
  factors <- c('order_connected','if_jyd','if_new_case','if_new_rtp','if_seek','last_activitive',
               'last_code','prd','svdays_payoff_times','thdays_payoff_times')
  data_factors <- data_high[,factors]
  
  ############################################################################################
  
  data_used <- data_factors[,c(1,2)]
  #----------------------------------------------------
  fitted_model <- data_used %>% 
    glm(data_used[,1]~data_used[,2]+0,
        family = "binomial",data=.)
  summary(fitted_model)
  ks.test(fitted_model$residuals,'pnorm')
  
  data_used %>% 
    ggplot(aes(x= data_used[,2],y=data_used[,1])) +
    geom_jitter(height = 0.05,width = 0.3,alpha=0.4) +
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #根据好坏分类
  classify <- function(probability) ifelse(probability<0.5,'not good','maybe')
  classified_malignant <- classify(predict(fitted_model,data_used,type='response'))
  classified <- classify(predict(fitted_model,data_used))
  
  #预测的准确性
  confusion_matrix <- table(data_used[,1],classified,dnn=c('order_connected','prediction'))
  (accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
 
  #来个表
  table(data_used[,1],classified_malignant)
  table(data_used[,1],classified_malignant,dnn=c("DATA","Predictions"))
  
  #--------------------------------------数值型变量--------------------------------------
  num <- c('order_connected','to_rtp_days','days_last_p','due_amount','to_rtp_days')
  rate <- c('return_to_loan_term','sv_due_rate','sx_eff_rate','sx_kp_rate','th_due_rate','th_eff_rate',
            'th_kp_rate','th_count_rate','sx_count_rate')
  data_rate <- data_high[,rate]
  
  data_rate$order_connected <- as.factor(data_high$order_connected)
  
  ggplot(data_rate,aes(x=order_connected,y=return_to_loan_term))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=sv_due_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=sx_eff_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=sx_kp_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=th_due_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=th_eff_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=th_kp_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=th_kp_rate))+geom_boxplot()
  ggplot(data_rate,aes(x=order_connected,y=th_kp_rate))+geom_boxplot()
 
  data_used <- data_rate[,c(1,2,3)]
  #-----------------------------------------------------------------------------------
  fitted_model <- data_used %>% 
    glm(data_used[,1]~data_used[,2]+data_used[,3]+0,
        family = "binomial",data=.)
  summary(fitted_model)
  ks.test(fitted_model,'pnorm')
  
  data_used %>% 
    ggplot(aes(x= data_used[,y],y=data_used[,1])) +
    geom_jitter(height = 0.05,width = 0.3,alpha=0.4) +
    geom_smooth(method='glm',
                method.args=list(family="binomial"))
  
  #根据好坏分类
  classify <- function(probability) ifelse(probability<0.9,'not good','maybe')         #
  classified_malignant <- classify(predict(fitted_model,data_used,type='response'))
  classified <- classify(predict(fitted_model,data_used))
  
  #预测的准确性
  confusion_matrix <- table(data_used[,1],classified,dnn=c('order_connected','prediction'))
  (accuracy <- sum(diag(confusion_matrix))/sum(confusion_matrix))
  
  #来个表
  table(data_used[,1],classified_malignant)
  table(data_used[,1],classified_malignant,dnn=c("DATA","Predictions"))
  