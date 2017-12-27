  data_model <- all_data
  x <- c("sort",'days_last_p','to_rtp_days','if_jyd','if_new_case','if_seek','sv_due_rate' ,'return_to_loan_term',
         'due_amount','lastpay_tonow','sx_eff_rate','age0','province','acdamic_type1','seven_days_logs','seven_days_eff_logs')
  data_transform <- data_model[,x]
  data_transform$sort <- ifelse(data_transform$sort != 500, 1, 0)
  
  ##自变量分类更加多，其将更加符合logistic曲线的假设
  break_day_last_p <- c(0,95,115,135,155,175,195,215,235,225,max(data_transform$days_last_p,na.rm = T))
  data_transform$days_last_p <- as.numeric(cut(data_transform$days_last_p,break_day_last_p))
  rm(break_day_last_p)
  
  break_lastpay_tonow <- c(0,110,120,130,140,150,160,170,180,190,max(data_transform$lastpay_tonow))
  data_transform$lastpay_tonow <- as.numeric(cut(data_transform$lastpay_tonow,break_lastpay_tonow))
  rm(break_lastpay_tonow)
  
  break_sx_eff_rate <- c(0,0.05,0.1,0.2,0.3,0.4,0.5,0.7,0.8,1)
  data_transform$sx_eff_rate <- as.numeric(cut(data_transform$sx_eff_rate,break_sx_eff_rate))
  rm(break_sx_eff_rate)
  
  break_return_loanterm <- c(0,0.2,0.4,0.6,0.8,max(data_transform$return_to_loan_term))
  data_transform$return_to_loan_term <- as.numeric(cut(data_transform$return_to_loan_term,break_return_loanterm))
  rm(break_return_loanterm)
  
  break_seven_days_logs <- c(0,2,10,20,30,40,max(data_transform$seven_days_logs))
  data_transform$seven_days_logs <- as.numeric(cut(data_transform$seven_days_logs,break_seven_days_logs))
  rm(break_seven_days_logs)
  
  break_due_amount <- c(0,7000,11000,13000,15000,16000,18000,20000,25000,28000,max(data_transform$due_amount))
  data_transform$due_amount <- as.numeric(cut(data_transform$due_amount,break_due_amount))
  rm(break_due_amount)
  
  break_to_rtp <- c(0,3,6,9,12,15,18,21,24,27,max(data_transform$to_rtp_days))
  data_transform$to_rtp_days <- as.numeric(cut(data_transform$to_rtp_days,break_to_rtp))
  rm(break_to_rtp)
  
  data_transform$acdamic_type1 <- ifelse(data_transform$acdamic_type1 > 3 ,1, 0)
  data_transform <- na.omit(data_transform)

  #-------------------------------------------------------------------------------------------------------------------
  md <- glm(sort~.,data=data_transform,family = binomial(link='logit'))
  summary(md)  
  anova(md,test='Chisq')
  md1 <- glm(sort~.-if_seek,data =data_transform,family = binomial(link='logit'))
  
  md2 <-glm(sort~.-if_seek-age0-sv_due_rate-seven_days_eff_logs-days_last_p,data=data_transform,family = binomial(link='logit'))
  summary(md2) 
  anova(md,md2,test = 'Chisq')
  
  md3 <-glm(sort~.-if_seek-age0-sv_due_rate-seven_days_eff_logs-days_last_p-seven_days_logs,data=data_transform,family = binomial(link='logit'))
  summary(md3) 
  anova(md2,md3,test = 'Chisq')
  
  
  md4 <-glm(sort~.-if_seek-age0-sv_due_rate-if_jyd-seven_days_eff_logs-days_last_p-seven_days_logs,data=data_transform,family = binomial(link='logit'))
  summary(md4) 
  anova(md3,md4,test = 'Chisq')
  a <- predict(md3,data_transform,type='response')
  a <- as.numeric(as.matrix(a))
  plot(a[order(a)])
  
  md5 <-glm(sort~.-if_seek-age0-sv_due_rate-if_jyd-seven_days_eff_logs-days_last_p-seven_days_logs,data=data_transform,family = binomial(link='logit'))
  anova(md4,md5,test = 'Chisq')
  