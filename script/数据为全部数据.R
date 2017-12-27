#######################################################################
  setwd("E:/order/data")
  library(sqldf)
  library(tcltk)
  library(dplyr)
  library(MASS)
#library(readr)
#-----------------------------------------------------------------------------------------------------------
  file_name <- 'full.csv'
  glm_sort <- read.csv(file_name,stringsAsFactors = F)
  age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)
  glm_sort <- glm_sort[glm_sort$dpd %in% c(4:6),]
  
  all_data <- merge(glm_sort,age_sex,by.x = "customer_code",by.y = "CUSTOMER_CODE",all.x = T)
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
  #测试之后，发现了观察值少了43个，总体数据有70多万个观察自变量有21个
####################################################################################################################
###########################################对总体的数据进行建模#####################################################
#选出账期在4:6的，建模数据为之前，预测为之后
  data_for_model <- all_data
  data_for_add <- data_for_model[data_for_model$sort == 500,]
  data_for_add <- data_for_add[sample(nrow(data_for_add),0.60*nrow(data_for_add)),]
  data_for_add1 <- data_for_model[data_for_model$sort != 500,]
  data_for_model <- rbind.data.frame(data_for_add,data_for_add1)
  rm(data_for_add,data_for_add1)
  
  data_for_model$enter_date2 <- as.Date(data_for_model$enter_date2)
  data_for_fit <- filter(data_for_model,enter_date2 <= '2016/3/31')
  data_for_test <- filter(data_for_model,enter_date2 > '2016/3/31')

  (nrow(data_for_fit)/nrow(data_for_model))
  (a <- sum(data_for_fit$sort != 500)/nrow(data_for_fit))
  (b <- sum(data_for_test$sort != 500)/nrow(data_for_test))
  change <- abs((a-b)/a)
  if(change > 0.1) winDialog(type=c('ok'),'------这个世界变化太快，赶快用enter_date2来倒带-----------')
  rm(a,b,change)
  
  
#####################################################变形金刚gogogo#######################
#data_transform为中间处理的，输出data_fit数据框用于建模
  x <- c("sort",'days_last_p','to_rtp_days','if_jyd','if_new_case','if_seek','sv_due_rate' ,'return_to_loan_term',
         'due_amount','lastpay_tonow','sx_eff_rate','age0','province','acdamic_type1','seven_days_logs','seven_days_eff_logs')
  data_transform <- data_for_fit[,x]
  
  #for(i in 2:15) boxplot(data_transform[,i],main=names(data_transform)[i])

  data_transform$days_last_p <- with(data_transform,ifelse(days_last_p <= 120,0,1))
  data_transform$sx_eff_rate <- with(data_transform,ifelse(sx_eff_rate==0,0,1))
  
  data_transform$if_jyd[is.na(data_transform$if_jyd)] <-0
  data_transform$if_new_case[is.na(data_transform$if_new_case)] <-0
  data_transform$if_seek[is.na(data_transform$if_seek)] <-0
  data_transform$acdamic_type1 <- ifelse(data_transform$acdamic_type1 > 3 ,1, 0)
  
  #数值的变量
  data_transform$sv_due_rate[is.na(data_transform$sv_due_rate)] <-0
  data_transform$return_to_loan_term[is.na(data_transform$return_to_loan_term)] <- 0 
  data_transform$due_amount[is.na(data_transform$due_amount)] <- 0

  data_transform$return_to_loan_term <- ifelse(data_transform$return_to_loan_term <0.2,1,ifelse(data_transform$return_to_loan_term >0.80,1,0))
  data_transform$sv_due_rate <- ifelse(data_transform$sv_due_rate <0.40,0,1)  #
  data_transform$lastpay_tonow <- ifelse(data_transform$lastpay_tonow >= 115,1,0)
  data_transform$due_amount <- ifelse(data_transform$due_amount < 10000,0,ifelse(data_transform$due_amount>25000,1,0))
  data_transform$to_rtp_days <- ifelse(data_transform$to_rtp_days <7,0,
                                  ifelse(data_transform$to_rtp_days <14,1,
                                       ifelse(data_transform$to_rtp_days <21,2,3)))
  data_transform$age0 <- ifelse(data_transform$age0 < 30,0,ifelse(data_transform$age0 >45,1,0))
  data_transform$seven_days_logs <- ifelse(data_transform$seven_days_logs>3,1,0)
  data_transform$seven_days_eff_logs <- ifelse(data_transform$seven_days_eff_logs > 3,1,0)
  
  
  #sort值的切割
  data_model2_fit <- data_transform
  data_model2_fit <- na.omit(data_model2_fit)
  data_model2_fit$sort <- ifelse(data_model2_fit$sort <6,1,
                                 ifelse(data_model2_fit$sort <12,2,
                                        ifelse(data_model2_fit$sort <18,3,
                                               ifelse(data_model2_fit$sort <24,4,5))))
  data_model2_fit$sort <- as.factor(data_model2_fit$sort)
  data_transform$sort <- ifelse(data_transform$sort < 500,1,0)
  data_transform <- na.omit(data_transform)
  
  #output the data for fit 
  data_fit <- data_transform
  
  sum(data_fit$sort==1)/nrow(data_fit)
  #################################################################################################################
  ############################################构建模型###############################
  #--first model
  md_glm <- data_fit %>% glm(sort~.-if_seek-seven_days_eff_logs-age0-sv_due_rate,data=.,family =binomial(link = "logit"),
                             control = glm.control(epsilon = 1e-6, maxit = 30, trace = FALSE))#binomial(link = 'logit')
  summary(md_glm)
  anova(md_glm, test="Chisq")
  plot(md_glm$residuals,main = '残差图')
  
  #模型得到了改进
  odd <- vector()
  for(i in 2:length(md_glm$coefficients)) {
    odd <- c(odd,exp(md_glm$coefficients[[i]]))
  }
  
  col <- names(md_glm$coefficients)[2:length(md_glm$coefficients)]
  odd <- data.frame(name = col,odd =odd)
  print(odd)
  
  pred <- predict(md_glm,data_for_fit,type='response')
  pred <- as.numeric(as.matrix(pred))
  plot(pred[order(pred)],main = 'Are you logistic curve ??')
  

  
  #---2 model
  md_glm2 <- glm(sort~.-if_seek-age0-sv_due_rate,data = data_fit,family = poisson(link = 'log'),
                 control = glm.control(epsilon = 1e-7, maxit = 30, trace = FALSE))
  anova(md_glm2,test = 'Chisq')
  summary(md_glm2)
  anova(md_glm,md_glm2,test = 'Chisq')  #第二个模型更好选用第二个模型
  
  #变化太快不能完成
  #md_glm3 <- lm(sort~.-if_seek-seven_days_logs-age0-sv_due_rate,data = data_fit,family =quasibinomial(link='logit'))
  ################################################################################################################
  #######################################用于预测############################################
  x <- c("sort",'USER_NAME','enter_date2','days_last_p','to_rtp_days','if_jyd','if_new_case','if_seek','sv_due_rate' ,
         'return_to_loan_term','due_amount','lastpay_tonow','sx_eff_rate','age0','province','acdamic_type1','seven_days_logs','seven_days_eff_logs')
  data_transform <- data_for_test[,x]
  
  #for(i in 2:15) boxplot(data_transform[,i],main=names(data_transform)[i])
  
  data_transform$days_last_p <- with(data_transform,ifelse(days_last_p <= 120,0,1))
  data_transform$sx_eff_rate <- with(data_transform,ifelse(sx_eff_rate==0,0,1))
  data_transform$if_jyd[is.na(data_transform$if_jyd)] <-0
  data_transform$if_new_case[is.na(data_transform$if_new_case)] <-0
  data_transform$if_seek[is.na(data_transform$if_seek)] <-0
  data_transform$acdamic_type1 <- ifelse(data_transform$acdamic_type1 > 3 ,1, 0)
  
  #数值的变量
  data_transform$sv_due_rate[is.na(data_transform$sv_due_rate)] <-0
  data_transform$return_to_loan_term[is.na(data_transform$return_to_loan_term)] <- 0 
  data_transform$due_amount[is.na(data_transform$due_amount)] <- 0
  
  data_transform$return_to_loan_term <- ifelse(data_transform$return_to_loan_term <0.2,1,ifelse(data_transform$return_to_loan_term >0.80,1,0))
  
  data_transform$sv_due_rate <- ifelse(data_transform$sv_due_rate <0.40,0,1)  #
  data_transform$lastpay_tonow <- ifelse(data_transform$lastpay_tonow >= 115,1,0)
  data_transform$due_amount <- ifelse(data_transform$due_amount < 10000,0,ifelse(data_transform$due_amount>25000,1,0))
  data_transform$to_rtp_days <- ifelse(data_transform$to_rtp_days <7,0,
                                       ifelse(data_transform$to_rtp_days <14,1,
                                              ifelse(data_transform$to_rtp_days <21,2,3)))
  data_transform$age0 <- ifelse(data_transform$age0 < 30,0,ifelse(data_transform$age0 >45,1,0))
  data_transform$seven_days_logs <- ifelse(data_transform$seven_days_logs>3,1,0)
  data_transform$seven_days_eff_logs <- ifelse(data_transform$seven_days_eff_logs > 3,1,0)
  #sort值的切割
  
  data_model2_test <- data_transform
  data_model2_test <- na.omit(data_model2_test)
  data_model2_test$sort <- ifelse(data_model2_test$sort <6,1,
                                 ifelse(data_model2_test$sort <12,2,
                                        ifelse(data_model2_test$sort <18,3,
                                               ifelse(data_model2_test$sort <24,4,5))))
  data_transform <- na.omit(data_transform)
  
  #output the data for fit 
  data_test <- data_transform
  
  ####################################################################################################
  ###########################测试的数据集上效果怎么样
  #md_glm--------------------------------------------------------------------------------------------
  a <- predict(md_glm,data_test[,-c(1,2,3)],type='response')
  a <- as.numeric(as.matrix(a))
  plot(a[order(a)],main = 'Hello logistic curve')
  data_test$pred <- a
  rm(a)
  
  data_test <-data_test %>%
    group_by(USER_NAME) %>% arrange(enter_date2,USER_NAME)
  
  #################------------------------------分组排序好不容易------------------------------------##########
  data <-aggregate(data_test$pred,data_test[,c(2,3)],FUN=rank,ties.method=c('first'))
  data$max <-lapply(data$x,max)
  sort <- vector()
  for(i in 1:nrow(data)){
    b <-data$max[[i]]-data$x[[i]] +1
    sort <- c(sort,b)
  }
  sort1 <-as.numeric(as.matrix(unlist(data$x)))
  data_test$sort_pred <- sort
  
  result_model1 <- data_test[,c(1,2,3,19,20)]
  result_detail <- result_model1
  result_model1$sort <- ifelse(result_model1$sort %in% c(1:4),1,ifelse(result_model1$sort %in% c(5:12),2,ifelse(result_model1$sort ==500,4,3)))
  result_model1$sort_pred <- ifelse(result_model1$sort_pred %in% c(1:4),1,ifelse(result_model1$sort_pred %in% c(5:12),2,
                                                                                 ifelse(result_model1$sort_pred %in% c(13,24),3,4)))
  #预测的标准与真值的差距  
  #(MSE <- sum((result[,1]-result[,4])^2)/nrow(result))
  #(RMSE <-sqrt(sum((result_model1[,1]-result_model1[,4])^2)/nrow(result)))
  #print(RMSE)
  #cor(result$sort,result$sort_pred,method = 'kendall')  计算时间太长
  #(MAE <- sum(abs(result[,1]-result[,4]))/nrow(result))
  #(SD <-sqrt(sum((result[,4]-mean(result[,1]))^2)/nrow(result)))
  table(result_model1$sort,result_model1$sort_pred)
  prop.table(table(result_model1$sort,result_model1$sort_pred),1)
  prop.table(table(result_model1$sort,result_model1$sort_pred),2)
  
  