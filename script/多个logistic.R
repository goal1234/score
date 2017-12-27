  #三个logistic模型

  #copy data no na
  data_model <- all_data
  
  #计算0值占比情况
  data_nrow <- nrow(data_model)
  per_zero <- vector()
  long　<- ncol(data_model)
  for(i in 4:long){
    a = sum(data_model[,i]==0)/data_nrow
    per_zero <- c(per_zero,a)
  }
  label <- colnames(data_model)[4:long]
  per_result <- data.frame(name = label,per = per_zero)
  per_result[order(per_result$per,decreasing = T),]
  rm(data_nrow,per_zero,label)
  
  barplot(per_result$per)
  axis(1,at = seq(1,22.8,by=1.2),labels = per_result$name,cex.axis = 0.5)
  text(1:19, per_result$per,per_result$name, cex = 1, pos = 4, col = 1)
  
  #对于本来变量的百分比做回归
  high_zero <- c("sort","USER_NAME","enter_date2","if_seek","if_new_case","seven_days_eff_logs","if_jyd","sx_eff_rate",
                 "sv_due_rate",'seven_days_logs','days_last_p')
  mid_zero <- c("sort","USER_NAME","enter_date2",'acdamic_type1','province','return_to_loan_term')
  low_zero <- c("sort","USER_NAME","enter_date2","due_amount","to_rtp_days","lastpay_tonow","age0")
  
  high_z <- all_data[,high_zero]
  high_z <- high_z[high_z$sort != 500,]

  mid_z <- all_data[,mid_zero]
  
  low_z <- all_data[,low_zero]
 
  #--------------------------------------high model----------------------------------------
  #计算0值占比
  how_z(high_z);print(per_result)
  
  #切割数据seven_days_eff_logs,sx_eff_rate,sv_due_rate,sv_due_rate,days_last_p
  #0值占比太多的自变量直接进行0，1分组
  #一个类似直线的概率，其中概率最大值为0.8.
  high_z$sort <- ifelse(high_z$sort < 9,1,0)    #<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<sort的取值比较敏感
                                                #3占比0.19,5占比0.39；6占比0.43，7为0.49 8为0.55 9为0.61
  sum(high_z$sort==1)/12876
  
  ks_data(high_z$seven_days_eff_logs,high_z$sort,20)
  (IVdlt <- sum(dltfun$IV数据源))
  (KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')
  
  ks_data(high_z$sv_due_rate,high_z$sort,20)
  (IVdlt <- sum(dltfun$IV数据源))
  (KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')
  
  ks_data(high_z$sx_eff_rate,high_z$sort,20)
  (IVdlt <- sum(dltfun$IV数据源))
  (KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')
  
  ks_data(high_z$days_last_p,high_z$sort,20)
  (IVdlt <- sum(dltfun$IV数据源))
  (KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')
  
  ks_data(high_z$seven_days_eff_logs,high_z$sort,20)
  (IVdlt <- sum(dltfun$IV数据源))
  (KSdlt <- abs(max(dltfun$KS数据源)));plot(dltfun$坏账率,type='l')
  
  md_high <- glm(sort~seven_days_eff_logs+if_jyd+sx_eff_rate+
                   sv_due_rate+seven_days_logs+days_last_p,data = high_z,family = binomial(link = 'logit'))
  pred1 <- predict.glm(md_high,high_z[,-c(1:3)],type='response')
  pred1 <- as.numeric(as.matrix(pred1))
  plot(pred1[order(pred1)],main = 'Are you logistic curve ??')
  x <- pred1[order(pred1)]
  
  #---------------------------------------mid model------------------------------------------
  #一个类似双曲的反映曲线，最大入选概率为0.040。
  mid_z$sort <- ifelse(mid_z$sort !=500 ,1,0)
  sum(mid_z$sort==1)/nrow(mid_z)  #0.029
  how_z(mid_z);print(per_result)
  md_mid <- glm(sort~.-USER_NAME-enter_date2,data = mid_z,family = binomial(link = 'logit'))
  pred2 <- predict(md_mid,mid_z,type='response')
  pred2 <- as.numeric(as.matrix(pred2))
  plot(pred2[order(pred2)],main = 'Are you coming for logistic ?')
  y <- pred2[order(pred2)]
  
  #---------------------------------------low_model--------------------------------------------
  #依旧不是一个logistic曲线，最大p为0.15
  low_z$sort <- ifelse(low_z$sort !=500, 1, 0)
  sum(low_z$sort==1)/nrow(low_z)
  how_z(low_z);print(per_result)
  
  #值需要切割下
  md_low <- glm(sort~.-USER_NAME-enter_date2,data = low_z,family = binomial(link = 'logit'))
  pred3 <- predict(md_low,low_z,type='response')
  pred3 <- as.numeric(as.matrix(pred3))
  plot(pred3[order(pred3)],main = 'Who are you ?')
  z <- pred3[order(pred3)]
  
  
  #三维散点图
  #source("http://bioconductor.org/biocLite.R")
  #biocLite("scatterplot3d")
  library("scatterplot3d")
  scatterplot3d(pred1[1:12786], pred2[1:12786], pred3[1:12786], highlight.3d=TRUE, col.axis="blue", col.grid="lightblue",
                main="scatterplot3d - 1", pch=20)
  
  plot3d(pred2[1:10000],pred3[1:10000],pred1[1:10000],col = rainbow(10000),size = 0.9)
  
  
  