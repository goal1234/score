  ##变量的单调性
  data_p_mon <- all_data
  x <- c('sort','due_amount','days_last_p','lastpay_tonow','sx_eff_rate','return_to_loan_term','seven_days_logs',
         'age0','sv_due_rate','to_rtp_days','seven_days_eff_logs')
  data_p_mon <- data_p_mon[,x]
  data_p_mon$sort <- ifelse(data_p_mon$sort != 500,1,0)
  
  
  ##-----------------------------按照百分百切割----------------------------------##
  #数据集的名字为data_p_mon，循环的顺序需要选择，开始到结束的位置
  for(j in 4:7){
    gaga <- data.frame(what = data_p_mon[,1], meat = data_p_mon[,j])
  
    ##从小到大
    gaga <- gaga[order(gaga$meat),]
  
    ##分组100个
    a <- rep(1:100,each=nrow(gaga)/100)
    b <- rep(1,nrow(gaga)%%100)
    kniff <- c(b,a)
    gaga$kniff <- kniff
  
    ##画出图来
    rate <- list()
    for(i in 1:100){
      gogo <- filter(gaga,kniff==i)
      rate[[i]] <- sum(gogo$what==1)/nrow(gogo)
    }
  plot(as.numeric(rate),type='l',lwd =3,main=names(data_p_mon)[j])
  }
  
  b <- list()
  for(i in 1:10){
    b <- kmeans(data_p_mon[,2],10)
    b$centers
    b[[i]] <- b
  }
  #----------------------------------------------------------------------------#
  
  ##############################################################################
  #按照数值切割
  
  for(j in 4:7){
  blade <- as.numeric(quantile(data_p_mon[,j],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  kniff <- as.numeric(cut(data_p_mon[,j],blade))
  
  ##形成需要的数据集
  gaga <- data.frame(what = data_p_mon[,1],meat = data_p_mon[,j],kniff = kniff)
  
  ##画出每个图
  rate <- list()
  num <- list()
    for(i in 1:100){
      gogo <- filter(gaga,kniff==i)
      num[[i]] <- nrow(gogo)
      rate[[i]] <- sum(gogo$what==1)/nrow(gogo)
    }
   rate <- na.omit(as.numeric(rate))
   num <- na.omit(as.numeric(num))
   num <- num[1:sum(num != 0)]
   par(mfrow=c(2,1))
    plot(as.numeric(rate),type='b',lwd =3,main=names(data_p_mon)[j])
    plot(as.numeric(num),type = 'h',lwd=5)
   par(mfrow = c(1,1))
  }
  
  
  ##切割后建模------
  #单变量
  #due_amount----------变为了4个分类的变量
  add_data <- data.frame(target = data_p_mon[,1])
  #-----------
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,2])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  (blade <- unique(blade))
  blade <- c(blade[1],blade[15],blade[23],blade[47],136659.7089)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$due_amount <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  
  #days_last_p
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,3])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[5],blade[11],blade[26],blade[39],887)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$days_last_p <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #lastpay_tonow
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,4])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[20],blade[36],451)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$lastpay_tonow <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  summary(md1)
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #sx_eff_rate--------------------------------------------------------------图形效果不好
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,5])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[4],blade[10],blade[17],1)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$sx_eff_rate <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))

  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #return_to_loan_term------------图形效果不好
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,6])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[21],blade[31],1)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$return_to_loan_term <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
 
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #seven_days_logs
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,7])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[3],blade[7],36)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$seven_days_logs <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #age0 ------------------------------------------图形不是调调
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,8])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[29],63)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$age0 <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #sv_due_rate
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,9])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[4],blade[9],1)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$sv_due_rate <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))

  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  #to_rtp_days
  one_pice <- data.frame(target = data_p_mon[,1], x = data_p_mon[,10])
  
  blade <- as.numeric(quantile(one_pice[,2],probs = seq(0,1,0.01)))
  blade <- unique(blade)
  print(blade)
  blade <- c(blade[1],blade[7],blade[15],blade[22],31)
  kniff <- as.numeric(cut(one_pice[,2],blade))
  add_data$to_rtp_days <- kniff
  md <- data.frame(target = data_p_mon[,1], x = kniff)
  md1 <- glm(target~kniff,data = md,family = binomial(link = 'logit'))
  
  p_value <- summary(md1)$coefficients[,4]
  if(p_value[2] < 0.05) print("maybe good")
  add <- glm(target~.,data=add_data,family = binomial(link = 'logit'))
  print(summary(add))
  
  #------------------------------------ks值和iv值----------------------------------------
  for(j in 2:11){
    blade <- as.numeric(quantile(data_p_mon[,j],probs = seq(0,1,0.01)))
    blade <- unique(blade)
    kniff <- as.numeric(cut(data_p_mon[,j],blade))
    
    ##形成需要的数据集
    gaga <- data.frame(what = data_p_mon[,1],meat = data_p_mon[,j],kniff = kniff)
    gaga <- na.omit(gaga)
    rows_bad <- sum(gaga$what==0);rows_good <- sum(gaga$what == 1)
    
    #ks值woe和iv值
    ks <- NULL
    woe <- NULL
    iv_kniff <- NULL
    good <-  NULL
    bad <- NULL
    for(i in 1:100){
      gogo <- filter(gaga,kniff==i)
      gogo <- na.omit(gogo)
    
      #ks值
      good1 <- sum(gogo$what==1)/rows_good
      bad1 <- sum(gogo$what==0)/rows_bad
      good <- c(good,good1)
      bad <- c(bad,bad1)
    
      #woe
      woe1 <- (sum(gogo$what==0)/rows_bad)/(sum(gogo$what==1)/rows_good)
      woe1 <- log(woe1)
      woe <- c(woe,woe1)

      #iv未累加
     iv_kniff1 <- (sum(gogo$what==0)/rows_bad) - (sum(gogo$what==1)/rows_good)
      iv_kniff1 <- iv_kniff1*woe1
      iv_kniff <- c(iv_kniff,iv_kniff1)
    }
    good <- cumsum(good)
    bad <- cumsum(bad)
    ks <- cummax((abs(good -bad)))*100
    iv <- cumsum(iv_kniff)
    plot(ks,type='l',lwd =3,col=4,main = names(data_p_mon)[j])
    plot(woe,type='l',lwd =3,col=5,main = names(data_p_mon)[j])
    plot(iv,type='l',lwd =3,col=6,main = names(data_p_mon)[j])
  }
  
  dlt <- data.frame(V1 = data_p_mon[,2],V2 = data_p_mon[,1])
  
  write.table(dlt,file = "授薪自营测试集混合KS数据源.csv",sep=',')
  