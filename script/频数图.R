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
  #---------------------------变为分类变量--------------------
  need <- c('what',
            'bal_percent',
            'days_last_p',
            'dpd',
            'due_amount',
            'hisdue_times',
            'if_jyd',
            'if_new_case',
            'if_new_rtp',
            'if_seek',
            'last_activitive',
            'last_code',
            'last_visit_days',
            'maxdue_days',
            'no_call_days',
            'on_ptp',
            'prd',
            'return_to_loan_term',
            'sv_due_rate',
            'svdays_payoff_times',
            'sx_eff_rate',
            'sx_kp_rate',
            'th_due_rate',
            'th_eff_rate',
            'th_kp_rate',
            'thdays_payoff_times',
            'tim',
            'to_rtp_days',
            'sort',
            'order_connected',
            'mobdays'
  )
  ################################################################
  data_all <- all_mob[,need]
  data_all$what <- as.factor(data_all$what)
  
  #跟进顺序1，2为1；3,4为0
  data_all$order_connected <- ifelse(data_all$order_connected %in% c(1,2),1,2)
  
  #率变为分类变量
  data_all$sv_due_rate <-ifelse(is.na(data_all$sv_due_rate)==TRUE,0,1)
  data_all$sx_eff_rate <-ifelse(is.na(data_all$sx_eff_rate)==TRUE,0,1)
  data_all$sx_kp_rate <-ifelse(is.na(data_all$sx_kp_rate)==TRUE,0,1)
  
  data_all$th_due_rate <-ifelse(is.na(data_all$th_due_rate)==TRUE,0,1)
  data_all$th_eff_rate <-ifelse(is.na(data_all$th_eff_rate)==TRUE,0,1)
  data_all$th_kp_rate <-ifelse(is.na(data_all$th_kp_rate)==TRUE,0,1)
  data_all$days_last_p <- ifelse(is.na(data_all$days_last_p)==TRUE,0,1)
  #账期分类
  data_all$dpd <- with(data_all,
                       ifelse(dpd %in% c(2,3),1,
                              ifelse(dpd %in% c(4,5,6),2,
                                     ifelse(dpd %in% c(7:35),3,4))))
  #-----------------------------------------------------------------------
  prob<-function(x,y){
  a <- table(x,y)
    b<-a[1]/(a[1]+a[3])*100  #行和
    c<-a[3]/(a[1]+a[3])*100
    d<-a[2]/(a[2]+a[4])*100
    e<-a[4]/(a[2]+a[4])*100
    f<-c(b,c,d,e)
    matrix_1<-matrix(f,nrow = 2,byrow = TRUE)
    rownames(matrix_1)<-c(0,1)
    colnames(matrix_1) <- c(0,1)
    print(matrix_1)
  }
  
  used <- c('what','dpd','order_connected','if_jyd','if_new_case','if_seek','on_ptp',
            'sv_due_rate','sx_eff_rate','sx_kp_rate','th_due_rate','th_eff_rate','th_kp_rate','days_last_p')
  data_used <- data_all[,used]
  for(k in 4:14){
  for(i in 1:3){
    data_test <- data_used[data_used$dpd %in% c(i),]
    for(j in 1:2){
      test <- data_test[data_test$order_connected %in% c(j),]
      prob(test$what,test[,k])
    }
  } 
  }
  data_test <- data_all[data_all$dpd %in% c(1),]
  data_test <- data_test[data_test$order_connected %in% c(2),]
  prob(data_test$what,data_test$if_jyd)
  
  #-------------------------------------hist----------------------
  data_all$rtp_rate <- data_all$hisdue_times/data_all$mobdays*100  # gaiming
  
  par(mfrow=c(1,2))
    hist(as.numeric(order_1$last_activitive),breaks=30,border = 'black',col='lightblue',main='hisdue_times--first')
    hist(as.numeric(order_2$last_activitive),breaks=30,border = 'black',col='lightgreen',main='hisdue_times--seconded')
  par(mfrow=c(1,1))
  
  hist(order_1$hisdue_times,breaks=30,border = 'black',col='lightblue',main='blue_fisrt VS green_seconded')
  hist(order_2$hisdue_times, breaks=30, border = 'black',col='lightgreen', add=T)
  
  #画出好坏组的情况
  hist(order_1[(order_1$what==0)&(order_1$dpd=2),]$no_call_days)
  hist(order_1[(order_1$what==1)&(order_1$dpd=2),]$no_call_days)
  
  #----------------------------
  data_time <- order_1[(order_1$what==1),]
  order_1 <- data_all[data_all$order_connected == 1,]
  order_2 <- data_all[data_all$order_connected==2,]
  
  good <- as.data.frame(table(order_1$what,order_1$to_rtp_days))
  bad <-as.data.frame(table(order_2$what,order_2$to_rtp_days))
  gbdif <- good$Freq/sum(good$Freq)-bad$Freq/sum(bad$Freq)
  plot(gbdif,type = 'b',col='blue',ylab = 'GOOD-BAD',main = 'GOOD-BAD的差')
  
  
  #-------------------------------------变为rate之后的水平------------------------
  
  order_first <- data_num[data_num$order_connect==1,]
  order_second<- data_num[data_num$order_connect==2,]
  good <- data_num[(data_num$what==1),]
  bad <- data_num[data_num$what==0,]
  
  mean(order_first$rtp_rate,na.rm = T);mean(order_second$rtp_rate,na.rm = T)
  median(order_first$rtp_rate,na.rm = T);median(order_second$rtp_rate,na.rm = T)
  
  md1 <- good %>% glm(order_connected~rtp_rate+0,family=poisson(link = "log"),.)
  bad %>% glm(order_connected~rtp_rate+0,family=poisson(link = "log"),.)
  
  histogram(~log10(rtp_rate)+last_visit_days+hisdue_times+
              as.numeric(last_activitive)+last_code|what,order_first)
  histogram(~thdays_payoff_times+no_call_days+svdays_payoff_times|what,order_first)
  
  
  
  histogram(~log10(rtp_rate)+|what,order_second)
  histogram(~log10(rtp_rate)|order_connected,good)
  histogram(~log10(rtp_rate)|order_connected,bad)
  
  #--------------------------------fenbutuduibi---------------------------
  data_all$rtp_rate <- data_all$hisdue_times/data_all$mobdays*100  # gaiming
  data_num<- data_all
  par(mfrow=c(2,1))
  hist(log10(data_num[(data_num$what==1),]$rtp_rate),main = 'good组的hisdue_times',col = 'lightblue')
  hist(log10(data_num[(data_num$what==0),]$rtp_rate),main = 'bad组的hisdue_times',col = 'lightgreen')
  hist(as.numeric(data_num[data_num$what==1,]$last_visit_days),main = 'good组的last_visit_days',col = 'lightblue')
  hist(as.numeric(data_num[data_num$what==0,]$last_visit_days),main = 'bad组的last_visit_days',col = 'lightgreen')
  
  #-------------------------------------------------------------------------
  data1 <- all_1[all_1$sex=="男",]
  condition_1<- sum(data1$what==1)/nrow(data1)
  condition_0 <-sum((all_1$sex=='女') & (all_1$what==1)) 
  
  #性别在好坏上的影响
  table(all_1$what,all_1$sex) 
  prop.table(table(all_1$what,all_1$sex),margin = 1)
  
  #----------------------年龄的分类情况
  for(i in 4:35){
    a <- all_1$age0[all_1$dpd==i]
    print(mean(a,na.rm = T))
    sd(a,na.rm = T)
  }
  
  for(i in 4:35){
    a <- all_1$age0[all_1$dpd==i]
    mean(a,na.rm = T)
    print(sd(a,na.rm = T))
  }
 
   #账期分类
  all_1$dpd <- with(all_1,
                       ifelse(dpd %in% c(2,3),1,
                              ifelse(dpd %in% c(4,5,6),2,
                                     ifelse(dpd %in% c(7:35),3,4))))
  #---------------------------------在联系上面的不同----------------------
  histogram(~age0|order_connected,data=all_1)
  histogram(~lastpay_tonow|order_connected,data=all_1)

  table(all_1$sex,all_1$order_connected) 
  prop.table(table(all_1$sex,all_1$order_connected),margin = 1)
  prop.table(table(all_1$sex,all_1$order_connected),margin = 2)
  
  #---------------------------------在好坏上面的不同----------------------
  histogram(~age0|what,data=all_1)
  histogram(~lastpay_tonow|what,data=all_1)
  
  table(all_1$sex,all_1$what) 
  prop.table(table(all_1$sex,all_1$what),margin = 1)
  prop.table(table(all_1$sex,all_1$what),margin = 2)