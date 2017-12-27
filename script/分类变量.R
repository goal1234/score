
  #率变为分类变量
  info_good$sv_due_rate <-ifelse(is.na(info_good$sv_due_rate)==TRUE,0,1)
  info_good$sx_eff_rate <-ifelse(is.na(info_good$sx_eff_rate)==TRUE,0,1)
  info_good$sx_kp_rate <-ifelse(is.na(info_good$sx_kp_rate)==TRUE,0,1)
  
  info_good$th_due_rate <-ifelse(is.na(info_good$th_due_rate)==TRUE,0,1)
  info_good$th_eff_rate <-ifelse(is.na(info_good$th_eff_rate)==TRUE,0,1)
  info_good$th_kp_rate <-ifelse(is.na(info_good$th_kp_rate)==TRUE,0,1)
  info_good$days_last_p <- ifelse(is.na(info_good$days_last_p)==TRUE,0,1)
  
  
  #------------------有联系的组--------------
  #DPD为4-35之间的
  condition<- info_good$dpd %in% c(4:25)
  data_4 <- info_good[condition,]
  data_connected <- data_4[data_4$sort<500,]  #发生了联系的
  data_no <- data_4[data_4$sort == 500,]  #没有发生联系
  
  #------------------------
  ###data_1为联系了的,bad组为没有联系的
  ###其中data_1只有14204条记录 对照组有465450条记录
  bad <- bad_space[sample(nrow(bad_space),nrow(data_1)),]  #生成一个对等的没有入选的BAD组
  
  
  
  #-------------------------------DPD>4和联系组中------------------------------------
  #connected 有13415个，no有425299 其比例约为 0.03的情况，催收员大概只是浏览了3%的案件
  unique(data_connected$sort)
  max(data_connected$sort)  #最大联系了265个人
  tail((data_connected))
  
  #---------------------------将自变量作为分类变量------------------------------------
  data_connected$prd <-with(data_connected,
                       ifelse(prd %in% c('精英贷','精英贷（银行合作）'),1,
                              ifelse(prd %in% c('新薪贷','薪薪贷（银行合作）'),2,
                                     ifelse(prd %in% c('新薪宜楼贷'),3,
                                            ifelse(prd %in% c('助业贷'),4,
                                                   ifelse(prd %in% c('助业宜楼贷'),5,6
                                                   ))))))
  
  data_connected$last_activitive[data_connected$last_activitive=='预催收']=1
  data_connected$last_activitive[data_connected$last_activitive=='手工电催']=2
  data_connected$last_activitive[data_connected$last_activitive=='反欺诈调查']=3
  data_connected$last_activitive[is.na(data_connected$last_activitive)]=4
  
  data_connected$last_code <-with(data_connected,
                            ifelse(last_code %in% c('承诺还款(PTP)','暂时经济困难但有还款意愿','客户提示已还款（自动外呼）','愿意代偿'),1,
                                   ifelse(last_code %in% c('协商跟进','无人应答(NoAnswer)','查找','他人转告','语音信箱(AnsweringMachine)','非本人直系亲属留言','要求回拨(Callback)'),2,
                                          ifelse(last_code %in% c('无法接通','非本人接听,留言(WPC，LeftMsg)','占线(Busy)','非本人接听,留言(WPC，LeftMsg)','停机','非本人号码(Wrong#)'),3,
                                                 ifelse(last_code %in% c('空坏号(Bad#)','拒接','其他','拒绝还款(RTP)'),4,5
                                                 )))))
  for(i in 4:35){
    a <- data_connected$bal_percent[data_connected$dpd==i]
    print(mean(a,na.rm = T))
    sd(a,na.rm = T)
  }
  
  for(i in 4:35){
    a <- data_connected$bal_percent[data_connected$dpd==i]
    mean(a,na.rm = T)
    print(sd(a,na.rm = T))
  }
  
  data_connected$tim <- with(data_connected,     #和账期相关性较强按照账期画分出来
                             ifelse(tim<200,0,
                                    ifelse(tim<300,1,2)))
  
  data_connected$due_amount <- with(data_connected,    #不同账期的分案并不相同
                         ifelse(due_amount<20000,0,
                                ifelse(due_amount<40000,1,2)))
  
  data_connected$return_to_loan_term <- with(data_connected,    #对于距离的划分
                                             ifelse(return_to_loan_term <0.4,0,1))
  data_connected$bal_percent <-with(data_connected,                #分类中比较平均
                                    ifelse(bal_percent>0.0055,1,0))
  
  
  #-------------------------加载年龄age_sex和maxrepaydates-----------------------------
  age_sex <- read.csv('age_sex.csv',stringsAsFactors = F)
  maxrepay <- read.csv('maxrepaydate.csv',stringsAsFactors = F)

  #连接数据
  data_1 <- sqldf('select a.*,
                  b.age0,
                  sex
                  from data_connected as a
                  left join age_sex as b
                  on a.customer_code = b.customer_code')

  data_1 <- sqldf('select a.*,
                  b.lastpay_tonow
                  from data_1 as a
                  left join maxrepay as b
                  on a.contractno = b.contractno and
                  a.enter_date2 = b.enter_date2')
  
  data_1$sex <- with(data_1,
                     ifelse(sex =='男',1,
                            ifelse(sex == '女',0,2)))
  