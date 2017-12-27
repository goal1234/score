  setwd("E:/工具箱/评分卡/data")
  filename <- "full_info_m2m3.csv"
  files <- read.csv(filename,stringsAsFactors = T)
  
  #-------------------------------------------公式----------------------------#
  how_zeros <- function(x){
    #caculate the rate of zeros 
    data_nrow <- nrow(x)
    per_zero <- vector()
    long <- ncol(x)
    for(i in 2:long){
      a = sum(x[,i]==0)/data_nrow
      per_zero <- c(per_zero,a)
    }
    label <- colnames(x)[2:long]
    per_result <<- data.frame(name = label,per = per_zero)
    per_result <<-per_result[order(per_result$per,decreasing = T),]
    rm(data_nrow,per_zero,label)
    return(per_result)
  }
  
  #删除空值的记录
  files <- na.omit(files)
  
  #对目标进行二分类的编码
  files$sort <- ifelse(files$sort != 500,1,0)
  
  #caculate the rate of zeros
  how_zeros(files)
  
  #ks,iv and frequency
  #某个变量下的
  library(smbinning)
  result <- smbinning(files,y='sort',x= 'bal_prin2',p=0.05)
  result$ivtable # Tabulation and Information Value
  result$iv # Information value
  result$bands # Bins or bands
  result$ctree # Decision tree from partykit
  
  # Plotting smbinning.sumiv
  sumivt=smbinning.sumiv(files,y="sort")
  sumivt # Display table with IV by characteristic
  smbinning.sumiv.plot(sumivt,cex=0.8) # Plot IV summary table
  
  #o,1分组不同
  # Training and testing samples (Just some basic formality for Modeling) 
  #sort应该是一个抽样的标签,从总体中抽样的标签，是一个2,8分类的标签值
  chileancredit.train=subset(files,sort==1)
  chileancredit.test=subset(files,sort==0)
  
  # EDA application
  smbinning.eda(chileancredit.train,rounding=3)$eda # Table with basic statistics.
  smbinning.eda(chileancredit.train,rounding=3)$edapct # Table with basic percentages
  
  #Summary IV application
  sumivt=smbinning.sumiv(chileancredit.train,y="FlagGB")
  sumivt # Display table with IV by characteristic
  smbinning.sumiv.plot(sumivt,cex=0.8) # Plot IV summary table
  
  #---------------------------------------选择需要的变量----------
  
  