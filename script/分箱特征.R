
  
binning<- function(y,x,z){
  
  gaga <- data.frame(target = y, value = x)
  z <-c(-Inf,z,Inf)
  nrows <- nrow(gaga)
  rows_good <- sum(gaga$target==1)
  rows_bad <- sum(gaga$target==0)
  
  #break之后的分组,最小组的表示为1
  cutpoint <- cut(gaga$value,z)
  levels(cutpoint) <- z
  group <- as.numeric(cut(gaga$value,z))
  gaga$group <- group
  
  rec <- vector()
  rec1 <- vector()
  good <- vector()
  bad <- vector()
  iv_kniff <- vector()
  iv_kniff1 <- vector()
  woe <- vector()
  cutp <- vector()
  
  kniff <<- unique(group)[order(unique(group))]
  
  for(i in kniff){
    group1 <- gaga[gaga$group==i,]
    a <- nrow(group1)
    b <- sum(group1$target==1)
    c <- sum(group1$target==0)
    d <- as.numeric(levels(cutpoint)[i+1])
  
    woe1 <- (sum(group1$target==1)/rows_good)/(sum(group1$target==0)/rows_bad)
    woe1 <- log(woe1)
    woe = c(woe,woe1)
    
    #iv未累加
    iv_kniff1 <- (sum(group1$target==1)/rows_good) - (sum(group1$target==0)/rows_bad)
    iv_kniff1 <- iv_kniff1*woe1
    iv_kniff = c(iv_kniff,iv_kniff1)
    
    cutp = c(cutp,d)
    rec = c(rec,a)
    good = c(good,b)
    bad = c(bad,c)
    rm(a,b,c,d)
  }

  cumrec <- cumsum(rec)
  pctrec <- cumrec/nrows
  cumgood <- cumsum(good)
  cumbad <- cumsum(bad)
  goodrate <- good/cumrec
  badrate <- bad/cumrec
  odds <- goodrate-badrate

  iv <- cumsum(iv_kniff)
  ks <- (cumgood/rows_good)-(cumbad/rows_bad)
  ks <- cummax(ks)
  
  result <<- data.frame(point = cutp,Rec=rec,Good=good,Bad=bad,Cumrec = cumrec, Cumgood=cumgood,
                        Cumbad = cumbad,Pctrec =pctrec,Goodrate = goodrate, Badrate =badrate,Odds = odds,
                        Woe = woe,IV=iv,Ks=ks,group_id=kniff)
 
  woe_data <- merge(gaga,result,by.x = 'group',by.y = 'group_id')
  woe_data <- woe_data[,c('target','value','Woe')]
  woe_data <<-woe_data
  
  print(result)
  }

  binning.quant<-function(y,x){
    #按照分为数
    quant <- quantile(x,probs = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
    z<-as.numeric(quant)
    binning(y,x,z)
  }

binning(y,x,c(1000,2000,4000,5000,8000,10000))


z <- c(1000,2000,4000,5000,8000,10000)


  binning.quant(y,x)
  
  #--------------------------------聚类后进行分箱----------------------------#
  binning.kmeans<-function(y,x,k){
  i <- 1
  b <- vector()
  while(i<30){
    cent1 <- kmeans(x,k,iter.max = 30,nstart = 3)
    a<-cent1$centers[order(cent1$centers)]
    b <- rbind(b,a)
    i <- i+1
  }
  z <- colMeans(b)
  binning(y,x,z)
  }
  
  binning.kmeans(y,x,6)
  
  #-----------------------层次聚类后分箱--------------------------#
  binning.bclust <- function(y,x,k){
    b <- vector()
    i<-1
    while(i<30){
      cent1 <- bclust(x,k)
      a<-cent1$centers[order(cent1$centers)]
      b <- rbind(b,a)
      i <- i+1
    }
    z <- colMeans(b)
    binning(y, x, z)
  }
  binning.bclust(y,x,10)
  
  #----------------------按照x所有出现的值进行分--------------------------#
  binning.all <- function(y,x){
    binning(y,x,unique(x[order(x)]))
    Collection <- result
    Collection$binningrate <-Collection$Cumgood/Collection$Cumrec
    print(Collection)
    pool <<-Collection
    par(mfrow = c(2,1))
    plot(pool$binningrate,type='l',lwd=2)
    plot(pool$Goodrate,type='h',lwd=2)
    abline(h=0.05)
    text(mean(pool$group_id),0.10,'Alert 0.05',lwd=3,col = 2)
    par(mfrow=c(1,1))
  }
  
  binning.all(y,x)
  