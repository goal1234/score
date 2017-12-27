a_data <- function(x){
  x <- na.omit(x)
  par(mfrow = c(2,2))
    plot(x)
    boxplot(x)
    hist(x)
    qqnorm(x);qqline(x)
  par(mfrow = c(1,1))
}


classify <- function(probability) ifelse(probability<0.5,'0','1')
classified_malignant <- classify(predict(fitted_model,data_model_test[,-1],type='response'))
classified <- classify(predict(fitted_model,data_model_test[,-1]))

#预测的准确性,来个表
confusion_matrix <- table(data_model_test[,1],classified,dnn=c('real','prediction'))
  a <- as.data.frame(confusion_matrix)
  a1 <-as.numeric(as.character(a$real))   #real
  a2<-as.numeric(as.character(a$prediction))  #prediction
accuracy <- sum(a$Freq[a1==a2])/sum(a$Freq)
Sensitivity <- sum(a$Freq[((a1==1)&(a2==1))])/sum(a$Freq[a1==1])
Specificity <- sum(a$Freq[((a1==0)&(a2==0))])/sum(a$Freq[a1==0])

real_in_prediction <- sum(a$Freq[(a1==1)&(a2==1)])/sum(a$Freq[a2==1])  #预测为真中是真金的比例 需要大于 0.5
predict_rate <- sum(a$Freq[a2==1])/sum(a$Freq)  #这个值的比例在0.05

##logistic模型的形状
x <- seq(from = -10, to = 10, by = 0.01)
y = exp(x)/(1+exp(x))
library(ggplot2)
p <- ggplot(data = NULL, mapping = aes(x = x,y = y))
p + geom_line(colour = 'blue')
+ annotate('text', x = 1, y = 0.3, label ='y==e^x / 1-e^x', parse = TRUE)
+ ggtitle('Logistic曲线')
