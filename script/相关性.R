  setwd('E:/案件排序项目/data')
  full_info <- read.csv('full_info.csv')
  
  #-----------------------------------------变量分类-----------------------------------#
  numer <- full_info[,c('bal_prin2','due_amount','list_bal_prin2','th_due_rate',
                         'sv_due_rate','return_to_loan_term','bal_percent')]
  
  sample <- full_info[sample(nrow(full_info),10000),]
  