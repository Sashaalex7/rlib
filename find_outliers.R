##
## https://stepik.org/lesson/31103/step/12?course=Анализ-данных-в-R-Часть-2&unit=11516
##
## Получает на вход dataframe с одной количественной и произвольным числом факторных переменных.
## Факторные переменные разбивают все наши наблюдения на определенное число групп. 
## Создаёт новую числовую переменную is_outlier: 1, если наблюдение в строке является выбросом
## в своей группе, и 0 иначе.
## Под выбросами будем понимать наблюдения, отклоняющиеся от среднего значения в группе
## более чем на два стандартных отклонения этой группы. 
##

find_outliers <- function(data) {
  fac.vars <- data[sapply(data, is.factor)]
  num.vars <- sapply(data, is.numeric)
  outliers <- function(x) abs(x - mean(x)) > sd(x) * 2
  # support any number of numer columns, is_outlier if any column is outlier.
  outliers.df <- function(df) apply(sapply(df[num.vars], outliers), 1, any) + 0
  data$is_outlier <- unsplit(by(data, fac.vars, outliers.df), fac.vars)
  return(data)
}

######### Unit Test - Start ########
if(F)
{
  find_outliers.by_unlist <- function(data) {
  fact.list <- as.list(data[sapply(data, is.factor)])
  num.cols <- sapply(data, is.numeric)
  is.outlier.vec <- function(x) abs(x - mean(x)) > sd(x) * 2
  is.outlier.df <- function(df) apply(apply(df[num.cols], 2, is.outlier.vec), 1, any)+0
  by.list <- by(data, fact.list, is.outlier.df)
  by.vec <- unlist(by.list)
  sorted.vec <- by.vec[order(as.numeric(names(by.vec)))]
  data$is_outlier <- sorted.vec
  return(data)
}

find_outliers.by_unsplit <- function(data) {
  fac.vars <- data[sapply(data, is.factor)]
  num.vars <- sapply(data, is.numeric)
  outliers <- function(x) as.numeric(abs(x - mean(x)) > sd(x) * 2)
  outliers.df <- function(df) outliers(df[,num.vars])
  data$is_outlier <- unsplit(by(data, fac.vars, outliers.df), fac.vars)
  return(data)
}

find_outliers.split_unsplit <- function(data) {
  fac.vars <- data[sapply(data, is.factor)]
  num.vars <- sapply(data, is.numeric)
  outliers <- function(x) as.numeric(abs(x-mean(x)) > sd(x)*2)
  outliers.df <- function(df) outliers(df[,num.vars])
  data$is_outlier <- unsplit(lapply(split(data, fac.vars), outliers.df), fac.vars)
  return(data)
}

library(dplyr)
find_outliers.dplyr_new <- function(data) {
  data %>% group_by_if(is.factor) %>%
  mutate_if(is.numeric, funs(is_outlier=as.numeric(abs(.-mean(.))>sd(.)*2))) %>% 
  data.frame()
}

library(dplyr)
find_outliers.dplyr_old <- function(data) {
  group_by_(data, .dots = names(data)[sapply(data, is.factor)]) %>%
    mutate_if(is.numeric, funs(is_outlier=as.numeric(abs(.-mean(.))>sd(.)*2))) %>% 
    data.frame()
}

library(data.table)
find_outliers.data_table <- function(data){
  data <- as.data.table(data)
  numv <- colnames(data)[sapply(data, is.numeric)]
  fvars <- colnames(data)[sapply(data, is.factor)]
  data[, is_outlier := as.numeric(abs(get(numv)-mean(get(numv)))>sd(get(numv))*2), by=fvars]
}

find_outliers <- find_outliers.by_unlist
find_outliers <- find_outliers.by_unsplit
find_outliers <- find_outliers.split_unsplit
find_outliers <- find_outliers.dplyr_new
find_outliers <- find_outliers.dplyr_old
find_outliers <- find_outliers.data_table
  
subset(find_outliers(transform(ToothGrowth, dose=factor(dose))), is_outlier==1)

test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")
answer <- find_outliers(test_data)
all.equal(answer, correct_answer)

test_data <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2_ans.csv")
answer <- find_outliers(test_data)
all.equal(answer, correct_answer)
}
######### Unit Test - End ########
