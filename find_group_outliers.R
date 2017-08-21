library(dplyr)
library(lazyeval)

find_outliers <- function(df){
  grouped <- group_by_(df, colnames(df)[sapply(df, is.factor)])
  gen_formula <- ~as.integer(abs(col - mean(col)) >= sd(col)*2)
  num_col <- as.name(colnames(df)[sapply(df, is.numeric)])
  result <- mutate_(grouped, is_outlier = interp(gen_formula, col=num_col))
  return(as.data.frame(result))
}

test_flag <- FALSE
if (test_flag) {
  ToothGrowth$dose <- factor(ToothGrowth$dose)
  test_data1 <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
  correct_answer1 <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")
  test_data2 <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
  correct_answer2 <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2_ans.csv")
  answer1 <- find_outliers(test_data1)
  all.equal(answer1, correct_answer1)
  answer2 <- find_outliers(test_data2)
  all.equal(answer2, correct_answer2)
  answer_tg <- find_outliers(ToothGrowth)
  subset(answer_tg, is_outlier==1)
}
