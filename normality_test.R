library(dplyr)
normality_test <- function(data) unlist(summarise_if(data, is.numeric, funs(shapiro.test(.)$p.value)))
