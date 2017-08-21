## https://stepik.org/lesson/45350/step/2?course=Основы-статистики-Часть-3&thread=solutions&unit=23653
## В этой задаче вам необходимо оценить 95% интервал для медианы
## при помощи basic bootstrap. Напишите функцию median_cl_boot,
## которая получает на вход числовой вектор произвольной длины
## и возвращает вектор из двух значений - верхней и нижней границы
## доверительного интервала для медианы.
## Для расчета доверительного интервала используйте симуляцию
## из 1000 бутстрапированных выборок.

gen_cl_boot <- function(x, FUN = mean, bs.size = 1000, conf.int = 0.95){
  gen.val <- FUN(x)
  bs.vec <- replicate(bs.size, FUN(sample(x, replace = TRUE)))
  gen.val + quantile(gen.val - bs.vec, probs = c((1-conf.int)/2, (1+conf.int)/2))
}

if(FALSE){
library(Hmisc)
x <- rnorm(100)
gen_cl_boot(x)
smean.cl.boot(x)
slope_cl_boot(data.frame(x=rnorm(100), y=rnorm(100)))
}

slope_cl_boot <- function(df, bs.size = 1000, conf.int = 0.95){
  slope <- function(d) coef(lm(y~x, d))[2]
  gen.val <- slope(df)
  bs.vec <- replicate(bs.size, slope(df[sample(nrow(df), replace = TRUE),]))
  gen.val + quantile(gen.val - bs.vec, probs = c((1-conf.int)/2, (1+conf.int)/2))
}
