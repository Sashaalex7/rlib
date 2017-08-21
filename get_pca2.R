##
## Функция должна рассчитать, какое минимальное число главных компонент объясняет больше 90% изменчивости
## в исходных данных, и добавлять значения этих компонент в исходный dataframe в виде новых переменных.
##
## See: https://stepik.org/lesson/26672/step/4?course=Основы-статистики-Часть-2&thread=solutions&unit=8484
##

get_pca2 <- function(data){
  fit <- prcomp(data)
  val_imp <- summary(fit)$importance['Cumulative Proportion',]
  n_imp <- min(which(val_imp > 0.90))
  return(cbind(data, fit$x[, 1:n_imp, drop=F]))
}
