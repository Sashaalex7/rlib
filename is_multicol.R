##
## https://stepik.org/lesson/26672/step/5?course=Основы-статистики-Часть-2&thread=solutions&unit=8484
## Функция проверяет наличие строгой мультиколлинеарности (линейной комбинации) между предикторами.
## Возвращает имена переменных, между которыми есть линейная зависимость или NULL
##
is_multicol <- function(data) {
  cor.mat <- abs(cor(data))
  diag(cor.mat) <- 0
  rownames(which(1-cor.mat < 1e-12, arr.ind=TRUE))
}
