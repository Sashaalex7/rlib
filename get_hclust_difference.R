###
## == See ==
## https://stepik.org/lesson/26672/step/2?course=Основы-статистики-Часть-2&unit=8484
## == Description ==
## Интересной особенностью кластерного анализа является тот факт, что мы получаем
## только итоговый ответ, к какому кластеру принадлежит каждое наблюдение.
## Однако мы не знаем, по каким переменным различаются выделенные кластеры.
## Поэтому, если нас интересует не только сам факт того, что мы смогли выделить кластеры
## в наших данных, но мы также хотим понять, чем же они различаются, разумно
## сравнить кластеры между собой по имеющимся переменным.
## Функция get_difference() должна вернуть названия переменных, по которым
## были обнаружены значимые различия между выделенными кластерами (p < 0.05).
## Иными словами, после того, как мы выделили заданное число кластеров,
## мы добавляем в исходные данные новую группирующую переменную — номер кластера,
## и сравниваем получившиеся группы между собой по количественным переменным
## при помощи дисперсионного анализа.
###

get_hclust_difference <- function(data, n_cluster) {
  cluster <- factor(cutree(hclust(dist(data)), n_cluster))
  get_aov_pval <- function(x) summary(aov(x ~ cluster))[[1]][1,'Pr(>F)']
  pvals <- sapply(data, get_aov_pval)
  names(pvals)[pvals < .05]
}

## Tests:
# test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_1.csv")
# get_hclust_difference(test_data, 2)
# test_data <- read.csv("https://stepic.org/media/attachments/course/524/cluster_2.csv")
# get_hclust_difference(test_data, 2)
##
