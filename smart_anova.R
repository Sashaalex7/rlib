smart_anova <- function(data){
  is_normal <- all(with(data, tapply(x, y, function(v) shapiro.test(v)$p.value)) >= .05)
  is_homosc <- bartlett.test(x ~ y, data)$p.value >= .05
  if (is_normal && is_homosc) {
    result <- summary(aov(x ~ y, data))
    return(c(ANOVA = result[[1]][1,'Pr(>F)']))
  } else {
    result <- kruskal.test(x ~ y, data)
    return(c(KW = result$p.value))
  }
}
