significant.glm.features <- function(data, target.factor = NULL){
  if (missing(target.factor))
    target.factor <- colnames(data)[1]
  term.str <- paste(target.factor, "~ .")
  model <- glm(as.formula(term.str), data, family="binomial")
  result <- anova(model, test="Chisq")
  p.vals <- result$`Pr(>Chi)`
  names <- na.omit(rownames(result)[p.vals < .05])
  return(names)
}
