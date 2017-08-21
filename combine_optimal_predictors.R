combine_optimal_predictors <- function(data, result.ind=1, fitter="linear", value=NULL) {
  if (fitter == "linear") {
    fitter = lm
    if (is.null(value))  value <- "adj.r.squared"
  } else if (fitter == "logistic") {
    fitter = function(formula, data) glm(formula, data, family="binomial")
    if (is.null(value))  value <- "-aic"
  } else {
    stopifnot(is.function(fitter) & is.function(value))
  }

  stopifnot(is.character(value) | is.function(value))
  if (is.function(value))
    value_f <- value
  else if (is.character(value) & substring(value,1,1)=="-")
    value_f <- function(summary) -summary[[substring(value,2)]]
  else
    value_f <- function(summary) summary[[value]]a

  if (is.character(result.ind)) {
    stopifnot(result.ind %in% colnames(data))
    result.ind = which(colnames(data) == result.ind)
  }
  x <- data.matrix(data[-result.ind])
  y <- data[[result.ind]]
  x_names <- colnames(x)
  y_name <- colnames(data[result.ind])

  paste_plus_f <- function(strings) paste0(strings, collapse="+")
  comb_names_f <- function(num) apply(combn(x_names, num), 2, paste_plus_f)
  x_comb_l <- sapply(1:length(x_names), comb_names_f)
  formulae_v <- paste0(y_name, '~', unlist(x_comb_l))

  fitter_value_f <- function(formula) value_f(summary(fitter(formula, data)))
  values_v <- sapply(formulae_v, fitter_value_f)
  formula_opt <- formulae_v[which.max(values_v)]

  return(list(formulae=formulae_v,
              values=values_v,
              formula.opt=formula_opt,
              fit.opt=fitter(formula_opt, data)))
}
