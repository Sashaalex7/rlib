# find optimal cutoff value for a logistic regression
# example:
#   opt_logres_cutoff(df$prob, df$admit)

library(ggplot2)
library(ROCR)

opt_logres_cutoff <- function(probs, observed, plot=T) {
  # get rid of NAs
  ind_not_na <- !is.na(observed)
  probs_nna <- probs[ind_not_na]
  observed_nna <- observed[ind_not_na]

  # produce prediction object (see ROCR)
  pred_obj <- prediction(probs_nna, observed_nna)

  # produce performance values: specificity, sensitivity, accuracy
  perf_spec <- performance(pred_obj, x.measure="cutoff", measure="spec")
  perf_sens <- performance(pred_obj, x.measure="cutoff", measure="sens")
  perf_accu <- performance(pred_obj, x.measure="cutoff", measure="acc")

  # extract performance values from class objects
  # and gather data in a single data frame for ggplot2
  pdf0 <- data.frame(x_spec = perf_spec@x.values[[1]],
                     y_spec = perf_spec@y.values[[1]],
                     x_sens = perf_sens@x.values[[1]],
                     y_sens = perf_sens@y.values[[1]],
                     x_accu = perf_accu@x.values[[1]],
                     y_accu = perf_accu@y.values[[1]])
  stopifnot(pdf0$x_spec == pdf0$x_sens, pdf0$x_spec == pdf0$x_accu)
  pdf <- data.frame(cutoff=pdf0$x_spec, spec=pdf0$y_spec, sens=pdf0$y_sens, acc=pdf0$y_accu)
  
  best <- pdf$cutoff[which.min(abs(pdf$sens - pdf$spec))]
  res <- list(cutoff=best)

  if (plot) {
    res$plot <- ggplot(pdf, aes(x=cutoff)) +
      geom_line(aes(y=spec), col="red") +
      geom_line(aes(y=sens), col="green") +
      geom_line(aes(y=acc), col="cyan") +
      xlab("cutoff") + ylab("rates") +
      theme_bw()
    res$perf <- pdf
  }

  return(res)
}
