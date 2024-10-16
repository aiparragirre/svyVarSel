
#' Plot welnet object
#'
#' @param x an object of class "welnet".
#'
#' @return a graph
#' @export
#'
#' @examples
#' data(simdata_lasso_binomial)
#' mcv <- welnet(data = simdata_lasso_binomial, col.y = "y", col.x = 1:50,
#'               family = "binomial", alpha = 0.5, cluster = "cluster", strata = "strata",
#'               weights = "weights", method = "dCV", k=5, R=1)
#' welnet.plot(mcv)
welnet.plot <- function(x){

  if(!inherits(x, "welnet")){stop("Please, insert an object of class 'welnet'.")}

  plot(x = log(x$lambda$grid), y = x$error$average, col = "red", pch = 20,
       xlab = bquote("log("~lambda~")"), ylab = "Average error")
  abline(v = log(x$lambda$min), lty = 2, col = "black")
  mtext(text = paste0("Variables in the selected model: ", x$model$min$df), side = 3)

}
