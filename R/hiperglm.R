#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  supported_model <- c("linear", "logit")
  if(!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }
  # MLE finder via pseudo-inverse
  if (length(option) == 0) {
    design.svd <- svd(design)
    design.Pinv <- design.svd$v %*% (1/design.svd$d * t(design.svdv$u))

  }
  # MLE finder via BFGS
  else if (option == "BFGS") {

  }

  # TODO: maximize likelihood
  hglm_out <- list()
  class(hglm_out) <- "hglm"
  return(hglm_out)
}

