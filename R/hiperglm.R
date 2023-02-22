#' @export
hiper_glm <- function(design, outcome, model = "linear", option = list()) {
  supported_model <- c("linear", "logit")
  if(!(model %in% supported_model)) {
    stop(sprintf("The model %s is not supported", model))
  }

  if(model == "logit") {
    stop("The logit model is not yet developed")
  }

  if(option != "BFGS") {
    stop("option must be 'BFGS'")
  }

  # MLE finder via pseudo-inverse
  if (length(option) == 0) {
    design.svd <- svd(design)
    design.Pinv <- design.svd$v %*% (1/design.svd$d * t(design.svd$u))
    par <- design.Pinv %*% outcome
    value <- logl(par, y=outcome, X=design)
    hglm_out <- list(par=par, value=value)
  }
  # MLE finder via BFGS
  else if (option == "BFGS") {
    init_guess <- rep(1,ncol(design))
    hglm_out <- stats::optim(par = init_guess, fn = logl, gr = logl_grad, method = "BFGS", y=outcome, X=design)
  }

  class(hglm_out) <- "hglm"
  return(hglm_out)
}

