testthat::test_that(
  "comparison between MLE estimated via pseudo-inverse and via BFGS",
  {
    n_obs <- 32; n_pred <- 4
    data <- simulate_data(n_obs, n_pred, model = 'linear', seed = 1918)
    design <- data$design; outcome <- data$outcome
    pseudo_inverse_out <- hiperglm::hiper_glm(design, outcome, model = "linear")
    bfgs_out <- hiperglm::hiper_glm(design, outcome, model = "linear", option = list(mle_solver = 'BFGS'))
    expect_true(are_all_close(
      coef.hglm(pseudo_inverse_out), coef.hglm(bfgs_out), abs_tol = 1e-2, rel_tol = 1e-2
    ))
  }
)
