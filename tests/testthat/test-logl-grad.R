testthat::test_that(
  "compare numerical (centered difference method) and analytical methods to calculate a gradient of log-likelihood function",
  {
    simulated_data <- simulate_data(n_obs = 32, n_pred = 4)
    X <- simulated_data$design
    y <- simulated_data$outcome
    coef_true <- simulated_data$coef_true

    analytical_grad <- logl_grad(coef_true, y=y, X=X)
    numerical_grad <- approx_grad(function(coef) logl(coef, y=y, X=X), coef_true)

    expect_true(are_all_close(
      analytical_grad, numerical_grad, abs_tol = Inf, rel_tol = 1e-3
    ))
  }
)




