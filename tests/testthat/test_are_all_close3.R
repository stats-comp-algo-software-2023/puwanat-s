testthat::test_that(
  "the relative error is below rel_tol but above abs_tol",
  {
    expect_false(are_all_close(1, 2, abs_tol = 1e-2, rel_tol = 2))
  }
)
