testthat::test_that(
  "the relative error is below abs_tol but above rel_tol",
  {
    expect_false(are_all_close(1, 2, abs_tol = 2, rel_tol = 1e-2))
  }
)
