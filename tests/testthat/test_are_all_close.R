testthat::test_that(
  "the relative error is below rel_tol and abs_tol",
  {
    expect_true(are_all_close(1, 1, abs_tol = 1e-2, rel_tol = 1e-2))
    expect_true(are_all_close(1, 2, abs_tol = 2, rel_tol = 1e-2))
    expect_true(are_all_close(1, 2, abs_tol = 1e-2, rel_tol = 2))
  }
)



