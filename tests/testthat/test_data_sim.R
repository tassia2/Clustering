context("data_sim")

testdata1 <- "This wont work."
testdata2 <- structure(1:24, dim = 2:4)
testdata3 <- c(1,1,6,6)
testdata4 <- c(1, 1, 6)

test_that("data_sim",
  {
    expect_error(data_sim(midpoints = testdata1), "Strings cannot be midpoints.")
    expect_error(data_sim(midpoints = testdata2), "The given datatype is not supported.")
    expect_error(data_sim(midpoints = testdata3, sd = c(0.5,0.5,0.5)), "Amount of standard deviations and amount of midpoints doesn't fit together.")
    expect_error(data_sim(midpoints = testdata4), "Midpoints does not have the correct dimension.")
  }
)
