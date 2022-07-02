#EM_Algorithmus
testdata1 <- c(-0.39,0.12,0.94,1.67,1.76,2.44,3.72,4.28,4.92,5.53,
               0.06,0.48,1.01,1.68,1.8,3.25,4.12,4.6,5.28,6.22)


this <- eucl_dist(em_algorithm(testdata1),0)

#Defaulttest
testdata2 <- data.frame(V1=c(1,2), V2= c(1,3))


context("Em_algorithm: if the mistake between the right guesses and our programm are small enough")

test_that("em-algorithm",
{
  expect_error(em_algorithm(testdata2), "The input needs to be one dimensional")
  expect_lt(abs(this - 4.931642),1)
})



