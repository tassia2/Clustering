testdata1 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                        V5 = c(1,1), V6 = c(2,1), V7 = c(3,1), V8 = c(1,2), V9 = c(2,2), V10 = c(1,3),
                        V11 = c(3,3), V12 = c(7,4), V13 = c(5,5), V14 = c(6,5), V15 = c(7,5), V16 = c(5,6),
                        V17 = c(6,6), V18 = c(7,6), V19 = c(6,7))
cluster1 <- testdata1
colnames(cluster1) <- c(rep("C1", 11), rep("C2", 8))


testdata2 <- data.frame(V1 = c(0,0,0), V2 = c(1,0,0), V3 = c(2,0,0), V4 = c(0,1,0),
                        V5 = c(1,1,0), V6 = c(2,1,0), V7 = c(3,1,0), V8 = c(1,2,0), V9 = c(2,2,0), V10 = c(1,3,0),
                        V11 = c(3,3,0), V12 = c(7,4,0), V13 = c(5,5,0), V14 = c(6,5,0), V15 = c(7,5,0), V16 = c(5,6,0),
                        V17 = c(6,6,0), V18 = c(7,6,0), V19 = c(6,7,0))
cluster2 <- testdata2
colnames(cluster2) <- c(rep("C1", 11), rep("C2", 8))

testdata3 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                        V5 = c(1,1), V6 = c(2,1), V7 = c(1,2), V8 = c(2,2),
                        V9 = c(6,5), V10 = c(7,5), V11 = c(8,5), V12 = c(6,6),
                        V13 = c(7,6), V14 = c(6,7), V15 = c(1,6), V16 = c(0,7),
                        V17 = c(1,7), V18 = c(2,7), V19 = c(1,8), V20 = c(2,8))
cluster3 <- testdata3
colnames(cluster3) <- c(rep("C1", 8), rep("C2", 6), rep("C3", 6))

testdata4 <- list(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                  V5 = c(1,1), V6 = c(2,1), V7 = c(3,1), V8 = c(1,2), V9 = c(2,2), V10 = c(1,3),
                  V11 = c(3,3), V12 = c(7,4), V13 = c(5,5), V14 = c(6,5), V15 = c(7,5), V16 = c(5,6),
                  V17 = c(6,6), V18 = c(7,6), V19 = c(6,7))
cluster4 <- as.data.frame(testdata4)
colnames(cluster4) <- c(rep("C1", 11), rep("C2", 8))

testdata5 <- matrix(c(0,0,1,0,2,0,0,1,1,1,2,1,3,1,1,2,2,2,1,3,3,3,7,4,5,5,6,5,7,5,5,6,6,6,7,6,6,7), nrow = 2)
cluster5 <- as.data.frame(testdata5)
colnames(cluster5) <- c(rep("C1", 11), rep("C2", 8))

testdata6 <- "This shouldn't work..."

testdata7 <- structure(1:24, dim = 2:4)

testdata8 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1))

testdata9 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1))



context("k medoids")

test_that("k_medoids",
          {
            expect_equal(k_medoids(testdata1, 2, medoids = matrix(c(1,1,7,6), 2, byrow = TRUE))[["cluster"]], cluster1)
            expect_equal(k_medoids(testdata2, 2, medoids = matrix(c(1,1,0,7,6,0), 2, byrow = TRUE))[["cluster"]], cluster2)
            expect_equal(k_medoids(testdata3, 3, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], cluster3)
            expect_equal(k_medoids(testdata4, 3, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], cluster4)
            expect_equal(k_medoids(testdata5, 3, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], cluster5)
            expect_error(k_medoids(testdata6, 3, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], "Strings cannot be clustered.")
            expect_error(k_medoids(testdata7, 3, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], "The given datatype is not supported.")
            expect_error(k_medoids(testdata8, 8, medoids = matrix(c(1,1,7,6,1,7), 3, byrow = TRUE))[["cluster"]], "More clusters required than given different datapoints. Every datapoint is his own cluster.")
            expect_error(k_medoids(testdata9, 3, medoids = matrix(c(1,1,7,6,1,7), 2, byrow = TRUE))[["cluster"]], "Wrong dimension or quantity of assigned medoids. Points are read by column here.")
          }
)
