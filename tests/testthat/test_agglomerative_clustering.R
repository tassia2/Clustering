context("agglomerative_clustering")

testdata1 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                        V5 = c(1,1), V6 = c(2,1), V7 = c(3,1), V8 = c(1,2), V9 = c(2,2), V10 = c(1,3),
                        V11 = c(3,3), V12 = c(7,4), V13 = c(5,5), V14 = c(6,5), V15 = c(7,5), V16 = c(5,6),
                        V17 = c(6,6), V18 = c(7,6), V19 = c(6,7))
cluster1 <- testdata1
colnames(cluster1) <- c(rep("C1", 11), rep("C2", 8))

testdata2 <- list(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                  V5 = c(1,1), V6 = c(2,1), V7 = c(3,1), V8 = c(1,2), V9 = c(2,2), V10 = c(1,3),
                  V11 = c(3,3), V12 = c(7,4), V13 = c(5,5), V14 = c(6,5), V15 = c(7,5), V16 = c(5,6),
                  V17 = c(6,6), V18 = c(7,6), V19 = c(6,7))
cluster2 <- as.data.frame(testdata2)
colnames(cluster2) <- c(rep("C1", 11), rep("C2", 8))

testdata3 <- matrix(c(0,0,1,0,2,0,0,1,1,1,2,1,3,1,1,2,2,2,1,3,3,3,7,4,5,5,6,5,7,5,5,6,6,6,7,6,6,7), nrow = 2)
cluster3 <- as.data.frame(testdata3)
colnames(cluster3) <- c(rep("C1", 11), rep("C2", 8))

testdata4 <- c(0,1,2,5,6,7)
cluster4 <- data.frame(0,1,2,5,6,7)
colnames(cluster4) <- c(rep("C1", 3), rep("C2", 3))

testdata5 <- data.frame(V1 = c(0,0,0), V2 = c(1,0,0), V3 = c(2,0,0), V4 = c(0,1,0),
                        V5 = c(1,1,0), V6 = c(2,1,0), V7 = c(3,1,0), V8 = c(1,2,0), V9 = c(2,2,0), V10 = c(1,3,0),
                        V11 = c(3,3,0), V12 = c(7,4,0), V13 = c(5,5,0), V14 = c(6,5,0), V15 = c(7,5,0), V16 = c(5,6,0),
                        V17 = c(6,6,0), V18 = c(7,6,0), V19 = c(6,7,0))
cluster5 <- testdata5
colnames(cluster5) <- c(rep("C1", 11), rep("C2", 8))

testdata6 <- "This shouldn't work..."

testdata7 <- structure(1:24, dim = 2:4)

testdata8 <- data.frame(V1 = c(0,0), V2 = c(1,0), V3 = c(2,0), V4 = c(0,1),
                        V5 = c(1,1), V6 = c(2,1), V7 = c(3,1), V8 = c(1,2), V9 = c(2,2), V10 = c(1,3),
                        V11 = c(3,3), V12 = c(7,4), V13 = c(5,5), V14 = c(6,5), V15 = c(7,5), V16 = c(5,6),
                        V17 = c(6,6), V18 = c(7,6), V19 = c(6,7))

test_that("agglomerative_clustering",
         {
           expect_equal(agglomerative_clustering(testdata1, "SL"), cluster1)
           expect_equal(agglomerative_clustering(testdata1, "CL"), cluster1)
           expect_equal(agglomerative_clustering(testdata1, "AL"), cluster1)
           expect_equal(agglomerative_clustering(testdata2), cluster2)
           expect_equal(agglomerative_clustering(testdata3), cluster3)
           expect_equal(agglomerative_clustering(testdata4), cluster4)
           expect_equal(agglomerative_clustering(testdata5), cluster5)
           expect_error(agglomerative_clustering(testdata6), "Strings cannot be clustered.")
           expect_error(agglomerative_clustering(testdata7), "The given datatype is not supported.")
           expect_error(agglomerative_clustering(testdata8, clus_type = "This isnt going to work"), "The given clustering technique is not supported.")
         }
        )
