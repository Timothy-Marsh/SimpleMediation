test_that("equality of the two methods for indirect effect", {
  med <- run_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[3], med[4], tolerance = 0.00001)
})

test_that("total effect is sum of direct and indirect effects", {
  med <- run_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[1], med[2] + med[3], tolerance = 0.00001)
})

test_that("total effect is sum of direct and indirect effects 2", {
  med <- run_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[1], med[2] + med[4], tolerance = 0.00001)
})
