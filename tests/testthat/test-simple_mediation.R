test_that("equality of the two methods for indirect effect", {
  med <- simple_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[[1]]$t0[3], med[[1]]$t0[4], tolerance = 0.00001)
})

test_that("total effect is sum of direct and indirect effects", {
  med <- simple_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[[1]]$t0[1], med[[1]]$t0[2] + med[[1]]$t0[3], tolerance = 0.00001)
})

test_that("total effect is sum of direct and indirect effects 2", {
  med <- simple_mediation(data.frame(c(1,2,3,4,5,6,7),c(1,2.3,4.2,4.9,5.6,6,6.7),c(3.4,5.4,6.5,7.9,8,10,11)))
  expect_equal(med[[1]]$t0[1], med[[1]]$t0[2] + med[[1]]$t0[4], tolerance = 0.00001)
})
