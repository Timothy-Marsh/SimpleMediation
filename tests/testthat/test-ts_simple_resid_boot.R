test_that("ts_simple_resid_boot works", {
  x <- rnorm(100)
  n <- 2
  R <- 10
  
  ts_simple_resid_boot(x,n,R)
  
  samp_X <- seq(1:100)*0.01 + rnorm(100)
  samp_M <- samp_X *0.4 + rnorm(100)
  samp_Y <- samp_X * -0.1 + samp_M *0.2 + rnorm(100)
  
  data <- data.frame(samp_X, samp_M, samp_Y)
  })
