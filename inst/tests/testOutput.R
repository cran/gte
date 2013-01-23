context("Test that gte() output is as expected")


test_that("gte() output value surv is as expected", {

  library(Epi)
  data(hivDK)
  for( i in 2:4 ) hivDK[,i] <- cal.yr( hivDK[,i] )
  hivDK[,3:4] <- hivDK[,3:4] - hivDK[,2]
  
  # one covariate, one estimation point
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth, data=hivDK, z=0)
  expect_that(dim(Fit$surv), equals(c(8,1)))
  
  # one covariate, more than one estimation point
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth, data=hivDK, z=c(-30, -10, 10))
  expect_that(dim(Fit$surv), equals(c(8,3)))
  
  # more than one covariate, one estimation point
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=c(0, 7))
  expect_that(dim(Fit$surv), equals(c(8,1)))  
  
  # more than one covariate, one estimation point
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=matrix(c(0, 7, -20, 20), byrow=TRUE, ncol=2))
  expect_that(dim(Fit$surv), equals(c(8,2)))  
  
  
  # plus d'une covariable, mais toutes constantes
  hivDKc <- cbind(hivDK, cst1=1, cst2=2)
  Fit1 <- gte(Surv(well, ill, type="interval2") ~ cst1, data=hivDKc, z=1)                  
  Fit2 <- gte(Surv(well, ill, type="interval2") ~ cst1 + cst2, data=hivDKc, z=c(1, 2))
  expect_that(Fit2$time, equals(Fit1$time))
  expect_that(Fit2$surv, equals(Fit1$surv))
  
})


test_that("gte() gives the same results as icfit() (interval pkg) with a constant covariate", {
  
  # Data set simul
  simulc <- cbind(simul, cst=1)  
  Fit <- gte(Surv(L, R, type="interval2") ~ cst, data=simulc, z=1)
  Fit_icfit <- icfit(Surv(L, R, type="interval2") ~ cst, data=simulc)  
  comptime <- Fit$time %in% Fit_icfit$intmap[2,]
  meandiff <- mean(abs(Fit$surv[comptime] - (1-cumsum(Fit_icfit$pf))))
  expect_that(meandiff < 0.01, is_true())  
  
  # Data set hivDK
  Fit <- gte(Surv(well, ill, type="interval2") ~ cst1, data=hivDKc, z=1)
  Fit_icfit <- icfit(Surv(well, ill, type="interval2") ~ 1, data=hivDKc)
  comptime <- Fit$time %in% Fit_icfit$intmap[2,]
  meandiff <- mean(abs(Fit$surv[comptime] - (1-cumsum(Fit_icfit$pf))))
  expect_that(meandiff < 0.001, is_true())  
})  


test_that("gte() gives the same results as survfit() (survival pkg) with a constant covariate", {
  
  # Data set simul
  Fit <- gte(Surv(L, R, type="interval2") ~ cst, data=simulc, z=1)
  Fit_survfit <- survfit(Surv(L, R, type="interval2") ~ 1, data=simulc)
  comptime <- rowSums(outer(Fit_survfit$time, Fit$time, ">")) + 1
  meandiff <- mean(abs(Fit$surv[comptime] - Fit_survfit$surv))
  expect_that(meandiff < 0.01, is_true())  
  
  # Data set hivDK
  Fit <- gte(Surv(well, ill, type="interval2") ~ cst1, data=hivDKc, z=1)
  Fit_survfit <- survfit(Surv(well, ill, type="interval2") ~ 1, data=hivDKc)
  comptime <- rowSums(outer(Fit_survfit$time[-11], Fit$time, ">")) + 1
  meandiff <- mean(abs(Fit$surv[comptime] - Fit_survfit$surv[-11]))
  expect_that(meandiff < 0.01, is_true())  
})  
          