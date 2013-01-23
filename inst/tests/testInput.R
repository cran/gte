context("Test that gte() correctly treats input values")


test_that("Rigth censored observations are treated correctly", {
  simul_inf <- simul_big <- simul
  simul_inf$R[is.na(simul$R)] <- Inf
  simul_big$R[is.na(simul$R)] <- 10000
  
  Fit_NA <- gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=c(10, 20))

  Fit_inf <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_inf, z=c(10, 20))  
  expect_that(Fit_inf$time, equals(Fit_NA$time))
  expect_that(Fit_inf$time[length(Fit_inf$time)], equals(Inf))
  expect_that(Fit_inf$surv, equals(Fit_NA$surv))

  Fit_big <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_big, z=c(10, 20))  
  expect_that(Fit_big$time[-length(Fit_big$time)], equals(Fit_NA$time[-length(Fit_NA$time)]))
  expect_that(Fit_big$time[length(Fit_big$time)], equals(10000))
  expect_that(Fit_big$surv, equals(Fit_NA$surv))  
})

test_that("Left censored observations are treated correctly", {
  simul_inf <- simul_0 <- simul_small <- simul
  simul_inf$L[is.na(simul$L)] <- -Inf
  simul_0$L[is.na(simul$L)] <- 0
  simul_small$L[is.na(simul$L)] <- 0.0000001
  
  Fit_NA <- gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=c(10, 20))
  
  Fit_inf <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_inf, z=c(10, 20))  
  expect_that(Fit_inf$time, equals(Fit_NA$time))
  expect_that(Fit_inf$surv, equals(Fit_NA$surv))

  Fit_0 <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_0, z=c(10, 20))  
  expect_that(Fit_0$time, equals(Fit_NA$time))
  expect_that(Fit_0$surv, equals(Fit_NA$surv))
  
  Fit_small <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_small, z=c(10, 20))  
  expect_that(Fit_small$time, equals(Fit_NA$time))
  expect_that(Fit_small$surv, equals(Fit_NA$surv))    
})

test_that("Events at exact time are treated correctly", {
  simul_exact <- simul
  id_exact <- ifelse(is.na(simul$L) | is.na(simul$R), FALSE, simul$R == simul$L)
  simul_exact$R[id_exact] <- simul_exact$L[id_exact] + 0.0000001
  
  Fit <- gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=c(10, 20))
  Fit_exact <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_exact, z=c(10, 20))
  
  # les vecteurs pour _exact devrait être plus longs du nombre de données exactes (ici 3)
  # identification des emplacements des temps omit
  toremove <- Fit_exact$time %in% simul$L[id_exact]
  expect_that(Fit_exact$time[!toremove], equals(Fit$time))
  expect_that(Fit_exact$surv[!toremove,], equals(Fit$surv))     
})


test_that("the formula argument is validated correctly", {
  # Some validation is done throught the Surv() and the model.frame() functions
  # Wrong variable name
  expect_that(gte(Surv(L1, R, type="interval2") ~ Z, data=simul, z=15),
              throws_error())
  # Not a formula
  expect_that(gte(1, data=simul, z=15),
              throws_error())
  # time2 < time
  expect_that(gte(Surv(R, L, type="interval2") ~ Z, data=simul, z=15),
              gives_warning())
  # Other incorrect input values are validated, they are not tested here

  
  # An error should be generated in these cases:
  # No covariate given
  expect_that(gte(Surv(L, R, type="interval2") ~ 1, data=simul, z=15),
              throws_error("at least one numeric covariate must be given in the formula"))
  # Surv() not used in formula
  expect_that(gte(L ~ Z, data=simul, z=15),
              throws_error("response must be a Surv object"))
  # Type in Surv() == counting
  simul_test <- data.frame(L = ifelse(is.na(simul$L), 0, simul$L), 
                           R = ifelse(is.na(simul$R), 1000000, simul$R + 0.000001),
                           Z = simul$Z,
                           event = sample(c(0, 1), nrow(simul), replace=TRUE))
  expect_that(gte(Surv(L, R, event, type="counting") ~ Z, data=simul_test, z=15),
              throws_error("Surv object type='counting' not supported"))  
  
  ### The function should work in these cases:  
  
  # Type in Surv() == interval2
  Fit <- gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15)
  event <- ifelse(is.na(simul$R), 0,
                  ifelse(is.na(simul$L), 2,
                         ifelse(simul$R==simul$L, 1, 3)))
  time <- ifelse(event==2, simul$R, simul$L)
  time2 <- ifelse(event==3, simul$R, NA)
  simul_event <- cbind(simul, time, time2, event)
  Fit_event <- gte(Surv(time, time2, event, type="interval") ~ Z, data=simul_event, z=15)
  expect_that(Fit_event$time, equals(Fit$time))
  expect_that(Fit_event$surv, equals(Fit$surv)) 
  
  # Type in Surv() == right
  simul_right <- simul_event[simul_event$event %in% c(0,1),]
  Fit <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_right, z=15)
  Fit_right <-gte(Surv(time, event, type="right") ~ Z, data = simul_right, z=15)
  expect_that(Fit_right$time, equals(Fit$time))
  expect_that(Fit_right$surv, equals(Fit$surv))
  
  # Type in Surv() == left
  simul_left <- simul_event[simul_event$event %in% c(1,2),]
  simul_left$event[simul_left$event==2] <- 0
  Fit <- gte(Surv(L, R, type="interval2") ~ Z, data=simul_left, z=15)
  Fit_left <-gte(Surv(time, event, type="left") ~ Z, data = simul_left, z=15)
  expect_that(Fit_left$time, equals(Fit$time))
  expect_that(Fit_left$surv, equals(Fit$surv))  
})

test_that("the data argument is validated correctly", {
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=TRUE, z=15),
              throws_error())
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=as.matrix(simul), z=15),
              throws_error())  
})
  
test_that("the z argument is validated correctly", {
  library(Epi)
  data(hivDK)
  for( i in 2:4 ) hivDK[,i] <- cal.yr( hivDK[,i] )
  hivDK[,3:4] <- hivDK[,3:4] - hivDK[,2]
  
  expect_that(gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=matrix(c(0, 7), ncol=1)),
              throws_error("columns, i.e. the number of covariates"))
  expect_that(gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=0),
              throws_error("must be of length"))
  expect_that(gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=TRUE),
              throws_error("must be numeric"))
  expect_that(gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z="0"),
              throws_error("must be numeric"))
  
  # z can be a vector, a matrix or a data frame
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth, data=hivDK, z=c(-30, -10, 10))
  Fit2 <- gte(Surv(well, ill, type="interval2") ~ bth, data=hivDK, z=matrix(c(-30, -10, 10), ncol=1))
  expect_that(Fit2$time, equals(Fit$time))
  expect_that(Fit2$surv, equals(Fit$surv))
  
  Fit <- gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=matrix(c(0, 7, -20, 20), byrow=TRUE, ncol=2))
  Fit2 <- gte(Surv(well, ill, type="interval2") ~ bth + pyr, data=hivDK, z=data.frame(bth=c(0, -20), pyr=c(7, 20)))
  expect_that(Fit2$time, equals(Fit$time))
  expect_that(Fit2$surv, equals(Fit$surv))
})

test_that("the h, itermax and tole arguments are validated correctly", {
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, h="oui"),
              throws_error("'h' must be numeric"))
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, h=c(2,3)),
              throws_error("'h' must be of length"))  

  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, itermax="oui"),
              throws_error("'itermax' must be a single positive integer"))
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, itermax=c(2,3)),
              throws_error("'itermax' must be a single positive integer"))  
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, itermax=-40),
              throws_error("'itermax' must be a single positive integer"))  
  
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, tole="oui"),
              throws_error("'tole' must be a single positive real"))
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, tole=c(2,3)),
              throws_error("'tole' must be a single positive real"))  
  expect_that(gte(Surv(L, R, type="interval2") ~ Z, data=simul, z=15, tole=-0.001),
              throws_error("'tole' must be a single positive real"))  
})

