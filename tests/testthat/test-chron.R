library(geoChronR)

test_that("simulateBam perturbs ages and returns expected structure", {
  set.seed(8)
  t <- seq(0, 1000, by = 10)
  X <- as.matrix(rnorm(length(t)))
  out <- simulateBam(X, as.matrix(t),
                     model = list(ns = 50, name = "poisson",
                                  param = 0.05, resize = 0),
                     ageEnsOut = TRUE)
  expect_true(is.list(out))
  expect_equal(nrow(out$ageEns), length(t))
  expect_equal(ncol(out$ageEns), 50)
  # perturbed chronologies stay monotonically non-decreasing
  expect_true(all(apply(out$ageEns, 2, function(x) all(diff(x) >= 0))))
})

test_that("runBam adds an ensemble to a minimal LiPD object", {
  set.seed(9)
  t <- seq(10, 500, by = 10)
  L <- list(dataSetName = "synthetic",
            paleoData = list(list(measurementTable = list(list(
              age = list(values = t, variableName = "age", units = "yr BP"),
              temp = list(values = rnorm(length(t)),
                          variableName = "temp", units = "degC")
            )))))
  Lb <- runBam(L, time.var = "age",
               paleo.num = 1, paleo.meas.table.num = 1, chron.num = 1,
               model.num = 1, ens.table.number = 1,
               make.new = TRUE, n.ens = 20,
               model = list(name = "poisson", param = 0.05,
                            resize = 0, ns = 20))
  ens.table <- Lb$chronData[[1]]$model[[1]]$ensembleTable[[1]]
  expect_false(is.null(ens.table))
  expect_equal(ncol(ens.table$ageEnsemble$values), 20)
  expect_equal(nrow(ens.table$ageEnsemble$values), length(t))
})

test_that("createSummaryTableFromEnsembleTable summarizes an ensemble", {
  set.seed(10)
  depth <- 1:50
  ageEns <- matrix(sort(runif(50, 0, 5000)), 50, 100) +
    matrix(rnorm(5000, sd = 20), 50, 100)
  L <- list(dataSetName = "synthetic",
            chronData = list(list(model = list(list(ensembleTable = list(list(
              depth = list(values = depth, variableName = "depth", units = "cm"),
              ageEnsemble = list(values = ageEns, variableName = "ageEnsemble",
                                 units = "yr BP")
            )))))))
  Ls <- createSummaryTableFromEnsembleTable(L, paleo.or.chron = "chronData",
                                            paleo.or.chron.num = 1, model.num = 1,
                                            ens.table.num = 1)
  st <- Ls$chronData[[1]]$model[[1]]$summaryTable[[1]]
  expect_false(is.null(st))
  expect_true("medianAge" %in% names(st) || "median" %in% names(st) ||
                any(grepl("median", names(st), ignore.case = TRUE)))
})

test_that("re-exports from ens and lipdViz are available", {
  expect_true(is.function(corEns))           # ens
  expect_true(is.function(bin))              # ens
  expect_true(is.function(gaussianize))      # ens
  expect_true(is.function(plotChronEns))     # lipdViz
  expect_true(is.function(plotTimeseriesEnsRibbons)) # lipdViz
  expect_true(is.function(mapLipd))          # lipdViz
})

test_that("stringifyVariables builds argument strings", {
  expect_true(is.character(stringifyVariables("a", 1)))
})
