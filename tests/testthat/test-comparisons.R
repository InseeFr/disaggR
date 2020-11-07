test_that("in_sample works with include.differenciation=TRUE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  obtained <- construction
  obtained <- window(obtained,start=tsp(obtained)[1]+1)
  expect_equal(simul,obtained)
  
  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  obtained <- fitted(prais(benchmark))
  obtained <- window(obtained,start=tsp(obtained)[1]+1)
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,1])
  obtained <- construction
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,2])
  attr(simul, "na.action") <- NULL
  obtained <- fitted(prais(benchmark))+lag(construction,-1)
  obtained <- window(obtained,start=tsp(obtained)[1]+1)
  expect_equal(simul,obtained)
})

test_that("in_sample works with include.differenciation=FALSE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  obtained <- construction
  obtained <- window(obtained,start=tsp(obtained)[1]+1)
  expect_equal(simul,obtained)

  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  obtained <- fitted(prais(benchmark))-lag(aggregate(construction),-1)
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,1])
  obtained <- construction
  expect_equal(simul,obtained)
  
  simul <- na.omit(in_sample(benchmark,type="levels")[,2])
  attr(simul, "na.action") <- NULL
  obtained <- fitted(prais(benchmark))
  obtained <- window(obtained,start=tsp(obtained)[1]+1)
  expect_equal(simul,obtained)
})

test_that("print in_sample prints",{
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  expect_known_output(print(in_sample(benchmark)),"outputs/in_sample.txt",update=FALSE)
})
