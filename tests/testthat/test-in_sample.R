test_that("in_sample works with include.differenciation=TRUE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = TRUE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  expect_equal(simul,window(construction,
                            start=start(simul),
                            end=end(simul),extend=TRUE))
  
  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  expect_equal(simul,window(fitted(prais(benchmark)),
                            start=start(simul),
                            end=end(simul),extend=TRUE))
})
test_that("in_sample works with include.differenciation=FALSE", {
  benchmark <- annualBenchmark(hfserie = turnover,
                               lfserie = construction,
                               include.differenciation = FALSE)
  
  simul <- lag(aggregate(construction),-1)*(100+in_sample(benchmark,type="changes")[,1])/100
  expect_equal(simul,window(construction,
                            start=start(simul),
                            end=end(simul),extend=TRUE))
  
  simul <- lag(aggregate(construction),-1)*(na.omit(in_sample(benchmark,type="changes")[,2]))/100
  expect_equal(simul,window(fitted(prais(benchmark))-lag(aggregate(construction),-1),
                            start=start(simul),
                            end=end(simul),extend=TRUE))
})
