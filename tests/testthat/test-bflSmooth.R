test_that("Only accepts ts", {
  expect_error(bflSmooth(1:10, 4))
})
test_that("Only accepts integer frequencies", {
  expect_error(bflSmooth(ts(1:10,freq=0.5), 1))
})
test_that("Only accepts strictly positive high frequencies", {
  expect_error(bflSmooth(ts(1:10,freq=1), 0))
})
test_that("Only accepts frequencies that are multiples of the lower one", {
  expect_error(bflSmooth(ts(1:10,freq=4), 5))
})
test_that("Only accepts one dimensional time-series", {
  expect_error(bflSmooth(ts(matrix(1:20,10,2),freq=4), 12))
})

test_that("Smoothing works", {
  expect_identical(bflSmooth(ts(1:10,start=c(2010,2),freq=4), 4),
                   ts(1:10,start=c(2010,2),freq=4))
  expect_identical(bflSmooth(ts(rep(3,12),start=c(2010,2),freq=4), 12),
                   ts(as.double(rep(1,36)),start=c(2010,4),freq=12))
  expect_equal(bflSmooth(ts((1:4)^2,freq=1,start=1990), 4),
               ts(c(0.1188207,0.1712924,0.2762359,0.4336510,
                    0.6435379,0.8700775,1.1132698,1.3731148,
                    1.6496125,2.0003666,2.4253770,2.9246439,
                    3.4981672,3.9283096,4.2150712,4.3584520),start=1990,freq=4))
  expect_equal(bflSmooth(ts(sin(4:7),start=c(2010,3),freq=4),12),
               ts(c(-0.22957412,-0.24659415,-0.28063423,
                    -0.33169433,-0.33541968,-0.29181026,
                    -0.20086608,-0.09733435,0.01878493,
                    0.14749174,0.23329629,0.27619856),start=c(2010,7),freq=12))
})

test_that("cache works for smoothing", {
  bflSmooth_matrices_cache <- bflSmooth_matrices_generator()
  expect_identical(bflSmooth_matrices_cache(20,12,NULL,TRUE),bflSmooth_matrices_impl(20,12,NULL,TRUE))
  expect_identical(bflSmooth_matrices_cache(20,12,NULL,TRUE),bflSmooth_matrices_impl(20,12,NULL,TRUE))
  set.seed(10)
  randomarg <- function(n) {
    lfserie <- ts(arima.sim(n,model = list(order=c(1,1,0),ar=0.7)),freq=sample(1:4,1,T),start=2010)
    hf_freq <- sample(1:4,1,T)*frequency(lfserie)
    list(lfserie,hf_freq)
  }
  randomargs <- lapply(rep(30,150),randomarg)
  randomres <- function(notused) lapply(randomargs,function(x) bflSmooth(x[[1]],x[[2]]))
  reslist <- lapply(rep(1,100),randomres)
  expect_true(all(sapply(reslist, FUN = identical, randomres(1))))
  
  set.seed(3)
  randomarg <- function(n) {
    lfserie <- ts(arima.sim(n,model = list(order=c(1,1,0),ar=0.7)),freq=sample(1:4,1,T),start=2010)
    hf_freq <- sample(1:4,1,T)*frequency(lfserie)
    weights <- ts(arima.sim(hf_freq/frequency(lfserie)*length(lfserie),model = list(order=c(1,1,0),ar=0.7))[-1],freq=hf_freq,start=2010)
    list(lfserie,hf_freq,weights)
  }
  randomargs <- lapply(rep(30,150),randomarg)
  randomres <- function(notused) lapply(randomargs,function(x) bflSmooth(x[[1]],x[[2]],x[[3]]))
  reslist <- lapply(rep(1,100),randomres)
  expect_true(all(sapply(reslist, FUN = identical, randomres(1))))
})

test_that("error weights", {
  expect_error(bflSmooth(construction,12,weights = 14),"must be either NULL or a one-dimensional ts")
  expect_error(bflSmooth(aggregate(turnover,1),12,weights=ts(1:(12*20),freq=4,start=2000)),"frequency of the weights")
  bflSmooth(aggregate(turnover,1),12,weights=window(turnover,start=c(2000,1),end=c(2019,12)))
  expect_error(bflSmooth(aggregate(turnover,1),12,weights=window(cbind(turnover,turnover),start=c(2000,1),end=c(2019,12)))," must be one-dimensional")
  expect_error(bflSmooth(aggregate(turnover,1),12,weights=window(turnover,start=c(1999,1),end=c(2018,12),extend=TRUE)),"same start than the expected high-frequency")
  expect_error(bflSmooth(aggregate(turnover,1),12,weights=window(turnover,start=c(2000,1),extend=TRUE)),"same end than the expected high-frequency")
})

test_that("weights", {
  expect_equal(aggregate.ts(bflSmooth(construction,12,window(turnover,end=c(2019,12))),1),
                            construction)
  ben <- bflSmooth(window(airmiles,start=1949),weights=AirPassengers,nfrequency = frequency(AirPassengers))
  expect_equal(aggregate.ts(ben),window(airmiles,start=1949))
  
  ben2 <- bflSmooth(window(airmiles,start=1949)/aggregate.ts(AirPassengers),weights=AirPassengers,nfrequency = frequency(AirPassengers),lfserie.is.rate = TRUE)
  expect_equal(ben/ben2,AirPassengers)
  expect_equal(ben2,ts(c(4.38416593613805, 4.38642057449218, 4.39105063539796, 
                         4.39833794864968, 4.40822212214856, 4.42054211029787, 4.43557974289189, 
                         4.45359671902528, 4.47459303869805, 4.49832713351511, 4.52445678158343, 
                         4.55268002240914, 4.58327868578653, 4.61459639823386, 4.64670193835782, 
                         4.67968909516756, 4.7135203530594, 4.74813318602728, 4.78367765648578, 
                         4.82028506904769, 4.857955423713, 4.89661368927441, 4.93610355071672, 
                         4.97630620862837, 5.01738423062517, 5.05579669714754, 5.09145169248946, 
                         5.12383448869724, 5.15322083288894, 5.17944527679372, 5.20239752156436, 
                         5.22169152123561, 5.23732727580746, 5.24958053239796, 5.25885572011357, 
                         5.26544696921355, 5.26898661687382, 5.27292827554719, 5.27729310370801, 
                         5.28211166359706, 5.28735574391516, 5.29303004654551, 5.29921685444401, 
                         5.30594437890982, 5.3132408312421, 5.32102863036815, 5.32926545933924, 
                         5.337906650265, 5.34700392386059, 5.35877690091137, 5.37322558141736, 
                         5.39089602730818, 5.41177458703559, 5.43577935131015, 5.46310144180723, 
                         5.49402754103989, 5.52866686139405, 5.56654159868128, 5.60729681264733, 
                         5.65050930529673, 5.69646575914253, 5.73842295122341, 5.77669454912878, 
                         5.81035915431476, 5.83957360057606, 5.8642006583423, 5.88365220088336, 
                         5.89718326767439, 5.90497029673444, 5.907679831691, 5.90589999927418, 
                         5.90014050931679, 5.88989165198604, 5.8790191935297, 5.86754632572516, 
                         5.85538543519115, 5.84253136819935, 5.82898154788562, 5.81462001536295, 
                         5.79932050428774, 5.78312682135063, 5.76612915679705, 5.74842543146492, 
                         5.73011098932796, 5.71108017895583, 5.69480481373812, 5.68121697777313, 
                         5.6707047619277, 5.66322935711514, 5.6588392746938, 5.65807784187722, 
                         5.66132344726057, 5.66849847267047, 5.6791178045234, 5.6927060315075, 
                         5.70892357411432, 5.7281100118523, 5.74336109137696, 5.75485171749777, 
                         5.76189476417748, 5.76459017702151, 5.76285050362513, 5.75583869954295, 
                         5.74301755714585, 5.72436209003249, 5.70065936984552, 5.67262150902355, 
                         5.64077322199503, 5.60472721953895, 5.57419102859664, 5.54880813195249, 
                         5.5292915640377, 5.51541445026052, 5.50741987054069, 5.50647460849287, 
                         5.51348616248405, 5.52868140710596, 5.55042360423246, 5.57798351410437, 
                         5.61056707565059, 5.64861183272664, 5.68035635104226, 5.70611564253547, 
                         5.72476966475999, 5.73649342434805, 5.74086690538228, 5.73698007337506, 
                         5.72350287792143, 5.70024281172592, 5.66887993845797, 5.6303942952581, 
                         5.58557341197137, 5.53366476007916, 5.48554433146994, 5.44097592996781, 
                         5.40021392068528, 5.36363985129116, 5.33135365093678, 5.30392764112533, 
                         5.28215217059932, 5.26588188786587, 5.25422651503113, 5.24675908208479, 
                         5.24283459177722), start=1949,freq=12))
})
  