test_that("omega inv sqrt monovar", {
  expect_equal(omega_inv_sqrt(c(1,5,6,7),0.5) %>% unname,
               c(sqrt(0.75),4.5,3.5,4))
})

test_that("error praislm", {
  expect_error(praislm(ts(matrix(1:10,ncol=1),start=2011),ts(matrix(1:10,ncol=1),start=2010),FALSE,FALSE,numeric(),NULL),
               "same windows and frequencies")
  expect_error(praislm(1:120,ts(1:10,frequency=1,start=2010),
                     TRUE,numeric(),NULL),"ts object")
  expect_error(praislm(ts(1:120,frequency=12,start=2010),1:10,
                       TRUE,numeric(),NULL),"ts object")
  expect_error(praislm(ts(matrix(1,1,1),frequency=1,start=2010),
                       ts(1,frequency=1,start=2010),
                       FALSE, TRUE, numeric(), NULL),"less than two observations")
})

test_that("max iter",{
  expect_warning(praislm_impl(matrix(c(1.324398899494450,-0.953033704627492,0.153551486265972),ncol=1),
                              c(26.080220590193644,-20.148635100560220,2.606520694629608),TRUE),
                 "Maximum iterations")
})
