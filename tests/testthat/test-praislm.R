test_that("omega inv sqrt monovar", {
  expect_equal(omega_inv_sqrt(ts(c(1,5,6,7),start=2010,frequency = 12),0.5) %>% unname,
               ts(c(sqrt(0.75),4.5,3.5,4),start=2010,frequency = 12))
})

test_that("error praislm", {
  expect_error(praislm(1:120,ts(1:10,frequency=1,start=2010),
                     TRUE,numeric(),NULL),"ts object")
  expect_error(praislm(ts(1:120,frequency=12,start=2010),1:10,
                       TRUE,numeric(),NULL),"ts object")
})