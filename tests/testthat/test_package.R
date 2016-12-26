test_that("make filenames", {
  expect_that(make_filename(2013), equals("accident_2013.csv.bz2"))
  expect_that(make_filename(2014), equals("accident_2014.csv.bz2"))
  expect_that(make_filename(2015), equals("accident_2015.csv.bz2"))
})

test_that("read files", {
  expect_that(nrow(fars_read("accident_2013.csv.bz2")), equals(30202))
  expect_that(nrow(fars_read("accident_2014.csv.bz2")), equals(30056))
  expect_that(nrow(fars_read("accident_2015.csv.bz2")), equals(32166))
})

test_that("read years", {
  data_2013_2015 <- fars_read_years(2013:2015)
  expect_that(length(data_2013_2015), equals(3))
  expect_that(nrow(data_2013_2015[[1]]), equals(30202))
  expect_that(nrow(data_2013_2015[[2]]), equals(30056))
  expect_that(nrow(data_2013_2015[[3]]), equals(32166))
})

test_that("summarize years", {
  sum_2013_2015 <- fars_summarize_years(2013:2015)
  expect_that(nrow(sum_2013_2015), equals(12))
  expect_that(sum(sum_2013_2015[,2]), equals(30202))
  expect_that(sum(sum_2013_2015[,3]), equals(30056))
  expect_that(sum(sum_2013_2015[,4]), equals(32166))
})
