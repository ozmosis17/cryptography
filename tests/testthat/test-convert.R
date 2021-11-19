test_that("letter2num works", {
  expect_equal(letter2num(c("a","b","c","d","`")), c(2,3,4,5,1))
})

test_that("num2letter works", {
  expect_equal(num2letter(c(1,2,3,4,27)), c("`","a","b","c","z"))
})
