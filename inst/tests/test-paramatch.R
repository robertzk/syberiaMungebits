context('paramatch')

test_that("It handles blank, space only, or punctuation entries (which should all be ignored", {
  expect_equal(paramatch(c("hello?", " ", "???", " =( "), 3, suppress.input = TRUE), 
               data.frame(col_HELLO = c(1,0,0,0)))
})

test_that("Only whole words are being counted", {
  expect_equal(paramatch(c("An ant annhililated an ardvark.", "Ardvarks love eating ants."), 2, suppress.input = TRUE),
               data.frame(col_AN = c(2, 0), col_ANT = c(1, 0)))
})

test_that("Check that N defaults the the number of unique words if set to high", {
  expect_equal(suppressMessages(paramatch(c("goldeen goldeen goldeen", "goldeen goldeen"), 6)), 
               paramatch(c("goldeen goldeen goldeen", "goldeen goldeen"), 1))
})

test_that("punctuation works in the same way that spaces do", {
  expect_equal(paramatch(c("Talking,like?Christopher...Walken*", "//Will// have# no<> effect =) . . ."), 2, suppress.input = TRUE), 
               paramatch(c("Talking like Christopher Walken", "Will have no effect"), 2, suppress.input = TRUE))
})

