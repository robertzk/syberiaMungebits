context('paramatch')
# Example Dataframes
df <- data.frame(
  id = 1:10,
  text = c("goldeen goldeen goldeen", 
           "goldeen goldeen",
           "(^_^)     hello? hello? hello?",
           " ", 
           " O==00==O ",
           "An ant annhililated an ardvark and another ant.", 
           "An Ardvark's favorite meal is an ant.",
           "Talking,like?Christopher...Walken* //Will// have# no<> effect =) . . .",
           "Talking like Christopher Walken will have no effect",
           "Only whole words are being counted"),
  stringsAsFactors = FALSE
)
df2 <- data.frame(
  id = 1:3,
  text = c("This is the end",
           "My only friend the end", 
           "My friend put it to the end"),
  stringsAsFactors = FALSE
)
# Testing Begins
test_that("The training data is perserved for use with predictions", {
  mp <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(paramatch)
  mb$run(mp, col = 'text', top_n = 3)
  mp2 <- mungebits:::mungeplane(df2)
  mb$run(mp2, col = 'text', top_n = 3)
  expect_equal(colnames(mp$data), colnames(mp2$data))
})
mb <- NULL
test_that("Check that N defaults the the number of unique words if set to high", {
  mp <- mungebits:::mungeplane(df[1:2, ])
  mp2 <- mp
  mb <- mungebits:::mungebit(paramatch)
  expect_equal(mb$run(mp, col = 'text', top_n = 1),
               suppressMessages(mb$run(mp2, col = 'text', top_n = 6)))
})
mb <- NULL
test_that("It handles blank, space only, or punctuation entries (which should all be ignored)", {
  mp <- mungebits:::mungeplane(df[3:5, ])
  mb <- mungebits:::mungebit(paramatch)
  mb$run(mp, col = 'text', top_n = 1)
  expect_equal(mp$data$col_HELLO, c(3,0,0))
  })
mb <- NULL
test_that("Only whole words are being counted", {
  mp <- mungebits:::mungeplane(df[6:7, ])
  mb <- mungebits:::mungebit(paramatch)
  mb$run(mp, col = 'text', top_n = 2, suppress.input = TRUE)
  rownames(mp$data) <- NULL
  expect_equal(mp$data[,-1], data.frame(col_AN = c(2, 2), col_ANT = c(2, 1)))
})
mb <- NULL
test_that("punctuation works in the same way that spaces do", {
  mp <- mungebits:::mungeplane(df[8:9, ])
  mb <- mungebits:::mungebit(paramatch)
  mb$run(mp, col = 'text', top_n = 4, suppress.input = TRUE)
  row1 <- mp$data[1, -1]
  row2 <- mp$data[2, -1]
  rownames(row1) <- NULL
  rownames(row2) <- NULL
  expect_equal(row1, row2)
})
mb <- NULL
test_that("The blacklist prevents words from participating in the top n columns", {
  mp <- mungebits:::mungeplane(df)
  mp2 <- mungebits:::mungeplane(df)
  mb <- mungebits:::mungebit(paramatch)
  mb$run(mp, col = 'text', top_n = 6)
  blacklist <- sapply(colnames(mp$data[3:5]), function(s) substring(s, 5))
  mb2 <- mungebits:::mungebit(paramatch)
  mb2$run(mp2, col = 'text', top_n = 3, blacklist = blacklist)
  expect_equal(mp$data[-c(3:5)], mp2$data)
})
mb <- NULL
mb2 <- NULL
