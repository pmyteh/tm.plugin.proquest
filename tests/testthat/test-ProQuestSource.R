test_that("the correct number of documents are loaded", {
  file <- system.file("emls", "sample.eml",
                      package = "tm.plugin.proquest")
  s <- ProQuestSource(file)
  expect_length(s$content, 100)
})

test_that("forwarded emails load", {
  file <- system.file("emls", "forwarded.eml",
                      package = "tm.plugin.proquest")
  s <- ProQuestSource(file)
  expect_length(s$content, 100)
})

