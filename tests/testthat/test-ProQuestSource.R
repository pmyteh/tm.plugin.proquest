test_that("the correct number of documents are loaded", {
  file <- system.file("emls", "sample.eml",
                      package = "tm.plugin.proquest")
  s <- ProQuestSource(file)
  expect_length(s$content, 100)
})
