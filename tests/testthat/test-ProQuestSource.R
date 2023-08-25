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

test_that("the correct number of documents are loaded from the v2-format file", {
  file <- system.file("emls", "format-v2.eml",
                      package = "tm.plugin.proquest")
  s <- ProQuestSource(file)
  expect_length(s$content, 20)
})


# If the mangled <html> tag beginning the first article isn't corrected, the
# parser will treat that article's closing </html> as an end of the whole
# document and return only the first article.
test_that("malformed <html> tags are handled", {
  file <- system.file("emls", "malformed-html-tag.eml",
                      package = "tm.plugin.proquest")
  s <- ProQuestSource(file)
  expect_length(s$content, 2)
})
