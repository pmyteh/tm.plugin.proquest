test_that("ordinary valid names standardise", {
  expect_equal(standardiseLanguage("English", "test"),
               "en")
  expect_equal(standardiseLanguage("english", "test"),
               "en")
  expect_equal(standardiseLanguage(c("English", "Invalid"), "test"),
               "en")
  expect_equal(standardiseLanguage("French", "test"),
               "fr")
  expect_equal(standardiseLanguage("German", "test"),
               "de")
})

test_that("multiple valid names standardise", {
  expect_equal(standardiseLanguage("Flemish", "test"),
               "nl")
  expect_equal(standardiseLanguage("Dutch", "test"),
               "nl")
})

test_that("invalid languages warn", {
  expect_warning(standardiseLanguage("Invalid", "test"),
                 regexp="Unable to parse language for test: Invalid.")
  expect_warning(standardiseLanguage(c("Invalid", "English"), "test"),
                 regexp="Unable to parse language for test: Invalid.")
})
