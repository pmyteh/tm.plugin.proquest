test_that("document 1 is fully correct", {
  library(lubridate)
  doc <- system.file("htmlfiles", "sample-1.html",
                     package = "tm.plugin.proquest") %>%
    readLines %>%
    paste0(collapse="\n") %>%
    tm::SimpleSource(encoding='UTF-8', length=1, content=.,
                     uri="sample-1.html", reader=readProQuest,
                     class="ProQuestSource") %>%
    VCorpus %>%
    `[[`(1)

  expect_equal(meta(doc, "origin"),
               "Los Angeles Times; Los Angeles, Calif.")
  expect_equal(meta(doc, "author"),
               "Olivarez-Giles, Nathan")
  expect_equal(meta(doc, "type"),
               "News")
  expect_equal(meta(doc, "section"),
               "LATExtra; Part AA; Business Desk")
  expect_equal(meta(doc, "graphic"),
               character(0))
  expect_equal(meta(doc, "correction"),
               character(0))
  expect_equal(meta(doc, "language"),
               "en")
  expect_equal(meta(doc, "wordcount"),
               239)
  expect_equal(meta(doc, "intro"),
               "With 3,000 employees, City National has no plans to radically change 1st Pacific's payroll of about 80 employees, Walker said. 1st Pacific's failure will cost the industry-financed deposit insurance fund $88 million, the FDIC estimated.")
  expect_equal(meta(doc, "people"),
               character(0))
  expect_equal(meta(doc, "subject"),
               c("Banking industry",
                 "Deposit insurance",
                 "Bank failures",
                 "Bank acquisitions & mergers"))
#  expect_equal(meta(doc, "coverage"),
#               "")
  expect_equal(meta(doc, "company"),
               c("Federal Deposit Insurance Corp",
                 "City National Bank-Beverly Hills CA",
                 "First Pacific Bank of California")
               )
  expect_equal(meta(doc, "rights"),
               "(Copyright (c) 2010 Los Angeles Times)")
  expect_equal(meta(doc, "heading"),
               "City National expands again; It acquires 1st Pacific Bank of California, a failed San Diego institution.")
  expect_equal(meta(doc, "datetimestamp"),
               lubridate::parse_date_time("2010-05-08", "ymd"))
  expect_equal(meta(doc, "publisher"),
               "Los Angeles Times Communications LLC")
#  expect_equal(meta(doc, "edition"),
#               "")
  expect_equal(meta(doc, "id"),
               "MSTAR_251439970")
  expect_equal(meta(doc, "page"),
               "AA.2")

  expect_equal(content(doc),
               c("Los Angeles-based City National Bank, the largest bank headquartered in Southern California, got a little bigger late Friday by acquiring a failed San Diego financial institution immediately after it was seized by federal regulators.",
                 "Under a deal with the Federal Deposit Insurance Corp., City National agreed to assume all $250 million of deposits of the local customers of 1st Pacific Bank of California.",
                 "1st Pacific's six branches will reopen Monday and continue to operate under 1st Pacific's name until they are rechristened as City National Bank offices in the next few months, said City National spokesman Cary Walker.",
                 "The L.A. bank, a unit of City National Corp., also acquired $320 million of 1st Pacific's loans and other assets, Walker said. The FDIC agreed to absorb an undisclosed portion of future losses on about $270 million in 1st Pacific loans.",
                 "City National had $20.1 billion in assets at the end of March and averaged $16.9 billion in deposits during the first quarter of this year.",
                 "With 3,000 employees, City National has no plans to radically change 1st Pacific's payroll of about 80 employees, Walker said.",
                 "1st Pacific's failure will cost the industry-financed deposit insurance fund $88 million, the FDIC estimated.",
                 "Friday's deal marks City National's second acquisition of a failed bank from the FDIC in five months. In December, the company bought Imperial Capital Bank of La Jolla.",
                 "--",
                 "nathan.olivarezgiles@latimes.com")
               )
})

test_that("specific parsing issues are solved", {
  corp <- system.file("emls", "sample.eml",
                      package = "tm.plugin.proquest") %>%
    ProQuestSource %>%
    VCorpus
  expect_equal(meta(corp[["MSTAR_422291783"]], "intro"),
               character(0))
  expect_equal(meta(corp[["MSTAR_422398973"]], "correction"),
               "Crocker Club: An article in Friday's Calendar about the Crocker Club in downtown L.A. said Vincent Terzian is its sole owner. Thomas Turner is a co-owner of the lounge.")
  expect_equal(meta(corp[["MSTAR_422282572"]], "graphic"),
               "Caption: PHOTO: REACTION: Rep. Carolyn Maloney praised Bank of America for its new overdraft policy.; PHOTOGRAPHER:Alex Wong Getty Images")

  count_missing <- function(field) {
    meta(corp, field) %>%
      sapply(identical, character(0)) %>%
      which %>%
      length
  }
  expect_equal(count_missing("heading"), 0)
  # "Anonymous" should be converted to character(0) and hence be missing
  expect_equal(count_missing("author"), 27)
})

test_that("forwarded emails parse", {
  corp <- system.file("emls", "forwarded.eml",
                      package = "tm.plugin.proquest") %>%
    ProQuestSource %>%
    VCorpus

  count_missing <- function(field) {
    meta(corp, field) %>%
      sapply(identical, character(0)) %>%
      which %>%
      length
  }
  expect_equal(count_missing("heading"), 0)
})
