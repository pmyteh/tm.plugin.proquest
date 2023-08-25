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

test_that("v2-format document 1 is fully correct", {
  library(lubridate)
  doc <- system.file("emls", "format-v2.eml",
                      package = "tm.plugin.proquest") %>%
    ProQuestSource %>%
    VCorpus %>%
    `[[`(1)


  expect_equal(meta(doc, "origin"),
               "New York Times,  Late Edition (East Coast); New York, N.Y.")
  expect_equal(meta(doc, "author"),
               c("Wines, Michael",
                 "Cramer, Maria")
  )
  expect_equal(meta(doc, "type"),
               "News")
  expect_equal(meta(doc, "section"),
               character(0))
  expect_equal(meta(doc, "graphic"),
               character(0))
  expect_equal(meta(doc, "correction"),
               character(0))
  expect_equal(meta(doc, "language"),
               "en")
  expect_equal(meta(doc, "wordcount"),
               1214)
  expect_equal(meta(doc, "intro"),
               character(0))
  expect_equal(meta(doc, "people"),
               character(0))
  expect_equal(meta(doc, "subject"),
               c("Hispanic Americans",
                 "Ethnicity",
                 "Minority & ethnic groups",
                 "Coronaviruses",
                 "Pandemics",
                 "Native North Americans",
                 "Census of Population",
                 "COVID-19" )
  )
  #  expect_equal(meta(doc, "coverage"),
  #               "")
  expect_equal(meta(doc, "company"),
               c("New York Times Co",
                 "Bureau of the Census",
                 "House of Representatives")
  )
  expect_equal(meta(doc, "rights"),
               "Copyright New York Times Company Mar 10, 2022")
  expect_equal(meta(doc, "heading"),
               "2020 Census Undercounted Hispanic, Black and Native American Residents")
  expect_equal(meta(doc, "datetimestamp"),
               lubridate::parse_date_time("2022-03-10", "ymd"))
  expect_equal(meta(doc, "publisher"),
               "New York Times Company")
  #  expect_equal(meta(doc, "edition"),
  #               "")
  expect_equal(meta(doc, "id"),
               "MSTAR_2637783411")
  expect_equal(meta(doc, "page"),
               character(0))

  expect_equal(content(doc),
               c("WASHINGTON — Saddled with daunting logistical and political obstacles, the 2020 census seriously undercounted the number of Hispanic, Black and Native American residents even though its overall population count was largely accurate, the Census Bureau said Thursday.",
                 "At the same time, the census overcounted white and Asian American residents, the bureau said.",
                 "In essence, the bureau’s report said, minority groups — mostly concentrated in cities and tribal areas — were underrepresented in census figures, even though the total population count in those areas often was fairly accurate. That could affect those groups’ political clout, and conceivably could sway decisions by businesses and governments over the next decade, from the allocation of city services to locations of stores.",
                 "Some minority advocacy groups threatened to challenge the results in court, but remedying the undercounts would be difficult if not impossible, experts said.",
                 "Robert Santos, the bureau’s director, said that despite the omissions, the results were consistent with recent censuses.",
                 "“This is notable, given the unprecedented challenges of 2020,” he said in a statement. “But the results also include some limitations; the 2020 census undercounted many of the same population groups we have historically undercounted, and it overcounted others.”",
                 "Those challenges included the coronavirus pandemic, which hamstrung efforts to interview people in person, and interference by the Trump administration, which tried to remove noncitizens from the count and, later, to end the counting process weeks early.",
                 "But some obstacles face every census count. The census has historically undercounted populations that are harder to reach through surveys, phone calls and door-to-door canvassing, including Native Americans on reservations, poor urban communities and immigrants living in the country illegally.",
                 "The estimates released Thursday covered the count of 323.2 million people who were living in households on April 1, 2020, the official census date. Counts of others, such as prison inmates and students in college dormitories, pushed the total population count to 331.45 million.\n",
                 "\nTerri Ann Lowenthal, a leading expert on the census and consultant to governments and others with a stake in the count, called the results “troubling but not entirely surprising.”",
                 "“Overall, the results are less accurate than in 2010,” she said.",
                 "\nBy the bureau’s estimates, the 2020 tally incorrectly counted 18.8 million residents, double-counting some 5.2 million people, wrongly including another 2 million and missing others entirely, even as it came extremely close to reaching an accurate count of the overall population. Many of the people whom the census originally failed to count were picked up and added to final census totals through a process called imputation — a statistical guess, using complex algorithms, of who was living in places that census takers could not reach.",
                 "Those imputations, however, did not guess the race or ethnicity of missing residents. That partly explains why the grand total count of residents was substantially accurate, even as many minorities were missed.",
                 "Although the bureau did not say how many people it missed entirely, they were mostly people of color, disproportionately young ones. The census missed counting 4.99 of every 100 Hispanics, 5.64 of every 100 Native Americans and 3.3 of every 100 African Americans.",
                 "In contrast, for every 100 residents counted, the census wrongly added 1.64 non-Hispanic whites and 2.62 ethnic Asians.",
                 "Young people in general were undercounted: by 2.79% for children under 5, and, among those ages 18 to 29, 2.25% for men and 0.98% for women. Older residents were overcounted, most significantly by 2.63% for women 50 and older.",
                 "The estimates released Thursday — in essence, a statistical adjustment of totals made public last year — are based on an examination of federal records and an extensive survey in which the bureau interviewed residents in some 10,000 census blocks — the smallest unit used in census tabulations. Bureau experts then compared their answers to the actual census results for those blocks.",
                 "The survey enabled the bureau to estimate how many residents it missed entirely in the 2020 count, how many people were counted twice and how many people — such as short-term visitors and people who died before or were born after the official April 1, 2020, census date — were counted mistakenly.",
                 "Some advocacy groups for minority populations expressed outrage at the undercounts. In a news release, Arturo Vargas, the CEO of the NALEO Educational Fund, which represents Latinos, called the counting errors a “five-alarm fire” and questioned whether the results should be used.",
                 "Marc Morial, the CEO of the National Urban League, called the undercount of Black people a tragedy, adding, “Any options we have, including litigation, will be on the table.”",
                 "Experts said, however, that those unhappy with the results had limited options. Earlier census figures already have been used to reapportion seats in the House of Representatives and state legislatures, among other governing bodies.",
                 "The estimates released Thursday do not affect redistricting. They reflect only errors in the national population count, and final refinements, due this summer, would only break down wrong counts by state.",
                 "While they decried the scope of the flaws in the 2020 census, some experts noted that many undercounts and other errors were not statistically different from those in the 2010 count. And that, they said, was a triumph all by itself.\n",
                 "\nThe most intriguing question is how the Census Bureau managed such an accurate count given the hurdles it faced, said Steve Jost, a former census official and consultant to the Census Project, a coalition of organizations that advocates an accurate count.",
                 "“To pull off the nation’s largest peacetime mobilization in the middle of a once-a-century pandemic and operational interference from the White House is a remarkable achievement,” he said.",
                 "\nThe 2020 census faced a series of challenges. The coronavirus pandemic shut down much of the count just as it was beginning in April 2020, forcing the bureau to extend its work by nearly two months. Later in the year, wildfires in the West and coastal hurricanes upended the bureau’s work just as door-knockers were fanning out to survey millions of households that had not filled out their forms.",
                 "The Trump administration later moved up the deadline to finish the counting, raising concerns about an undercount. The problems led many experts, including some senior Census Bureau officials, to worry that the final count would be fatally flawed.",
                 "In September, a 59-page analysis of the 2020 census commissioned and reviewed by the American Statistical Association said the count appeared accurate enough for its overriding constitutional purpose: reallocating the 435 seats in the House of Representatives.",
                 "But the experts who drafted the report limited their findings to the overall national tally and counts in the 50 states and the District of Columbia. Much more study would be needed, they said, to gauge the reliability of local population totals and characteristics such as race and ethnicity that are vital parts of every census.\nThis article originally appeared in The New York Times.\n"       )
  )
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
