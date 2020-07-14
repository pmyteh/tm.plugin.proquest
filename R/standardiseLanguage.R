#' Take the language field given by ProQuest and try to convert into a
#' two-letter ISO_639_2 code.
#'
#' @description Constructs a Source to handle documents sent by email from
#'  ProQuest, in .eml format.
#'
#' @param v A character vector containing the language string to look up
#'  (only the first element is used).
#' @param tid An ID, for use in error reporting
#'
#' @return A string representing a ISO 639:2 language code, or character(0)
#' @import ISOcodes
standardiseLanguage <- function(v, tid) {
  #
  if(length(v) > 0) {
    ISO_639_2 <- ISOcodes::ISO_639_2
    langstr <- strsplit(trimws(v), "; ")[[1]][1]
    lang <- ISO_639_2[match(tolower(langstr),
                            tolower(ISO_639_2[["Name"]])),
                      "Alpha_2"]
    # Some alpha2 codes have multiple English 'language' names: 'Dutch; Flemish'
    # is one prominent one. This will not match above. Try harder (just with
    # those languages that *have* an alpha2 code, not Middle Dutch)
    if(is.na(lang)) {
      pos <- head(grep(tolower(langstr), tolower(ISO_639_2[!is.na(ISO_639_2$Alpha_2),][["Name"]]), fixed=TRUE), n=1L)
      if (length(pos) > 0 && !is.na(pos)) lang <- ISO_639_2[!is.na(ISO_639_2$Alpha_2),]$Alpha_2[pos]
    }
    if(is.na(lang)) {
      warning("Unable to parse language for ", tid, ": ", langstr, ".\n")
      lang <- tolower(langstr)
    }
    lang
  }
  else {
    character(0)
  }
}
