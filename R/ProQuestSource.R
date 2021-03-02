#'A Source to handle .eml output from ProQuest
#'
#'@description Constructs a Source to handle documents sent by email from
#'  ProQuest, in .eml format.
#'
#'@param x A filename string or connection
#'
#'@details An attempt is made to fill up the standard `tm` metadata fields if
#'  explicit mappings are not provided (or produce NULL). The `datetimestamp`
#'  output field is taken as the first available of the following:
#'  `datetimestamp` input field, `modtime` input field, `firstpubtime` input
#'  field, `fetchtime` input field, or (if all else fails) the current date and
#'  time, with a warning. `author` is taken by concatenating (with commas) the
#'  `bylines` input field. `description` is taken from the input `summary`
#'  field, and `heading` from `headline`. `origin` is taken from `source`,
#'  failing which the filename of the input file is used.
#'
#'  The returned documents have IDs constructed to be unique under all normal
#'  circumstances. This is based on the input file's `source` field (if
#'  available), the document date (drawn from the input file's `modtime`,
#'  failing that `firstpubtime`, failing that `fetchtime`, failing that the
#'  current date with a warning), the document's sequence number in the input
#'  file, and an 8 character MD5 digest of the document's metadata list.
#'
#'  Note that an attempt is made to handle forwarded emails (rather than those
#'  directly from ProQuest) but this may well not be successful.
#'
#'@return An object of class `ProQuestSource`, inheriting from
#'  [tm::SimpleSource].
#'
#' @examples
#' \dontrun{
#'
#' s <- ProQuestSource('input.eml')
#' corp <- VCorpus(s)
#'}
#'@import xml2
#'@import magrittr
#'@importFrom utils head tail
#'@importFrom tm getElem
#'@importFrom assertthat assert_that
#'@importFrom glue glue
#'@importFrom stringr str_count
#'@export
ProQuestSource <- function(x) {
  rawemail <- readLines(x, encoding="UTF-8")

  # If this email is straight from ProQuest, it seems to have the following
  # format:
  #
  # Header lines (including a Content-Type: multipart/mixed header)
  #
  # Part boundary header for the main section with Content-Type: text/html,
  # Content-Transfer-Encoding: quoted-printable
  #
  # Main HTML section:
  #   Starting with an XHTML 1.0 Strict doctype and header
  #   Continuing with a <table> containing a copyright notice
  #   Then continuing with a note saying that {user} sent the following
  #   Then with an email sequence number for the download
  #   *Then a series of top-level <div>s each containing a document*
  #   Then ending with a copyright footer
  #
  # Part boundary header for the ProQuest logo, Content-Type: image/gif, base64
  #
  # base64 image data for ProQuest logo
  #
  # Part boundary header for library logo (always? don't know)
  #
  # base64 image data for library logo
  #
  # Final part boundary

  # We also try to handle *some* cases where the email has been forwarded, with
  # different headers, potentially an extra message added, and possibly with the
  # parts in a different order. We cannot successfully handle all forwards: at
  # worst, it will have been forwarded plain text, and none of the HTML
  # information at all will be available, which will stop the entire parsing
  # strategy working. But we do our best.


  # Identify the separator for the multipart/mixed message
#  assert_that(any(grepl("^Content-Type: multipart/mixed;$", rawemail)))

  boundaries <- grep('\tboundary="', rawemail)
  if (length(boundaries) == 0) {
    stop(glue("{x}: Unable to find multipart boundary markers. Is this a .eml from ProQuest?"))
  } else if (length(boundaries) > 1) {
    message(glue("{x}: This email looks like it might have been forwarded. Doing our best to extract the articles."))
  }
  probably_forwarded <- length(boundaries) > 1

  part_boundaries <- boundaries %>%
    tail(1L) %>%
    getElement(rawemail, .) %>%
    gsub('^.*boundary="', '', .) %>%
    gsub('"$', '', .) %>%
    paste0('--', .) %>%
    grep(rawemail)

  # We extract the inner HTML document by pulling pulling out the first section,
  # undoing the quoted-printable line continuations ('=' at EOL) while adding
  # explicit newlines to the other lines, and then undoing all the remaining
  # quoted-printable encoding using qp_decode().
  #
  # We should probably search for the main content section rather than asserting
  # that it's the first section.

  for (i in seq_along(head(part_boundaries, length(part_boundaries)-1))) {
    working_boundary <- part_boundaries[i]

    part_body_start <- rawemail[working_boundary:length(rawemail)] %>%
      grep('^$', .) %>%
      head(1) %>%
      `+`(working_boundary)

    part_header <- rawemail[(working_boundary+1):(part_body_start-2)]

    part_body <- rawemail[part_body_start:(part_boundaries[i+1]-1)]

    if(any(grepl('Content-Type: text/html',
                 part_header,
                 fixed=TRUE)
      ) &&
      any(grepl('Content-Transfer-Encoding: quoted-printable',
                part_header,
                fixed=TRUE)
      ) &&
      length(part_body) > 10
    ) break
    warning(glue("{x}: Couldn't find a suitable text/html section; trying with the last segment."))
  }

  html_txt <- part_body %>%
    gsub("([^=])$", '\\1\n', .) %>%
    gsub("=$", "", .) %>%
    qp_decode() %>%
    paste0(collapse="") %>%
    # A few of the embedded HTML documents containing articles are mangled, and
    # invalid. They have a missing <html> tag. The parser then cheerfully pairs
    # off the closing </html> tag for the article with the opening <html> of the
    # *whole set of documents*, dropping any articles after the malformed one.
    #
    # This is a grotty fix to a grotty piece of encoding.
    gsub(">html&gt;<head", "><html><head", ., fixed=TRUE)

  # Before we go, take an *estimate* of the number of articles that are present
  # in the raw email, so we can check for possible extraction errors later.
  # This may not be accurate: it works for all the documents I have but YMMV.
  num_articles <- stringr::str_count(html_txt, 'DocID_MSTAR_')
  html_doc <- read_html(html_txt)

  html_head <- if (probably_forwarded) {
    xml_find_first(html_doc, '//blockquote/div')
  } else xml_find_first(html_doc, 'head')

  html_body <- if (probably_forwarded) {
    xml_find_first(html_doc, '//blockquote//div[table]')
  } else xml_find_first(html_doc, 'body')

  # have_meta <- xml_find_all(html_head, './meta') %>%
  #   xml_attr('content') %>%
  #   grepl('^Apache Tapestry Framework', .) %>%
  #   any
  #
  # if (!have_meta) {
  #   warning(glue("{x}: Can't find the <meta> tag at the start of the documents"))
  #   browser()
  # }

  # Remove copyright header
  xml_remove(xml_find_first(html_body, './table'))
  # Remove "John Smith sent the following"
  xml_remove(xml_find_first(html_body, './div[@class="mainCopyBlock"]'))
  # Remove leading "Email x of y"
  xml_remove(xml_find_first(html_body, './p[contains(text(), "Email")]'))
  # Remove bibliography, if present
  xml_remove(xml_find_first(html_body, './div[a[@name="Bibliography"]]'))
  # Remove trailing copyright footer
  xml_remove(xml_find_first(html_body, './div[@class="copyright"]'))

  # The remaining top-level <div>s represent individual documents. Build a
  # vector of textual representations of them.
  content <- xml_find_all(html_body, './div') %>%
    sapply(as.character)

  if (length(content) != num_articles) {
    warning(x, ": Found ", num_articles, " ID numbers but extracted ",
            length(content), " articles.\n")
  }

  tm::SimpleSource(encoding='UTF-8', length=length(content), content=content,
                   uri=x, reader=readProQuest, class="ProQuestSource")
}

#' @export
getElem.ProQuestSource <- function(x) list(content = x$content[[x$position]], uri = x$uri)
