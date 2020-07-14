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
#'@importFrom tm getElem
#'@importFrom assertthat assert_that
#'@export
ProQuestSource <- function(x) {
  rawemail <- readLines(x, encoding="UTF-8")

  # Identify the separator for the multipart/mixed message
  assert_that(any(grepl("^Content-Type: multipart/mixed;$", rawemail)))

  part_boundaries <- grep('\tboundary="', rawemail) %>%
    getElement(rawemail, .) %>%
    gsub('^.*boundary="', '', .) %>%
    gsub('"$', '', .) %>%
    paste0('--', .) %>%
    grep(rawemail)

  # We extract the inner HTML document by pulling pulling out the first section,
  # undoing the quoted-printable line continuations ('=' at EOL) while adding
  # explicit newlines to the other lines, and then undoing all the remaining
  # quoted-printable encoding using emayili::qp_decode().
  #
  # We should probably search for the main content section rather than asserting
  # that it's the first section.
  assert_that(grepl('Content-Type: text/html',
                    rawemail[part_boundaries[1]+1],
                    fixed = TRUE),
              grepl('Content-Transfer-Encoding: quoted-printable',
                    rawemail[part_boundaries[1]+2],
                    fixed=TRUE))

  html_content <- rawemail[(part_boundaries[1]+4):(part_boundaries[2]-1)] %>%#
    gsub("([^=])$", '\\1\n', .) %>%
    gsub("=$", "", .) %>%
    emayili::qp_decode() %>%
    paste0(collapse="") %>%
    read_html()

  # Remove copyright header
  xml_remove(xml_find_first(html_content, 'body/table'))
  # Remove "John Smith sent the following"
  xml_remove(xml_find_first(html_content, 'body/div[@class="mainCopyBlock"]'))
  # Remove leading "Email x of y"
  xml_remove(xml_find_first(html_content, 'body/p[contains(text(), "Email")]'))
  # Remove trailing copyright footer
  xml_remove(xml_find_first(html_content, 'body/div[@class="copyright"]'))

  # The remaining top-level <div>s represent individual documents. Build a
  # vector of textual representations of them.
  content <- xml_find_all(html_content, 'body/div') %>%
    sapply(as.character)

  tm::SimpleSource(encoding='UTF-8', length=length(content), content=content,
                   uri=x, reader=readProQuest, class="ProQuestSource")
}

#' @export
getElem.ProQuestSource <- function(x) list(content = x$content[[x$position]], uri = x$uri)
