#' A Reader function generator for ProQuest .eml data
#'
#' @return A [tm::Reader] function which returns objects inheriting from
#'   [tm::PlainTextDocument]
#' @seealso [ProQuestSource]
#' @import xml2
#' @import magrittr
#' @importFrom tm PlainTextDocument
#' @importFrom lubridate parse_date_time
#' @export
readProQuest <- function() {
  function(elem, language, id) {

    #####
    # 1: Parsing and chunking
    #####

    # Textual representation of HTML sent from ProQuestSource containing a
    # single top-level <div>
    tree <- read_html(elem$content, asText=TRUE, encoding="UTF-8")

    # Everything under the single <div> is a top-level node
    nodes <- xml_find_all(tree, 'body/div/*')

    # # Set up master metadata list
    m <- list()


    #####
    # 2: Easy XPath extractions from the source tree
    #####

    # The first (empty) <a> has the ID as an attribute
    m[["id"]] <- xml_find_first(tree, '//a[@name]') %>%
      xml_attr("name", default=paste0(basename(elem$uri), ":", id)) %>%
      gsub('^DocID_', '', .)

    # We can get graphics directly too
    m[["graphic"]] <- xml_find_all(tree, '//div[contains(@class, "Illustration_content")]//text()') %>%
      as.character

    # There is an embedded //text element that contains the wordcount
    # plus a representation of the full bodytext.
    txtblock <- xml_find_first(tree, '//text')

    m[["wordcount"]] <- xml_attr(txtblock, "wordcount", default=NA) %>%
      as.integer

    bodytext <- xml_find_all(txtblock, 'p') %>%
      xml_text

    # Corrections need to be extracted from the bodytext
    correlems <- grep('^CORRECTION: ', bodytext)
    m[["correction"]] <- bodytext[correlems] %>%
      gsub('^CORRECTION: *', '', .)
    if (any(correlems)) bodytext <- bodytext[-correlems]

    # Trim empty and spurious nodes
    nodes <- nodes[xml_text(nodes) != ""]
    nodes <- nodes[xml_text(nodes) != "ProQuest document link"]

    #####
    # 3: Extract and delete by tagname or fixed position
    #####

    # By position: The first item is the headline
    m[["heading"]] <- xml_text(nodes[1])
    nodes <- nodes[-1]

    # Make a lookup table for the rest, which can be extracted by tag
    slugs <- lapply(nodes, xml_find_first, 'strong') %>%
      sapply(xml_text) %>%
      gsub(': *$', '', .)

    get_from_nodes_by_slug <- function(slug) {
      if (slug %in% slugs) {
        nodes[which(slugs==slug)] %>%
          xml_text(trim=TRUE) %>%
          gsub(paste0('^', slug, ': *'), '', .)
      } else NULL
    }

    slug_lookup_tab <- slugs[!is.na(slugs)] %>%
      sapply(get_from_nodes_by_slug, USE.NAMES=TRUE)

    # Fix "Abstract", where the text is actually in the subsequent node
    if ("Abstract" %in% slugs && is.na(slugs[which(slugs=="Abstract")+1])) {
      slug_lookup_tab["Abstract"] <- nodes[which(slugs=="Abstract")+1] %>%
        xml_text(trim=TRUE)
    } else {
      warning(m[["id"]], ": Unable to parse Abstract chunk")
    }

    # TODO: The slug values could be abstracted through a lookup table to handle
    # multiple languages instead of English only.
    do_slug_lookup <- function(slug) {
      if (slug %in% names(slug_lookup_tab)) {
        unname(slug_lookup_tab[slug])
      } else {
        character(0)
      }
    }

    split_chunk <- function(s) {
      s %>%
        strsplit('; *') %>%
        unlist %>%
        as.character
    }

    m[["origin"]] <- do_slug_lookup("Publication title")
    m[["author"]] <- do_slug_lookup("Author") %>%
      split_chunk
    m[["type"]] <- do_slug_lookup("Document type")
    m[["section"]] <- do_slug_lookup("Section")
    m[["language"]] <- do_slug_lookup("Language of publication") %>%
      standardiseLanguage
    m[["intro"]] <- do_slug_lookup("Abstract")
    m[["subject"]] <- do_slug_lookup("Subject") %>%
      split_chunk
    m[["people"]] <- do_slug_lookup("People") %>%
      split_chunk
    m[["company"]] <- do_slug_lookup("Company / organization") %>%
      split_chunk %>%
      grep('Name:', ., value=TRUE) %>%
      gsub('^Name: *', '', .)
    m[["rights"]] <- do_slug_lookup("Copyright")
    m[["datetimestamp"]] <- do_slug_lookup("Publication date") %>%
      parse_date_time("bdY") # "May 8, 2010"
    m[["publisher"]] <- do_slug_lookup("Publisher")
    m[["page"]] <- do_slug_lookup("Pages")


    #####
    # 4: Ensure extracted items are sensible
    #####

    # No content may happen occasionally, almost always because the article is
    # a photo (possibly with a heading and a caption) without a text body.
    if ((all(is.na(bodytext)) || length(bodytext) == 0 || identical(bodytext,"")) &&
        length(m["graphic"]) == 0) {
      warning(m[["id"]], "No body text (and no graphic tag) found.\n")
      bodytext <- ""
    }

    m[["wordcount"]] <- if(!is.na(m[["wordcount"]])) m[["wordcount"]] else {
      warning(m[["id"]], ": No wordcount found.\n")
      integer(0)
    }


    #####
    # 5: Generate and return a PlainTextDocument
    #####

    PlainTextDocument(x = bodytext, meta = m)

  }
}
class(readProQuest) <- c("FunctionGenerator", "function")
