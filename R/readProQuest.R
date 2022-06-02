#' A Reader function generator for ProQuest .eml data
#'
#' @return A [tm::Reader] function which returns objects inheriting from
#'   [tm::PlainTextDocument]
#' @seealso [ProQuestSource]
#' @import xml2
#' @import magrittr
#' @importFrom utils head tail
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
      gsub('^CORRECTION: *(SEE CORRECTION APPENDED; )?', '', .)
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
      gsub(':[ \u00a0]*$', '', .)

    get_from_nodes_by_slug <- function(slug) {
      if (slug %in% slugs) {
        nodes[which(slugs==slug)] %>%
          xml_text(trim=TRUE) %>%
          gsub(paste0('^', slug, ': *'), '', .)
      } else NULL
    }

    slug_lookup_tab <- slugs[!is.na(slugs)] %>%
      sapply(get_from_nodes_by_slug, USE.NAMES=TRUE)

    # TODO: The slug values could be abstracted through a lookup table to handle
    # multiple languages instead of English only.
    do_slug_lookup <- function(slug) {
      trim_string <- function(s) {
        s %>%
          gsub('^[ \u00a0]*', '', .) %>%
          gsub('[ \u00a0]*$', '', .)
      }

      if (slug %in% names(slug_lookup_tab)) {
        return(trim_string(unname(slug_lookup_tab[slug])))
      }

      # Try harder :-(
      #
      # There is a persistent problem with random missing spaces in (our parses
      # of) the files that come out of ProQuest. That means that we should try
      # variations on a theme. If we don't find a lookup, and the slug contains
      # spaces, we delete each in turn until we find one that works. Only if
      # they all fail do we give up. Note that this is not robust to two spaces
      # being missing (unlikely?)
      # if (grepl(' ', slug) && rec==FALSE) {
      #   for (spos in gregexpr(' ', slug)[[1]]) {
      #     newslug <- paste0(substr(slug, 1, spos-1),
      #                       substr(slug, spos+1, nchar(slug)))
      #     if (newslug %in% names(slug_lookup_tab)) {
      #       return(trim_string(unname(slug_lookup_tab[newslug])))
      #     }
      #   }
      # }

      # In fact, this probably works better: simply stripping all spaces and
      # checking against a space-stripped version of the slug table.

      # Slightly inefficient redoing this every time.
      newslug <- gsub(' ', '', slug)
      new_slug_lookup_tab <- slug_lookup_tab
      names(new_slug_lookup_tab) <- gsub(' ', '', names(new_slug_lookup_tab))

      if (newslug %in% names(new_slug_lookup_tab)) {
        return(trim_string(unname(new_slug_lookup_tab[newslug])))
      }


      character(0)
    }

    split_chunk <- function(s) {
      s %>%
        strsplit('; *') %>%
        unlist %>%
        as.character
    }

    extract_datetime <- function(s) {
      # "May 08, 2012"
      dt <- parse_date_time(s, "bdY", quiet=TRUE)
      if(length(dt) == 0) {
        return(integer())
      }
      if(is.na(dt)) {
        # Try harder, by stripping the spaces out. This can fix
        # some of the problems found (e.g. "Jan 11, 2 009" parses
        # fine as "Jan11,2009").
        s <- gsub(' ', '', s)
        dt <- parse_date_time(s, "bdY")
      }
      dt
    }

    m[["origin"]] <- do_slug_lookup("Publication title")
    m[["author"]] <- do_slug_lookup("Author") %>%
      split_chunk
    m[["type"]] <- do_slug_lookup("Document type")
    m[["section"]] <- do_slug_lookup("Section")
    m[["language"]] <- do_slug_lookup("Language of publication") %>%
      standardiseLanguage(m[["id"]])
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
      extract_datetime
    m[["publisher"]] <- do_slug_lookup("Publisher")
    m[["page"]] <- do_slug_lookup("Pages")

    # "Abstract" is a bit more involved, as the text may actually be in the
    # subsequent node.
    m[["intro"]] <- if ("Abstract" %in% slugs) {
      if (do_slug_lookup("Abstract") %in% c("None available.", "Noneavailable.")) {
        character(0)
      } else if (is.na(slugs[which(slugs=="Abstract")+1])) {
        nodes[which(slugs=="Abstract")+1] %>%
          xml_text(trim=TRUE)
      } else {
        # TODO: Some of the files have strange parse trees that look like this:
        #
        # <p style="margin-bottom:5pt; margin-top:0; margin-right:0; margin-left:0; padding:0;">
        #   <strong>Abstract:  </strong>
        # </p>"The wage gap seems to be stuck," said Vicki Shabo, vice [rest of line]
        #Only in two occupational categories =E2=80=94 =E2=80=9Cdining [rest of line]
        #[...]persistent pay gaps are less about pay and more about [rest of line]
        #  <div style="clear:both"></div>

        # Here, the abstract text is neither in the same node as the heading,
        # nor in the 'next' node in the list: It's outside the surrounding
        # paragraph tag. If all else fails (which it has, if we're here) then we
        # should maybe try to parse from there.
        #
        # Note, though, that the HTML here is not valid, so isn't properly
        # parsed by xml2. That means that if we *do* do it, we're going to be
        # regexing it from the original textual representation, with something
        # like the following:
        if (grepl('<strong>Abstract:[ \u00a0]*</strong></p>.*<div', elem$content)) {
          temp <- gsub('^.*<strong>Abstract:[ \u00a0]*</strong></p>', '', elem$content) %>%
            gsub('<div.*', '', .)
#          warning(m[["id"]],
#                  ": Broken Abstract chunk. Extracted the following:\n",
#                  temp,
#                  "\n")
          temp
        } else {
          warning(m[["id"]], ": Can't find Abstract content.\n")
          character(0)
        }
      }
    } else {
      warning(m[["id"]], ": No Abstract chunk.\n")
      character(0)
    }

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

    if (length(m[["datetimestamp"]]) == 0) warning(glue("{m[['id']]}: Can't extract datetime.\n"))
    if (length(m[["language"]]) == 0) {
      if (length(language) == 1) {
        m[["language"]] <- language
        warning(glue("{m[['id']]}: Can't extract language. Falling back to '{m['language']}'.\n"))
      } else warning(glue("{m[['id']]}: Can't extract language.\n"))
    }

    # For consistency with other tm.plugin.*, a non-bylined article is
    # character(0)
    m[["author"]] <- if(!identical(m[["author"]], "Anonymous")) m[["author"]] else character(0)

    #####
    # 5: Generate and return a PlainTextDocument
    #####

    PlainTextDocument(x = bodytext, meta = m)

  }
}
class(readProQuest) <- c("FunctionGenerator", "function")
