#' Get Citations
#'
#' Get the citations from an Rmd file.
#'
#' @param rmdfile The name of the file
#'
#' @return A vector of all the bibtex keys from the Rmd file.
#' @export
#'
#' @examples
#' \dontrun{
#' citations <- cites_get("Introduction.Rmd")
#' }
#'
cites_get <- function(rmdfile) {

  text <- readLines(rmdfile, warn = F)
  text <- paste(text, collapse = " ")

  citations <- stringr::str_match_all(text, "@(\\w*)[ ;\\]]")[[1]][,2]
  citations <- unique(citations)

  return(citations)
}

#' Add to the project-specific bibtex file
#'
#' Adds the new references from a set of citations to a project-specific bibtex
#' file from a reference bibtex file.
#'
#' @param biblib_file The location of the project-specific bibtex file.
#' @param bibref_file The location of the reference bibtex file.
#' @param citations The citations used in the text to add to the
#'   project-specific bibtex file.
#'
#' @return Creates the revised project-specific bibtex file.
#' @export
#'
#' @examples
#' \dontrun{
#' bib_add("Introduction.Rmd")
#' }
bib_add <- function(biblib_file, bibref_file, citations) {

  biblib <- bib2df::bib2df(biblib_file)
  bibref <- bib2df::bib2df(bibref_file)

  not_in_biblib <- citations[!citations %in% biblib$BIBTEXKEY]

  if(length(not_in_biblib) == 0) {
    return()
  }

  not_in_bibref <- citations[!not_in_biblib %in% bibref$BIBTEXKEY]

  if(length(not_in_bibref) > 0) {
    warning(paste("The following references are not in the reference library:",
                  paste(not_in_bibref, collapse = " "), sep = " "))
  }

    add_from_bibref <- not_in_biblib[not_in_biblib %in% bibref]
  bibref_add <- dplyr::filter(bibref, BIBTEXKEY %in% add_from_bibref)

  biblib <- dplyr::bind_rows(biblib, bibref_add)
  biblib <- dplyr::arrange(biblib, CATEGORY, BIBTEXKEY)

  bib2df::df2bib(biblib, biblib_file)

}

#' Trim the project-specific bibtex
#'
#' Trims the project-specific bibtex file so that it includes only a given set of references
#'
#' @param biblib_file The location of the project-specific bibtex file.
#' @param citationsThe citations used in the text to check against
#'   the project-specific bibtex file.
#'
#' @return Creates the revised project-specific bibtex file.
#' @export
#'
#' @examples
bib_trim <- function(biblib_file, citations) {

  biblib <- bib2df::bib2df(biblib_file)
  biblib <- dplyr::filter(biblib, BIBTEXKEY %in% citations)

  bib2df::df2bib(biblib, biblib_file)
}

#' Get citations from multiple Rmd files
#'
#' Extracts all citations from several Rmd files
#'
#' @param rmdfiles A vector of Rmd files.
#'
#' @return A vector of all unique bibtex keys used across all the Rmd files.
#' @export
#'
#' @examples
cites_get_all <- function(rmdfiles) {

  cites_all <- lapply(rmdfiles, function(x) cites_get(x))
  cites_all <- do.call(c, cites_all)
  cites_all <- unique(cites_all)

  return(cites_all)
}

#' Find all Rmd files
#'
#' Finds all Rmd files in a given folder
#'
#' @param folder The folder to search within
#' @param recursive Logical - should the search be recursive?
#' @param ... Extra arguments for \code{list.files}.
#'
#' @return A vector of all the Rmd files in a given folder
#' @export
#'
#' @examples
rmd_get_all <- function(rmdfolder = getwd(), recursive=T, ...) {

  list.files(path = rmdfolder, pattern = "*.Rmd", recursive = recursive,
             include.dirs = F, ...)
}

#' Check which reference bibtex file exists
#'
#' This function is useful for people like me who write across different
#' computers, upon which the bibtex file is saved in different locations (such
#' as automatically by Mendeley). This function searches for which of the given
#' locations contains the reference file.
#'
#' @param bibref_file_locations A vector of potential file locations for the
#'   reference bibtex file
#'
#' @return The name of the reference bibtex file which exists
#' @export
#'
#' @examples
bibref_check <- function(bibref_file_locations) {

  bibref_exists <- file.exists(bibref_file_locations)
  bibref_file <- bibref_file_locations[bibref_exists]

  if(length(bibref_file) > 1) {
    stop(paste("More than one bibref file found from list.",
               "Create a more exclusive list."))
  }

  if(length(bibref_file) == 0) {
    stop(paste("No bibref files found from list."))
  }

  return(bibref_file)

}


#' BibTex Megafix
#'
#' A default procedure for fixing up the project-specific BibTeX file.
#'
#' @param rmdfolder The folder in which all Rmd files are located.
#' @param biblib_file The location of the project-specific BibTex library.
#' @param bibref_file_locations The potential locations of the bibtex reference
#'   library locations.
#' @param trim Should the project-specific library be trimmed? Default FALSE.
#'
#' @return
#' @export
#'
#' @examples
bib_megafix <- function(rmdfolder = getwd(), biblib_file, bibref_file_locations, trim=F) {

  rmdfiles <- rmd_get_all(rmdfolder)
  citations <- cites_get_all(rmdfiles)
  bibref_file <- bibref_check(bibref_file_locations)

  bib_add(biblib_file, bibref_file, citations)

  if(trim) { bib_trim(biblib_file, citations) }

}
