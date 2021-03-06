% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bibtrim.R
\name{bib_megafix}
\alias{bib_megafix}
\title{BibTex Megafix}
\usage{
bib_megafix(rmdfolder = getwd(), biblib_file, bibref_file_locations,
  trim = F)
}
\arguments{
\item{rmdfolder}{The folder in which all Rmd files are located.}

\item{biblib_file}{The location of the project-specific BibTex library.}

\item{bibref_file_locations}{The potential locations of the bibtex reference
library locations.}

\item{trim}{Should the project-specific library be trimmed? Default FALSE.}
}
\description{
A default procedure for fixing up the project-specific BibTeX file.
}
