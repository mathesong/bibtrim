
<!-- README.md is generated from README.Rmd. Please edit that file -->
bibtrim
=======

The goal of bibtrim is to make it easier to create project-specific BibTex files from a reference library, and to make it easier for multiple users, or the same user on multiple computers, to keep an updated project-specific library.

This is important for users who use something like my workflow of using Mendeley or Zotero to store references and outputting a large full-library BibTeX file, and using different computers to work on. This results in a problem of having to change the library location when changing computers, as well as not being able to share a monstrosity of a BibTeX library.

Installation
------------

You can install the development version of bibtrim from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("mathesong/bibtrim")
```

Example
-------

The main do-it-all function is called `bib_megafix()`, which

1.  Finds all the Rmd files in a given folder
2.  Extracts all the citations from those files
3.  Checks all the places that the reference BibTeX file could be (i.e. for different users or for the same user on multiple computers)
4.  Adds the new citations from the Rmd files into the project-specific BibTex file (presumably stored within the repo)
5.  *Optionally* trims the project-specific reference library down to include only the references in the text (worth not doing all the time, in case references come and go based on editing)

``` r
bib_megafix(rmdfolder = getwd(),
            biblib_file = 'library/library.bib',
            bibref_file_locations = c(
              "C:/OfficePC/Documents/BibTeX_files/library.bib", # Office
              "E:/HomePC/Documents/BibTeX_files/library.bib",   # Home
              "C:/Laptop/Documents/BibTeX_files/library.bib"    # Laptop
            ), 
            trim = F)
```
