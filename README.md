# cacti
r functions to facilitate the creation of CACTI requests and the preparation of their results

author: Daniel Romero-Mujalli
email:  daniel.romero@supsi.ch

## Installation
(requires devtools)
type on R console
> devtools::install_github("funaction/cacti")

## Dependencies
- readxl

## USAGE
(NOT RUN)
### read chemistry data

datapath <- "chemistry/cacti_data/"

filename <- list.files(datapath, pattern = "Portugal")

chem <- cacti::read_cacti(filename = filename, path = datapath)

### bind chemistry with funaction data

selection <- unique(x$siteID[x$siteID %in% cacti::get_siteID(chem)])

x <- x[x$siteID %in% selection,]

foo <- x[order(x$siteID),]

foo <- cbind(x, chem[,c(4:dim(chem)[2])])

x <- foo

### create cacti request
?cacti::crt_cacti_request
