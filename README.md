# cacti
r functions to facilitate the creation of CACTI requests and the preparation of their results

author: Daniel Romero-Mujalli<br>
email:  daniel.romero@supsi.ch

## Installation
(requires devtools)<br>
type on R console
> devtools::install_github("funaction/cacti")

## Dependencies
- readxl

## USAGE
(NOT RUN)
### read chemistry data

>datapath <- "chemistry/cacti_data"

>filename <- list.files(datapath, pattern = "Portugal", full.names = TRUE)

>chem <- cacti::read_cacti(filename = filename)

### bind chemistry with funaction data<br>
### assuming that x is the kobo dataframe:
>selection <- unique(x$USID[x$USID %in% cacti::get_siteID(chem)])

>foo <- x[x$USID %in% selection,]

### grant that the site Ids (USID) in foo and chem follow the same order
>foo  <- foo[order(foo$USID),]
>chem <- chem[order(cacti::get_siteID(chem)),]

>foo <- cbind(x, chem[,c(4:dim(chem)[2])])

>x <- foo

### create cacti request
>?cacti::crt_cacti_request
