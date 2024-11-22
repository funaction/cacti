######################################################################
#
#                           CACTI FUNCTIONS
#                   
#
# description:  This script has functions to 
#               - create a csv file to request chemistry analyses 
#                 at CACTI, Vigo
#               - prepare and clean data received from CACTI
#               - get unit siteID from cacti chemistry data
#
# author:       Daniel Romero Mujalli
# email:        daniel.romero@supsi.ch
#
# last update:  20241015
#######################################################################
###############################################################
#' crt_cacti_request
#'
#' DESCRIPTION
#' create a csv file to request chemistry analyses
#'
#' PARAMETERS
#' @param funaction_df funaction kobo csv data
#'
#' @param country full name, e.g., "Switzerland"
#'
#' @param cacti_data chemistry results received from cacti (if any)
#' Default 0
#'
#' @param volume  approx. volume of the water samples (in ml)
#' Default 40
#'
#' @param tag_unfiltered tag used to identify unfiltered samples
#' Default "_C1"
#'
#' @param tag_filtered tag used to identify filtered samples
#' Default "_C2"
#'
#' @param outfname name of the output file (path/filename)
#'
#' OUTPUT
#' @return message with filepath and filename
#'
#' @export
crt_cacti_request <- function(funaction_df
                             ,country  
                             ,cacti_data = 0 
                             ,volume = 40
                             ,tag_unfiltered = "_C1"
                             ,tag_filtered   = "_C2"
                             ,outfname = "cacti_request.csv"
                             )
{
    # select the samples in need of chemistry analysis
    id <- unique(funaction_df$USID[funaction_df$Country == country])

    # remove samples that have already been processed by cacti
    if(!is.null(dim(cacti_data))) 
        id <- id[!id %in% get_siteID(cacti_data)]
    
    # create request
    # two water samples for chemical analyses per site
    # sample types: one filtered, one unfiltered
    n <- length(id) * 2

    idC  <- paste0(id, tag_unfiltered)  # not filtered
    idCF <- paste0(id, tag_filtered) # filtered

    type <- rep("water", times = 2*length(id))

    volume <- rep(volume, times = 2*length(id))

    analysis <- rep(c("Total carbon, Total nitrogen, Total phosphorus"
                     ,"F-, Cl-, K+, Na+, Ca2+, Mg2+, NH4, NO3, NO2, PO4, SO4"
                     )
                    ,times = length(id)
                   )

    id <- c(idC,idCF)[order(c(idC,idCF))]

    filtered <- grepl(pattern = tag_filtered, x = id)

    # combine into dataframe
    df <- as.data.frame(cbind(id, type, filtered, volume, analysis))

    # adjust names:
    names(df) <- c("id_code", "sample_type", "filtered?", "volume(ml)"
              ,"analysis_to_perform")

    # save df to file
    write.csv(x = df, file = outfname,quote = FALSE, row.names = FALSE)
    # print filepath and filename
    writeLines(c(paste0("filename, cacti request: ",outfname)))

}
###############################################################
#' read_cacti
#'
#' DESCRIPTION
#' read, prepare and clean cacti data
#'
#' PARAMETERS
#' @param fname the original path/filename *.xlsx cacti data
#'
#' @param show_units show measurement units along variable names
#' DEFAULT FALSE
#'
#' OUTPUT
#' @return dataframe, cleaned cacti data
#'
#' @export
read_cacti <- function(fname
                      ,show_units = FALSE
                      )
{
    # CACTI data has the chemistry results on sheets 2 and 3 of
    # the excel file
    for(sheet in c(2:3))
    {
        # using read_excel method from readxl package
        x <- readxl::read_excel(path  = fname
                               ,sheet = sheet
                               ,skip  = 1 # skip first row
                               )
        # if requested,
        # to each nutrient name, append the corresponding unit 
        # (e.g., Ca++(mg/l))
        newnames <- names(x)[3:dim(x)[2]]
        # vector of units
        if(show_units){
            units <- x[2,-c(1,2)]
            #newnames <- paste0(newnames,"(",units,")")
        }
        
        names(x)[3:dim(x)[2]] <- newnames
        
        if(sheet < 3)
        {
            df <- x
        } else {
            df <- cbind(df,x[,-1])
        }
    } # sheet loop end here
    
    # adjust order, such that cacti IDs are next to each other
    ids <- grep(pattern = "ID", x = names(df))
    selection <- seq(from = 2, to = length(colnames(df)), by = 1)
    selection <- selection[!selection %in% ids] 
    df <- df[,c(1,ids,selection)]

    # drop unnecessary rows and columns
    df <- df[-c(1:3),]
    # grant that measurements are in numeric format
    df[,4:dim(df)[2]] <- unlist(lapply(X = df[,4:dim(df)[2]]
                                      ,FUN = as.numeric
                                      )
                               )
    # remove punctuation characters and spaces from variable names
    names(df) <- gsub(pattern = " ", replacement = ""
                     ,x = names(df)
                     )
    names(df) <- gsub(pattern = "[[:punct:]]"
                     ,replacement = ""
                     ,x = names(df)
                     )
    # adjust phosphates names
    names(df) <- sub(pattern = "Fosfatos", replacement = "", x = names(df))
    names(df) <- sub(pattern = "Ptotal",    replacement = "TP", x = names(df))

    # show units along the names
    if(show_units)
      names(df)[-c(1:3)] <- paste0(names(df)[-c(1:3)],"(",units,")")

    # remove un/filtered tag from site ID and order data rows by id
    # then, change "Muestra" colname by "USID" to match the identifier
    # used in kobo
    df$Muestra <- get_siteID(df)
    df <- df[order(df$Muestra),]
    names(df)[1] <- "USID"
    
    # return prepared cacti data frame
     return(df)
}
###############################################################
#' get_siteID
#'
#' DESCRIPTION
#' extract unique site ID from cleaned cacti dataframe
#'
#' PARAMETERS
#' @param df cleaned cacti dataframe (see read_cacti function)
#'
#' OUTPUT
#' @return vector with site IDs
#'
#' @export
get_siteID <- function(df)
{
    siteID <- sub(pattern = "_C1", replacement = "", x  = df[,1])
    siteID <- sub(pattern = "_C2", replacement = "", x  = siteID)
    siteID <- gsub(pattern = "_CF", replacement = "", x = siteID)
    return(siteID)
}



