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
# last update:  20250623
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
#' @param use_cacti_precision report results using cacti decimal
#' precision
#' DEFAULT FALSE
#'
#' @param lines_to_skip skip this many lines when reading the 
#' file
#' DEFAULT 1
#'
#' @param sheets from which sheets to read
#' DEFAULT c(2,3)
#'
#' OUTPUT
#' @return dataframe, cleaned cacti data
#'
#' @export
read_cacti <- function(fname
                      ,show_units = FALSE
                      ,use_cacti_precision = FALSE
                      ,lines_to_skip = 1
                      ,sheets = c(2,3)
                      )
{
    # CACTI data has the chemistry results on sheets 2 and 3 of
    # the excel file
    if(length(sheets) < 2)
    {
        x <- readxl::read_excel(path  = fname
                               ,sheet = sheets
                               ,skip  = lines_to_skip
                               )
    } else {
        for(sheet in sheets)
        {
            # using read_excel method from readxl package
            x <- readxl::read_excel(path  = fname
                               ,sheet = sheet
                               ,skip  = lines_to_skip
                               )
            if(sheet == sheets[1])
            {
                df <- x
            } else {
                # if for some reason, row numbers in different sheets
                # differ, then adjust that now
                # e.g., due to sample loss
                if(nrow(df) > nrow(x))
                {
                    dummy <- matrix(NA,nrow = nrow(df) - nrow(x)
                                   ,ncol = dim(x)[2]
                                   )
                    dummy <- as.data.frame(dummy)
                    names(dummy) <- names(x)
                    x <- rbind(x,dummy)
                }
                
                df <- cbind(df,x[,-1])
                   
                
            }
        }
        x <- df
    }
    
    # remove columns with unname data
    # readxl assigns dots and numbers to unname data
    # thus, 1st remove the dots, then the column names with a number
    # as name 
    selection <- sub(pattern = "\\...",replacement = "",x = names(x))
    selection <- which(!is.na(as.numeric(selection)))
    if(length(selection) > 0)
        x <- x[, -selection]

    # identify and fix the USDI to match funaction site identifier
    if(length(sheets) < 2)
    {
        usid_name <- names(x)[names(x) %in% c("REF", "Muestra")]
        if(length(usid_name) > 1)
        {
            # find the correct usid based on the number of caracters
            index <- which(nchar(x[,usid_name]) == max(nchar(x[,usid_name])))
            names(x)[names(x) %in% usid_name[index]] <- "USID"

        } else {
            names(x)[names(x) %in% c("REF", "Muestra")] <- "USID"
        }
        
        # remove unnecesary rows
        x <- x[-c(1:2),]
    }
            
    if(length(sheets) > 1)
    {
        names(x)[names(x) == "code"] <- "Muestra"
        names(x)[names(x) == "Muestra"] <- "USID"
        # remove unnecesary rows
        if(sum(sheets == c(2,3)))
            x <- x[-(1:3),]
        if(sum(sheets == c(1,2)) == 2) # a case with Estonia data
            x <- x[-(1:2),]
    }
            
    # remove unnecessary columns
    selection <- grep("solic|muest|most|cacti|nº", tolower(names(x)))
    x <- x[, -selection]
    # fix the USID
    usid <- x$USID
    if(sum(sheets == 1) && sum(grepl("VA|TP", usid)) > 0)
    { # Italy
        selection <- usid[grep("VA|TP", usid)]
        selection <- which(nchar(selection) > 6)
        usid[selection] <- sub("0", "", usid[selection])
    }
    if(length(sheets) > 1 && sum(grepl("TI|EN", usid)) > 0)
    { # Switzerland
        # remove records not related to TI or ENG
        # e.g., ZHCF1
        selection <- grep("TI|EN", usid)
        x <- x[selection,]
        usid <- usid[selection]
        # fix usid
        # T1 results have different cacti id names than new ones
        if(sum(grep("EN", usid)) > 0)
        {
            usid <- sub("_CF", "", usid)
        } else {
            usid <- sub("CF", "T", usid)
            usid <- sub("-","", usid)
        }
    }
    # standardize USID by removing special characters
    usid <- gsub("_","",usid)

    if(sum(sheets == c(1,2)) == 2) # fix Estonia ids
    {
        usid <- gsub(" ", "", usid)
        usid <- sub("^\\d+", "", usid)
        # it seems that names with "CF" belong to T1
        # thus, using regexpr() and substr() to find
        # and remove characters after CF. Then,
        # sub() to replace CF by T1
        selection <- grep(pattern = "CF", x = usid)
        index <- regexpr(pattern = "F", text = usid[selection])
        index <- index[1:length(index)]
        usid[selection] <- substr(x = usid[selection]
                                 ,start = 1
                                 ,stop = index
                                 )
        usid <- sub(pattern = "CF", replacement = "T1"
                   ,x = usid
                   )
        
        # remove the chacter "C" from usid
        usid <- sub("C","",usid)
    }

    x$USID <- usid
    # end of fix the USID

    # fix variable names
    names(x)[-1] <- fix_varnames(names(x)[-1])
    # grant that all data will follow the same order
    foo <- x[,-1][,order(names(x[-1]))]
    x[,-1] <- foo
    names(x)[-1] <- names(foo)

    # remove repeated parameter measurements
    # Estonia dataset
    if(sum(sheets == c(1,2)) == 2) # a case with Estonia data
        x <- x[, !names(x) %in% "TOC.1"]

    # add units, if requested, based on agreement on colnames
    # see chemistry_colnames.csv
    if(show_units)
        names(x)[-1] <- append_units(names(x)[-1]) 

    # replace values indicated by "<" with zeros
    x[] <- lapply(x, sub, pattern = "<", replacement = 0)

    # make sure that chemistry variables are of numeric type
    x[,-1] <- as.data.frame(sapply(x[,-1], as.numeric))

    # adjust precision (as reported by cacti)
    if(use_cacti_precision)
        for (var in names(x)[-1])
        {
            precision <- get_precision(var)
            if(precision)
                x[, var] <- round(x = x[, var], digits = precision)
        }
    
    # adjust order based on USID
    x <- x[order(x$USID),]
    
    # catch repeated ids
    if(length(unique(x$USID)) != dim(x)[1]){
        y <- table(x$USID)
        warning(paste0("repeated USID: ", names(y)[y > 1])
               ,immediate. = TRUE
               )
    }

    # remove C1 / C2 chemistry code from USID if still present
    # at the end of the name string USID code
    usid <- x$USID
    #selection <- grep(pattern = "C1|C2", x = usid)
    index <- regexpr(pattern = "C1|C2", text = usid)
    index <- index[1:length(index)]
    ids <- usid[index + 1 == nchar(usid)]
    usid[usid %in% ids] <- substr(x = usid[usid %in% ids],
                              start = 1,
                              stop = index - 1
                             )
    x$USID <- usid
    
    # apply decision regarding the limits of detection LOD and of 
    # quantification LOQ:
    # it consist on using a constant value for all values below
    # the LOQ. They will have high uncertainty anyway, and it is
    # recommended to avoid absolute zero.
    # the decision is to use loq / 2
    for(var in names(x)){
        loq <- getLOQ(var)
        if(length(var) > 0)
            x[,var][x[,var] < loq] <- loq / 2
    }

    
    # return prepared cacti data frame
     return(x)
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



###############################################################
#' fix_varnames
#'
#' DESCRIPTION
#' standardization of variable names
#'
#' PARAMETERS
#' @param x vector of current names
#'
#' OUTPUT
#' @return new simplified names
#'
fix_varnames <- function(x)
{
    x[grep("Ca|Cal|cal", x)] <- "Ca"
    x[grep("K|Pot|pot", x)] <- "K"
    x[grep("Mg|Mag|mag", x)] <- "Mg"
    x[grep("Na|Sod|sod", x)] <- "Na"
    x[grep("NO2|nitri|Nitri", x)] <- "NO2"
    x[grep("PO4|Fosfa|fosfa", x)] <- "PO4"
    x[grep("NH4|Amon|amon", x)] <- "NH4"
    x[grep("F|Flu|flu", x)] <- "F"
    x[grep("Cl|Clo|clo", x)] <- "Cl"
    x[grep("NO3|Nitra|nitra", x)] <- "NO3"
    x[grep("SO4|Sulfa|sulfa", x)] <- "SO4"
    x[grep("TP|PT|P to", x)] <- "TP"
    selection <- which(nchar(x) < 2)
    x[selection][grep("P", x[selection])] <- "TP"
    x[grep("TOC", x)] <- "TOC"
    x[grep("TIC|IC", x)] <- "TIC"
    x[grep("TC", x)] <- "TC"
    x[grep("TN", x)] <- "TN"

    return(x)
}



###############################################################
#' append_units
#'
#' DESCRIPTION
#' append units to variable names
#'
#' PARAMETERS
#' @param x vector of current names
#'
#' OUTPUT
#' @return variable names with units
#'
append_units <- function(x)
{
    return (
            c("Ca(mg/l)"
             ,"Cl(mg/l)"
             ,"F(mg/l)"
             ,"K(mg/l)"
             ,"Mg(mg/l)"
             ,"Na(mg/l)"
             ,"NH4(µg/l)"
             ,"NO2(µg/l)"
             ,"NO3(mg/l)"
             ,"PO4(µg/l)"
             ,"SO4(mg/l)"
             ,"TC(mg/l)"
             ,"TIC(mg/l)"
             ,"TN(mg/l)"
             ,"TOC(mg/l)"
             ,"TP(mg/l)"
            )
        )  
}



###############################################################
#' get_precision
#'
#' DESCRIPTION
#' report the corresponding precision of the variable of
#' interest
#'
#' PARAMETERS
#' @param x target variable
#'
#' OUTPUT
#' @return precision of target variable
#'
get_precision <- function(x)
{
    precision <- c(
        3, # "Ca(mg/l)""
        2, # "Cl(mg/l)"
        2, # "F(mg/l)"
        3, # "K(mg/l)"
        3, # "Mg(mg/l)"
        3, # "Na(mg/l)"
        2, # "NH4(µg/l)"
        2, # "NO2(µg/l)"
        2, # "NO3(mg/l)"
        2, # "PO4(µg/l)"
        2, # "SO4(mg/l)"
        2, # "TC(mg/l)"
        2, # "TIC(mg/l)"
        2, # "TN(mg/l)"
        2, # "TOC(mg/l)"
        3 # "TP(mg/l)"
    )
    var_list <- c(
        "Ca(mg/l)",
        "Cl(mg/l)",
        "F(mg/l)",
        "K(mg/l)",
        "Mg(mg/l)",
        "Na(mg/l)",
        "NH4(µg/l)",
        "NO2(µg/l)",
        "NO3(mg/l)",
        "PO4(µg/l)",
        "SO4(mg/l)",
        "TC(mg/l)",
        "TIC(mg/l)",
        "TN(mg/l)",
        "TOC(mg/l)",
        "TP(mg/l)"
    )
    var <- grep(x, var_list)
    return (precision[var])
}



###############################################################
#' getLOQ
#'
#' DESCRIPTION
#' report the corresponding limit of quantification LOQ of the 
#' variable of interest
#'
#' PARAMETERS
#' @param x target variable
#'
#' OUTPUT
#' @return precision of target variable
#'
getLOQ <- function(x)
{
    loq <- c(
        0.03, # "Ca(mg/l)""
        0.1, # "Cl(mg/l)"
        0.12, # "F(mg/l)"
        0.05, # "K(mg/l)"
        0.02, # "Mg(mg/l)"
        0.05, # "Na(mg/l)"
        0.07, # "NH4(µg/l)"
        0.08, # "NO2(µg/l)"
        0.08, # "NO3(mg/l)"
        0.03, # "PO4(µg/l)"
        0.08, # "SO4(mg/l)"
        0.1, # "TC(mg/l)"
        0.1, # "TIC(mg/l)"
        0.1, # "TN(mg/l)"
        0.1, # "TOC(mg/l)"
        0.01 # "TP(mg/l)"
    )
    var_list <- c(
        "Ca(mg/l)",
        "Cl(mg/l)",
        "F(mg/l)",
        "K(mg/l)",
        "Mg(mg/l)",
        "Na(mg/l)",
        "NH4(µg/l)",
        "NO2(µg/l)",
        "NO3(mg/l)",
        "PO4(µg/l)",
        "SO4(mg/l)",
        "TC(mg/l)",
        "TIC(mg/l)",
        "TN(mg/l)",
        "TOC(mg/l)",
        "TP(mg/l)"
    )
    var <- grep(x, var_list)
    return (loq[var])
}
