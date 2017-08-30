#' Calculate the logit of a number
#'
#' @param x A number between 0 and 1.
#' @return log( x / ( 1 - x ) )
#' @examples
#' logit(0.9)
#'
logit <- function( x ) {
  if( x >= 0 & x <= 1 ) {
    log( x / ( 1 - x ) )
  } else {
    warning("I'm not sure what you heard, but this logit is not defined for x outside of [0,1]")
  }
}

#' Print numbers to an arbitrary number of decimal places
#'
#' @param .x A number
#' @param .dec An integer indicating the number of decimal places to use and round to
#'
#' @examples
#' printDec(345.23, 4)
#' [1] "345.2300"
#'
printDec <- function(.x, .dec = 2) {
  gsub(" {1,}", "", format(round(.x, .dec), nsmall = .dec))
}

#' Calculate the inverse logit of a number
#'
#' @param x A number.
#' @return exp(x) / (1 + exp(x))
#' @examples
#' alogit(0.9)
#' [1] 0.7109495
#'
alogit <- function(x) {
  exp( x ) / ( 1 + exp(x) )
}

#' Ten to the xth
#'
#' @param x A number.
#' @return 10^x
#' @examples
#' exp10(0.9)
#'
exp10 <- function(x) {
  return (10^x)
}


#' Calculate IQR and IQR width
#'
#' Uses default parameters
#' in quantile() function
#'
#' @param x An array of numbers.
#' @param width A boolean indicating whether function should return with or endpoints
#' @param na.rm A boolean indicating whether NAs should be removed
#'
#' @return x's IQR or width of x's IQR
#' @examples
#' iqr(1:100, width = T)
#' iqr(1:100, width = F)

iqr <- function(x, width = T, na.rm = F) {
  if( width ) {
    .ret_val <- quantile(x, 0.75, na.rm = na.rm) -
      quantile(x, 0.25, na.rm = na.rm)[1]
    unname(.ret_val)
  } else {
    .ret_val <- quantile(x, c(0.25, 0.75),
                         na.rm = na.rm)[1:2]
    unname(.ret_val)
  }
}


#' Get classes for each column in a dataframe
#'
#' @param .df a data frame
#' @return a data frame containing column class information
#' @examples
#' colClasses(data.frame(x = pi, y = factor(letters), z = letters, stringsAsFactors = F) )

colClasses <- function(.df) {
  if (class(.df) != "data.frame") {
    warning("Input should be a data frame")
  } else {
    .cols <- .class <- rep(NA, ncol(.df) )

    for (.col in 1:ncol(.df) ) {
      .cols[.col] <- names(.df)[.col]
      .class[.col] <- paste0(class(.df[[.col]]), collapse = " ")
    }
    return(data.frame(column = .cols, class = .class, stringsAsFactors = F) )
  }
}

#' Create a folder
#'
#' Creates a folder .name at specified .path
#' If .name already exists at .path, creates a
#' folder called .name_copy_x where x is an auto-
#' incremented integer. Uses forward slashes, yo.
#'
#' @param .path path to directory where folder should be created
#' @param .name name of folder to be created
#' @return the path to the newly created folder
#' @examples
#' createFolder("~/", "newFolder")
#'
#'
createFolder <- function(.path, .name) {
  .increment <- 1
  .folder_name <- .name
  # if folder name already exists, appends an underscore and
  # an integer to the file name
  # integer is incremented until an unused file name is found
  while ( !dir.create(paste0(.path, .folder_name), showWarnings = F) ) {
    cat("Folder name", .folder_name, "taken, ")
    .folder_name <- paste0(.name, "_", .increment)
    cat("trying", .folder_name, "\n")
    .increment <- .increment + 1
  }
  return(paste0(.path, .folder_name, "/"))
}

#' Removes rows with NAs in a data frame
#'
#' Removes rows with NAs
#' in columns specified by .vars argument
#'
#' @param .df a data frame
#' @param .vars a charactor vector containing
#'     names of columns to search for NAs
#' @return a data.frame purged of all rows with NAs
#'  in the columns specified by .vars
#' @examples
#' test_df <- diag(3)
#' diag(test_df) <- NA
#' test_df <- data.frame(test_df)
#' removeNAs(test_df, c("X1", "X3"))
#' > 1 (33.3%) missing value(s) found in X1
#' > 1 (33.3%) missing value(s) found in X3
#' > 2 observation(s) with missing values removed from dataset,
#' > 1 of 3 (33.3%) remaining
#' > X1 X2 X3
#' > 2  0 NA  0
#'
removeNAs <- function(.df, .vars) {

  to_remove <- llply(.vars, function(..var, ..df) {
    ..na_vals <- which(is.na(..df[[..var]]))
    if (length(..na_vals) < 1) {
      cat(paste0("No missing values found in ", ..var), "\n")
      return(NA)
    } else {
      cat(paste0(length(..na_vals),
                 " (", round( length(..na_vals)/length(..df[[..var]]),3)*100,
                 "%) missing value(s) found in ", ..var), "\n")
      return(..na_vals)
    }
  }, .df)

  to_remove <- unique( unlist(to_remove))
  to_remove <- to_remove[!is.na(to_remove)]

  if (length(to_remove) < 1) {
    cat(paste0("No missing values found in specified variables,\nreturning original dataset with ",
               nrow(.df), " observations."), "\n")
    return(.df)
  }
  if (length(to_remove) == nrow(.df)) {
    cat(paste0("All observations have missing values, returning empty dataset"), "\n")
    return(.df[-to_remove, ])
  }
  if (length(to_remove) >= 1){
    cat(paste0(length(to_remove),
               " observation(s) with missing values removed from dataset,\n",
               nrow(.df[-to_remove, ]), " of ", nrow(.df),
               " (", 100*round(nrow(.df[-to_remove, ])/nrow(.df),3), "%)",
               " remaining"), "\n")
    return(.df[-to_remove, ])
  }
}



#' Count the number of unique values in an array
#'
#' Returns the number of unique values in an array
#'
#' @param x an array
#' @param count.na a boolean, if T, NAs will be counted as unique values
#' @return the number of unique values in x
#' @examples
#' > cUnique(x = letters)
#' 26
#' > cUnique(x = factor(letters))
#' 26
#' > cUnique(x = as.numeric(letters), count.na = F)
#' 0
#' > cUnique(x = as.numeric(letters), count.na = T)
#' 1
#
cUnique <- function(x, count.na = FALSE) {
  members <- unique(x)
  if (NA %in% members & !count.na) {
    return(length(members) - 1)
  } else {
    return(length(members))
  }
}


#' Loads and combines files in a folder that match a specified
#' string
#'
#' Returns either a list containing all loaded files, or
#' a data frame containing merged loaded files
#'
#' @param directory the directory in which to search for files
#' @param match_string all files whose names grep match this string will be loaded
#' @param merge T/F should loaded files be merged into a data frame
#' @param sep field separator character
#' @param na.strings a character vector of strings whic are to be interpreted as NA values
#' @param header a T/F value indicating whether the file contains the names of the variables as its first line. If missing, the value is determined from the file format: header is set to TRUE if and only if the first row contains one fewer field than the number of columns.
#' @param fill T/F If TRUE then in case the rows have unequal length, blank fields are implicitly added
#' @param skip the number of lines of the data file to skip before beginning to read data
#' @return a list or data frame containing load data from all files matching match_string in directory
#'
openFilesInDirectory <- function(directory,
                                 match_string,
                                 merge = FALSE,
                                 sep = ",",
                                 na.strings = "NA",
                                 header = T,
                                 fill=T,
                                 skip = 0) {
  require(plyr)
  file_array <-  paste0(directory, "/", list.files(directory)[grep(pattern=match_string, list.files(directory))])

  data_list <- llply(file_array, function(file_path, delim_str) {
    cat(file_path, "\n")
    to_return <- read.table(file = file_path, header = header, sep = sep, stringsAsFactors = FALSE, fill=fill, quote="\"", na.strings = na.strings, skip = skip )
    to_return["loaded_file_name"] <- tail(strsplit(file_path, "/")[[1]],1)
    return(to_return)
  }, delim_str)

  if(merge |  length(file_array) == 1) {
    data_list <- ldply(data_list, identity)
  }

  return(data_list)
}


#' Converts factors into numeric or character arrays
#'
#' @param var an array of factors to be converted
#' @param to a string indicating whether factor should be converted to a character (to = "c") or a numeric (to = "n")
#' @return a vector containing a converted version of var
#'
#' @examples
#' > factorConvert(var = as.factor(letters[1:5]), to = "c")
#' [1] "a" "b" "c" "d" "e"
#' > factorConvert(var = as.factor(1:5), to = "c")
#' [1] "1" "2" "3" "4" "5"
#' > factorConvert(var = as.factor(1:5), to = "n")
#' [1] 1 2 3 4 5
#'
factorConvert <- function(var, to = "numeric") {

  if (class(var) != "factor" ) {
    cat("var is not a factor\n")
  } else {

    if (to %in% c("numeric", "n") ){
      to_return <- as.numeric(levels(var)[as.numeric(var)])
    }

    if (to %in% c("character", "c") ){
      to_return <- levels(var)[as.numeric(var)]
    }

    return(to_return)
  }
}

#' Shows the extent of NAs in a data frame
#'
#' @param the_only_argument_is_a_data_frame a data frame whose missingness is a mystery
#' @return a data frame containing count and proportion missing for each variable in the_only_argument_is_a_data_frame
#'
#' @examples
#' > showNAs(ChickWeight)
#'      var NA_count NA_mean
#' 1 weight        0       0
#' 2   Time        0       0
#' 3  Chick        0       0
#' 4   Diet        0       0
#'
#' > ChickWeightNA <- ChickWeight
#' > ChickWeightNA$weight[1:300] <- NA
#' > ChickWeightNA$Time <- NA
#' > showNAs(ChickWeightNA)
#'      var NA_count   NA_mean
#' 1 weight      300 0.5190311
#' 2   Time      578 1.0000000
#' 3  Chick        0 0.0000000
#' 4   Diet        0 0.0000000
#'
showNAs <- function(the_only_argument_is_a_data_frame) {
  df_names <- names(the_only_argument_is_a_data_frame)
  to_return <- ldply(df_names, function(var_name, temp_df) {
    to_return <- temp_df[[var_name]]
    to_return[to_return == "MISSING"] <- NA
    to_return[to_return == ""] <- NA
    return( data.frame(var = var_name,
                       NA_count = sum(is.na(to_return)),
                       NA_proportion = mean(is.na(to_return)),
                       stringsAsFactors = F))
  }, the_only_argument_is_a_data_frame)
  return(to_return)
}


#' Transform survfit object to data.frame
#'
#' @description Transforms a survfit object into a data frame
#' suitable for generating Kaplan-Meier plots
#'
#' @param .survfit is a survfit object
#' @return a data frame containing data from .survfit in a more-easily-plotable format
#'
#' @examples
#' > survival_fit <- survfit(Surv(time, status) ~ x, data = aml)
#' > plot_data <- getKMData(survival_fit)
#' > ggplot(data = plot_data, aes(x = time, y = surv, color = strata)) +
#' + geom_line()
getKMData <- function(.survfit) {

  ldply(names(.survfit$strata), function(..strata, ..survfit) {
    if (which(names(..survfit$strata) == ..strata) == 1) {
      .indices <- 1:cumsum(..survfit$strata)[1]
    } else {
      .indices <- (cumsum(..survfit$strata)[which(names(..survfit$strata) == ..strata) - 1] + 1):cumsum(..survfit$strata)[which(names(..survfit$strata) == ..strata)]
    }
    .tor <- data.frame(strata = ..strata,
                       time = ..survfit$time[.indices],
                       n_risk = ..survfit$n.risk[.indices],
                       n_event  = ..survfit$n.event[.indices],
                       n_censor = ..survfit$n.censor[.indices],
                       surv = ..survfit$surv[.indices],
                       std_err = ..survfit$std.err[.indices],
                       upper = ..survfit$upper[.indices],
                       lower = ..survfit$lower[.indices],
                       conf_type = ..survfit$conf.type,
                       conf_int = ..survfit$conf.int)
    .tor

  }, .survfit)
}

#' Remove columns from a data frame
#'
#' @description Use regexp matching to remove columns from a data frame
#' @param .df is a data frame
#' @param .match_string is a string for grepl matching
#' @return .df with columns matched by .match_string removed
#'
#' @examples
#' > test <- data.frame(x = c(1,2,3),
#'                      y = c(4,5,6),
#'                      xa = c(7,8,9),
#'                      apple = c(5,5,5))
#' > removeCols(test, "x")
#'   y apple
#' 1 4     5
#' 2 5     5
#' 3 6     5
#' > removeCols(test, "x|a")
#'   y
#' 1 4
#' 2 5
#' 3 6
#'
removeCols <- function(.df, .match_string) {
  return(.df[ ,!grepl(.match_string, names(.df)), drop = FALSE])
}


#' A theme for facet plots
#' #' @description Use regexp matching to remove columns from a data frame
#' @return a theme object

theme_facet <- function() {
  return(theme_bw() + theme(strip.background = element_rect(color="black", fill = "white"),
                            panel.margin = unit(0.05,"null") ))
}

#' A theme for cleaner looking heat maps
theme_heatmap <- function() {
  return(theme_bw() + theme(panel.background = element_blank(),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.border = element_blank(),
                            axis.ticks = element_blank(),
                            panel.margin = rep(unit(0,"null"),4))
  )
}

#' A blank theme, copied mostly or completely from a blog post
#' that I can't find anymore
blankground <- function() {
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),

        panel.margin = unit(0,"null"),
        plot.margin = rep(unit(0,"null"),4),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank()

  )
}
