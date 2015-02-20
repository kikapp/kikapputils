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

#' Calculate the inverse logit of a number
#'
#' @param x A number.
#' @return exp(x) / (1 + exp(x))
#' @examples
#' alogit(0.9)
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
  if (length(to_remove) > 1){
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
#' cUnique(x = letters)
#' > 26
#' cUnique(x = factor(letters))
#' > 26
#' cUnique(x = as.numeric(letters), count.na = F)
#' > 0
#' cUnique(x = as.numeric(letters), count.na = T)
#' > 1
#
cUnique <- function(x, count.na = FALSE) {
  members <- unique(x)
  if (NA %in% members & !count.na) {
    return(length(members) - 1)
  } else {
    return(length(members))
  }
}

devtools::document()
# Loads and combines files in a folder that match a specified
# string
# if merge == TRUE, a dataframe of merged files is returned,
#   else a list with an element corresponding to each file is returned
openFilesInDirectory <- function(directory,
                                 match_string,
                                 merge = FALSE,
                                 delim_str =",",
                                 na.strings = ".",
                                 header = T,
                                 fill=T,
                                 skip = 0) {
  require(plyr)
  file_array <-  paste0(directory, "/", list.files(directory)[grep(pattern=match_string, list.files(directory))])

  data_list <- llply(file_array, function(file_path, delim_str) {
    print(file_path)
    to_return <- read.table(file = file_path, header = header, sep = delim_str, stringsAsFactors = FALSE, fill=fill, quote="\"", na.strings = na.strings, skip = skip )
    to_return["loaded_file_name"] <- tail(strsplit(file_path, "/")[[1]],1)
    return(to_return)
  }, delim_str)

  if(merge |  length(file_array) == 1) {
    data_list <- ldply(data_list, identity)
  }

  return(data_list)
}

# Converts factors into numeric or character arrays
factorConvert <- function(var, to_type = "numeric") {

  if (to_type %in% c("numeric", "n") ){
    to_return <- as.numeric(levels(var)[as.numeric(var)])
  }

  if (to_type %in% c("character", "c") ){
    to_return <- levels(var)[as.numeric(var)]
  }
  return(to_return)
}

# Shows the extent of NAs in a data frame
showNAs <- function(temp_df) {
  df_names <- names(temp_df)
  to_return <- ldply(df_names, function(var_name, temp_df) {
    to_return <- temp_df[[var_name]]
    to_return[to_return == "MISSING"] <- NA
    to_return[to_return == ""] <- NA
    return( data.frame(var = var_name,
                       NA_count = sum(is.na(to_return)),
                       NA_mean = mean(is.na(to_return)),
                       stringsAsFactors = F))
  }, temp_df)
  return(to_return)
}

# Shows the unique values for each factor or character
# variable in a data frame temp_df
# nchars specifies how many characters to display for each
#  unique value
showUniqueValues <- function(temp_df, nchars = 5) {
  df_names <- names(temp_df)
  if (nchars == 0) {nchars = 10e6}
  to_return <- ldply(df_names, function(var_name, temp_df) {
    to_return <- temp_df[[var_name]]
    if (class(to_return) == "character" | class(to_return) == "factor") {
      return( data.frame(var = var_name,
                         values = paste0(substr(unique(to_return), 1, nchars), collapse = ", "),
                         kurtosis = NA))
    }
    if (!(class(to_return) == "character" | class(to_return) == "factor")) {
      return( data.frame(var = var_name,
                         values = "NA",
                         kurtosis = kurtosis(to_return, na.rm = TRUE)))
    }
  }, temp_df)
  return(to_return)
}

# Shows the first word of each unique value for character or
# factor in a temp_df
showUniqueValuesFirstWord <- function(temp_df) {
  df_names <- names(temp_df)
  to_return <- ldply(df_names, function(var_name, temp_df) {
    to_return <- temp_df[[var_name]]

    if (class(to_return) == "character" | class(to_return) == "factor") {
      return( data.frame(var = var_name,
                         values = paste0(substr(unique(to_return), 1,
                                                laply(strsplit(unique(to_return), " "), function(entry) {
                                                  return(nchar(entry[1]))
                                                }) ),
                                         collapse = ", "),
                         kurtosis = NA))
    }

    if (!(class(to_return) == "character" | class(to_return) == "factor")) {
      return( data.frame(var = var_name,
                         values = "NA",
                         kurtosis = kurtosis(to_return, na.rm = TRUE)))
    }
  }, temp_df)
  return(to_return)
}

# removes all columns whose names are matched by match_string
# from data frame .df
removeCols <- function(.df, match_string) {
  return(.df[ ,!grepl(match_string, names(.df))])
}

##### KK 2013-04-08: Added manual list of k values at which to calculate gap widths
##### From here: https://svn.r-project.org/R-packages/trunk/cluster/R/clusGap.R

#### Originally from orphaned package SLmisc
#### (Version: 1.4.1, 2007-04-12, Maintainer: Matthias Kohl <kohl@sirs-lab.com>)
#### License: GPL (version 2 or later)
####
#### which said
####  "function corresponds to function gap in package SAGx"

## MM: SAGx is now in Bioconductor --- 1.10.1{devel} or 1.11.1{release}
##     had gap() *corrected* to re-cluster using FUNcluster --> see ./gap-SAGx.R.~orig~
##
## MM: Package 'lga' -- has gap() and lga and robust lga [-> UBC]
##    - it uses  boot() nicely  [2012-01: ORPHANED because  Justin Harrington is amiss]
## MM: renamed arguments, and changed almost everything

# x <- sampled_data[, cause_index]
# FUNcluster <- pam1
# k_arr <- seq(2,10,4)
# B <- 10
clusGapManualK <- function (x, FUNcluster, K.max, k_arr = seq(2,10,1), B = 100, verbose = interactive(), ...)
{
  k_length <- length(k_arr)
  K.max <- max(k_arr)
  stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 2,
            (n <- nrow(x)) >= 1, (p <- ncol(x)) >= 1)
  if(B != (B. <- as.integer(B)) || (B <- B.) <= 0)
    stop("'B' has to be a positive integer")

  if(is.data.frame(x))
    x <- as.matrix(x)
  ii <- seq_len(n)
  W.k <- function(X, kk) {
    #     X <- x
    #     kk <- k_arr[k]
    clus <- if(kk > 1) FUNcluster(X, kk)$cluster else rep.int(1L, nrow(X))
    ##                 ---------- =  =       -------- kmeans() has 'cluster'; pam() 'clustering'
    0.5* sum(vapply(split(ii, clus),
                    function(I) { xs <- X[I,, drop=FALSE]
                    sum(dist(xs)/nrow(xs)) }, 0.))
  }
  logW <- E.logW <- SE.sim <- numeric(k_length)
  if(verbose) cat("Clustering k = ", k_arr[1], ", ", k_arr[2], ",..., K.max (= ",K.max,"): .. ", sep='')
  for(k in 1:k_length)
    logW[k] <- log(W.k(x, k_arr[k]))
  if(verbose) cat("done\n")

  ## Scale 'x' into "hypercube" -- we later fill with H0-generated data
  xs <- scale(x, center=TRUE, scale=FALSE)
  m.x <- rep(attr(xs,"scaled:center"), each = n)# for back transforming
  V.sx <- svd(xs)$v
  rng.x1 <- apply(xs %*% V.sx, # = transformed(x)
                  2, range)

  logWks <- matrix(0., B, k_length)
  if(verbose) cat("Bootstrapping, b = 1,2,..., B (= ", B,
                  ")  [one \".\" per sample]:\n", sep="")
  for (b in 1:B) {
    #     b <- 2
    ## Generate "H0"-data as "parametric bootstrap sample" :
    z1 <- apply(rng.x1, 2,
                function(M, nn) runif(nn, min=M[1], max=M[2]),
                nn=n)
    z <- tcrossprod(z1, V.sx) + m.x # back transformed
    for(k in 1:k_length) {
      logWks[b,k] <- log(W.k(z,k_arr[k]))
    }
    if(verbose) cat(".", if(b %% 50 == 0) paste(b,"\n"))
  }
  if(verbose && (B %% 50 != 0)) cat("",B,"\n")
  E.logW <- colMeans(logWks)
  SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
  structure(class = "clusGap",
            list(Tab = cbind(k = matrix(k_arr, ncol = 1), logW, E.logW, gap = E.logW - logW, SE.sim),
                 ## K.max == nrow(T)
                 n = n, B = B, FUNcluster=FUNcluster))
}



miscRLoaded <- TRUE




