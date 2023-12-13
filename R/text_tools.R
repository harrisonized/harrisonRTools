## Functions
## dotsep_to_snake_case
## title_to_snake_case
## txt_strip
## substr_right


#' Converts "Column.Title" to column_title
#'
#' @export
dotsep_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[.]')), collapse='_')
        )
    )
}


#' Converts "Column Title" to column_title
#'
#' @export
title_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[ ]')), collapse='_')
        )
    )
}


#' Removes special characters from beginning and end of a string
#'
#' @export
txt_strip <- function(x, chars=' ') {
    chars <- unique(strsplit(chars, '')[[1]])

    for (char in chars) {

        # special characters
        if (char %in% c('(', ')')) {
            char <- paste0('\\', char)
        }

        x <- gsub(paste0('.*^', char, '+'), '', x)
        x <- gsub(paste0('+', char, '$'), '', x)
    }
    return(x)
}


#' see: https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#'
#' @export
substr_right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

