## import::here(stringr, 'str_to_title')

## Functions
## camel_to_snake_case
## dotsep_to_snake_case
## title_to_snake_case
## snake_to_title_case
## substr_right
## txt_strip


#' Standardize CamelCase Titles
#' 
#' @description Converts "ColumnTitle" to column_title
#' 
#' @examples
#' camel_to_snake_case('ColumnTitle')
#' 
#' @export
camel_to_snake_case <- function(text) {
    return(tolower(gsub("([a-z])([A-Z])", "\\1_\\L\\2", text, perl = TRUE)))
}


#' Standardize Dot-separated Titles
#' 
#' @description Converts "Column.Title" to column_title
#' 
#' @examples
#' dotsep_to_snake_case('Column.Title')
#' 
#' @export
dotsep_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[.]')), collapse='_')
        )
    )
}


#' Standardize Space-separated Titles
#' 
#' @description Converts "Column Title" to column_title
#' 
#' @examples
#' title_to_snake_case('Column Title')
#' 
#' @export
title_to_snake_case <- function(text) {
    return(tolower(
        paste(
            unlist(strsplit(text, '[ ]')), collapse='_')
        )
    )
}


#' Standardize snake_case Titles
#' 
#' @description Converts column_title to "Column Title"
#' 
#' @examples
#' snake_to_title_case('column_title')
#' 
#' @export
snake_to_title_case <- function(text) {
    return(stringr::str_to_title(gsub("_", " ", text)))
}


#' Extract last n characters
#' 
#' @description Extracts the last n characters from a string
#' 
#' @examples
#' substr_right('hi there', 5)
#' 
#' @references
#' \href{https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r}{StackOverflow post}
#'
#' @export
substr_right <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


#' Remove leading and traling characters
#' 
#' @description Removes special characters from beginning and end of a string
#' 
#' @examples
#' txt_strip(" hi there ", chars=" ")
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
