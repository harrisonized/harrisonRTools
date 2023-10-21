## Functions
## dotsep_to_snake_case
## title_to_snake_case
## txt_strip


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
