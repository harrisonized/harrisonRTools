#' readText
#' @description
#' Convenience function for reading text files as string
#' @export readText
readText <- function(
    filePath,
    encoding='UTF-8',
    sep='\n'
) {

    con = file(filePath, encoding=encoding)
    lines <- readLines(con)
    close(con)

    rawString <- paste(lines, collapse = sep)

    return(rawString)
}