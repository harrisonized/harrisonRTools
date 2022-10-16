#' readText
#' @description
#' Convenience function for reading text files as string
#' @export readText
readText <- function(
    filePath,
    encoding='UTF-8'
) {

    con = file(filePath, encoding=encoding)
    rawData <- readLines(con)
    close(con)

    return(rawData)
}