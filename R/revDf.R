#' revDf
#' @description reverse a df rowwise
#' @export revDf
revDf <- function(df) {
    return(df[dim(df)[1]:1,])
}