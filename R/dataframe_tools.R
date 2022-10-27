## Functions
## rev_df
## smelt


#' Reverse a dataframe
#' 
#' @export
rev_df <- function(df, how='row') {
    if (how == 'row') {
        return(df[dim(df)[1]:1,])
    } else if (how == 'col') {
        return(rev(df))
    } else {
        stop("Choose how='row' or how='col'")
    }
}


#' Special Melt
#' 
#' Convert a dataframe from a table to long format
#' This is an alternative to melt that doesn't throw errors
#' See: \href{https://stackoverflow.com/questions/28355377/how-to-add-index-of-a-list-item-after-melt-in-r}{Stack Overflow link}
#' 
#' @export
smelt <- function(
   df,
   rowname='row',
   colname='col',
   valname='val'
) {
   melted <- transform(stack(setNames(df, colnames(df))), id=rownames(df))
   colnames(melted) <- c(valname, colname, rowname)
   return(rev(melted))
}