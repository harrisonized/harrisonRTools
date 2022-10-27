## Functions
## read_text
## read_csv_from_text


#' Read files ending in .txt
#'
#' Concatenates all the lines into a single string
#'
#' @export
read_text <- function(
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

#' Reads and parses csv embedded in .txt files
#'
#' This is useful for data exported from plate readers
#' 
#' @examples
#' # for 96-well plates:
#' df <- read_csv_from_text(
#'   file_path,
#'   skiprows=3, nrows=8,
#'   skipcols=2, ncols=12,
#'   index=LETTERS[1:8],
#'   columns=seq(1, 12)
#' )
#' @export
read_csv_from_text <- function(
    file_path,
    encoding='UTF-16', sep='\t',
    skiprows=0, nrows=NULL,
    skipcols=0, ncols=NULL,
    index=NULL,
    columns=NULL,
    numeric=FALSE
) {

    con = file(filePath, encoding=encoding)
    rawData <- readLines(con)
    close(con)
    
    # autodetermine ranges if not specified
    if(is.null(nrows)) {
        nrows <- length(rawData)
    }
    if(is.null(ncols)) {
        rowArr = unlist(strsplit(rawData[1+skiprows], split='\t'))
        ncols = length(rowArr)-skipcols
    }
    
    # instantiate empty dataframe and append row-by-row
    df <- data.frame(matrix(ncol=ncols, nrow=0))
    for (row in rawData[(1+skiprows):(nrows+skiprows)]) {
        rowArr <- unlist(strsplit(row, split='\t'))
        df[nrow(df) + 1,] = rowArr[(1+skipcols):(ncols+skipcols)]
    }
    
    # rename columns
    colnames(df) <- columns
    rownames(df) <- index

    if(numeric) {
        df[] <- lapply(df, function(x) as.numeric(as.character(x)))
    }

    return(df)
}