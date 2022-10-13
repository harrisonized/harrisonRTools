#' Convenience function for reading text files where csv may be embedded
#' Use the following settings for 96-well plates:
#' df <- read_csv_from_txt(
#'            filePath,
#'            skiprows=3, nrows=8,
#'            skipcols=2, ncols=12,
#'            index=LETTERS[1:8],
#'            columns=seq(1, 12)
#' )
readCsvFromText <- function(
    filePath,
    encoding='UTF-16', sep='\t',
    skiprows=0, nrows=NULL,
    skipcols=0, ncols=NULL,
    index=NULL,
    columns=NULL
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
          
    return(df)
}