# library('Matrix')
wd = dirname(dirname(this.path::here()))
source(file.path(wd, 'R', 'functions', 'list_tools.R')) 

## Functions
## list_files
## join_many_csv
## append_many_csv
## read_text
## read_csv_from_text
## read_10x


#' list all files in all subdirectories with a given extension
#' 
#' @export
list_files <- function(dir_path, ext=NULL, recursive = TRUE) {
    all_files = list.files(dir_path, recursive = recursive, full.name=TRUE)

    if (!is.null(ext)) {
        # See: https://stackoverflow.com/questions/7187442/filter-a-vector-of-strings-based-on-string-matching
        return (all_files[tools::file_ext(all_files)==ext])
    } else {
        return (all_files)
    }
}


#' Read all the csv files from a directory and left join them into a single dataframe
#' See: https://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-all_reads-frames
#' index_cols=c('gene_id', 'gene_name', 'chromosome')
#' index_cols=c('count')
#' 
#' @export
join_many_csv <- function(dir_path, index_cols, value_cols, ext='csv', recursive=TRUE, sep=',') {
    filepaths <- list_files(dir_path, ext=ext, recursive=recursive)
    if (length(filepaths)==0) {
        stop(paste("no files found in: ", dir_path))
    }
    filenames = c(tools::file_path_sans_ext(basename(filepaths)))
    
    # read dfs and left join on index_cols
    df_list <- lapply(filepaths, read.csv, sep=sep)

    # Warning: column names ‘count.x’, ‘count.y’ are duplicated in the result
    # See: https://stackoverflow.com/questions/38603668/suppress-any-emission-of-a-particular-warning-message
    withCallingHandlers({
        all_reads <- Reduce(
            function(...) merge(..., by=index_cols),
            lapply(df_list, "[", c(index_cols, value_cols))
        )
    }, warning = function(w) {
        # print(conditionMessage(w))
        if (startsWith(conditionMessage(w), "column names")) {
            invokeRestart( "muffleWarning" )
        }
    })
    
    # rename columns
    colnames(all_reads) = c(
        index_cols,  # index_cols
        as.list(outer(value_cols, filenames, paste, sep='-'))  # suffix value_cols with filename
    )
    return(all_reads)
}


#' Read all the csv files from a directory and append them into a single dataframe
#' This is missing the filename column for now
#' 
#' @export
append_many_csv <- function(dir_path, sep='\t', row_names=1) {
    filenames <- list.files(dir_path, full.names=TRUE)
    csv <- lapply(filenames, read.csv, sep=sep, row.names=row_names)
    data <- do.call(rbind, csv)
    return(data)
}


#' Read files ending in .txt
#'
#' Concatenates all the lines into a single string
#'
#' @export
read_text <- function(
    file_path,
    encoding='UTF-8',
    sep='\n'
) {

    con = file(file_path, encoding=encoding)
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

    con = file(file_path, encoding=encoding)
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


#' Alternative to Seurat::Read10x that enables you to specify the filenames
#'
#' @export
read_10x <- function(
    data_dir,
    matrix_file='matrix.mtx',
    genes_file='genes.tsv',
    barcodes_file='barcodes.tsv'
) {

    if (!file.exists(file.path(data_dir, matrix_file)) |
        !file.exists(file.path(data_dir, genes_file)) |
        !file.exists(file.path(data_dir, barcodes_file))) {

        filenames = basename(list_files(data_dir))
        matrix_file = filter_list_for_match(filenames, 'matrix')
        genes_file = filter_list_for_match(filenames, 'genes')
        barcodes_file = filter_list_for_match(filenames, 'barcodes')
    }

    expr_mtx <- Matrix::readMM(file.path(data_dir, matrix_file))
    genes <- read_tsv(file.path(data_dir, genes_file), col_names=FALSE, show_col_types = FALSE)
    barcodes <- read_tsv(file.path(data_dir, barcodes_file), col_names=FALSE, show_col_types = FALSE)

    colnames(expr_mtx) <- barcodes[['X1']]  # barcode sequence
    rownames(expr_mtx) <- genes[['X2']]  # gene names

    # Return dgCMatrix instead of dgTMatrix
    # See: https://slowkow.com/notes/sparse-matrix/#the-triplet-format-in-dgtmatrix
    expr_mtx <- as(expr_mtx, "CsparseMatrix")

    return(expr_mtx)
}
