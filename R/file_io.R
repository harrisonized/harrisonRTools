import::here(readxl, 'read_excel')
import::here(Matrix, 'readMM')
import::here(readr, 'read_tsv')
import::here(ggplot2, 'ggsave', 'last_plot')
import::here(grid, 'grid.newpage', 'grid.draw')
# import::here(Seurat, 'Read10X')
# import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
#     'filter_list_for_match', .character_only=TRUE)

## Functions
## list_files
## append_many_csv
## join_many_csv
## load_rdata
## read_csv_or_tsv
## read_excel_or_csv
## read_text
## read_tsv_from_text
## read_10x
## savefig


#' List all files with a specific extension
#' 
#' @description
#' This is a thin wrapper around [list.files()].
#' 
#' @references
#' \href{https://stackoverflow.com/questions/7187442/filter-a-vector-of-strings-based-on-string-matching}{StackOverflow post}
#' 
#' @export
list_files <- function(dir_path, ext=NULL, recursive = TRUE) {
    all_files = list.files(dir_path, recursive = recursive, full.name=TRUE)

    if (!is.null(ext)) {
        # See: 
        return (all_files[tools::file_ext(all_files)==ext])
    } else {
        return (all_files)
    }
}


#' Aggregate csv files by appending them rowwise
#' 
#' @description
#' Read all the csv files from a directory and append them into a single dataframe
#' 
#' @export
append_many_csv <- function(dir_path, sep=',', row_names=NULL) {
    filenames <- list.files(dir_path, full.names=TRUE)
    csv <- lapply(filenames, read.csv, sep=sep, row.names=row_names)
    data <- do.call(rbind, csv)
    return(data)
}


#' Aggregate csv files by joining them column-wise
#' 
#' @description
#' Read all the csv files from a directory and left join them into a single dataframe
#' 
#' @references
#' \href{https://stackoverflow.com/questions/5319839/read-multiple-csv-files-into-separate-all_reads-frames}{StackOverflow post}
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
        merged <- Reduce(
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
    colnames(merged) = c(
        index_cols,  # index_cols
        as.list(outer(value_cols, filenames, paste, sep='-'))  # suffix value_cols with filename
    )
    return(merged)
}


#' Loads an RData file and allows you to store it in a chosen variable
#' 
#' @description
#' Without this function, base R uses the filename as the variable name
#' 
#' @references
#' \href{https://stackoverflow.com/questions/5577221/can-i-load-a-saved-r-object-into-a-new-object-name}{StackOverflow post}
#' 
#' @export
load_rdata <- function(filepath){
    load(filepath)
    return( get(ls()[ls() != "filepath"]) )
}


#' Switch case to read csv or tsv based on the extension
#'
#' @description Mainly used to simplify scripts
#' 
#' @export
read_csv_or_tsv <- function(file, header=TRUE, sep=',', check_names=FALSE) {
    ext = tools::file_ext(file)
    if (ext == 'tsv') {
        sep='\t'
    }

    data = read.csv(file, header=header, sep=sep, check.names=check_names)
    return(data)
}


#' Switch case to read excel or csv based on the extension
#'
#' @description Mainly used to simplify scripts
#' 
#' @export
read_excel_or_csv <- function(filepath) {
    ext=tools::file_ext(filepath)
    if (ext == 'xlsx') {
        df <- read_excel(filepath, .name_repair = "unique_quiet")
    } else if (ext == 'csv') {
        df <- read.csv(filepath, header=TRUE, check.names=FALSE)
    } else {
        log_print(paste(Sys.time(), 'Please enter a xlsx or csv file.'))
        stop()
    }
    return(df)
}


#' Read files ending in .txt
#'
#' @description Concatenates all the lines into a single string, separated by newline
#'
#' @export
read_text <- function(
    file_path,
    encoding='UTF-8',
    sep='\n'
) {

    con = file(file_path, encoding=encoding)
    withCallingHandlers({
        rawData <- readLines(con)
    }, warning = function(w) {
        # print(conditionMessage(w))
        if (startsWith(conditionMessage(w), "incomplete final line")) {
            invokeRestart( "muffleWarning" )
        }
    })
    close(con)

    rawString <- paste(lines, collapse = sep)

    return(rawString)
}


#' Parse tab-delimited text line-by-line
#'
#' @description
#' This is useful for parsing data exported from plate readers, as it allows you to retrieve a table internal to a spreadsheet.
#' Note that there is a Mac-specific bug that prevents readLines from reading files encoded in UTF-16.
#' 
#' @usage
#' # for 96-well plates:
#' df <- read_csv_from_text(
#'   file_path,
#'   skiprows=3, nrows=8,
#'   skipcols=2, ncols=12,
#'   index=LETTERS[1:8],
#'   columns=seq(1, 12),
#'   numeric=TRUE
#' )
#' 
#' @export
read_tsv_from_text <- function(
    file_path,
    encoding='UTF-8', sep='\t',
    skiprows=0, nrows=NULL,
    skipcols=0, ncols=NULL,
    index=NULL,
    columns=NULL,
    numeric=FALSE
) {

    con = file(file_path, encoding=encoding)
    withCallingHandlers({
        rawData <- readLines(con)
    }, warning = function(w) {
        # print(conditionMessage(w))
        if (startsWith(conditionMessage(w), "incomplete final line")) {
            invokeRestart( "muffleWarning" )
        }
    })
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


#' Read 10X
#'
#' @description Alternative to [Seurat::Read10X()] that enables you to specify the filenames
#' 
#' @references
#'\href{https://github.com/satijalab/seurat/issues/4096}{Github issue}
#'
#' @seealso [Seurat::Read10X()]
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

    expr_mtx <- readMM(file.path(data_dir, matrix_file))
    genes <- read_tsv(file.path(data_dir, genes_file), col_names=FALSE, show_col_types = FALSE)
    barcodes <- read_tsv(file.path(data_dir, barcodes_file), col_names=FALSE, show_col_types = FALSE)

    colnames(expr_mtx) <- barcodes[['X1']]  # barcode sequence
    rownames(expr_mtx) <- genes[['X2']]  # gene names

    # Return dgCMatrix instead of dgTMatrix
    # See: https://slowkow.com/notes/sparse-matrix/#the-triplet-format-in-dgtmatrix
    expr_mtx <- as(expr_mtx, "CsparseMatrix")

    # Note: the above is equivalent to this, but is more explicit
    # expr_mtx <- Matrix::ReadMtx(
    #     mtx=file.path(data_dir, matrix_file),
    #     cells=file.path(data_dir, barcodes_file),
    #     features=file.path(data_dir, genes_file),
    #     feature.column=2
    # )

    return(expr_mtx)
}


#' Save Figure
#' 
#' @description Switch case to reduce the number of lines in the main script
#' 
#' @export
savefig <- function(
    filepath,
    fig=NULL,
    height=800, width=1200, dpi=300, units="px", scaling=0.5,
    makedir=FALSE,
    troubleshooting=FALSE,
    lib='ggplot',  # choose: ggplot, grid
    default_ext = '.png'
) {
    if (!troubleshooting) {

        # make directory
        dirpath <- dirname(filepath)
        if (makedir && !dir.exists(dirpath)) {
            dir.create(dirpath, recursive=TRUE)
        }

        # add file extension if not included
        if (tools::file_ext(filepath)=='') {
            filepath <- paste0(filepath, default_ext)
        }

        if (lib=='ggplot') {
            if (!inherits(fig, "ggplot")) {
                fig <- last_plot()
            }

            withCallingHandlers({
                ggsave(
                    filepath,
                    plot=fig,
                    height=height, width=width, dpi=dpi, units=units, scaling=scaling
                )
            }, warning = function(w) {
                if ( any(grepl("rows containing non-finite values", w),
                         grepl("fewer than two data points", w)) ) {
                    invokeRestart("muffleWarning")
                }
            })

        } else if (lib=='grid') {
            png(filepath,
                height=height, width=width, res=dpi, units=units
            )
            grid.newpage()
            grid.draw(fig$gtable)
            dev.off()
        } else {
            warning(paste0("lib='", lib, "' not found"))
        }
    }
}
