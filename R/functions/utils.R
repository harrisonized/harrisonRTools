## Common use functions


# ----------------------------------------------------------------------
# List tools


#' https://stackoverflow.com/questions/10298662/find-elements-not-in-smaller-character-vector-list-but-in-big-list
#' 
#' @export
items_in_a_not_b <- function(a, b) {
    return((new <- a[which(!a %in% b)]))
}


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


#' return elements of a list matching a particular substring
#'
#' @examples
#' filter_list_for_match(c("gene_id_pat", "gene_id_mat", "count"), "pat")
#' 
#' @export
filter_list_for_match <- function(items, pattern) {
    # filter
    for (i in 1:length(pattern)){
        items <- lapply(items, grep, pattern=pattern[[i]], value=TRUE)
    }
    return (unlist(items[!sapply(items, identical, character(0))]))  # remove character(0)
}


# ----------------------------------------------------------------------
# Dataframe manipulation


#' See: https://stackoverflow.com/questions/36396911/r-move-index-column-to-first-column
#' 
#' @export
reset_index <- function(df, index_name='index') {
    df <- cbind(index = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    colnames(df)[colnames(df) == "index"] = index_name
    return (df)
}


#' @export
filter_dataframe_column_by_list <- function(dataframe, colname, items, index_name='index', return_index=FALSE) {
    
    data <- reset_index(dataframe, index_name=index_name)
    rownames(data) <- data[, colname]
    data <- (data[intersect(data[, colname], items),])  # filter genes shared by both gtf files
    rownames(data) <- data[, index_name]  # optional preserve index for troubleshooting
    
    if (return_index==TRUE) {
        return (data)
    } else {
        return (data[, items_in_a_not_b(colnames(data), 'index')])
    }
}


#' tidyr returns a tibble object instead of a dataframe
#' This function returns a dataframe
#' 
#' @export
pivot <- function(df, columns, values) {

    # Warning: Using an external vector in selections was deprecated in tidyselect 1.1.0.
    # See: http://romainfrancois.blog.free.fr/index.php?post/2009/05/20/Disable-specific-warnings
    withCallingHandlers({
        tibble_obj = tidyr::pivot_wider(
            df,
            names_from = columns,
            values_from = values,
            values_fn = list,
            names_glue = if (length(values)==1){"{.value}_{.name}"}
        )
    }, warning = function(w) {
        if ( any(grepl("Using an external vector", w)) ) {
            invokeRestart("muffleWarning")
        }
    })
    
    dataframe = data.frame(lapply(tibble_obj, unlist))
    
    return (dataframe)
}


# ----------------------------------------------------------------------
# File IO


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
