import::here(tidyr, 'pivot_wider')
# import::here(file.path(wd, 'R', 'utils', 'list_tools.R'),
#     'items_in_a_not_b', 'replace_specific_items', .character_only=TRUE)

## Functions
## rename_columns
## rev_df
## smelt
## reset_index
## filter_dataframe_column_by_list
## pivot


#' rename specific dataframe columns
#' 
#' @export
<<<<<<< HEAD:R/utils/df_tools.R
rename_columns <- function(df, columns) {
    colnames(df) <- replace_specific_items(colnames(df), columns)
=======
rename_columns <- function(df, columns, inplace=FALSE) {
    df_name <- deparse(substitute(df))
    colnames(df) <- harrisonRTools::replace_specific_items(colnames(df), columns)
    if (inplace) {
        # see: https://stackoverflow.com/questions/3969852/update-data-frame-via-function-doesnt-work
        assign(df_name, df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' fill a specific column with na
#' 
#' @export
fillna <- function(df, cols, val=0, inplace=FALSE) {
    df_name <- deparse(substitute(df))
    for (col in cols) {
        df[is.na(df[, col]), col] <- val
    }
    if (inplace) {
        assign(df_name, df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' get unique values from each column
#' 
#' @export
get_unique_values <- function(df, cols) {
    items <- c()
    for (col in cols) {
        items <- append(items, unique(df[[col]]))
    }
    return(sort(unique(items)))
>>>>>>> 6e9d5e2 (convert repo to r package, move scripts outside R directory):R/df_tools.R
}


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


<<<<<<< HEAD:R/utils/df_tools.R
=======
#' See: https://stackoverflow.com/questions/36396911/r-move-index-column-to-first-column
#' 
#' @export
reset_index <- function(df, index_name='index', drop=FALSE) {
    df <- cbind(index = rownames(df), df)
    rownames(df) <- 1:nrow(df)
    colnames(df)[colnames(df) == "index"] = index_name
    if (drop == TRUE) {
        df <- df[, harrisonRTools::items_in_a_not_b(colnames(df), 'index')]
    }
    return (df)
}


#' Appends df1 with df2
#' Avoids errors like this one: Can't combine `..1$1` <character> and `..2$1` <double>.
#'
#' @export
append_dataframe <- function(df1, df2, infront=FALSE, reset_index=TRUE) {

    missing_cols <- harrisonRTools::items_in_a_not_b(colnames(df1), colnames(df2))
    for (col in missing_cols) {
        df2[[col]] <- NA
    }
    if (infront) {
        df <- rbind(df2[, intersect(colnames(df1), colnames(df2))], df1)
    } else {
        df <- rbind(df1, df2[, intersect(colnames(df1), colnames(df2))])
    }
    if (reset_index) {
        df <- reset_index(df, drop=TRUE)
    }

    return(df)
}


#' @export
filter_dataframe_column_by_list <- function(
    dataframe,
    colname,
    items,
    index_name='index',
    return_index=FALSE
) {
    
    data <- reset_index(dataframe, index_name=index_name)
    rownames(data) <- data[, colname]
    data <- (data[intersect(data[, colname], items),])  # filter data
    rownames(data) <- data[, index_name]  # optional preserve index for troubleshooting
    
    if (return_index==TRUE) {
        return (data)
    } else {
        return (data[, harrisonRTools::items_in_a_not_b(colnames(data), 'index')])
    }
}


#' convert a named list into a row in a dataframe
#'
#' @export
dataframe_row_from_named_list <- function(items) {
    df <- data.frame('1'=unname(items))
    df <- as.data.frame(t(df))  # transpose
    rownames(df) <- 1
    colnames(df) <- names(items)
    return(df)
}


#' convenience function to also set the column name with the transpose
#'
#' @export
stranspose <- function(df, colname=NULL) {
    tdf <- as.data.frame(t(df))
    if (!is.null(colname)) {
        colnames(tdf) <- tdf[colname,]
        tdf <- tdf[harrisonRTools::items_in_a_not_b(rownames(tdf), colname), ]
    }
    return(tdf)
}


>>>>>>> 6e9d5e2 (convert repo to r package, move scripts outside R directory):R/df_tools.R
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
        tibble_obj = pivot_wider(
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
