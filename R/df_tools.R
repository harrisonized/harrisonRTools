import::here(tidyr, 'pivot_wider')
# import::here(reshape2, 'melt')
# import::here(file.path(wd, 'R', 'tools', 'list_tools.R'),
#     'items_in_a_not_b', 'replace_specific_items', .character_only=TRUE)

## Functions
## append_dataframe
## coalesce1
## coalesce_colnames
## dataframe_row_from_named_list
## df_to_plate
## fillna
## filter_dataframe_column_by_list
## get_unique_values
## multi_melt
## most_frequent_item
## pivot
## rename_columns
## reset_index
## rev_df
## smelt
## stranspose


#' Appends df1 with df2
#' 
#' @description
#' Use this to avoids errors like this one:
#' 
#' Can't combine `..1$1` <character> and `..2$1` <double>.
#' 
#' If you combine dataframes with different types, the data gets coerced to `character`.
#'
#' @export
append_dataframe <- function(df1, df2, infront=FALSE, reset_index=TRUE) {

    missing_cols <- items_in_a_not_b(colnames(df1), colnames(df2))
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


#' Coalesce
#' 
#' @references
#' \href{https://stackoverflow.com/questions/19253820/how-to-implement-coalesce-efficiently-in-r}{StackOverflow post}
#' 
#' 
coalesce1 <- function(...) {
    ans <- ..1
    for (elt in list(...)[-1]) {
        i <- is.na(ans)
        ans[i] <- elt[i]
    }
    ans
}


#' Coalesce Column Names
#' 
#' @description
#' Given a dataframe with columns with one-hot encodings,
#' aggregates the column names into a comma-separated list
#' 
#' @export
coalesce_colnames <- function(df, cols, sep=', ') {

    output <- ''
    for (col in cols) {
        output <- gsub('1', col, paste(output, gsub('0', '', df[[col]]), sep=sep))
        output <- gsub(' ,', '', output)
    }
    output <- sub('^, ', '', output)
    output <- sub(', $', '', output)
    return(output)
}


#' Convert a named list into a row in a dataframe
#'
#' @description
#' Use this to create a custom dataframe with a single row.
#' Helpful for appending a metadata row to an existing dataframe.
#'
#' @examples
#' dataframe_row_from_named_list(c('col1'=1, 'col2'=2, 'col3'=3))
#' 
#' @export
dataframe_row_from_named_list <- function(items) {
    df <- data.frame('1'=unname(items))
    df <- as.data.frame(t(df))  # transpose
    rownames(df) <- 1
    colnames(df) <- names(items)
    return(df)
}


#' Dataframe to Plate
#' 
#' @description
#' Reshape a column dataframe to a plate format
#' 
#' @export
df_to_plate <- function(
    df, well_id='well_position', value='ct',
    num_wells=0
) {

    tmp <- df[, c(well_id, value)] %>%
        group_by(!!sym(well_id)) %>%
        summarize(!!(paste0('mean_', value)) := mean(!!sym(value), na.rm=TRUE))
    tmp[['row']] = lapply(tmp[well_id], function(x) gsub("[^a-zA-Z]", "", x))[[1]]
    tmp[['col']] = as.numeric(lapply(tmp[well_id], function(x) gsub("[^0-9]", "", x))[[1]])
    tmp <- tmp[order(tmp$row, tmp$col),]

    plate <- as.data.frame(pivot_wider(
        tmp,
        id_cols='row',
        names_from='col',
        values_from=paste0('mean_', value)
    ))

    # enforce grid
    if (num_wells==96) {

        # missing cols
        for (col in items_in_a_not_b(1:12, colnames(plate))) {
            plate[as.character(col)] <- NA
        }

        # misisng rows
        missing_rownames <- items_in_a_not_b(LETTERS[1:8], plate[['row']])
        if (length(missing_rownames) >= 1) {
            missing_rows <- merge(
                data.frame("row"=missing_rownames),
                t(data.frame(rep(NA, 12), row.names=1:12))
            )
            plate <- rbind(plate, missing_rows)
        }
    }

    plate <- plate[order(rownames(plate)), mixedsort((colnames(plate)))]
    plate <- set_index(plate, colname='row')

    return(plate)
}


#' Fill specific column with NA
#' 
#' @description Mirrors Pandas' \href{https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.fillna.html}{fillna}
#' 
#' @param df a dataframe
#' @param cols a list of columns to replace
#' @param val the value to fill with
#' @param inplace TRUE allows you to avoid re-assigning the variable
#' @return Returns a dataframe.
#' 
#' @examples
#' mtcars['new_col'] <- NA
#' head(fillna(mtcars, c('new_col'), 1))
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


#' Filter dataframe column by list
#' 
#' @description
#' Use this to search rows for a list of values.
#' 
#' @param df a dataframe
#' @param colname select a column
#' @param items used to filter
#' @return Returns a dataframe with rows in which items match the column
#' 
#' @examples
#' filter_dataframe_column_by_list(mtcars, 'cyl',  c(4, 6))
#' 
#' @export
filter_dataframe_column_by_list <- function(
    df,
    colname,
    items
) {
    return(df[which(df[, colname] %in% items), ])
}


#' Get unique values from multiple dataframe columns
#' 
#' @param df a dataframe
#' @param cols columns to search
#' @return Returns a list of unique items
#' 
#' @examples
#' df <- data.frame(
#'     mouse = c('101', '102'),
#'     father = c("1", "1"),
#'     mother = c("2", "2")
#' )
#' get_unique_values(df, c('father', 'mother'))
#' 
#' @export
get_unique_values <- function(df, cols) {
    items <- c()
    for (col in cols) {
        items <- append(items, unique(df[[col]]))
    }
    return(sort(unique(items)))
}


#' Multiple melt
#' 
#' @description Unpivot multiple columns simulataneously
#' 
#' @export
multi_melt <- function(
    df,
    id_vars=c('id', 'name'),
    values=list(
        'metric_1'=c('metric_1_x', 'metric_1_y'),
        'metric_2'=c('metric_2_x', 'metric_2_y')
    ),
    var_name='variable'
) {

    dfs = new.env()
    for (value_name in names(values)) {
        value_vars <- values[[value_name]]
        tmp = reshape2::melt(
            df[, c(id_vars, value_vars)],
            row=id_vars,
            measure.vars=value_vars,
            value.name=value_name,
            variable.name=var_name
        )

        tmp[var_name] = sub(paste0(value_name, '_'), '', tmp[[var_name]])
        
        dfs[[value_name]] <- tmp
    }
    dfs <- as.list(dfs)

    merged <- Reduce(
        function(...) merge(..., by=(c(id_vars, var_name))),
        dfs
    )

    return(merged)
}


#' Get the most frequently occurring item in a dataframe column
#'
#' @export
most_frequent_item <- function(df, colname) {
    return(df[which.max(factor(df[, colname])), colname])
}


#' Pivot
#' 
#' @description Thin wrapper around [tidyr::pivot_wider()]. Return a native dataframe instead of a tibble
#' 
#' @examples
#' pivot(reset_index(mtcars), columns='index', values='mpg')
#' 
#' @seealso
#' [tidyr::pivot_wider()]
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


#' Rename dataframe columns
#' 
#' @description This is a thin wrapper around replace_specific_items that acts on dataframe columns
#' 
#' @param df a dataframe
#' @param columns a named list of replacements. uses names to match and values to replace
#' @param inplace TRUE allows you to avoid re-assigning the variable
#' @return Returns a dataframe.
#' 
#' @examples
#' head(rename_columns(mtcars, c('mpg'="MPG", 'disp'="DISP")))
#' 
#' @export
rename_columns <- function(df, columns, inplace=FALSE) {
    df_name <- deparse(substitute(df))
    colnames(df) <- replace_specific_items(colnames(df), columns)
    if (inplace) {
        assign(df_name, df, envir=.GlobalEnv)
    } else {
        return(df)
    }
}


#' Reset index
#' 
#' @description
#' Moves the values in the index to a column. Resets the index to the default integer index.
#' Mirrors Pandas' \href{https://pandas.pydata.org/docs/reference/api/pandas.DataFrame.reset_index.html}{reset_index}.
#' 
#' @param df a dataframe
#' @param index_name select a new name for the index column
#' @param drop if TRUE, does not copy the index values to the new column
#' @return Returns a dataframe with index renamed and new integer index 
#' 
#' @examples
#' reset_index(mtcars, index_name='model')
#' 
#' @references
#' \href{https://stackoverflow.com/questions/36396911/r-move-index-column-to-first-column}{StackOverflow post}
#' 
#' @export
reset_index <- function(df, index_name='index', drop=FALSE) {
    if (!drop) {
        df <- cbind(index = rownames(df), df)
        colnames(df)[colnames(df) == "index"] = index_name
    }
    rownames(df) <- 1:nrow(df)
    return (df)
}


#' Reverse the order of a dataframe
#' 
#' @param df a dataframe
#' @param how 'row' or 'col'
#' @return Returns the reversed dataframe.
#' 
#' @examples
#' rev_df(mtcars, how='row')
#' rev_df(mtcars, how='col')
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
#' @description
#' Convert a dataframe from a table to long format
#' This is an alternative to melt that doesn't throw errors
#' 
#' @examples
#' smelt(mtcars[, c('cyl', 'mpg')])
#' 
#' @references
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


#' Special transpose
#' 
#' @description Allows you to choose which column becomes the new column name
#'
#' @param df dataframe
#' @param colname name of the column that will become the new column name
#' @return Returns the transposed dataframe
#' 
#' @export
stranspose <- function(df, colname=NULL) {
    tdf <- as.data.frame(t(df))
    if (!is.null(colname)) {
        colnames(tdf) <- tdf[colname,]
        tdf <- tdf[items_in_a_not_b(rownames(tdf), colname), ]
    }
    return(tdf)
}
