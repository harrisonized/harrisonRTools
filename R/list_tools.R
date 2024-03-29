import::here(stringi, 'stri_replace_all_regex')

## Functions
## check_if_a_in_b
## chunker
## collate
## dict_zip
## filter_list_for_match
## find_first_match_index
## items_in_a_not_b
## multiple_replacement
## n_items_from_list
## replace_specific_items


#' Check if item a is in vector b
#' 
#' @description
#' Returns TRUE If item a is found in vector b and FALSE otherwise.
#' 
#' @param a any value
#' @param b list or vector
#' @return Returns TRUE or FALSE
#' 
#' @examples
#' check_if_a_in_b(1, c(1, 2, 3))
#' check_if_a_in_b(0, c(1, 2, 3))
#'
#' @references
#' \href{https://stackoverflow.com/questions/53086053/how-to-check-if-a-list-contains-a-certain-element-in-r}{Stack Overflow}
#' 
#' @export
check_if_a_in_b <- function(a, b) {
    return (as.logical(
        sum(unlist( lapply(b, function(x) ifelse(x==a,1,0)) ))
    ))
}


#' Chunker
#' 
#' @description Group array elements into chunks for iteration
#' @examples
#' chunker(c(1, 2, 3, 4, 5, 6), 4)
#' list(c(1, 2, 3, 4), c(5, 6))
#' 
#' @export
chunker <- function(array_obj, num_elem=2) {

    num_groups = ceiling(length(array_obj)/num_elem)
    chunked <- vector("list", length = num_groups)
    for (group_idx in 1:num_groups) {
        start <- 1+(group_idx-1)*num_elem
        stop <- ifelse(group_idx*num_elem > length(array_obj), length(array_obj), group_idx*num_elem)
        chunked[[group_idx]] <- array_obj[start:stop]
    }
    return(chunked)
}


#' Collate
#' 
#' @description Collate array elements for iteration
#' @examples
#' collate(c(1, 2, 3, 4, 5, 6), 4)
#' list(c(1, 5), c(2, 6), 3, 4)
#' 
#' @export
collate <- function(array_obj, num_groups=2) {

    if (num_groups >= length(array_obj)) {
        return(as.list(array_obj))
    }

    collated <- vector("list", length = num_groups)
    for (group_idx in 1:num_groups) {
        collated[[group_idx]] <- array_obj[seq(group_idx, length(array_obj), num_groups)]
    }
    return(collated)
}


#' Dictionary
#' 
#' @description Simple dictionary implementation using R environment
#'
#' @export
dict_zip <- function(keys, values) {
    if (!is.list(values)) {
        values <- as.list(values)
    }
    named_list <- setNames(values, keys)
    env <- list2env(named_list)
    return(env)
}


#' Find matches based on substring
#' 
#' @description Return elements of a list matching a particular substring
#' 
#' @param items list or vector
#' @param patterns a string or collection of strings
#' @return Returns a list or vector with any matching items
#' 
#' @examples
#' filter_list_for_match(c("a_suffix", "b_suffix", "c"), "suffix")
#' 
#' @export
filter_list_for_match <- function(items, patterns) {
    # filter
    for (i in 1:length(patterns)){
        items <- lapply(items, grep, pattern=patterns[[i]], value=TRUE)
    }
    return (unlist(items[!sapply(items, identical, character(0))]))  # remove character(0)
}


#' Returns the first match given a pattern
#' 
#' @description Thin wrapper around [grep()]
#' 
#' @param items list or vector
#' @param pattern a single regex pattern, can also be an exact string
#' @return Returns the index of the first match
#' 
#' @examples
#' find_first_match_index('_suffix', c('a', 'b_suffix', 'c'))
#' 
#' @export
find_first_match_index <- function(pattern, items) {
    return (grep(pattern, items)[[1]])
}


#' Return items unique to vector a
#' 
#' @description
#' Return all the items found in a and not b. Useful for filtering dataframe columns.
#' 
#' @param a list or vector
#' @param b list or vector
#' @return Returns the filtered list or vector (same type as a)
#' 
#' @references
#' \href{https://stackoverflow.com/questions/10298662/find-elements-not-in-smaller-character-vector-list-but-in-big-list}{Stack Overflow}
#' 
#' @examples
#' items_in_a_not_b(c('a', 'b', 'c', '1', '2'), c('1', '2'))
#' 
#' @export
items_in_a_not_b <- function(a, b) {
    return((new <- a[which(!a %in% b)]))
}


#' Replaces each item in a list or vector with all replacements
#' 
#' @description
#' Convenience function to perform multiple replacements on a list or dataframe column.
#' Unlike [replace_specific_items()], `multiple_replacement()` can recognize patterns.
#' 
#' @param items list or vector
#' @param replacements a named list of replacements. uses names to match and values to replace.
#' @return Returns a vector with replaced items
#' 
#' @examples
#' replacements <- c('prefix_' = '', '_suffix' = '')
#' items <- c('prefix_a_suffix', 'prefix_b_suffix')
#' multiple_replacement(items, replacements)
#' 
#' @seealso [replace_specific_items()], [stringi::stri_replace_all_regex()]
#' 
#' @export
multiple_replacement <- function(items, replacements) {

    patterns <- names(replacements)
    replacements <- sapply(unname(replacements), function(x) gsub('\\\\', '$', x))
    
    items <- unname(sapply(items,
        function(x) stringi::stri_replace_all_regex(
            x,
            pattern = patterns,
            replacement = replacements,
            vectorize_all = FALSE)
    ))

    return (items)
}


#' Cycle
#' 
#' @description
#' Repeats the list until the number of elements desired is returned
#' 
#' @param items list or vector
#' @param n number of elements
#' 
#' @examples
#' n_items_from_list(c('blue', 'orange', 'green', 'red', 'purple'), 8)
#' 
#' @export
n_items_from_list <- function(items, n) {
    num_items <- length(items)
    num_cycles <- ceiling(n / num_items)
    return(items[rep(1:num_items, num_cycles)[1:n]])
}


#' Replaces each item in a list or vector with all replacements
#' 
#' @description Use this to rename columns
#' 
#' @param items list or vector
#' @param replacements a named list of replacements. uses names to match and values to replace.
#' @return Returns a vector with replaced items
#' 
#' @examples
#' replace_specific_items(c('a', 'b', 'c'), c('a'="A", 'c'="C"))
#' 
#' @seealso [multiple_replacement()]
#' 
#' @export
replace_specific_items <- function(items, replacements) {
    replace_ids <- which(items %in% intersect(names(replacements), items))
    for (idx in replace_ids) {
        items[idx] <- replacements[items[idx]]
    }
    return(items)
}
