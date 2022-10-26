#' plotLine
#' @description
#' plots a line plot with error bars
#' @examples
#' plotLine(df, title="Raw Data")
#' @import ggplot2
#' @export plotLine
plotLine <- function(
   df,
   x='x',
   y='y',
   error_y=NULL,
   error_y_minus=NULL,
   xlabel=NULL,
   ylabel=NULL,
   title=NULL,
   show_xlabel=TRUE,
   show_ylabel=TRUE,
   show_legend=TRUE,
   palette="Set1",
   error_bar_width = 1
) {

    df[, 'group'] = x
    
    # axis labels
    if (show_xlabel) {
        x_title_elem = ggplot2::element_text()
    } else {
        x_title_elem = ggplot2::element_blank()
    }
    if (show_ylabel) {
        y_title_elem = ggplot2::element_text()
    } else {
        y_title_elem = ggplot2::element_blank()
    }

    # error bars
    yerrorbars = (!is.null(error_y_minus) || !is.null(error_y))
    
    df[, 'xmin'] <- df[, x] - error_bar_width
    df[, 'xmax'] <- df[, x] + error_bar_width

    if (!is.null(error_y)) {
        df[, 'ymax'] <- df[, y] + df[, error_y]
    } else {
        df[, 'ymax'] <- df[, y]
    }

    if (!is.null(error_y_minus)) {
        df[, 'ymin'] <- df[, y] - df[, error_y_minus]
    } else {
        df[, 'ymin'] <- df[, y] 
    }


    # plot
    ggplot2::ggplot(df, ggplot2::aes_string(x=x, y=y, color='group')) +
        ggplot2::geom_line(ggplot2::aes_string(color='group'), show.legend = show_legend) +
        ggplot2::geom_point(ggplot2::aes_string(color='group'), show.legend = show_legend) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10),
                       axis.title.x = x_title_elem,
                       axis.title.y = y_title_elem) +
        ggplot2::labs(title=title, x=xlabel, y=ylabel) +
        ggplot2::scale_colour_brewer(palette=palette) +

        # one-sided error bars
        list(
            if (yerrorbars) {
                ggplot2::geom_errorbar(
                    ggplot2::aes_string(ymin="ymin", ymax="ymax"),
                    width = 0,
                    show.legend = show_legend
                )
            },
            if (yerrorbars && !is.null(error_y)) {
                ggplot2::geom_segment(
                    data=df,
                    ggplot2::aes_string(
                        y="ymax", yend="ymax",
                        x="xmin", xend="xmax"
                    ),
                    show.legend = show_legend
                )
            },
            if (yerrorbars && !is.null(error_y_minus)) {
                ggplot2::geom_segment(
                    data=df,
                    ggplot2::aes_string(
                        y="ymin", yend="ymin",
                        x="xmin", xend="xmax"
                    ),
                    show.legend = show_legend
                )
            }
        )
}
