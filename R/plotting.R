## Objects
## default_colors_plotly

## Functions
## plot_heatmap
## plot_single_line


#' Plotly's default colors
#' 
#' @examples
#' default_colors_plotly
#' 
#' @references
#' \href{https://stackoverflow.com/questions/41761654/plotly-where-can-i-find-the-default-color-palette-used-in-plotly-package}{StackOverflow post}
#' 
#' @export
default_colors_plotly = c(
    "#1f77b4",  # blue
    "#ff7f0e",  # orange
    "#2ca02c",  # green
    "#d62728",  # red
    "#9467bd",  # purple
    "#8c564b",  # brown
    "#e377c2",  # pink
    "#7f7f7f",  # gray
    "#bcbd22",  # yellow
    "#17becf"  # aqua
)


#' Create a heatmap
#'
#' @usage
#' plot_heatmap(
#'   df,
#'   title="Raw Data",
#'   show_xlabel=TRUE,
#'   show_ylabel=TRUE,
#'   annotations=TRUE,
#'   scientific_notation=FALSE,
#'   digits=0
#' )
#' 
#' @section Vignette:
#' See `vignettes/plot_heatmap.Rmd`
#' 
#' @export
plot_heatmap <- function(
   df,
   x='col',
   y='row',
   fill='val',
   title=NULL,
   show_xlabel=TRUE,
   show_ylabel=TRUE,
   annotations=FALSE,
   scientific_notation=FALSE,
   digits=1
) {
    
    tab <- smelt(rev_df(df))  # reshape

    # axis labels
    if (show_xlabel) {
        xlabel = ggplot2::element_text()
    } else {
        xlabel = ggplot2::element_blank()
    }
    if (show_ylabel) {
        ylabel = ggplot2::element_text()
    } else {
        ylabel = ggplot2::element_blank()
    }

    # annotations
    if (annotations) {
    	label = 'label'
        if (scientific_notation) {
            tab['label'] = lapply(
                tab['val'], 
                function(x) formatC(x, format='e', digits=2)
            )
        } else {
        	tab['label'] = lapply(
                tab['val'], 
                function(x) round(x, digits)
            )
        }
    } else {
        label = NULL
    }

    # plot
    fig <- ggplot2::ggplot(tab, ggplot2::aes_string(x=x, y=y, fill=fill)) +
        ggplot2::geom_tile(color="white", lwd=0.3, linetype=1) +
        ggplot2::coord_fixed(expand=TRUE) +
        ggplot2::scale_y_discrete(limits=rev) +
        ggplot2::labs(title = title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10),
                       axis.title.x = xlabel,
                       axis.title.y = ylabel) +
        ggplot2::scale_fill_gradient(low="#FFF8F8", high="#A50026") +
        if (annotations) {
            ggplot2::geom_text(
                ggplot2::aes_string(label=label),
                color = 'black',
                size = 2
            )
        }

    return (fig)
}


#' Create a line plot
#' 
#' Plot a single line plot with directional error bars
#' 
#' @usage
#' plot_single_line(
#'   df,
#'   x='x', y='y',
#'   xlabel='X Axis', ylabel='Y Axis',
#'   title='Raw Data',
#'   error_y='plus_e',  # error_y_minus='minus_e',
#'   show_legend=FALSE,
#'   error_bar_width=2,
#'   palette="Set2"
#' )
#' 
#' @export
plot_single_line <- function(
   df,
   x='x',
   y='y',
   xlabel=NULL,
   ylabel=NULL,
   title=NULL,
   error_y=NULL,
   error_y_minus=NULL,
   show_xlabel=TRUE,
   show_ylabel=TRUE,
   show_legend=TRUE,
   error_bar_width = 1,
   palette="Set2"
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
    fig <- ggplot2::ggplot(df, ggplot2::aes_string(x=x, y=y, color='group')) +
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

    return (fig)
}
