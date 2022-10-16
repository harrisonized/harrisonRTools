#' plotHeatmap
#' @description
#' plots a heatmap
#' @examples
#' plotHeatmap(df, title="Raw Data", show_xlabel=TRUE, show_ylabel=TRUE, scientific_notation=FALSE, annotations=TRUE)
#' @export plotHeatmap
plotHeatmap <- function(
   df,
   x='col',
   y='row',
   fill='val',
   title=NULL,
   show_xlabel=TRUE,
   show_ylabel=TRUE,
   annotations=FALSE,
   scientific_notation=FALSE
) {
    
    tab <- smelt(revDf(df))  # reshape

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
	    if (scientific_notation) {
	        tab['label'] = lapply(tab['val'], function(x) formatC(x, format='e', digits=2))
	        label='label'
	    } else {
	        label = 'val'
	    }
	} else {
    	label = NULL
    }

    # plot
    ggplot2::ggplot(tab, ggplot2::aes_string(x=x, y=y, fill=fill)) +
        ggplot2::geom_tile(color="white", lwd=0.3, linetype=1) +
        ggplot2::coord_fixed(expand=TRUE) +
        ggplot2::scale_y_discrete(limits=rev) +
        ggplot2::labs(title = title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10),
                       axis.title.x = xlabel,
                       axis.title.y = ylabel) +
        ggplot2::scale_fill_gradient(low="#FFF8F8", high="#A50026") +
        if (annotations) {ggplot2::geom_text(ggplot2::aes_string(label=label), color = 'black', size = 2)}
}