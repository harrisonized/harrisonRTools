#' plotHeatmap
#' @description
#' plots a heatmap
#' @export plotHeatmap
plotHeatmap <- function(
   df
) {
    tab <- smelt(revDf(df))
    tab['label'] = lapply(tab['val'], function(x) formatC(x, format='e', digits=2))
    ggplot2::ggplot(tab, ggplot2::aes(x=col, y=row, fill=val)) +
        ggplot2::geom_tile(color="white", lwd=0.3, linetype=1) +
        ggplot2::geom_text(ggplot2::aes(label=label), color = 'black', size = 2) +
        ggplot2::coord_fixed(expand=TRUE) +
        ggplot2::scale_y_discrete(limits=rev) +
        ggplot2::labs(title = "Raw Data") +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 10)) +
        ggplot2::scale_fill_gradient(low="#FFF8F8", high="#A50026")
}