import::here(magrittr, '%>%')
import::here(rlang, 'sym')
import::here(ggplot2,
    'ggplot', 'aes', 'theme', 'theme_bw', 'labs', 
    'geom_violin', 'geom_boxplot', 'geom_jitter',
    'geom_point', 'geom_hline', 'geom_vline', 'geom_segment', 'geom_bar',
    'guides', 'guide_axis', 'guide_legend',
    'xlim', 'ylim', 'scale_x_log10', 'scale_y_log10', 'scale_x_discrete',
    'annotate', 'element_text', 'element_blank')
import::here(scales, 'trans_breaks', 'trans_format', 'math_format')
import::here(cowplot, theme_cowplot)
import::here(patchwork, 'wrap_plots', 'plot_layout')
# import::here(plotly, 'plot_ly', 'add_trace', 'layout')

## Objects
## default_colors_plotly

## Functions
## plot_bar
## plot_gel
## plot_heatmap
## plot_multiscatter
## plot_scatter
## plot_single_line
## plot_violin
## plot_volcano
## plot_waterfall


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


#' Plot Bar
#' 
#' @description Plot a simple bar plot.
#' 
#' @export
plot_bar <- function(
    df,
    x='cell_type',
    y='value',
    group.by=NULL,  # gene of interest
    fill=NULL,  # steelblue, overwrites group.by
    xlabel=NULL,
    ylabel="Number of Genes",
    title="Number of Cells",
    xaxis_angle=45,
    legend_position='bottom',
    sort=TRUE
) {

    # color
    if (is.null(group.by)) { color <- NULL } else { color <- sym(group.by) }
    if (is.null(fill)) {
        bar <- geom_bar(stat="identity")
    } else {
        bar <- geom_bar(stat="identity", fill=fill)
    }

    # plot base
    if (sort) {
        base_plot <- ggplot(data=df,
            aes(x=reorder(.data[[x]], .data[[y]], decreasing=TRUE),
                y=.data[[y]], fill=!!color ))
    } else {
        base_plot <- ggplot(data=df,
            aes(x=.data[[x]], y=.data[[y]], fill=!!color ))
    }

    fig <- base_plot +
        bar +
        labs(x=xlabel, y=ylabel, title=title) +
        scale_x_discrete( guide=guide_axis(angle = xaxis_angle) ) +
        theme(legend.position=legend_position)

    return(fig)
}


#' Plot Gel
#' 
#' @description
#' Insert a dataframe like this one:
#' +-----------+------+-------------+-----------+------------+-----------+
#' | gene_name | lane | exon_length | num_reads |    srpm    | intensity |
#' +-----------+------+-------------+-----------+------------+-----------+
#' | Tmsb4x    | mat  |     1371    |   25571   |  2878.255  |  1.000000 |
#' | Arhgap4   | mat  |     8105    |    4681   |   526.889  |  0.346293 |
#' | Xist      | mat  |    17946    |      58   |     6.476  |  0.201613 |
#' | Tmsb4x    | pat  |     1370    |      67   |     7.541  |  0.201909 |
#' | Arhgap4   | pat  |     8105    |      22   |     2.476  |  0.200501 |
#' | Xist      | pat  |    16791    |    21964  |  2472.253  |  0.887127 |
#' +-----------+------+-------------+-----------+------------+-----------+
#' 
#' @export
plot_gel <- function(
    df,
    lane='lane',
    genes='gene_name',
    size='size',
    intensity_col='intensity',
    text_col='intensity',
    column_order=c(),
    ylabel=NULL,
    title=NULL,
    yrange=c(NA, NA),
    showlegend=FALSE,
    font_size=12,
    tickangle=-45,
    band_width=0.04,
    lane_width=100,
    top_margin=150,
    height=800,
    min_width=200
) {

    num_lanes = nrow(unique(df[lane]))
    yrange = log(yrange, base=10)

    fig <- plot_ly(
        height =  height,
        width = min_width+lane_width*num_lanes,
        type = 'bar'
    )
    for (gene in unique(df[[genes]])) {
        subset = df[(df[genes] == gene), ]
        fig <- fig %>% add_trace(
            base=subset[[size]],
            x=subset[[lane]],
            y=subset[[size]]*band_width,
            text=subset[[text_col]],
            legendgroup=gene,
            alignmentgroup=TRUE,
            marker=list(
                color= c(paste0('rgba(0, 0, 0, ', subset[[intensity_col]]))
            ),
            name=gene,
            orientation='v',
            showlegend=TRUE,
            textposition="none",
            hovertemplate=paste0(
                'lane=%{x}<br>',
                'gene=', gene, '<br>',
                'size=%{base}<br>',
                text_col, '=%{text}<br>',
                '<extra></extra>'
            ),
            type='bar'
        )
    }

    fig <- fig %>% layout(
        title = list(
            text = title,
            x = 0
        ),
        xaxis = list(
            side = 'top',
            tickangle = tickangle,
            type = 'category',
            # autorange = 'reversed',
            categoryorder = 'array',
            categoryarray = column_order
        ),
        yaxis = list(
            showgrid = FALSE,
            zeroline = FALSE,
            type = 'log',
            range = yrange,
            ticks = 'outside',
            title_text = ylabel
        ),
        legend = list(
            title = list(text = 'genes'),
            tracegroupgap = 0
        ),
        margin = list(
            t = top_margin
        ),
        barmode = 'overlay',
        autosize = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend =showlegend,
        font = list(
            size = font_size
        ),
        hovermode = 'closest'
    )

    return(fig)
}


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


#' Plot Multiple Scatter
#' 
#' @export
plot_multiscatter <- function(
    df, x, y, color, size=NULL,
    xlabel=NULL, ylabel=NULL, clabel=NULL, title=NULL,
    xmin=NULL, xmax=NULL,
    ymin=NULL, ymax=NULL,
    hover_data=c(),
    xaxis_type='linear',
    yaxis_type='linear',
    color_discrete_map=NULL
) {

    # Range params
    if (is.null(xmin)) {
        xmin <- min(df[[x]])
    }
    if (is.null(xmax)) {
        xmax <- max(df[[x]])
    }
    if (xaxis_type=='linear') {
        xrange = c(xmin - (xmax-xmin) * 0.05, xmax + (xmax-xmin) * 0.05)  # add padding
    }
    if (xaxis_type=='log') {
        xrange = sapply(c(xmin, xmax), function(x) if (x > 0) {log(x, base=10)} else {NULL} )
    }

    if (is.null(ymin)) {
        ymin <- min(df[[y]])
    }
    if (is.null(ymax)) {
        ymax <- max(df[[y]])
    }
    if (yaxis_type=='linear') {
        yrange = c(ymin - (ymax-ymin) * 0.1, ymax + (ymax-ymin) * 0.1)  # add padding
    }
    if (yaxis_type=='log') {
        yrange = sapply(c(ymin, ymax), function(y) if (y > 0) {log(y, base=10)} else {NULL} )
    }

    # hoverdata
    hovertext <- ''
    for (field in c(color, x, y, size, hover_data)) {
        if (!is.null(field)) {
            hovertext <- paste0(hovertext, field, "=", df[[field]], "<br>")
        }
    }

    fig <- plotly::plot_ly() + plotly::add_trace(
        data = df,
        x = df[[x]],
        y = df[[y]],
        color = df[[color]],
        colors = color_discrete_map,
        size = df[[size]],
        fill = '',
        type = 'scatter',
        mode = 'markers',
        hoverinfo = 'text',
        hovertext = hovertext
    ) %>% plotly::layout(
        title = list(
            text = title,
            x = 0
        ),
        xaxis = list(
            title_text = xlabel,
            showgrid = TRUE, zeroline = FALSE,
            range = xrange,
            type = xaxis_type
        ),
        yaxis = list(
            title_text = ylabel,
            showgrid = TRUE, gridcolor = '#E4EAF2', zeroline = FALSE,
            range = yrange,
            type = yaxis_type
        ),
        plot_bgcolor = 'rgba(0,0,0,0)',
        showlegend = TRUE,
        hovermode = 'closest'
    )

    return(fig)
}


#' Plot Scatter
#' 
#' @description Plot a simple 2D scatterplot
#' 
#' @export
plot_scatter <- function(
    df,
    x='nCount_RNA',
    y='nFeature_RNA',
    color='sample_name',  # NULL for no groups
    xlabel="Read Counts",
    ylabel="Number of Genes",
    title="Read Counts vs Sequencing Depth",
    alpha=0.7,
    point_size=0.5,
    log_x=FALSE,
    log_y=FALSE,
    legend_large_circle=TRUE
) {

    # group.by
    if (is.null(color)) { colour <- NULL } else { colour <- sym(color) }

    # scale axes
    if (log_x) {
        scale_x <- scale_x_log10(
            breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x))
        )
    } else { scale_x <- list() }
    if (log_y) {
        scale_y <- scale_y_log10(
            breaks = trans_breaks("log10", function(x) 10^x),
            labels = trans_format("log10", math_format(10^.x))
        )
    } else { scale_y <- list() }

    if (legend_large_circle) {
        guide <- guides( colour=guide_legend(override.aes=list(size=4L,alpha=1)) )
    } else {
        guide <- list()
    }

    # plot
    fig <- ggplot(df,
              aes(x=.data[[x]], y=.data[[y]],
                  colour=!!colour) ) +
       geom_point(alpha=alpha, size=point_size) +
       labs(x=xlabel, y=ylabel, title=title) + 
       guide +
       scale_x +
       scale_y

    return(fig)
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


#' Plot Violin
#' 
#' @description Similar to [Seurat::VlnPlot()], but prettier.
#' 
#' @export
plot_violin <- function(
    seurat_obj,
    cols=c('nCount_RNA', 'nFeature_RNA', 'percent.mt'),
    group.by='orig.ident',
    threshold_data=data.frame(
        'sample_name'=c(rep('bm1', each=5), rep('bm2', each=5)),
        'col'=rep(c('nCount_RNA', 'nCount_RNA', 'nFeature_RNA', 'nFeature_RNA', 'percent.mt'), n=2),
        'threshold'=c(c(1000, 20000, 300, 2500, 5),
                      c(500, 10000, 500, 4000, 10))
    ),
    box=TRUE,
    pt.size = 0.05,
    alpha = 0.1,
    angle = 45,
    title.size = 12,
    showlegend=FALSE,
    legend.position = 'right'
) {
    subfigs <- new.env()
    for (i in 1:length(cols)) {
        col <- cols[i]

        # boxplot
        if (box) {
            boxplot <- geom_boxplot(width=0.1, alpha=1, outlier.size = pt.size)
        } else {
            boxplot <- list()
        }

        # thresholds
        if (is.null(threshold_data)) {
            segment_plot <- list()
        } else {

            threshold_data['group_num'] <- as.numeric(factor(threshold_data[[group.by]]))
            threshold_data[['x']] = threshold_data[['group_num']]-0.4
            threshold_data[['xend']] = threshold_data[['group_num']]+0.4
            threshold_data[['y']] = threshold_data[['threshold']]
            threshold_data[['yend']] = threshold_data[['threshold']]

            groups <- unname(unlist(unique( seurat_obj[[group.by]] )))

            segment_plot <- geom_segment(
                data = threshold_data[
                    (threshold_data[, group.by] %in% groups) &
                    (threshold_data['col']==col), 
                ],
                aes(x = x, y = y, xend = xend, yend = yend),
                inherit.aes=TRUE
            )
        }

        subfig <- ggplot(
            seurat_obj@meta.data,
            aes(x=.data[[group.by]],
                y=.data[[col]],
                fill=.data[[group.by]])) +
        geom_violin() +
        boxplot +
        geom_jitter(size = pt.size, alpha = alpha) +
        segment_plot +
        labs(title = col,
             x = NULL,
             y = NULL,
             fill=group.by) +
        cowplot::theme_cowplot() +
        theme(plot.title = element_text(size=title.size, hjust=0.5, vjust=0.5),
              axis.text.x = element_text(angle = angle, hjust=1),
              legend.title = element_text(size=12, face = "bold")) +
        ylim(0, NA)

        subfigs[[paste(i)]] <- subfig
    }

    fig <- patchwork::wrap_plots(as.list(subfigs)) +
        patchwork::plot_layout(guides = "collect") &
        theme(legend.position = ifelse(showlegend, legend.position, "none"))

    return(fig)
}


#' Plot Volcano
#' 
#' @export
plot_volcano <- function(
    df,
    x='avg_log2FC',
    y='p_val_adj',
    gene='gene',
    title="Volcano plot"
) {

    fig <- ggplot(df,
        aes(x=.data[['avg_log2FC']], y=-log10(.data[['p_val_adj']]),
            text=paste("Symbol:", .data[['gene']]))
    ) +
        geom_point(size=0.5) +
        labs(title=title) +
        theme_bw() +
        geom_hline(yintercept = -log10(0.01), linetype="longdash", colour="grey", linewidth=1) +
        geom_vline(xintercept = 1, linetype="longdash", colour="#BE684D", size=1) +
        geom_vline(xintercept = -1, linetype="longdash", colour="#2C467A", size=1) +
        annotate("rect", xmin = 1, xmax = 2, ymin = -log10(0.01), ymax = 7.5, alpha=.2, fill="#BE684D") +
        annotate("rect", xmin = -1, xmax = -2, ymin = -log10(0.01), ymax = 7.5, alpha=.2, fill="#2C467A")
    
    return(fig)
}


#' Plot Waterfall
#' 
#' @description Thin wrapper around plot_scatter
#' 
#' @references
#' \href{SingleCellPlus - HKU Workshop}{https://sydneybiox.github.io/SingleCellPlus/qc.html#3_qc1:_waterfall_plot}
#' 
#' @export
plot_waterfall <- function(
    barcode_ranks,
    x='rank',
    y='total',
    xlabel="Counts Per Cell",
    ylabel="Cell Rank",
    title="Read Counts",
    show_thresholds=TRUE
) {

    if (show_thresholds) {
        thresholds <- data.frame(
            type = c("knee",
                     "inflection"),
            value = c(barcode_ranks@metadata$knee,
                      barcode_ranks@metadata$inflection)
        )
        hline <- geom_hline(
            data=thresholds,
            aes(yintercept = value, colour = type),
            linetype = 2
        )
    } else { hline <- list() }

    fig <- plot_scatter(
        as.data.frame(barcode_ranks),
        x=x, y=y, color=NULL,
        xlabel=xlabel, ylabel=ylabel, title=title,
        log_x=TRUE, log_y=TRUE,
        legend_large_circle = FALSE
    ) +
    hline +
    guides( color=guide_legend( override.aes=list(shape=c(NA, NA)) ))
    
    return(fig)
}
