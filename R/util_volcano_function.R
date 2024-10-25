#' Volcano plot plotting function with ggplot2
#'
#' @param volcano a data.frame
#' @param delabSize from input$delabSize
#' @param maxOverlap from input$maxOverlap
#' @param title Contrasts title
#' @param logFCcutoff from input$logFCcutoff
#' @param PvalCutoff from input$PvalCutoff
#' @param DnCol from input$DnCol. Determines the colour of downregulated genes.
#' @param notDEcol from input$notDEcol.
#' @param UpCol from input$UpCol. Determines the colour of upregulated genes.
#'
#' @return A [ggplot2::geom_point()] object
#' @keywords internal
#'
#' @author Seung J. Kim
.volcanoFunction <- function(volcano, delabSize, maxOverlap, title,
                             logFCcutoff, PvalCutoff,
                             DnCol, notDEcol, UpCol) {
    ggplot2::ggplot(
        data = volcano,
        ggplot2::aes(
            x = logFC,
            y = -log10(adj.P.Val),
            col = de,
            label = deLab
        )
    ) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::theme(
            axis.ticks = ggplot2::element_line(colour = "black"),
            panel.border = ggplot2::element_rect(colour = "black"),
            text = ggplot2::element_text(size = 16, color = "black"),
            title = ggplot2::element_text(size = 16, hjust = 0.5),
            axis.text = ggplot2::element_text(size = 16, color = "black"),
            axis.title = ggplot2::element_text(size = 16),
            plot.margin = grid::unit(c(1, 1, 1, 1), "mm"),
            plot.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            panel.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            panel.grid = ggplot2::element_blank(),
            legend.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            legend.box.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            legend.key = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            legend.position = "none"
        ) +
        ggplot2::geom_vline(
            xintercept = c(-(logFCcutoff), logFCcutoff), col = "black",
            linetype = 3
        ) +
        ggplot2::geom_hline(
            yintercept = -log10(PvalCutoff), col = "black",
            linetype = 3
        ) +
        ggrepel::geom_text_repel(
            size = delabSize,
            segment.colour = "black",
            segment.linetype = 3,
            data = volcano %>%
                dplyr::filter(logFC >= logFCcutoff | logFC <= -(logFCcutoff)),
            ggplot2::aes(label = deLab),
            min.segment.length = 0,
            max.overlaps = maxOverlap
        )+
        ggplot2::scale_color_manual(
            values = c(DnCol, notDEcol, UpCol),
            breaks = c("DN", "NA", "UP")
        ) +
        ggplot2::xlab(expression(log[2] ~ fold ~ change)) +
        ggplot2::ylab(expression(-log[10] ~ italic(P) ~ value)) +
        ggplot2::theme(aspect.ratio = 1, rect = ggplot2::element_rect(
            fill = "transparent"
        )) +
        ggplot2::labs(title = title)
}
