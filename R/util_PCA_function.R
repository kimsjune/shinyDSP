#' PCA plotting function with ggplot2
#'
#' @param spe A [SpatialExperiment::] output from [standR::readGeoMx()]
#' @param precomputed Output from [SingleCellExperiment::reducedDim()]
#' @param colourShapeBy From input$selected_types
#' @param selectedVar Either "Type" or input$selected_batch
#' @param ROIshapes User input shapes from .PCA_customization() function OR
#' PCA_customization_batch()
#' @param ROIcolours User input colours from .PCA_customization() function OR
#' PCA_customization_batch()
#'
#' @return A [ggplot2] object
#' @keywords internal
#'
#' @author Seung J. Kim
.PCAFunction <- function(spe, precomputed, colourShapeBy, selectedVar,
                         ROIshapes, ROIcolours) {
    standR::drawPCA(spe, precomputed = precomputed) +

        ## really need as.name() plus !! because of factor()
        ggplot2::geom_point(ggplot2::aes(
            shape = factor(!!as.name(colourShapeBy), levels = selectedVar),
            fill = factor(!!as.name(colourShapeBy), levels = selectedVar)
        ), size = 3, colour = "black", stroke = 0.5) +

        ggplot2::scale_shape_manual(colourShapeBy,
            values = as.integer(unlist(ROIshapes)) # as.integer() crucial
        ) +
        ggplot2::scale_fill_manual(colourShapeBy,
            ## colour() is a base function, so must be avoided
            values = unlist(ROIcolours)
        ) +
        ggplot2::scale_y_continuous(
            labels = scales::number_format(accuracy = 0.1)
        ) +
        ggplot2::scale_x_continuous(
            labels = scales::number_format(accuracy = 0.1)
        ) +
        ggplot2::theme(
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_blank(),
            axis.text = ggplot2::element_text(color = "black", size = 16),
            axis.line = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_line(colour = "black"),
            axis.title = ggplot2::element_text(size = 16),
            legend.title = ggplot2::element_text(
                size = 16, vjust = 0.5, hjust = 0.5,
                face = "bold", family = "sans"
            ),
            legend.text = ggplot2::element_text(
                size = 16, vjust = 0.5, hjust = 0,
                face = "bold", family = "sans"
            ),
            plot.margin = grid::unit(c(1, 1, 1, 1), "mm"),
            plot.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
            plot.title = ggplot2::element_text(
                size = 16, hjust = 0.5, face = "bold",
                family = "sans"
            ),
            panel.border = ggplot2::element_rect(
                colour = "black",
                linewidth = 0.4
            ),
            panel.background = ggplot2::element_rect(
                fill = "transparent",
                colour = NA
            ),
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
            legend.position = "bottom",
            aspect.ratio = 1
        ) +
        ggplot2::guides(
            fill = ggplot2::guide_legend(
                nrow = length(selectedVar),
                title.position = "top"
            ),
            shape = ggplot2::guide_legend(
                nrow = length(selectedVar),
                title.position = "top"
            )
        )
}
