#' Create server block for Shiny
#'
#' @return Server for [shiny:shinyApp()]
#' @keywords internal
#'
#' @author Seung J. Kim
.build_server <- function() {
  function(input, output, session) {
    source("R/util_process_excel.R", local = TRUE)

    source("R/util_PCA_function.R", local = TRUE)
    source("R/util_create_PCA_customization.R", local = TRUE)
    source("R/util_create_PCA_by_CPM.R", local = TRUE)
    source("R/util_create_PCA_by_Q3.R", local = TRUE)
    source("R/util_create_PCA_by_RUV4.R", local = TRUE)

    source("R/util_differential_gene_exp.R", local = TRUE)

    source("R/util_create_table.R", local = TRUE)

    source("R/util_volcano_function.R", local = TRUE)
    source("R/util_create_volcano.R", local = TRUE)

    # shiny::observeEvent(input$run,{
    #   if (input$main == "introPage"){
    #     nav_select(id = "main", selected = "PCA")
    #   }
    # })

    # nocov start
    observeEvent(input$useSampleData, {
      if (input$useSampleData == FALSE) {
        shinyjs::show("uploadedFile")
      } else {
        shinyjs::hide("uploadedFile")
      }
    })
    # nocov end



    # nocov start
    shiny::observeEvent(input$togglePCAcustom, {
      shinyjs::toggle(id = "PCAcustom")
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$togglePCAcustom, {
      shinyjs::toggle(id = "PCAcustomBatch")
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$load, {
      output$selectYourK <- shiny::renderUI({
        shiny::numericInput(
          inputId = "k",
          "k value for RUV4 norm.",
          value = 2,
          min = 1,
          max = 20
        )
      })
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$run, {
      output$selectYourNorm <- shiny::renderUI({
        shinyWidgets::radioGroupButtons(
          inputId = "selectedNorm",
          choices = list(
            "CPM" = "speCPM()",
            "Q3" = "speQ3()",
            "RUV4" = "speRUV()"
          ),
          size = "sm",
          justified = TRUE,
          selected = "RUV4",
          label = "Normalization"
        )
      })
    })
    # nocov end

    # nocov start
    output$selectYourType <- shiny::renderUI({
      data <- data()[[4]]
      shiny::selectInput(
        inputId = "selectedTypes",
        label = "Choose types",
        choices = data,
        multiple = TRUE,
        selectize = TRUE,
        selected = NULL
      )
    })
    # nocov end

    # nocov start
    output$selectYourBatch <- shiny::renderUI({
      data4 <- data()[[4]]
      shiny::selectInput(
        inputId = "selectedTypes",
        label = "Choose groups you want to plot",
        choices = data4,
        multiple = TRUE,
        selectize = TRUE,
        selected = NULL
      )

      data5 <- data()[[5]]
      shiny::selectInput(
        inputId = "selectedBatch",
        label = "Choose sample batch",
        choices = data5,
        selected = "SlideName"
      )
    })
    # nocov end

    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customization <- shiny::renderUI({
      shiny::req(input$selectedTypes)
      .PCAcustomization()
    })
    # nocov end

    # nocov start
    # Opens a grouped widgets to pick shape and colour
    output$customizationBatch <- shiny::renderUI({
      shiny::req(input$selectedTypes)
      PCAcustomizationBatch()
    })
    # nocov end

    # nocov start
    output$pcaPlotCpm <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) & shiny::isTruthy(input$selectedTypes),
          "'Load' data then select groups first!"
        )
      )
      shiny::renderPlot(pcaPlotCpm())
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3 <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotQ3())
    })
    # nocov end

    # nocov start
    output$pcaPlotRuv <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotRuv())
    })
    # nocov end

    # nocov start
    output$pcaPlotCpmBatch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotCpmBatch())
    })
    # nocov end

    # nocov start
    output$pcaPlotQ3Batch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotQ3Batch())
    })
    # nocov end

    # nocov start
    output$pcaPlotRuvBatch <- shiny::renderUI({
      shiny::req(input$load, input$selectedTypes)
      shiny::renderPlot(pcaPlotRuvBatch())
    })
    # nocov end

    # nocov start
    output$downloadPCA <- shiny::downloadHandler(
      filename = function() {
        base::paste0("pca_", Sys.Date(), ".png")
      },
      content = function(file) {
        png(file)
        pcaPlot()
        dev.off()
      },
      contentType = "image/png"
    )
    # nocov end

    # nocov start
    output$table <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) &
            shiny::isTruthy(input$selectedTypes) &
            shiny::isTruthy(input$selectedNorm) &
            shiny::isTruthy(input$run),
          "'Load' data, 'choose types', select 'normalization', then hit 'run'!"
        )
      )
      DT::renderDT(
        topTabDF() %>%
          DT::datatable() %>%
          DT::formatSignif(columns = c(3:ncol(topTabDF())), digits = 4)
      )
    })
    # nocov end

    # nocov start
    output$downloadTable <- shiny::downloadHandler(
      filename = "output.csv",
      content = function(file) {
        utils::write.table(topTabDF(), file, sep = ",", row.names = FALSE)
      }
    )
    # nocov end

    # nocov start
    output$customRange <- shiny::renderUI({
      htmltools::div(
        shiny::sliderInput(
          inputId = "customX",
          "logFC", min = -10, max = 10, dragRange = FALSE, value = c(-4, 4)
        ),
        shiny::numericInput(
          inputId = "customY",
          "-log P value", value = 10
        )
      )
    })
    # nocov end

    # nocov start
    shiny::observeEvent(input$toggleCustomRange, {
      shinyjs::toggle("showCustomRange")
    })
    # nocov end


    # nocov start
    output$volcanoUI <- shiny::renderUI({
      shiny::validate(
        shiny::need(
          shiny::isTruthy(input$load) &
            shiny::isTruthy(input$selectedTypes) &
            shiny::isTruthy(input$selectedNorm) &
            shiny::isTruthy(input$run) &
            shiny::isTruthy(input$generateVolcano),
          "'Load' data, 'choose types', select 'normalization', hit 'run',
          then hit 'update'!"
        )
      )
      plotHeight <- shiny::reactive(350 * ncol(contrast()))

      # Plots as a list can be grouped into a grid and output
      shiny::renderPlot(
        # grid.arrange(grobs = plots)
        cowplot::plot_grid(
          plotlist = volcano(), ncol = 1, align = "v",
          axis = "lr"
        ),
        height = plotHeight()
      )
    })
    # nocov end

    # nocov start
    output$downloadVolcano <- shiny::downloadHandler(
      filename = function() {
        paste0("volcano_", Sys.Date(), ".png")
      },
      content = function(file) {
        cowplot::save_plot(file, volcanoPlots())
      },
      contentType = "image/png"
    )
    # no cov




    # top_n_genes <- shiny::reactive({
    #     input$top_n_genes
    # })
    #
    # heatmap_col <- shiny::reactive({
    #     input$heatmap_col
    # })
    #
    # heatmap_range <- shiny::reactive({
    #     input$heatmap_range
    # })
    #
    # heatmap_size <- shiny::reactive({
    #     input$heatmap_size
    # })
    #
    # heatmap_fontsize <- shiny::reactive({
    #     input$heatmap_fontsize
    # })
    #
    #
    #
    #
    # lcpm_subset_scale <- shiny::reactive({
    #     mydata <- list()
    #     for (i in seq_along(shiny::reactiveRun())) {
    #         mydata[[i]] <- assay(spe_ruv_subset(), 2)[, colData(spe_ruv_subset())$anno_type == shiny::reactiveRun()[i]]
    #     }
    #
    #
    #     lcpm_subset_scale <- t(scale(t(data.frame(mydata))))
    #
    #     return(lcpm_subset_scale)
    # })
    #
    # colnames4heatmap <- shiny::reactive({
    #     mydata <- list()
    #     for (i in seq_along(shiny::reactiveRun())) {
    #         mydata[[i]] <- assay(spe_ruv_subset(), 2)[, colData(spe_ruv_subset())$anno_type == shiny::reactiveRun()[i]]
    #     }
    #
    #
    #     return(colnames(do.call(cbind, mydata)))
    # })
    #
    #
    #
    #
    # lcpm_subset_scale_topGenes <- shiny::reactive({
    #     ## BEWARE! top_n() reorders rows by some column value. Must use slice_head() to pick first n rows
    #     lcpm_subset_scale_topGenes <- lcpm_subset_scale()[topTabDF() %>%
    #                                                           slice_head(n = top_n_genes()) %>%
    #                                                           select(Gene) %>%
    #                                                           unlist() %>%
    #                                                           unname(), ]
    #
    #     return(lcpm_subset_scale_topGenes)
    # })
    #
    #
    # heatmap <- shiny::reactive({
    #     col_fun <- colorRamp2(c(heatmap_range()[1], 0, heatmap_range()[2]), hcl_palette = heatmap_col())
    #
    #     chm <- Heatmap(lcpm_subset_scale_topGenes(),
    #                    cluster_columns = F,
    #                    col = col_fun,
    #                    # width = unit(dim(lcpm_subset_scale_topGenes())[2]*15, "mm"),
    #                    # height = unit(dim(lcpm_subset_scale_topGenes())[1]*15, "mm"),
    #                    width = unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[2], "mm"),
    #                    height = unit(as.numeric(heatmap_size()) / 2 * dim(lcpm_subset_scale_topGenes())[1], "mm"),
    #                    heatmap_legend_param = list(
    #                        border = "black",
    #                        title = "Z score",
    #                        title_gp = gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
    #                        labels_gp = gpar(fontsize = heatmap_fontsize(), fontface = "plain", fontfamily = "sans"),
    #                        legend_height = unit(3 * as.numeric(heatmap_size()), "mm")
    #                    ),
    #                    top_annotation = HeatmapAnnotation(
    #                        foo = anno_block(
    #                            gp = gpar(lty = 0, fill = "transparent"),
    #                            labels = unlist(shiny::reactiveRun()),
    #                            labels_gp = gpar(col = "black", fontsize = 14, fontfamily = "sans", fontface = "bold"),
    #                            labels_rot = 0, labels_just = "center", labels_offset = unit(4.5, "mm")
    #                        )
    #                    ),
    #                    border_gp = gpar(col = "black", lwd = 0.2),
    #                    row_names_gp = gpar(fontfamily = "sans", fontface = "italic", fontsize = heatmap_fontsize()),
    #                    show_column_names = F,
    #
    #                    # top_annotation= HeatmapAnnotation(
    #                    #   foo = anno_block(
    #                    #     gp = gpar(lty=0, fill="transparent"),
    #                    #     labels = unlist(shiny::reactiveRun()),
    #                    #     labels_gp = gpar(col="black", fontsize=7, fontfamily = "sans", fontface = "bold"),
    #                    #     labels_rot = 20, labels_just = "center", labels_offset = unit(4,"mm")
    #                    #   )
    #                    # ),
    #                    column_split = rep(LETTERS[seq_along(shiny::reactiveRun())],
    #                                       # times = as.numeric(unname(table(colData(spe_ruv_subset())[colnames(lcpm_subset_scale()), "anno_type"])))
    #                                       times = as.numeric(unname(table(colData(spe_ruv_subset())[colnames4heatmap(), "anno_type"])))
    #                    ),
    #                    column_title = NULL
    #     )
    #
    #     return(chm)
    # })
    #
    # output$heatmapUI <- shiny::renderUI({
    #     shiny::renderPlot(
    #         heatmap(),
    #         height = 1.5 * as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1]
    #         # height = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1],
    #         # width = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[2]
    #     )
    # })
    #
    # output$downloadHeatmap <- shiny::downloadHandler(
    #     filename = function() {
    #         paste0("heatmap_", Sys.Date(), ".png")
    #     },
    #     content = function(file) {
    #         png(file, width = dim(lcpm_subset_scale_topGenes())[2] * 50, height = dim(lcpm_subset_scale_topGenes())[1] * 50, bg = "transparent")
    #         draw(heatmap())
    #         dev.off()
    #     },
    #     contentType = "image/png"
    # )

  }
}
