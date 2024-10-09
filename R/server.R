# if (interactive()) {
#     library(shiny)
#     library(shinycssloaders)
#     library(standR)
#
#     library(dplyr)
#     library(shinyjs)
#     library(bslib)
#     library(DT)
#
#     library(standR)
#     library(ggplot2)
#
#     library(tibble)
#
#     library(readxl)
#     library(scater)
#
#     library(edgeR)
#     library(limma)
#
# }




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

        # observeEvent(input$run,{
        #
        #     print(contrast())
        #
        # })

        observeEvent(input$useSampleData,{
            if (input$useSampleData == FALSE) {
                shinyjs::show("uploadedFile")
            } else {
                shinyjs::hide("uploadedFile")
            }
        })




        shiny::observeEvent(input$togglePCAcustom, {
            shinyjs::toggle(id = "PCAcustom")
        })

        shiny::observeEvent(input$togglePCAcustom, {
            shinyjs::toggle(id = "PCAcustomBatch")
        })

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

        shiny::observeEvent(input$run, {
            output$selectYourNorm <- shiny::renderUI({
                shinyWidgets::radioGroupButtons(
                    inputId = "selectedNorm",
                    choices = list(
                        "CPM" = "speCPM()",
                        "Q3" = "speQ3()",
                        "RUV4" = "speRUV()"
                    ),
                    size = 'sm',
                    justified = TRUE,
                    selected = "RUV4",
                    label = "Normalization"
                )
            })
        })

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
                # choices = c("SlideName" = "SlideName",
                #             "ScanLabel" = "ScanLabel"),
                choices = data5,
                selected = "SlideName"
            )
        })


        # Opens a grouped widgets to pick shape and colour
        output$customization <- shiny::renderUI({
            shiny::req(input$selectedTypes)
            .PCAcustomization()
        })




        # Opens a grouped widgets to pick shape and colour
        output$customizationBatch <- shiny::renderUI({
            shiny::req(input$selectedTypes)
            PCAcustomizationBatch()
        })








        output$pcaPlotCpm <- shiny::renderUI({
            shiny::validate(
                shiny::need(shiny::isTruthy(input$load) & shiny::isTruthy(input$selectedTypes),
                    "'Load' data then select groups first!"))
            shiny::renderPlot(pcaPlotCpm())
        })
        output$pcaPlotQ3 <- shiny::renderUI({
            shiny::req(input$load, input$selectedTypes)
            shiny::renderPlot(pcaPlotQ3())
        })
        output$pcaPlotRuv <- shiny::renderUI({
            shiny::req(input$load, input$selectedTypes)
            shiny::renderPlot(pcaPlotRuv())
        })
        output$pcaPlotCpmBatch <- shiny::renderUI({
            shiny::req(input$load, input$selectedTypes)
            shiny::renderPlot(pcaPlotCpmBatch())
        })
        output$pcaPlotQ3Batch <- shiny::renderUI({
            shiny::req(input$load, input$selectedTypes)
            shiny::renderPlot(pcaPlotQ3Batch())
        })
        output$pcaPlotRuvBatch <- shiny::renderUI({
            shiny::req(input$load, input$selectedTypes)
            shiny::renderPlot(pcaPlotRuvBatch())
        })

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



        output$table <- shiny::renderUI({
            shiny::validate(
                shiny::need(shiny::isTruthy(input$load) &
                         shiny::isTruthy(input$selectedTypes) &
                         shiny::isTruthy(input$selectedNorm) &
                         shiny::isTruthy(input$run),
                     "'Load' data, 'choose types', select 'normalization', then hit 'run'!"))

            DT::renderDT(
                # No need to create another shiny::reactive expression for the dataTABLE version of dataframe
                topTabDF() %>%
                    DT::datatable() %>%
                    DT::formatSignif(columns = c(3:ncol(topTabDF())), digits = 4)
            )
        })






        output$downloadTable <- shiny::downloadHandler(
            filename = "output.csv",
            content = function(file) {
                utils::write.table(topTabDF(), file, sep = ",", row.names = FALSE)
            }
        )




        plotHeight <- shiny::reactive(350 * ncol(contrast()))


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


        shiny::observeEvent(input$toggleCustomRange, {
            shinyjs::toggle("showCustomRange")
        })



        output$volcanoUI <- shiny::renderUI({
            shiny::validate(
                shiny::need(shiny::isTruthy(input$load) &
                         shiny::isTruthy(input$selectedTypes) &
                         shiny::isTruthy(input$selectedNorm) &
                         shiny::isTruthy(input$run) &
                         shiny::isTruthy(input$generateVolcano),
                     "'Load' data, 'choose types', select 'normalization', hit 'run', then hit 'update'!"))
            # Plots as a list can be grouped into a grid and output
            shiny::renderPlot(
                # grid.arrange(grobs = plots)
                cowplot::plot_grid(plotlist = volcano(), ncol = 1, align = "v", axis = "lr"),
                height = plotHeight()
            )
        })

        output$downloadVolcano <- shiny::downloadHandler(
            filename = function() {
                paste0("volcano_", Sys.Date(), ".png")
            },
            content = function(file) {
                cowplot::save_plot(file, volcanoPlots())
            },
            contentType = "image/png"
        )




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

        # # check_credentials returns a function to authenticate users
        # res_auth <- secure_server(
        #   check_credentials = check_credentials(credentials)
        # )
        #
        # output$auth_output <- renderPrint({
        #   shiny::reactiveValuesToList(res_auth)
        # })
    }
}
