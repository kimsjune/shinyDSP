#' Creates the shiny app, ready to be loaded
#'
#' @return A [shiny::shinyApp()] object
#' @export
#'
#' @author Seung J. Kim
#'
#' @examples
#' library(shinyDSP)
#' app <- shinyDSP()
#' if (interactive()) {
#'   shiny::runApp(app)
#' }
shinyDSP <- function() {
  ui <- bslib::page_navbar(
    htmltools::tags$head(htmltools::tags$link(
      rel = "shortcut icon",
      href = "favicon.ico/lung.png"
    )),
    htmltools::tags$style(
      "
    .introText {
    padding-left: 100px;
    padding-right: 100px;
    }
    "
    ),
    shinyjs::useShinyjs(),
    title = "shinyDSP",
    id = "main",
    fillable = TRUE,
    sidebar = .interfaceSidebar(),
    .interfaceIntroNavPanel(),
    .interfacePcaNavPanel(),
    .interfaceTableNavPanel(),
    .interfaceVolcanoNavPanel(),
    .interfaceHeatmapNavPanel()







    # bslib::nav_panel(
    #   "Appendix",
    #   div(
    #     tags$p("These are all possible colours schmes for the heatmap. Enter the names on the top exactly without quotes."),
    #     tags$img(src = "images/hcl.svg", alt = "hcl_palette")
    #   )
    # )

    # bslib::nav_panel(
    #   "Credit",
    #   div(
    #     tags$p("Amin Manji and Meggie Vo helped with beta testing, and grammar, respectively."),
    #     tags$p("Funded by the AMOSO foundation and PSI),
    #     tags$img(src = "images/amoso-logo.png", alt = "amoso logo")
    #   )
    # )


    # nav_spacer(),
    #
    # nav_menu(
    #   title = "Links",
    #   nav_item(
    #     tags$a(
    #       shiny::icon("github"), "", href = "https://github.com/rstudio/shiny"))
    # )
  )

  server <- function(input, output, session) {
    options(shiny.maxRequestSize = 50 * 1024^2)


    source("R/util_process_excel.R", local = TRUE)$value
    source("R/util_PCA_function.R", local = TRUE)$value
    source("R/util_create_PCA_customization.R", local = TRUE)$value
    source("R/util_create_PCA_by_CPM.R", local = TRUE)$value
    source("R/util_create_PCA_by_Q3.R", local = TRUE)$value
    source("R/util_create_PCA_by_RUV4.R", local = TRUE)$value
    source("R/util_differential_gene_exp.R", local = TRUE)$value
    source("R/util_create_table.R", local = TRUE)$value
    source("R/util_create_volcano.R", local = TRUE)$value
    source("R/util_create_heatmap.R", local = TRUE)$value




    # shiny::observeEvent(input$run,{
    #   if (input$main == "introPage"){
    #     nav_select(id = "main", selected = "PCA")
    #   }
    # })
    ## --------------------Sidebar observe--------------------------------------
    .observeEvent_sidebar(input, output)

    ## --------------------Sidebar outputs--------------------------------------
    source("R/output_sidebar.R", local = TRUE)$value
    .outputSidebar(input, output)

    ## --------------------PCA nav panel observe--------------------------------
    .observeEvent_pca_nav_panel(input)

    ## --------------------PCA nav panel outputs--------------------------------
    source("R/output_pca_nav_panel.R", local = TRUE)$value
    .outputPcaNavPanel(input, output)


    ## --------------------Table nav panel output-------------------------------
    source("R/output_table_nav_panel.R", local = TRUE)$value
    .outputTableNavPanel(input, output)

    ## --------------------Volcano observe--------------------------------------
    # nocov start
    shiny::observeEvent(input$toggleCustomRange, {
      shinyjs::toggle("showCustomRange")
    })
    # nocov end

    ## --------------------Volcano outputs--------------------------------------
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
    # output$downloadVolcano <- shiny::downloadHandler(
    #   filename = function() {
    #     paste0("volcano_", Sys.Date(), ".png")
    #   },
    #   content = function(file) {
    #     cowplot::save_plot(file, volcanoPlots())
    #   },
    #   contentType = "image/png"
    # )
    # no cov

    #### --------------------Heatmap outputs---------------------------------------


    top_n_genes <- shiny::reactive({
        input$top_n_genes
    })

    heatmap_col <- shiny::reactive({
        input$heatmap_col
    })

    heatmap_range <- shiny::reactive({
        input$heatmap_range
    })

    heatmap_size <- shiny::reactive({
        input$heatmap_size
    })

    heatmap_fontsize <- shiny::reactive({
        input$heatmap_fontsize
    })
    #
    #
    #
    #
    observe({
      print(head(lcpm_subset_scale()))
    })

    #
    output$heatmapUI <- shiny::renderUI({
        shiny::renderPlot(
            heatmap(),
            height = 1.5 * as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1]
            # height = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[1],
            # width = as.numeric(heatmap_size()) * dim(lcpm_subset_scale_topGenes())[2]
        )
    })
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
  shiny::shinyApp(ui = ui, server = server)
}
