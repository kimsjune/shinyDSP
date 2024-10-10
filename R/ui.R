options(shiny.maxRequestSize = 30 * 1024^2)


#' Create UI block for Shiny
#'
#' @return UI for [shiny::shinyApp()]
#' @keywords internal
#'
#' @author Seung J. Kim
.build_ui <- function() {
  bslib::page_navbar(
    htmltools::tags$head(htmltools::tags$link(rel = "shortcut icon", href = "favicon.ico/logo.png")),
    htmltools::tags$style(
      "
    .introText {
    padding-left: 100px;
    padding-right: 100px;
    }
    "
    ),
    shinyjs::useShinyjs(),
    title = "ShinyDSP",
    id = "main",
    fillable = TRUE,
    sidebar = .interfaceSidebar(),
    .interfaceIntroNavPanel(),
    .interfacePcaNavPanel(),
    .interfaceTableNavPanel(),
    .interfaceVolcanoNavPanel(),
    # .create_heatmap_nav_panel()







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
}
