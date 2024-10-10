#' Create the "QC" nav panel
#'
#' @return [bslib::nav_panel()]
#' @keywords internal
#'
#' @author Seung J. Kim
.interfaceIntroNavPanel <- function() {
  bslib::nav_panel(
    "QC",
    id = "QCpage",
    value = "QCpage",
    # htmlOutput(""),
    # tags$div(class = "row",
    # tags$div(class = "",
    #          tags$h3(""),

    # ),

    # tags$div(class = "introText",
    #          # tags$div(
    #          tags$h3("Citation"),
    #          # p("If you use this resource, please cite ",
    #          #   a(href="TBD.com", "Kim et. al. 2024")),
    #          # ),
    #          # tags$div(
    #          #  tags$h3("Links"),
    #
    #          # p(
    #          #   a(shiny::icon("github"), " ",
    #          #     style = "padding: 10px; text-decoration: none;",
    #          #     href = "https://github.com/rstudio/shiny")
    #          # a(shiny::icon("linkedin-in")," ",
    #          #   style = "padding: 10px; text-decoration: none;",
    #          #   href= "https://www.linkedin.com/in/joon-kim-7a140b90/")
    #          #   )
    # )
  )
}
