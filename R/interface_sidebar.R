.interfaceSidebar <- function() {
    sidebar(
        shinyWidgets::prettySwitch(
            inputId = "useSampleData",
            label = "Use demo data",
            value = FALSE,
            fill = TRUE
        ),
        shinyjs::hidden(
        shiny::fileInput(
            inputId = "uploadedFile",
            label = "Upload your file",
            accept = c(".xlsx")
        )
        ),
        shiny::actionButton(
            inputId = "load",
            "Load",
            style = "display: inline-block; padding: 4px"
        ),

        # shiny::checkboxInput(
        #     inputId = "useSampleData",
        #     label = "Check to use sample data",
        #     value = FALSE
        # ),

        shiny::uiOutput("selectYourType") %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
        shiny::uiOutput("selectYourBatch") %>% shinycssloaders::withSpinner(type = 4, size = 0.5, proxy.height = 50),
        shiny::actionButton(
            inputId = "run",
            "Run",
            style = "display: inline-block; padding: 4px"
        ),
        shiny::tags$hr(),

        shiny::uiOutput("selectYourNorm"),
        shiny::uiOutput("selectYourK"),

        shiny::tags$hr(),


        shiny::numericInput(
            inputId = "lfc",
            label = "log2 fold change cutoff",
            value = 1
        )
        # helpText(
        #   tags$div(
        #     tags$p("IPF: idiopathic pulmonary fibrosis"),
        #     tags$p("NSIP: non-specific interstitial pneumonia"),
        #     tags$p("CHP: chronic hypersensitivity pneumonitis"),
        #     tags$p("UNC: unclassified"),
        #     tags$p("NOR:", em("bona fide"), "normal"),
        #     hr(),
        #     tags$p("neutral: uninvolved"),
        #     tags$p("fibroblast: fibroblastic foci"),
        #     hr(),
        #     tags$p(version)
        #   )
        # ),

        # tags$div(
        #   tags$p(
        #     a(shiny::icon("github"), " ",
        #       style = "padding: 10px; text-decoration: none;",
        #       href = "https://github.com/kimsjune/ild-shiny-app")
        #   )
        # )
    )
}
