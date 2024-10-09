options(shiny.maxRequestSize = 30 * 1024^2)


annotation_condition <- c(
    "fibroblast_IPF", "fibrosis_IPF", "neutral_IPF",
    "inflammatory_NSIP", "central_NSIP", "peripheral_NSIP"
)



# A named list organizes annotations by name (condition)
annotation_condition_list <- list(
    IPF = c("fibroblast_IPF", "fibrosis_IPF", "neutral_IPF", "lymphoid_IPF"),
    NSIP = c("central_NSIP", "inflammatory_NSIP", "peripheral_NSIP", "airway_NSIP"),
    CHP = c("granuloma_CHP", "inflammatory_CHP", "fibrosis_CHP", "neutral_CHP"),
    UNC = c("fibroblast_UNC", "lymphoid_UNC", "fibrosis_UNC", "neutral_UNC"),
    Normal = c("peripheral_NOR", "central_NOR", "airway_NOR", "pleura_NOR")
)

# sources
# source("R/interface_pca_nav_panel.R", local = TRUE)
# source("R/interface_sidebar.R", local = TRUE)





# Define UI ----
.build_ui <- function(...) {
    bslib::page_navbar(
        htmltools::tags$head(htmltools::tags$link(rel = "shortcut icon", href = "favicon.ico/lung.png")),
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
