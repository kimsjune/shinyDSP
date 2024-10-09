.interfaceIntroNavPanel <- function(){
    bslib::nav_panel(
        "Introduction",
        id = "introPage",
        value = "introPage",
        #htmlOutput("introduction"),
        # tags$div(class = "row",
        # tags$div(class = "introText",
        #          tags$h3("Background"),
        #          p("Fibrosing interstitial lung diseases (ILDs) constitute a diverse group of scarring disorders of the lungs that cause progressive respiratory failure. The most common fibrosing ILDs are idiopathic pulmonary fibrosis (IPF), chronic hypersensitivity pneumonitis (CHP) and non-specific interstitial pneumonia (NSIP). Despite some therapeutic advances, fibrosing ILDs are the leading indication for lung transplantation worldwide. Furthermore, the inherent diversity of fibrosing ILDs poses a challenge to diagnostic agreement and clinical management. Thus, there is an urgent need to dissect their molecular underpinnings to develop new diagnostic tools and better match patients with specific treatments."),
        #          p("Recently, gene expression profiles have been leveraged to delineate these diseases from each other, and to gain insight into the mechanism of disease progression. However, the current literature falls short in several important aspects. First, traditional bulk tissue RNA-seq data cannot depict the complexity of IPF and CHP that are characterized by regional, temporal and cellular heterogeneity. Indeed, recent single cell RNA-seq (scRNA-seq) studies have shown that diverse cell types are found in the lung. Second, the use of explant lung samples from advanced disease is often overlooked. Deregulated gene expression profiles in end-stage disease are unlikely to be actionable targets to reverse disease progression, especially for fibrosis. Lastly, there is a relative paucity of gene expression data for CHP and NSIP compared to IPF.")
        # ),
        # tags$div(class = "introText",
        #          tags$h3("Scope"),
        #          p("This app allows users to interactively visualize spatial transcriptomics data. Select any number of annotation-ILD subtype from the left bar, and hit run. Four output types are available with a few customization options: PCA, table, Volcano and Heatmap."),
        #          tags$h3("Rationale"),
        #          p("This app was created to facilitate open access to our data and biological interpretation. This is more challenging for spatial transcriptomics data compared to traditional bulk RNA-seq. In bulk RNA-seq, there are generally fewer groups and logical pairwise comparisons. For example, there might be 2-3 treatment groups across one or two categorical variables such as genotype. The number of comparisons is limited, and the full breadth of analysis can be conveyed in a manuscript. On the other hand, there are far more biological groups, and subsequently more comparisons, that can be made in spatial transcriptomic data. For example, there are 20 biological groups in our data. It is often reasonable to choose more than two groups per analysis, such as comparing all 4 annotations within one ILD subtype. Theoretically, there are 1 048 555 combinations of k choices (where k = 2~20) in total. Although most of these may not be biologically meaningful, even their subset cannot be represented easily in a manuscript. With this app, clinicians and scientists can easily explore the data and draw conclusions."),
        #          # withMathJax(p(style ="inline-block",  "\\(\\displaystyle\\sum_{k=2}^{20}\\binom{20}{k}\\)"))
        # ),
        # #       ),
        # # tags$div(class = "row",
        # tags$div(class = "introText",
        #          # tags$div(
        #          tags$h3("Conditions and annotations"),
        #          tags$div(style = "text-align: center;",
        #                   tags$img(src = "images/alluvial_plot.svg", alt = "alluvial")
        #          )
        #
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
