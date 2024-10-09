topTabDF <- shiny::reactive({
    # If there are more than two groups, must sort by F, not P...
    # but it might sort by default? could be redundant


    if (length(input$selectedTypes) > 2) {
        dt <- limma::topTable(efit(), coef = seq_len(ncol(contrast())), number = Inf,
                              p.value = 0.05, sort.by = "F", adjust.method = "BH",
                              lfc = lfc()) %>%
            tibble::rownames_to_column(var = "Gene") %>%
            dplyr::select(!c("ProbeName", "GeneID", "HUGOSymbol", "ProbeDisplayName",
                             "Accessions", "GenomeBuild", "AnalyteType", "CodeClass",
                             "ProbePool", "TargetGroup"))
        # mutate(across(which(is.numeric))) is not compatible with renderDT which is a 'datatable' object.

        # this part cannot be piped together with the previous section. ncol(dt) does not evaluate
        # columns = -c(1:2) does not work
        # keep 4 sigfigs for all columns up to n except for the first two
        # dt_sigfigs <- dt %>% datatable() %>%  formatSignif(columns=c(3:ncol(dt)), digits=4)
    } else {
        dt <- limma::topTable(efit(), coef = seq_len(ncol(contrast())), number = Inf,
                              p.value = 0.05, sort.by = "P", adjust.method = "BH",
                              lfc = lfc()) %>%
            tibble::rownames_to_column(var="Gene") %>%
            dplyr::select(!c('ProbeName','GeneID', 'HUGOSymbol', 'ProbeDisplayName',
                             'Accessions', 'GenomeBuild', 'AnalyteType', 'CodeClass',
                             'ProbePool', 'TargetGroup'))
    }
    return(dt)

})
