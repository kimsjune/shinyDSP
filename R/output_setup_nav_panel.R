.outputSetupNavPanel2 <- function(input, output, rv) { # nocov start
    
    output$countFile <- shiny::renderUI({
        shiny::validate(
            shiny::need(
                shiny::isTruthy(input$useSampleData) ||
                    shiny::isTruthy(input$uploadedCountFile),
                "'Use demo data' or upload your own!"
            )
        )
        DT::renderDT(
            rv$data()$countFile %>%
                dplyr::slice_head(n = 100) %>%
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1a01b71 (Peer review fix: enabled paging, adj. P val cutoff, removed 'turbo' and added all 115 palettes under hcl.pals())
                # To show all 100 rows across five pages
                DT::datatable(
                    options = list(
                        paging = TRUE
                    )
                )
<<<<<<< HEAD
=======
                DT::datatable()
>>>>>>> c11708a (Partial peer review fixes)
=======
>>>>>>> 1a01b71 (Peer review fix: enabled paging, adj. P val cutoff, removed 'turbo' and added all 115 palettes under hcl.pals())
        )
    })  




    
    output$sampleAnnoFile <- shiny::renderUI({
        shiny::req(input$load, rv$data())

        DT::renderDT(
            rv$data()$sampleAnnoFile %>%
                dplyr::slice_head(n = 100) %>%
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1a01b71 (Peer review fix: enabled paging, adj. P val cutoff, removed 'turbo' and added all 115 palettes under hcl.pals())
                # To show all 100 rows across five pages
                DT::datatable(
                    options = list(
                        paging = TRUE
                    )
                )
<<<<<<< HEAD
=======
                DT::datatable()
>>>>>>> c11708a (Partial peer review fixes)
=======
>>>>>>> 1a01b71 (Peer review fix: enabled paging, adj. P val cutoff, removed 'turbo' and added all 115 palettes under hcl.pals())
        )
    })
    
} # nocov end
