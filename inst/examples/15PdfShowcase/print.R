library(future)

autoInvalidate <- reactiveTimer(1000)

observe({
    autoInvalidate()

    if (!is.null(s1$f)) {

        if (resolved(s1$f)) {

            s1$printProgress$set(message = "Finished", value = 1)
            s1$printProgress$close()
            shinyjs::runjs('$.unblockUI();')

            if (batchMode) {
                if (batchArgs[1] == 'MP')
                    batchMPState$count <- batchMPState$count + 1
                if (batchArgs[1] == 'MR')
                    batchMRState$count <- batchMRState$count + 1
            }

            ret <- value(s1$f)

            if (ret$status == 'goed') {

                s1$overzicht$tmpDir <- ret$tmpDir

                if (!batchMode) {
                    showModal(modalDialog(
                        downloadButton("pdf", "pdf"),
                        footer = NULL,
                        size = 's',
                        fade = TRUE))
                } else {
                    file.copy(from = paste0(ret$tmpDir,'/tmp.pdf'), to = paste0(s1$overzicht$outDir,'/',s1$overzicht$pdfFileName), overwrite = TRUE)
                }

            } else {
                shinyjs::alert('Fout bij genereren pdf')
                shinyjs::logjs(ret$txt)
            }

            s1$f <- NULL
        } else {

            s1$progressCount <- s1$progressCount + 2
            s1$printProgress$set(message = "Genereren pdf", value = s1$progressCount/10)
        }

    }
    
    if (!is.null(s1$g)) {
        
        if (resolved(s1$g)) {
            printMPState$state <- 'print'
            s1$g <- NULL
        } else {
            s1$progressCount <- s1$progressCount + 1
            s1$printProgress$set(message = "Transformeren", value = s1$progressCount/10)
        }
        
    }
})

output$pdf <- downloadHandler(
    filename = function() {
        s1$overzicht$pdfFileName
    },
    
    content = function(file) {
        removeModal()
        file.rename(paste0(s1$overzicht$tmpDir,'/tmp.pdf'), file)
        
    }
)

