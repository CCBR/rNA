library(shiny)
library(tibble)
#library(shiny.semantic)
library(jsonlite)
library(RCurl)
library(DT)
library(shinyBS)
library(httr)
library(knitr)
source("download_tools.R")

#https://rdrr.io/github/Appsilon/shiny.semantic/man/register_search.html

#https://rinterface.com/shiny/talks/RPharma2020/?panelset1=r-code2#34
rv <- reactiveValues(row=0)
mysession<-reactiveValues()
mysession[['token']] = "eyJhbGciOiJIUzI1NiJ9.eyJEYXRhTWFuYWdlbWVudEFjY291bnQiOiJ7XCJwYXNzd29yZFwiOlwiOChpT2tMLD5cIixcImludGVncmF0ZWRTeXN0ZW1cIjpcIklST0RTXCIsXCJwcm9wZXJ0aWVzXCI6e1wiUFJPWFlfTkFNRVwiOlwiXCIsXCJQT1JUXCI6XCIxMjQ3XCIsXCJERUZBVUxUX1NUT1JBR0VfUkVTT1VSQ0VcIjpcIm5jaWZwcm9kUmVzY1wiLFwiSE9NRV9ESVJFQ1RPUllcIjpcIlwiLFwiWk9ORVwiOlwibmNpZnByb2Rab25lXCIsXCJIT1NUXCI6XCJmc2RtZWwtaXJvZHMwMXAubmNpZmNyZi5nb3ZcIixcIlBST1hZX1pPTkVcIjpcIlwifSxcInVzZXJuYW1lXCI6XCJrcmFtZXJyc1wifSIsIlVzZXJOYW1lIjoia3JhbWVycnMiLCJEYXRhTWFuYWdlbWVudEFjY291bnRFeHBpcmF0aW9uIjoxNjI2MjEyNzIxOTg5LCJleHAiOjE4MDYxODM5MjF9.CgQkzv6xIalA6J5oOiqLpCMspirCCRKX4ktVPTsIHtY"

  
mysession[["selected_path"]]<-NULL
dme_url="https://hpcdmeweb.nci.nih.gov/sso?redirect_uri="


ndf<-NULL

uiapp = fluidPage(navbarPage(id = "NB","DME Browser!",
                   
                   tabPanel(tabName="Projects","Projects",
                            DTOutput('table')
                   ),
                   tabPanel(tabName="MultiQC","MultiQC",
                            htmlOutput("inc")
                   ),
                   tabPanel(tabName="rNAQC","rNA QC",
                            htmlOutput("inc2")
                   ),
                   tabPanel(tabName="Authenticate","Authenticate",
                            fluidRow(
                              
                              uiOutput("open_url")
                            )
                   )
))

shinyApp(
  #ui = uiapp
  ui = uiapp
  ,
  server = function(input, output, session) {

      output$table <-
        renderDT({
          ndf<<-get_projects(mysession[["token"]],session$user)
          ndf[, c(
          "project_title",
          "project_affiliation",
          "project_description",
          "method",
          "data_owner",
          "access",
          "number_of_cases"
        )]}, selection = "single")
        
   
    
    
    
    
    search_api_url <-
      session$registerDataObj("auth", list(), function(data,
                                                       request) {
        reqInput <- request$rook.input
        
        # read a chuck of size 2^16 bytes, should suffice for our test
        buf <- reqInput$read(2 ^ 16)
        tok <- fromJSON(rawToChar(buf))
        print(tok$token)
        mysession[['token']] <<- tok$token
        
        shiny:::httpResponse(
          200,
          'text/html; charset=UTF-8',
          "Thank you for authenticating, you may close this window and resume using DME browser."
        )
      })
    
    output$open_url <- renderUI({
      tags$a(
        "Open",
        class = "ui button",
        href = glue::glue(
          dme_url,
          "{session$clientData$url_protocol}//{session$clientData$url_hostname}",
          "/dme2/",
          "{search_api_url}",
          "&urlencoded=false"
        ),
        target = "_blank"
      )
    })
    
    
    
     
  
     observeEvent(input$table_cell_clicked, {
       if(length(input$table_rows_selected)>0){
         showModal(modalDialog({
           renderDataTable({
             
             print(input$table_rows_selected)
             t(as.data.frame(ndf[input$table_rows_selected, ]))
             
             
           },
           colnames = c(""), options = list(
             paging = FALSE,
             processing = FALSE,
             searching = FALSE,
             initComplete = I("function(settings, json) {alert('Done.');}")
           )
           )},
           footer = tagList(
             modalButton("Cancel"),
             actionButton("ViewQC", "View QC")
           )
         ))
         
       }
    })
     observeEvent(input$ViewQC, {
       print(mysession[["selected_path"]])
       mysession[["selected_path"]]<-ndf[input$table_rows_selected,"path" ]
       removeModal()
     
       print(mysession[["selected_path"]])
       updateTabsetPanel(session, "NB",
                         selected = "rNA QC")
    
     }) 
     output$inc<-renderUI({
       print("rendering")
       path<-mysession[["selected_path"]]
       token<-mysession[['token']]
       print(path)
       if (!is.null(token) & !is.null(path)) {
         
         
      
         
         html_std<-getfilelocation('"%multiqc_report.html%"',path,token,session$user)
        html_file<-getfile(path=html_std,filename="www/multiqc_report_basic.html",token,session$user)
        print("done download")
        tags$iframe(src = "multiqc_report_basic.html", seamless=TRUE, height=1800, width=1920)
         
       }
       
       
       })
     output$inc2<-renderUI({
       print("rendering")
       path<-mysession[["selected_path"]]
       token<-mysession[['token']]
       print(path)
       if (!is.null(token) & !is.null(path)) {
         
         
         print("1")
         multiqcpath<-getfilelocation(pattern='"%multiqc_matrix.tsv%"',collection=path,token,session$user)
         print("1.1")
         print(multiqcpath)
         multiqc<-getfile(path=multiqcpath,filename="multiqc_matrix.tsv",token,session$user)
         print(multiqc)
         print("2")
         RSEMpath<-getfilelocation('"%RSEM_genes_expected_counts.tsv%"',path,token,session$user)
         RSEM<-getfile(path=RSEMpath,filename="RSEM_genes_expected_counts.tsv",token,session$user)
         print(RSEM)
         print("3")
         TINpath<-getfilelocation('"%combined_TIN.tsv%"',path,token,session$user)
         TIN<-getfile(path=TINpath,filename="combined_TIN.tsv",token,session$user)
         print(TIN)
         myhtml<-function(raw,tin,qc){return(markdown::markdownToHTML(rmarkdown::render('rNA_flowcells.Rmd', quiet = TRUE, output_file= "www/qcreport.html",params = list(
           #title="test123",
           raw=raw,
           tin=tin,
           qc=qc,
           # raw="data/Test_Raw_RSEM_Genes_Dataset.txt",
           #    tin="data/Test_TIN_Dataset.txt",
           #    qc="data/multiqc_matrix.tsv",
           wdir=".",
           # odir=".",
           annot= "FALSE"
         ))))}
         
         #  html_std<-getfilelocation('"%multiqc_report.html%"',path,token,session$user)
         # html_file<-getfile(path=html_std,filename="www/multiqc_report.html",token,session$user)
         mysession[["html_v"]]<-myhtml(raw=RSEM,tin=TIN,qc=multiqc)
         print("done download")
       }
       
       tags$iframe(src = "qcreport.html", seamless=TRUE, height=1800, width=1920)
     })
  }
)



