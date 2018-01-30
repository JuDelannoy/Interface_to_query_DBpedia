

server <- function(input, output) {
  
  #subject
  typeA_prec <- reactive({ 
    switch(input$typeA,
           selectInput("typeB","Precision about the type of the subject:",sub_categories[[input$typeA]],selected = "Athlete"))
    
  })
  
  output$typeA_prec <- renderUI({typeA_prec()})

  
  
  #predicate
  predicat_typeA <- reactive({
    switch(input$typeA,
    selectInput("predicat","Information you want about the subject:",predicates_categories[[input$typeA]])
    )
  })
  
  
  predicat <- reactive({
    switch(input$typeB,
           "All" = predicat_typeA(),
           selectInput("predicat","Information you want about the subject:",predicates_categories[[input$typeB]],selected = "no")
    )
  })
    
  output$uipredicat <- renderUI({predicat()})

  placetype <- reactive({
    if (input$predicat == "no"){
      selectInput("typeofplace",
                  "Precision:",
                  c("All"),selected = "All")}
    
    
    #object
    #if the choose subject has its "unprecise_place" column filled by TRUE, you can specify the type of place
    else if (predicates_dictionnary$unprecise_place[which(input$predicat == predicates_dictionnary$subtitle)] == TRUE){
    selectInput("typeofplace",
                "Precision:",
                c("All","City","Country"),selected = "All")}
    else{
      selectInput("typeofplace",
                  "Precision:",
                  c("All"),selected = "All")}
  })
  
  output$typeofplace <- renderUI({placetype()})

  rangedatemin <- reactive({switch(grepl("date",input$predicat),
         "TRUE" = numericInput("rangedatemin", label = "minimum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                               width = NULL))})
  
  output$uirangedatemin <- renderUI({rangedatemin()})
  
  rangedatemax <- reactive({switch(grepl("date",input$predicat),
                                   "TRUE" = numericInput("rangedatemax", label = "maximum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  output$uirangedatemax <- renderUI({rangedatemax()})
   
  #plot table of results
    querying <- eventReactive(input$goButton,{query_DBpedia(typeA = input$typeA,
                                                            typeAprec=input$typeB,
                                                            placesubject = input$placesubject,
                                                            namesubject=input$nameSubject,
                                                            exactsubject = input$exactsubject,
                                                            verb=input$predicat,
                                                            nameobject = input$nameObject,
                                                            placeobject = input$typeofplace,
                                                            exactobject = input$exactobject,
                                                            mindate = input$rangedatemin,
                                                            maxdate = input$rangedatemax,
                                                            nbresults=input$slider)}) 
  
  output$table <- renderDataTable({
    #input$goButton
    #isolate(querying())
    querying()
    })
  
  #export
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dbpediaExtract", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(querying(), file, row.names = FALSE)
    }
  )
  
  
}
