

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
  
  
  predicat2 <- reactive({
    switch(input$typeB,
           "All" = predicat_typeA(),
           selectInput("predicat2","Information you want about the subject:",predicates_categories[[input$typeB]],selected = "no")
    )
  })
  
  output$uipredicat2 <- renderUI({predicat2()})

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
  
  placetype2 <- reactive({
    if (input$predicat2 == "no"){
      selectInput("typeofplace2",
                  "Precision:",
                  c("All"),selected = "All")}
    
    #object
    #if the choose subject has its "unprecise_place" column filled by TRUE, you can specify the type of place
    else if (predicates_dictionnary$unprecise_place[which(input$predicat2 == predicates_dictionnary$subtitle)] == TRUE){
      selectInput("typeofplace2",
                  "Precision:",
                  c("All","City","Country"),selected = "All")}
    else{
      selectInput("typeofplace2",
                  "Precision:",
                  c("All"),selected = "All")}
  })
  
  output$typeofplace2 <- renderUI({placetype2()})

  rangedatemin <- reactive({switch(grepl("date",input$predicat),
         "TRUE" = numericInput("rangedatemin", label = "minimum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                               width = NULL))})
  
  output$uirangedatemin <- renderUI({rangedatemin()})
  
  rangedatemax <- reactive({switch(grepl("date",input$predicat),
                                   "TRUE" = numericInput("rangedatemax", label = "maximum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  output$uirangedatemax <- renderUI({rangedatemax()})
  
  
  rangedatemin2 <- reactive({switch(grepl("date",input$predicat2),
                                   "TRUE" = numericInput("rangedatemin2", label = "minimum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  output$uirangedatemin2 <- renderUI({rangedatemin2()})
  
  rangedatemax2 <- reactive({switch(grepl("date",input$predicat2),
                                   "TRUE" = numericInput("rangedatemax2", label = "maximum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  output$uirangedatemax2 <- renderUI({rangedatemax2()})
   
  #plot table of results
    querying <- eventReactive(input$goButton,{query_DBpedia(typeA = input$typeA,
                                                            typeAprec=input$typeB,
                                                            placesubject = input$placesubject,
                                                            namesubject=input$nameSubject,
                                                            exactsubject = input$exactsubject,
                                                            verb=input$predicat,
                                                            verb2 = input$predicat2,
                                                            nameobject = input$nameObject,
                                                            exactobject = input$exactobject,
                                                            placeobject = input$typeofplace,
                                                            nameobject2 = input$nameObject2,
                                                            exactobject2 = input$exactobject2,
                                                            placeobject2 = input$typeofplace2,
                                                            mindate = input$rangedatemin,
                                                            maxdate = input$rangedatemax,
                                                            mindate2 = input$rangedatemin2,
                                                            maxdate2 = input$rangedatemax2,
                                                            nbresults = input$slider)}) 
  
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
