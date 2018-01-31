

server <- function(input, output) {
  
  #########################################################################################################################################
  #SUBJECT
  typeA_prec <- reactive({ 
    switch(input$typeA,
           selectInput("typeB","Precision about the type of the subject:",sub_categories[[input$typeA]],selected = "All"))
    
  })
  
  output$typeA_prec <- renderUI({typeA_prec()})

  
  #########################################################################################################################################
  #PREDICATE1
  
  #SELECT INPUT FOR PREDICATE 1
  output$uipredicat <- renderUI({predicat1()})
  
  predicat1 <- reactive({
    switch(input$typeB,
           "All" = predicat1_typeA(),
           selectInput("predicat","Information you want about the subject:",predicates_categories[[input$typeB]],selected = "no"))
  })
  
  predicat1_typeA <- reactive({
    switch(input$typeA,
    selectInput("predicat","Information you want about the subject:",predicates_categories[[input$typeA]])
    )
  })


  #TYPE OF PLACE for located predicates
  #SELECT INPUT WITH CITY/COUNTRY WHEN PLACE
  
  output$typeofplace <- renderUI({placetype1()})
  
  placetype1 <- reactive({
    if (input$predicat == "no"){
      selectInput("typeofplace",
                  "Precision:",
                  c("All"),selected = "All")}

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
  

  
  #RANGE DATE for temporal predicates
  
  output$uirangedatemin <- renderUI({rangedatemin()})
  
  rangedatemin <- reactive({switch(grepl("date",input$predicat),
                                   "TRUE" = numericInput("rangedatemin", label = "minimum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  output$uirangedatemax <- renderUI({rangedatemax()})
  
  rangedatemax <- reactive({switch(grepl("date",input$predicat),
                                   "TRUE" = numericInput("rangedatemax", label = "maximum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  
  #########################################################################################################################################
  #PREDICATE2  
  
  #SELECT INPUT FOR PREDICATE 2
  output$uipredicat2 <- renderUI({predicat2()})
  
  predicat2 <- reactive({
    switch(input$typeB,
           "All" = predicat2_typeA(),
           selectInput("predicat2","Information you want about the subject:",predicates_categories[[input$typeB]],selected = "no")
    )
  })
  
  predicat2_typeA <- reactive({
    switch(input$typeA,
           selectInput("predicat2","Information you want about the subject:",predicates_categories[[input$typeA]])
    )
  })
  
  
  #TYPE OF PLACE for located predicates
  #SELECT INPUT WITH CITY/COUNTRY WHEN PLACE
  
  output$typeofplace2 <- renderUI({placetype2()})
  
  placetype2 <- reactive({
    if (input$predicat2 == "no"){
      selectInput("typeofplace2",
                  "Precision:",
                  c("All"),selected = "All")}
    
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
  
  
  #RANGE DATE for temporal predicates
  output$uirangedatemin2 <- renderUI({rangedatemin2()})
  
  rangedatemin2 <- reactive({switch(grepl("date",input$predicat2),
                                   "TRUE" = numericInput("rangedatemin2", label = "minimum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  

  output$uirangedatemax2 <- renderUI({rangedatemax2()})
  
  rangedatemax2 <- reactive({switch(grepl("date",input$predicat2),
                                   "TRUE" = numericInput("rangedatemax2", label = "maximum date (yyyy)", value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  

   
  
  #########################################################################################################################################  
  #PLOT RESULTS
  
    output$table <- renderDataTable({querying()})
  
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
  

  
  #########################################################################################################################################  
  #EXPORT
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dbpediaExtract", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(querying(), file, row.names = FALSE)
    }
  )
  
  
  #########################################################################################################################################
    #MAP
    
    output$mymap <- renderLeaflet({
      # define the leaflet map object
      leaflet() %>%
        addTiles() %>%
        #setView(lng = 2.35, lat = 48.85 , zoom = 2) %>%
        addMarkers(lng = 78.0419, lat = 27.1750, popup = "Taj Mahal, Agra, India") %>%
        addMarkers(lng = 7.0419, lat = 7.1750, popup = "Taj Mahal, Agra, India") #%>%
        #addPopups(lng = 78.0419, lat = 27.1750, popup = "Taj Mahal, Agra, India") 
      
    })
    
}

