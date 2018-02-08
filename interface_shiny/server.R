

server <- function(input, output) {
  
  #########################################################################################################################################
  #SUBJECT
  typeA_prec <- reactive({ 
    switch(input$typeA,
           selectInput("typeB",h4("Precision about the type of the subject"),sub_categories[[input$typeA]],selected = "All"))
    
  })
  
  output$typeA_prec <- renderUI({typeA_prec()})

  
  #########################################################################################################################################
  #PREDICATE1
  
  #SELECT INPUT FOR PREDICATE 1
  output$uipredicat <- renderUI({predicat1()})
  
  predicat1 <- reactive({
    switch(input$typeB,
           "All" = predicat1_typeA(),
           selectInput("predicat",h4("Information you want about the subject"),predicates_categories[[input$typeB]],selected = "no"))
  })
  
  predicat1_typeA <- reactive({
    switch(input$typeA,
    selectInput("predicat",h4("Information you want about the subject"),predicates_categories[[input$typeA]])
    )
  })


  #TYPE OF PLACE for located predicates
  #SELECT INPUT WITH CITY/COUNTRY WHEN PLACE
  
  output$typeofplace <- renderUI({placetype1()})
  
  placetype1 <- reactive({
    if (input$predicat == "no"){
      selectInput("typeofplace",
                  h4("Precision"),
                  c("All"),selected = "All")}

    #if the choose subject has its "unprecise_place" column filled by TRUE, you can specify the type of place
    else if (predicates_dictionnary$unprecise_place[which(input$predicat == predicates_dictionnary$subtitle)] == TRUE){
    selectInput("typeofplace",
                h4("Precision"),
                c("All","City","Country"),selected = "All")}
    else{
      selectInput("typeofplace",
                  h4("Precision"),
                  c("All"),selected = "All")}
  })
  

  
  #RANGE DATE for temporal predicates
  
  output$uirangedatemin <- renderUI({rangedatemin()})
  
  rangedatemin <- reactive({switch(grepl("date",input$predicat)|grepl("epoch",input$predicat),
                                   "TRUE" = numericInput("rangedatemin", label = h4("minimum date (yyyy)"), value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  output$uirangedatemax <- renderUI({rangedatemax()})
  
  rangedatemax <- reactive({switch(grepl("date",input$predicat)|grepl("epoch",input$predicat),
                                   "TRUE" = numericInput("rangedatemax", label = h4("maximum date (yyyy)"), value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  
  
  #########################################################################################################################################
  #PREDICATE2  
  
  #SELECT INPUT FOR PREDICATE 2
  output$uipredicat2 <- renderUI({predicat2()})
  
  predicat2 <- reactive({
    switch(input$typeB,
           "All" = predicat2_typeA(),
           selectInput("predicat2",h4("Information you want about the subject"),predicates_categories[[input$typeB]],selected = "no")
    )
  })
  
  predicat2_typeA <- reactive({
    switch(input$typeA,
           selectInput("predicat2",h4("Information you want about the subject"),predicates_categories[[input$typeA]])
    )
  })
  
  
  #TYPE OF PLACE for located predicates
  #SELECT INPUT WITH CITY/COUNTRY WHEN PLACE
  
  output$typeofplace2 <- renderUI({placetype2()})
  
  placetype2 <- reactive({
    if (input$predicat2 == "no"){
      selectInput("typeofplace2",
                  h4("Precision"),
                  c("All"),selected = "All")}
    
    #if the choose subject has its "unprecise_place" column filled by TRUE, you can specify the type of place
    else if (predicates_dictionnary$unprecise_place[which(input$predicat2 == predicates_dictionnary$subtitle)] == TRUE){
      selectInput("typeofplace2",
                  h4("Precision"),
                  c("All","City","Country"),selected = "All")}
    else{
      selectInput("typeofplace2",
                  h4("Precision"),
                  c("All"),selected = "All")}
  })
  
  
  #RANGE DATE for temporal predicates
  output$uirangedatemin2 <- renderUI({rangedatemin2()})
  
  rangedatemin2 <- reactive({switch(grepl("date",input$predicat2)|grepl("epoch",input$predicat2),
                                   "TRUE" = numericInput("rangedatemin2", label = h4("minimum date (yyyy)"), value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  

  output$uirangedatemax2 <- renderUI({rangedatemax2()})
  
  rangedatemax2 <- reactive({switch(grepl("date",input$predicat2)|grepl("epoch",input$predicat2),
                                   "TRUE" = numericInput("rangedatemax2", label = h4("maximum date (yyyy)"), value = "yyyy", min = NA, max = NA, step = NA,
                                                         width = NULL))})
  

   
  
  #########################################################################################################################################  
  #PLOT RESULTS
  
    output$table <- renderDataTable({querying()})
  
    querying <- eventReactive(input$goButton,{query_DBpedia(typeA = input$typeA,
                                                            typeAprec=input$typeB,
                                                            placesubject = input$placesubject,
                                                            namesubject=input$nameSubject,
                                                            exactsubject = input$exactsubject,
                                                            verb = input$predicat,
                                                            secondpredicate = input$nbpredicates,
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
      #if the subject is located
      if (locatedsubject()){
        plotmappreciseplace()}
      else if (locatedobject1()){
        #if the oject 1 and the object 2 are located
        if(locatedobject2()){
          plotmapobjects()}
        #if only the object 1 is located
        else {plotmapobject1()}
      }
      #if only the object 2 is located
      else if (locatedobject2()){plotmapobject2()}
    })
    

#if a column is called latitude, then the subject is located    
locatedsubject <- reactive(
  "latitude" %in% colnames(querying())
)    

#if a column is called latitude_of_the_place, then the object1 is located    
locatedobject1 <- reactive(
  "latitude_of_the_place" %in% colnames(querying())
)    

#if a column is called latitude_of_the_place2, then the object2 is located   
locatedobject2 <- reactive(
  "latitude_of_the_place2" %in% colnames(querying())
)

#plotting the map

#map for located subject
#(if the object is also located, only the subject is plot as more precise - an event is more precisely located than the country itself)
plotmappreciseplace <- reactive(
  if (input$predicat == "no"){
    leaflet(data = querying()) %>% addTiles() %>%
      addMarkers(~as.numeric(longitude), ~as.numeric(latitude),label = ~as.character(querying()[[input$typeA]]),
                 popup = ~as.character(paste0(
                   strong("Name :"),
                   querying()[[input$typeA]])))
  }
  #if there are some information about the object in the table, it will be plot on the map too
  else {
    leaflet(data = querying()) %>% addTiles() %>%
      addMarkers(~as.numeric(longitude), ~as.numeric(latitude),label = ~as.character(querying()[[input$typeA]]),
                 popup = ~as.character(paste0(
                   strong("Name :"),
                   querying()[[input$typeA]],
                   br(),
                   strong(input$predicat),":",
                   querying()[[input$predicat]])))
  }
)

#map for located object1
plotmapobject1 <- reactive(
  leaflet(data = querying()) %>% addTiles() %>%
    addMarkers(~as.numeric(longitude_of_the_place),
               ~as.numeric(latitude_of_the_place),
               label = ~as.character(querying()[[input$typeA]]),
               popup = ~as.character(paste0(
                 strong("Name :"),
                 querying()[[input$typeA]],
                 br(),
                 strong(colnames(querying())[grep("place",colnames(querying()))[1]]),":",
                 querying()[[grep("place",colnames(querying()))[1]]])))
)

#map for located object2
plotmapobject2 <- reactive(
  leaflet(data = querying()) %>% addTiles() %>%
    addMarkers(~as.numeric(longitude_of_the_place2),
               ~as.numeric(latitude_of_the_place2),
               label = ~as.character(querying()[[input$typeA]]),
               popup = ~as.character(paste0(
                 strong("Name :"),
                 querying()[[input$typeA]],
                 br(),
                 strong(colnames(querying())[grep("place",colnames(querying()))[1]]),":",
                 querying()[[grep("place",colnames(querying()))[1]]])))
)

#map for both located object 1 and 2
#both are plotted, the object2 with another marker
#on the popup you have information on both object -whatever object 1 or 2 is clicked)
plotmapobjects <- reactive(
  leaflet(data = querying()) %>% addTiles() %>%
    addMarkers(~as.numeric(longitude_of_the_place),
               ~as.numeric(latitude_of_the_place),
               label = ~as.character(querying()[[input$typeA]]),
               popup = ~as.character(paste0(
                 strong("Name :"),
                 querying()[[input$typeA]],
                 br(),
                 strong(colnames(querying())[grep("place",colnames(querying()))[1]]),"(here):",
                 querying()[[grep("place",colnames(querying()))[1]]],
                 br(),
                 strong(colnames(querying())[grep("place",colnames(querying()))[2]]),":",
                 querying()[[grep("place",colnames(querying()))[2]]])))%>%
    addMarkers(~as.numeric(longitude_of_the_place2),
               ~as.numeric(latitude_of_the_place2),
               label = ~as.character(querying()[[input$typeA]]),icon=~coloredIcons["orange"],
               popup = ~as.character(
                 paste0(
                   strong("Name :"),
                   querying()[[input$typeA]],
                   br(),
                   strong(colnames(querying())[grep("place",colnames(querying()))[1]]),":",
                   querying()[[grep("place",colnames(querying()))[1]]],
                   br(),
                   strong(colnames(querying())[grep("place",colnames(querying()))[2]]),"(here):",
                   querying()[[grep("place",colnames(querying()))[2]]])))
                 
               )



#function to choose a different marker (in case of 2 spatialized objects)
coloredIcons <- iconList(green = makeIcon("DATA/green_marker.png", iconWidth = 32),
                         red = makeIcon("DATA/red_marker.png", iconWidth = 32),
                         orange = makeIcon("DATA/orange_marker.png", iconWidth = 32))

} 
