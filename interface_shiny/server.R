

server <- function(input, output) {
  typeA_prec <- reactive({ 
    switch(input$typeA,
           selectInput("typeB","Type précis du sujet:",sub_categories[[input$typeA]]))
    
  })
  
  output$typeA_prec <- renderUI({typeA_prec()})
  
  predicat_typeA <- reactive({
    switch(input$typeA,
    selectInput("predicat","Précision sur le sujet:",predicates_categories[[input$typeA]])
    )
  })
  
  
  predicat <- reactive({
    switch(input$typeB,
           "All" = predicat_typeA(),
           selectInput("predicat","Précision sur le sujet:",predicates_categories[[input$typeB]])
    )
  })
    
  output$uipredicat <- renderUI({predicat()})
  
  output$table <- renderDataTable(query_DBpedia(typeA = input$typeA,
                                                typeAprec=input$typeB,
                                                namesubject=input$nameSubject,
                                                verb=input$predicat,
                                                nameobject = input$nameObject,
                                                nbresults=input$slider))

  
}
