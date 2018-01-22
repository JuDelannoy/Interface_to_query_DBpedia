

server <- function(input, output) {
  typeA_prec <- reactive({ 
    switch(input$typeA,
           "Activity" = selectInput("typeB","Type précis du sujet:",sub_categories$Activity),
           "Organisation" = selectInput("typeB","Type précis du sujet:",sub_categories$Organisation),
           "Person" = selectInput("typeB","Type précis du sujet:",sub_categories$Person),
           "Place" = selectInput("typeB","Type précis du sujet:",sub_categories$Place),
           "Work" = selectInput("typeB","Type précis du sujet:",sub_categories$Work),
           selectInput("typeB","Type précis du sujet:",c("No subcategory")))
    
  })
  
  output$typeA_prec <- renderUI({typeA_prec()})
  
  output$table <- renderDataTable(query_DBpedia(typeA = input$typeA,
                                                typeAprec=input$typeB,
                                                nbresults=input$slider))
  
  #output$typeA_prec <- typeA_prec
  
  #output$table <- renderDataTable(affiche_element(output$type_prec))
  
}
