library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Let's query Wikipedia !"),
  dashboardSidebar(
    sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", tabName = "widgets", icon = icon("th"))
  )),
  dashboardBody(
    tabItems(
      # dans l'item dashboard
      tabItem(tabName = "dashboard",
    # Boxes need to be put in a row (or column)
    fluidRow(
      #element : select Input
      box(
        title = "Sujet",
        selectInput("typeA",
                    "Type du sujet:",
                    main_categories$type),
        selectInput("typeB",
                    "Précision sur le sujet:",
                    main_categories$type),
        textInput(inputId = "nameSubject", label = "Nom du sujet",
                  value = "optionnel"),
        textInput(inputId = "titleSubject", label = "Précision sur le sujet",
                  value = "optionnel")
      ),
      box(
        title = "Prédicat",
        selectInput("predicat",
                    "Précision sur le sujet:",
                    main_categories$type)
      ),
      box(
        title = "Objet",
        selectInput("objet",
                    "Précision sur le sujet:",
                    main_categories$type)
      )),
    fluidRow(
      box(
        title = "Ordre",
        selectInput("order",
                    "Ordonné par : ",
                    main_categories$type)
      ),
      box(
        title = "Résultats",
        sliderInput("slider", "Nombre de résultats:", min=0, max=1000, value=100,step = 50)
      )
    ),
    fluidRow(
      #new row : on affiche plot1
      dataTableOutput("table"))
      ),
    # Second tab content
    tabItem(tabName = "widgets",
            h2("Widgets tab content")
    )
  )
)
)

server <- function(input, output) {
  output$table <- renderDataTable(query_DBpedia(input$typeA,input$slider))
}

shinyApp(ui, server)
