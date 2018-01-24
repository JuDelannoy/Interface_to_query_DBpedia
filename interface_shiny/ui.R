





ui <- dashboardPage(
  dashboardHeader(title = "Let's query Wikipedia !"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Query", tabName = "dashboard", icon = icon("search")),
      menuItem("Introduction", tabName = "introduction", icon = icon("question-circle"))
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
                  uiOutput("typeA_prec"),
                  textInput(inputId = "nameSubject", label = "Nom du sujet",
                            value = "optionnel")
                ),
                box(
                  title = "Prédicat",
                  uiOutput("uipredicat")
                ),
                box(
                  title = "Objet",
                  textInput(inputId = "nameObject", label = "Nom de l'objet (optionnel)",
                            value = "optionnel")
                )),
              fluidRow(
                box(
                  title = "Ordre",
                  selectInput("order",
                              "Ordonné par : ",
                              c("Sujet","Objet"))
                ),
                box(
                  title = "Résultats",
                  sliderInput("slider", "Nombre de résultats:", min=0, max=1000, value=100,step = 50)
                )
              ),
              fluidRow(
                #new row : on affiche plot1
                withSpinner(dataTableOutput("table")))
      ),
      # Second tab content
      tabItem(tabName = "introduction",
              h2("How to use this tool ?"),
              "Choose the simplest and more straightforward form. 
              Ex : If I want all the movies played by Diane Kruger, I start by 'My subjet is a Person, who is an actress.'"
      )
      )
    )
  )