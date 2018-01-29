ui <- dashboardPage(
  dashboardHeader(title = "Let's query Wikipedia !"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Query", tabName = "dashboard", icon = icon("search")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("User Guide", tabName = "introduction", icon = icon("question-circle"))
    )),
  dashboardBody(
    tabItems(
      # in dashboard panel
      tabItem(tabName = "dashboard",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(
                  title = "Subject",
                  selectInput("typeA",
                              "Type of the subject:",
                              main_categories$type,
                              selected = "Person"),
                  uiOutput("typeA_prec"),
                  conditionalPanel("input.typeB == 'PopulatedPlace'", selectInput("placesubject","Precision:",c("All","City","Country"),selected = "All")),
                  textInput(inputId = "nameSubject", label = "Name of the subject",
                            value = "optionnal"),
                  checkboxInput(inputId = "exactsubject",
                                label = "Exact match",
                                value = FALSE),
                  
                  # Export button
                  downloadButton("downloadData", "Download")
                ),
                box(
                  title = "Predicate",
                  uiOutput("uipredicat"),
                  uiOutput("typeofplace")
                ),
                box(
                  title = "Object",
                  textInput(inputId = "nameObject", label = "Name of the object",
                            value = "optionnal"),
                  checkboxInput(inputId = "exactobject",
                                label = "Exact match",
                                value = FALSE)
                )),
              fluidRow(
                box(
                  title = "Results",
                  sliderInput("slider", "Number of results:", min=0, max=1000, value=100,step = 50)
                )
              ),
              fluidRow(
                actionButton("goButton", "Go!"),
                #new row : plot the table
                withSpinner(dataTableOutput("table")))
      ),
      # Second tab content : explanations
      tabItem(tabName = "introduction",
              h2("How to use this tool ?"),
              "Choose the simplest and more straightforward form. 
              Ex : If I want all the movies played by Diane Kruger, I start by 'My subjet is a Person, who is an actress.'"
      )
      )
    )
  )