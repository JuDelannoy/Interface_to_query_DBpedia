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
              # 1 : subject
              fluidRow(h3("Subject")),
              fluidRow(
                box( width = 4,height = 180,
                  selectInput("typeA",
                              "Type of the subject:",
                              main_categories$type,
                              selected = "Person")),
                box( width = 4,height = 180,
                  uiOutput("typeA_prec"),
                  conditionalPanel("input.typeB == 'PopulatedPlace'", selectInput("placesubject","Precision:",c("All","City","Country"),selected = "All"))),
                box( width = 4,height = 180,
                  textInput(inputId = "nameSubject", label = "Name of the subject",
                            value = "optionnal"),
                  checkboxInput(inputId = "exactsubject",
                                label = "Exact match",
                                value = FALSE))
                  
                ),
              #2 : predicates and objects
              fluidRow(h3("Predicates and objects"),
                       selectInput("nbpredicates","Number",c("1","2"),selected = "1",width = 4)),
              fluidRow(
                box(title = "Predicate",
                  uiOutput("uipredicat"),
                  uiOutput("typeofplace"),
                  conditionalPanel('input.nbpredicates == "2"',
                                   "Predicate 2",
                                   uiOutput("uipredicat2"),
                                   uiOutput("typeofplace2"))
                ),
                box(title = "Object",
                  textInput(inputId = "nameObject", label = "Name of the object",
                            value = "optionnal"),
                  checkboxInput(inputId = "exactobject",
                                label = "Exact match",
                                value = FALSE),
                  uiOutput("uirangedatemin"),
                  uiOutput("uirangedatemax"),
                  conditionalPanel('input.nbpredicates == "2"',
                                  " Object 2 :",
                                   textInput(inputId = "nameObject2", label = "Name of the object",value = "optionnal"),
                                    checkboxInput(inputId = "exactobject2",label = "Exact match",value = FALSE),
                                    uiOutput("uirangedatemin2"),
                                    uiOutput("uirangedatemax2"))
                )),
              #2 : details
              fluidRow(h3("Preferences")),
              fluidRow(
                box(
                  title = "Results",
                  sliderInput("slider", "Number of results:", min=0, max=1000, value=100,step = 50)
                ),
              
                actionButton("goButton", "Go!"),
                # Export button
                downloadButton("downloadData", "Download")
              ),
          fluidRow(                
            #new row : plot the table
            withSpinner(dataTableOutput("table"))
        )),
      # Second tab content : explanations
      tabItem(tabName = "introduction",
              h2("How to use this tool ?"),
              "Choose the simplest and more straightforward form. 
              Ex : If I want all the movies played by Diane Kruger, I start by 'My subjet is a Person, who is an actress.'"
      ),
      tabItem(tabName = "map",
              leafletOutput("mymap")
      )
    )
)
)