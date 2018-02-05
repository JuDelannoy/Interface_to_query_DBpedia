ui <- dashboardPage( skin = "green",
  
  dashboardHeader(title = "Let's query Wikipedia !"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "introduction", icon = icon("question-circle")),
      menuItem("Query", tabName = "dashboard", icon = icon("search")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Further Information", tabName = "details", icon = icon("info"))
    )),
  
  dashboardBody(
    tabItems(
      # in dashboard panel
      tabItem(tabName = "dashboard",
              # 1 : subject
              fluidRow(h3("Subject")),
              fluidRow(
                box( width = 4,height = 180,  status = "warning",
                  selectInput("typeA",
                              h4("Type of the subject"),
                              main_categories$type,
                              selected = "Person")),
                box( width = 4,height = 180,status = "warning",
                  uiOutput("typeA_prec"),
                  conditionalPanel("input.typeB == 'PopulatedPlace'", selectInput("placesubject","Precision",c("All","City","Country"),selected = "All"))),
                box( width = 4,height = 180,status = "warning",
                  textInput(inputId = "nameSubject", label = h4("Name of the subject"),
                            value = "optionnal"),
                  checkboxInput(inputId = "exactsubject",
                                label = "Exact match",
                                value = FALSE))
                  
                ),
              #2 : predicates and objects
              fluidRow(h3("Predicates and objects")),
              fluidRow(
                box(title = "Predicate",status = "warning",
                  uiOutput("uipredicat"),
                  uiOutput("typeofplace"))
                ,
                box(title = "Object",status = "warning",
                  textInput(inputId = "nameObject", label = h4("Name of the object"),
                            value = "optionnal"),
                  checkboxInput(inputId = "exactobject",
                                label = "Exact match",
                                value = FALSE),
                  uiOutput("uirangedatemin"),
                  uiOutput("uirangedatemax"))
                
                ),
              checkboxInput(inputId = "nbpredicates",
                            label = "Add a new predicate",
                            value = FALSE),
              fluidRow(conditionalPanel('input.nbpredicates == true',
                                 box(status = "warning",h4(strong("Predicate 2")),
                                     uiOutput("uipredicat2"),
                                     uiOutput("typeofplace2")),
                                 box(status = "warning",h4(strong(" Object 2")),
                                     textInput(inputId = "nameObject2", label = h4("Name of the object"),value = "optionnal"),
                                     checkboxInput(inputId = "exactobject2",label = "Exact match",value = FALSE),
                                     uiOutput("uirangedatemin2"),
                                     uiOutput("uirangedatemax2")))
              ),
              #2 : details
              fluidRow(h3("Preferences")),
              fluidRow(
                box(status = "warning",
                  title = "Results",
                  sliderInput("slider", h4("Number of results"), min=0, max=10000, value=100,step = 100)
                ),
                actionButton("goButton", h4("Go!"),width = '30%',icon = icon("paper-plane")),
                # Export button
                downloadButton("downloadData", h4("Download"))
              ),
          fluidRow(      
            #new row : plot the table
            withSpinner(dataTableOutput("table"),color = "green")
        )),
      # Second tab content : explanations
      tabItem(tabName = "introduction",
              includeCSS("style.css"),
              tags$h1("Welcome to this app ! "),
                tags$h3("What is it for ?"),
                br(),
              "Most people search on Wikipedia when they need an information about a person, an event, a place or whatsoever.",
                br(),
                "But if you search for more specific information like, let's say,",
                tags$i("all the athlete who were born in France since 1950"),
                ", it’s getting more complicated ! ",
              br(),
                tags$b("You will easily get the results you want through this app !"),
                br(),
                tags$h3("What you need to know before starting"),
              br(),
                tags$ul(
                  tags$li('	Pick up the subject, verbs (called here predicate) and objects of your sentence.',
                          "Example: All the Athlete born in France since 1950",
                          tags$ul(
                            tags$li("Subject : athlete"),
                            tags$li("Verb 1 : birth place"),
                            tags$li("Object  1 : France"),
                            tags$li("Verb 2 : birth date"),
                            tags$li("Object 2 : minimum date : 1950")
                  )),
                  tags$li(strong('Click on Go Button and wait for the answer! ')),
                  tags$li(strong('Click on Map item to see the results on a map.')),
                  tags$li(strong('Download the results if you want to re-use them. '))
                ),
              tags$h3("How does it work?"),
              br(),
              "Few years ago, Wikipedia team has started to store all the data of the website in a database called DBpedia.",
              br(),
              "Anyone can query it to have any kind of information available, but you need to learn the SPARQL language.",
              br(),
              "This interface was made to avoid this,",
              tags$b("you just have to select the information you want and you’ll get the answer!"),
              tags$h3("Examples"),
              tags$h4("Example 1"),
              "Let's say you want information about the athlete Teddy Riner.",
              br(),
              "type of the subject: Person",
              br(),
              "precise type: Athlete",
              br(),
              "name: Teddy Riner",
              br(),
              "Then choose the information you want in the list of predicates.",
              br(),
              tags$h4("Example 2"),
              "Let's say you want to study the birthplace of people born after 1960..",
              br(),
              "type of the subject: Person",
              br(),
              "predicate 1 : birthplace",
              br(),
              "predicate 2 : birthdate",
              br(),
              "object 2, minimum : 1960",
              br(),
              "Ypu can select up to 10 000 results to have a wide sample of people."
      ),
      tabItem(tabName = "map",
              tags$h1("See your results on a map !"),
              withSpinner(leafletOutput("mymap",height = 475),color = "green"),
              "In case of spatialized query, that is if coordinates appear in your table, a map is displayed with all located results on it.",
              tags$h2("How does it work ?"),
              "One blue marker for one result, so one line of the table !",
              "If you have selected 2 spatialized predicates (ex: birth and death place), the second one will be displayed with orange markers",
             # tags$img(src = "C:/Users/Juliette/Documents/cartagéo/projet info/interface_shiny/DATA/orange_marker.png",contentType = 'image/png', width = "32"),
              "The name of the subjet will be displayed by hovering the marker.",
             br(),
             br(),
             "Lot of people have several information, so for one result you can have both the country and the city.",
             br(),
             "If you have several markers for one element (Person, Organisation ..), you should select City or Country in the options."
      ),
    tabItem(tabName = "details",
            tags$h3("Potential problems"),
            tags$h2("Query without results ?"),
            tags$h4("Too precise subject"),
            "In LiveDBpedia The subject could be saved in a wrong category or less precise one. If you have selected a precise type, try with only the general type.",
            br(),
            "For example, Usain Bolt is an Athlete, but no athlete is called like that. LiveDBpedia stored Usain Bolt only as a Person. ",
            tags$h4("Predicates not filled out"),
            "Not all the predicates are specified for all the subject. SO try without predicates first.",
            br(),
            "For example, some people have no nationality filled so they won't appear if you select 'nationality' predicate.",
            tags$h4("Precision of the place"),
            "For example, if you want all athletes born in France, you will only get people whose birth place = France.",
            "If someone has only 'Bordeaux' as birth place, you won't catch its result.",
            tags$h2("Strange results ?"),
            "You got some places in birth date field ? It is due internal LiveDBpedia problems.",
            "DBpedia is filled by automatic reading of Wikipedia, so some mistakes are present in the database.",
            br(),
            tags$h2("Less results than expected ?"),
            tags$h4("Works category"),
            "If you have selected WOrks category and you find less results than expected, you could try another way.",
            br(),
            "For example if you search people who played in a film, instead of having the film as subject, try with the actor as subject.",
            "Indeed most of the time there are more information this way.",
            br(),
            br(),
            tags$h3("More information"),
            "More information about Semantic Web ?",
            a(href="https://github.com/JuDelannoy/Interface_to_query_DBpedia/tree/master/sparql", target="_blank", "French detailed explanations here"),
            br(),         
            helpText(a(href="https://github.com/JuDelannoy/Interface_to_query_DBpedia", target="_blank", "View code here")),
            br(),
            tags$h3("Contact"),
            "You can contact me if you see issues or if you need further information:",
            tags$i(" juliette.delannoy@outlook.fr")
            )
    )
)
)
