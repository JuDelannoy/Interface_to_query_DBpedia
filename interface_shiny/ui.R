ui <- dashboardPage( skin = "green",
  #header
  dashboardHeader(title = "Let's query Wikipedia !"),
  #sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "introduction", icon = icon("question-circle")),
      menuItem("Query", tabName = "dashboard", icon = icon("search")),
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Further Information", tabName = "details", icon = icon("info"))
    )),
  #body
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
                              c("All",main_categories$type),
                              selected = "Person")),
                box( width = 4,height = 180,status = "warning",
                  uiOutput("typeA_prec"),
                  conditionalPanel("input.typeB == 'PopulatedPlace'", selectInput("placesubject","Precision",c("All","City","Country"),selected = "All"))),
                box( width = 4,height = 180,status = "warning",
                  textInput(inputId = "nameSubject", label = h4("Value of the subject"),
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
                  textInput(inputId = "nameObject", label = h4("Value of the object"),
                            value = "optionnal"),
                  checkboxInput(inputId = "exactobject",
                                label = "Exact match",
                                value = FALSE),
                  uiOutput("uirangedatemin"),
                  uiOutput("uirangedatemax"))
                
                ),
              checkboxInput(inputId = "nbpredicates",
                            label = strong("Add a new predicate"),
                            value = FALSE),
              
              #2bis : predicate2 and object2
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
              #3 : details
              fluidRow(h3("Preferences")),
              fluidRow(
                box(status = "warning",
                  title = "Results",
                  sliderInput("slider", h4("Number of results"), min=0, max=3000, value=100,step = 100)
                ),
                actionButton("goButton", h4("Go!"),width = '30%',icon = icon("paper-plane")),
                # Export button
                downloadButton("downloadData", h4("Download"))
              ),
          fluidRow(      
            #new row : plot the table
            withSpinner(dataTableOutput("table"),color = "green")
        )),
      
      # Second tab content : explanations for user
      tabItem(tabName = "introduction",
              includeCSS("style.css"),
              tags$h1("Welcome to baby Deep Thought ! "),
                tags$h3("What is it for ?"),
                br(),
              "Most people search on Wikipedia when they need an information about a person, an event, a place or whatever.",
                br(),
                "But if you search for more specific information like, let's say,",
                tags$i("all the athlete who were born in France since 1950"),
                ", it’s getting more complicated ! ",
              br(),
                tags$b("You will easily get the results you want through this app !"),
                br(),
                tags$h3("What you need to know before starting"),
              br(),
              "Every query can be splitted into several triples called 'Subject Predicate Object'.",
                tags$ul(
                  tags$li('	Pick up the subject, verbs (called here predicate) and objects of your sentence.',
                          "Example: All the Athlete born in France since 1950",
                          tags$ul(
                            tags$li("Subject : athlete"),
                            tags$li("Predicate 1 : birth place"),
                            tags$li("Object  1 : France"),
                            tags$li("Predicate 2 : birth date"),
                            tags$li("Object 2 : minimum date : 1950")
                  )),
                  "Be careful to write the accents and uppercase ! 'France' is not 'france'. ",
                  br(),
                  "Specifying the object is optionnal. You could just ask for the birth date without specifying a start or an end.",
                  tags$li(strong('Click on Go Button and wait for the answer! ')),
                  tags$li(strong('Click on Map item to see the results on a map.')),
                  tags$li(strong('Download the results if you want to re-use them. '))
                ),
              tags$h3("How does it work?"),
              br(),
              "Few years ago, a project was created to store Wikipedia data in a database called DBpedia.",
              br(),
              "Anyone can query it to have any kind of information available, but you need to learn the SPARQL language (see Further Information for more information).",
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
              "Let's say you want to study the birthplace of people born after 1960.",
              br(),
              "type of the subject: Person",
              br(),
              "predicate 1 : birthplace",
              br(),
              "predicate 2 : birthdate",
              br(),
              "object 2, minimum : 1960",
              br(),
              "You can select up to 10 000 results to have a wide sample of people.",
              tags$h4("Example 3"),
              "Let's say you want artists born same year than you, knowing that you were born in 1990.",
              br(),
              "type of the subject: Artist",
              br(),
              "predicate: birthdate",
              br(),
              "object, minimum : 1990",
              br(),
              "object, maximum : 1990",
              br(),
              "or name object : 1960"
      ),
      
      #map for located results
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
      
      
     #further information
    tabItem(tabName = "details",
            tags$h3("Potential problems"),
            tags$h2("Query without results ?"),
            tags$h4("Too precise subject"),
            "In LiveDBpedia The subject could be saved in a wrong category or less precise one. If you have selected a precise type, try with only the general type.",
            br(),
            "For example, Will Smith, as an actor and singer, is an Artist, but Live DBpedia stored him only as a Person.",
            br(),
            "Likewise, Usain Bolt is an Athlete, but no athlete is called like that. LiveDBpedia not even stored Usain Bolt as a Person. So you can find him by chosing 'All' in subject.",
            tags$h4("Predicates not filled out"),
            "Not all the predicates are specified for all the subject. So try without predicates first.",
            br(),
            "For example, some people have no nationality filled so they won't appear if you select 'nationality' predicate.",
            tags$h4("Precision of the place"),
            "For example, if you want all athletes born in France, you will only get people whose birth place = France.",
            "If someone has only 'Bordeaux' as birth place, you won't catch its result.",
            tags$h2("Strange results ?"),
            "You got some places in birth date field ? Or some date instead of birthname ? It is due internal LiveDBpedia problems.",
            br(),
            "DBpedia is filled by automatic reading of Wikipedia, so some mistakes are present in the database.",
            br(),
            "For example, the birth name of Émile Rigaud appears to be a date : 1824-03-27.",
            br(),
            tags$h2("Less results than expected ?"),
            tags$h4("Works category"),
            "If you have selected Works category and you find less results than expected, you could try another way.",
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
            "You can contact me if you encounter issues or if you need further information:",
            tags$i(" juliette.delannoy@outlook.fr")
            )
    )
)
)
