


ui <- dashboardPage(
  title = "Macro Patents Explorer",
  dashboardHeader(title = "Macro Patents Explorer",
                  titleWidth = 250),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "main_menu",
      menuItem("Cross-sectional", tabName = "cs", icon = icon("bar-chart")),
      menuItem("Time-series", tabName = "ts", icon = icon("line-chart")),
      menuItem("Panel", tabName = "pnl", icon = icon("th")),
      menuItem("Help", tabName = "hlp", icon = icon("question")),


      # tags$br(),
      # tags$br(),

      conditionalPanel("input.main_menu == 'cs' && input.cs_panel == 'Table'",
         selectizeInput("cs.year",
                        label = "Year",
                        choices = year.choices,
                        selected = year.choices.default,
                        multiple = FALSE)
       ),

      conditionalPanel("input.main_menu == 'pnl' || input.main_menu == 'ts'",
                       sliderInput("pnl.years",
                                   label = "Year range",
                                   min = min(patents$YEAR),
                                   max = max(patents$YEAR),
                                   value = range(patents$YEAR),
                                   step = 1,
                                   round = TRUE,
                                   sep = ""
                                   )
      ),


      conditionalPanel("(input.main_menu == 'cs'  && input.cs_panel != 'Map') || input.main_menu == 'pnl'",
                       selectizeInput("cs.countries",
                                      label = "Countries",
                                      choices = country.choices,
                                      selected = country.choices.default,
                                      multiple = TRUE)
      ),

      conditionalPanel("input.main_menu == 'cs' || input.main_menu == 'pnl'",
        selectizeInput("cs.patent.type",
                       label = "Patent type",
                       choices = patent.type.choices,
                       selected = patent.type.choices.default,
                       multiple = FALSE),

        selectizeInput("cs.reference.country",
                       label = "Reference country",
                       choices = reference.country.choices,
                       selected = reference.country.choices.default,
                       multiple = FALSE),

        selectizeInput("cs.reference.date",
                       label = "Reference date",
                       choices = reference.date.choices,
                       selected = reference.date.choices.default,
                       multiple = FALSE)
      ),


      conditionalPanel("input.main_menu == 'ts'",
         selectizeInput("ts.country",
                        label = "Country",
                        choices = country.choices,
                        selected = country.choices.default[1],
                        multiple = FALSE),

         selectizeInput("ts.patent.types",
                        label = "Patent type",
                        choices = patent.type.choices,
                        selected = patent.type.choices,
                        multiple = TRUE),

         selectizeInput("ts.reference.countries",
                        label = "Reference country",
                        choices = reference.country.choices,
                        selected = reference.country.choices.default,
                        multiple = TRUE),

         selectizeInput("ts.reference.dates",
                        label = "Reference date",
                        choices = reference.date.choices,
                        selected = reference.date.choices.default,
                        multiple = TRUE)
      )

    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "cs",
              tabsetPanel(
                id = "cs_panel",
                tabPanel("Plot", helpText(help_press_play), plotlyOutput("cs.dotchart", height = "600px", width = "100%")),
                tabPanel("Map", helpText(help_press_play), plotlyOutput("cs.map", height = "600px", width = "100%")),
                tabPanel("GERD", helpText(help_press_play), plotlyOutput("cs.rd", height = "600px", width = "100%")),
                tabPanel("Table", dataTableOutput("cs.table"))
              )
      ),

      tabItem(tabName = "ts",
              tabsetPanel(
                id = "ts_panel",
                tabPanel("Plot", plotlyOutput("ts.chart", height = "600px", width = "100%")),
                tabPanel("Table", dataTableOutput("ts.table"))

              )
      ),

      tabItem(tabName = "pnl",
              tabsetPanel(
                id = "pnl_panel",
                tabPanel("Plot", plotlyOutput("pnl.chart", height = "600px", width = "100%")),
                tabPanel("Table", dataTableOutput("pnl.table"))
              )
      ),

      tabItem(tabName = "hlp",
              column(fluidRow(box(htmlOutput("help"), width = 12)), width = 8, offset = 2)
              # tabsetPanel(
                # id = "hlp_panel",
              # )
      )
    ),
    HTML("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
         (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
         m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
         })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

         ga('create', 'UA-98396812-5', 'auto');
         ga('send', 'pageview');

         </script>")
  ),
  skin = "black"
)
