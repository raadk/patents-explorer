

server <- function(input, output) {


  cs.query <- reactive({
    with(patents,
         KINDPATENT == input$cs.patent.type &
           KINDCOUNTRY == input$cs.reference.country &
           KINDDATE == input$cs.reference.date)

  })

  cs.data <- reactive({
    query <- cs.query() & patents$COUNTRY %in% input$cs.countries
    dat <- patents[query, ]
    return(dat)
  })

  output$cs.dotchart <- renderPlotly({
    dat <- cs.data()
    # dat$COUNTRY <- factor(dat$COUNTRY, levels = dat$COUNTRY[order(dat$patent_applications, decreasing = FALSE)])
    g <- ggplot(dat, aes(patent_applications, COUNTRY, frame = YEAR)) +
      geom_point(size = 3) +
      scale_x_continuous(breaks = pretty_breaks(20), labels = comma) +
      labs(x = input$cs.patent.type, y = "")
    g
  })


  output$cs.table <- renderDataTable({
    query <- cs.query() & patents$YEAR == input$cs.year & patents$COUNTRY %in% input$cs.countries
    dat <- patents[query, ]
    dat$LOCATION <- NULL
    dat
  }, rownames = FALSE)



  output$cs.map <- renderPlotly({
    query <- cs.query() # & patents$YEAR == input$cs.year
    dat <- patents[query, ]
    dat <- dat[dat$LOCATION %in% setdiff(dat$LOCATION, c("WLD", "YUG", "EU28")), ]

    p <- plot_geo(dat) %>%
      add_trace(
        z = ~ patent_applications,
        color = ~ patent_applications,
        colors = 'Blues',
        text = ~ COUNTRY,
        locations = ~ LOCATION,
        frame = ~ YEAR,
        marker = list(line = list(
          color = toRGB("grey"), width = 0.6
        ))
      ) %>%
      colorbar(title = '', limits = range(dat$patent_applications)) %>%
      layout(
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'Mercator')
        )
      )
  })


  output$cs.rd <- renderPlotly({
    # R&D
    dat <- cs.data()

    plot_ly(
      dat,
      x = ~ GERD,
      y = ~ patent_applications,
      frame = ~ YEAR,
      type = "scatter",
      mode = 'text',
      text = ~ LOCATION,
      textposition = 'middle right',
      textfont = list(color = '#000000', size = 16)
    )

  })

  pnl.query <- reactive({
    with(patents,
         KINDPATENT == input$cs.patent.type &
           KINDCOUNTRY == input$cs.reference.country &
           COUNTRY %in% input$cs.countries &
           KINDDATE == input$cs.reference.date &
           YEAR >= input$pnl.years[1] &
           YEAR <= input$pnl.years[2])
  })

  pnl.data <- reactive({
    patents[pnl.query(), ]
  })

  output$pnl.chart <- renderPlotly({
    dat <- pnl.data()
    g <- ggplot(dat, aes(YEAR, patent_applications, color = COUNTRY)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = pretty_breaks(10)) +
      scale_y_continuous(breaks = pretty_breaks(20), labels = comma)
    g
  })


  output$pnl.table <- renderDataTable({
    dat <- pnl.data()
    dcast(dat, COUNTRY ~ YEAR)
  }, rownames = FALSE, options = list(scrollX = TRUE))



  ts.query <- reactive({
    with(patents, COUNTRY == input$ts.country &
           KINDPATENT %in% input$ts.patent.types &
           KINDCOUNTRY %in% input$ts.reference.countries &
           KINDDATE %in% input$ts.reference.dates &
           YEAR >= input$pnl.years[1] &
           YEAR <= input$pnl.years[2])
  })

  ts.data <- reactive({
    patents[ts.query(), ]
  })

  output$ts.chart <- renderPlotly({

    dat <- ts.data()
    dat$series <- with(dat, interaction(KINDPATENT, KINDCOUNTRY, KINDDATE, sep = ", "))
    ggplot(dat, aes(YEAR, patent_applications,
                    color = series)) +
      geom_point() +
      geom_line() +
      labs(color = "") +
      scale_x_continuous(breaks = pretty_breaks(10)) +
      scale_y_continuous(breaks = pretty_breaks(20), labels = comma)
  })

  output$ts.table <- renderDataTable({
    ts.data()
  }, rownames = FALSE)



  output$help <- renderText({
    "
      <div>

      <h2>
        Description
      </h2>

      <p>
         This is a working project by Raad Khraishi (2017).
         The information contained within this website is provided for informational purposes only
      </p>
      <p>
        This app was developed to assist in visualizing differences in patent applications as well as R&D across countries and time.
        In addition, this application may be used to compare different patent measures such as those derived from PCT, WIPO, and USPTO patent families.
      </p>
      <p>
        More information on the variables can be found directly on the <a href='https://stats.oecd.org/OECDStat_Metadata/ShowMetadata.ashx?Dataset=PATS_IPC&Lang=en'> OECD website </a>
      </p>


      <h2>
        Data
      </h2>
      <p>
        <strong> The OECD is not affiliated with this application in any way. </strong>

        <ul>
          <li> OECD - Patents by technology: http://stats.oecd.org/Index.aspx?DataSetCode=PATS_IPC
          <li> OECD - Gross domestic expenditure on R&D: https://stats.oecd.org/Index.aspx?DataSetCode=MSTI_PUB
        </ul>

        Note that some of the data has been linearly interpolated.
      <p>

      <h2>
        R Packages
      </h2>
      <p>
        <ul>
        <li> Carson Sievert, Chris Parmer, Toby Hocking, Scott Chamberlain, Karthik Ram, Marianne Corvellec and Pedro Despouy (2017). plotly: Create Interactive Web Graphics via 'plotly.js'. R package version 4.6.0. https://CRAN.R-project.org/package=plotly
        <li> H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.
        <li> Hadley Wickham (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20. URL http://www.jstatsoft.org/v21/i12/.
        <li> Hadley Wickham (2016). scales: Scale Functions for Visualization. R package version 0.4.1. https://CRAN.R-project.org/package=scales
        <li> Roger Bivand and Colin Rundel (2017). rgeos: Interface to Geometry Engine - Open Source (GEOS). R package version 0.3-23. https://CRAN.R-project.org/package=rgeos
        <li> Roger Bivand and Nicholas Lewin-Koh (2017). maptools: Tools for Reading and Handling Spatial Objects. R package version 0.9-2. https://CRAN.R-project.org/package=maptools
        <li> Roger Bivand, Tim Keitt and Barry Rowlingson (2017). rgdal: Bindings for the Geospatial Data Abstraction Library. R package version 1.2-7. https://CRAN.R-project.org/package=rgdal
        <li> Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.3. https://CRAN.R-project.org/package=shiny
        <li> Winston Chang (2016). shinydashboard: Create Dashboards with 'Shiny'. R package version 0.5.3. https://CRAN.R-project.org/package=shinydashboard
        <li> Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2. https://CRAN.R-project.org/package=DT

      </ul>
      </p>
      </div>
    "

  })
}



