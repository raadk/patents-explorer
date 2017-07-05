
## Dotchart
query <- with(patents, KINDPATENT == "EPO_A" & KINDCOUNTRY == "INVENTORS" & KINDDATE == "PRIORITY" ) # & YEAR == 2012
dat <- patents[query, ]
g <- ggplot(dat, aes(patent_applications, LOCATION, frame = YEAR)) +
  geom_point() +
  scale_x_continuous(breaks = pretty_breaks(20), labels = comma)

ggplotly(g)



## Map
query <- with(patents, KINDPATENT == "EPO_A" & KINDCOUNTRY == "INVENTORS" & KINDDATE == "PRIORITY" & YEAR == 2012)
dat <- patents[query, ]
dat <- dat[dat$LOCATION %in% setdiff(dat$LOCATION, c("WLD", "YUG", "EU28")), ]
p <- plot_geo(dat) %>%
  add_trace(
    z = ~ patent_applications,
    color = ~ patent_applications,
    colors = 'Blues',
    text = ~ COUNTRY,
    locations = ~ LOCATION,
    marker = list(line = list(
      color = toRGB("grey"), width = 0.5
    ))
  ) %>%
  colorbar(title = '') %>%
  layout(
    geo = list(
      showframe = FALSE,
      showcoastlines = TRUE,
      projection = list(type = 'Mercator')
    )
  )


## Time series
query <- with(patents, COUNTRY == "Japan" & KINDCOUNTRY == "INVENTORS" & KINDDATE == "PRIORITY")
dat <- patents[query, ]

ggplot(dat, aes(YEAR, patent_applications, color = interaction(KINDPATENT, KINDCOUNTRY, KINDDATE, sep = ", "))) +
  geom_point() +
  geom_line() +
  labs(color = "")



## Panel
query <- with(patents, KINDPATENT == "EPO_A" & KINDCOUNTRY == "INVENTORS" & KINDDATE == "PRIORITY")
dat <- patents[query, ]
g <- ggplot(dat, aes(YEAR, patent_applications, color = COUNTRY)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = pretty_breaks(20), labels = comma)


# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html



# R&D
query <- with(patents, KINDPATENT == "EPO_A" & KINDCOUNTRY == "INVENTORS" & KINDDATE == "PRIORITY" ) # & YEAR == 2012
dat <- patents[query, ]

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


