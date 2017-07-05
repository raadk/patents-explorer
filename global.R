

# Setup -------------------------------------------------------------------
rm(list = ls())
REFRESH_RDS <- FALSE


# Packages ----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(scales)
library(DT)
library(zoo)
library(rgeos)
library(rgdal)
library(maptools)
library(reshape2)




# Data -----------------------------------------------------------------
# https://stats.oecd.org/Index.aspx?DataSetCode=PATS_IPC#

load(file.path("data", "patents-2016.Rds"))



# Functions ---------------------------------------------------------------
my.na.approx <- function(x, maxgap = 5) {
  if (all(is.na(x))) rep(NA, length(x)) else na.approx(x, na.rm = FALSE, maxgap = 5)
}

# Interpolate R&D ---------------------------------------------------------

d <- unique(patents[, c("YEAR", "LOCATION", "GERD")])
d <- dcast(d, YEAR ~ LOCATION, value.var = "GERD")



d[, -1] <- lapply(d[, -1], my.na.approx)

d <- melt(d, id = "YEAR", value.name = "GERD", variable.name = "LOCATION")

patents$GERD <- NULL
patents <- merge(patents, d, all.x = TRUE)




# Interpolate patents
patents$ID <- interaction(patents[, c("LOCATION", "KINDPATENT", "KINDCOUNTRY", "KINDDATE")])


for (id in unique(as.character(patents$ID))) {
  patents[patents$ID == id, "patent_applications"] <- my.na.approx(patents[patents$ID == id, "patent_applications"])
}



# UI ----------------------------------------------------------------------
help_press_play <- 'Press play below to run the animation or use the slider to change the year'


# location.choices <- structure$LOCATION[structure$LOCATION$id %in% unique(patents$LOCATION), ]
# location.choices <- setNames(location.choices[, 1], location.choices[, 2])
country.choices <- setNames(, unique(patents$COUNTRY))
country.choices.default <- c("Japan", "United States", "United Kingdom", "Italy", "France", "Canada", "Germany")


patent.type.choices <- structure$KINDPATENT[structure$KINDPATENT$id %in% unique(patents$KINDPATENT), ]
patent.type.choices <- setNames(patent.type.choices[, 1], patent.type.choices[, 2])
patent.type.choices.default <- patent.type.choices[1]

reference.country.choices <- structure$KINDCOUNTRY[structure$KINDCOUNTRY$id %in% unique(patents$KINDCOUNTRY), ]
reference.country.choices <- setNames(reference.country.choices[, 1], reference.country.choices[, 2])
reference.country.choices.default <- reference.country.choices[1]

reference.date.choices <- structure$KINDDATE[structure$KINDDATE$id %in% unique(patents$KINDDATE), ]
reference.date.choices <- setNames(reference.date.choices[, 1], reference.date.choices[, 2])
reference.date.choices.default <- reference.date.choices[1]

year.choices <- sort(unique(na.omit(patents[, c("YEAR", "patent_applications")])$YEAR), decreasing = TRUE)
year.choices.default <- 2014




