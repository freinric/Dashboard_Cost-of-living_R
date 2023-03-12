library(dash)
#library(dashCoreComponents)
#library(dashHtmlComponents)
library(ggplot2)
library(plotly)

### DEFINING ###
df <- read.csv("ricky/data_extra.csv", header = TRUE, sep = ",")
provs <- sort(unique(df$province))


### APP LAYOUT ###
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(
  htmlDiv(
    list(
      htmlLabel('Dropdown'), ## adding label on top of dropdown
      dccDropdown(
        options = list(list(label = "New York City", value = "NYC"), # options is list of lists
                       list(label = "Montreal", value = "MTL"),
                       list(label = "San Francisco", value = "SF")),
        value = list('MTL', 'SF'), multi=TRUE
      ),
      htmlH1('Does this work'), # text underneath
      htmlLabel('Slider'),
      dccSlider(
        min = 1,
        max = 10,
        marks = list(
          "1" = "1°C",
          "5" = "5°C",
          "10" = "10°C"
        ),
        value = 5
      )
    )
  )
)

app$run_server(debug = T)
