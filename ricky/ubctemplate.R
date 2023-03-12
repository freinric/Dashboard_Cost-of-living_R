# Layout example for DashR
# This layout includes four basic plots in a 2x2 grid, two dropdown boxes, a title bar, and a sidebar
# Author: Matthew Connell
# Date: February 2020 

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app$layout(list(
  htmlDiv(
    list(
      htmlH2("Motor Trend Cars")
    ), style = list('columnCount'=1, 
                    'background-color'= 'lightgrey', 
                    'color'='black')
  )))


app$run_server(debug = T)
