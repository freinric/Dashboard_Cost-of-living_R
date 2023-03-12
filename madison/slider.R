library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

df <- read.csv("ricky/data_extra.csv", header = TRUE, sep = ",")

app$layout(
  htmlDiv(
    list(
      dccRangeSlider(
        id='population',
        min=0,
        max=2800000, step = 1000,
        marks = list('100000'='100k',
                     '500000'= '500k',
                     '1000000'='1M',
                     '1500000'='1.5M',
          '2000000'= '2M',
          '2500000'='2.5M',
          '3000000'='3M'),
        value=list(0,2800000)
      ),
      htmlDiv(id='slider-output')
    )
  )
)

app$callback(
  output(id = 'slider-output', property='population'),
  params=list(input(id='population', property='value')),
  function(value) {
    sprintf('You have selected [%0.1f, %0.1f]', value[1], value[2])
  })

app$run_server()