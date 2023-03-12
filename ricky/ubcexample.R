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

df <- mtcars

options(repr.plot.width = 10, repr.plot.height = 10)

histplot <- function(xaxis="mpg") {
  
  hist <- ggplot(data = df, aes(x=!!sym(xaxis))) +
    geom_histogram(bins = 30) +
    labs(y = "Number of cars", x = xaxis) +
    theme_bw(20)
  
  ggplotly(hist)
}

scatterplot <- function(xaxis="mpg", yaxis="mpg") {
  
  scatter_plot <- df %>% 
    ggplot(aes(x=!!sym(xaxis), y =!!sym(yaxis))) +
    geom_point() + 
    geom_smooth(se=FALSE) +
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(scatter_plot)
}

barplot <- function(xaxis="mpg", yaxis="mpg") {
  
  bar_plot <- df %>% 
    ggplot(aes(x=!!sym(xaxis), y =!!sym(yaxis))) +
    geom_col(position = "dodge") + 
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(bar_plot) 
}


lineplot <- function(xaxis="mpg", yaxis="mpg") {
  
  line_plot <- df %>% 
    ggplot(aes(x=!!sym(xaxis), y=!!sym(yaxis))) +
    geom_line() + 
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(line_plot) %>% layout(showlegend = FALSE)
}

# Create a dropdown box for the xaxis
xaxis <- dccDropdown(
  id = "xaxis",
  # Set the options for the dropdown (all the columns of the df)
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  # Assign a default value for the dropdown
  value = 'mpg'
)

# Do the same for the y-axis
yaxis <- dccDropdown(
  id = "yaxis",
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  value = 'mpg'
)


histogram <- histplot()
graph_hist <- dccGraph(id='histogram',
                       figure=histogram,
                       config = list('displaylogo' = FALSE))

scatter <- scatterplot()
graph_area <- dccGraph(id='scatter_plot',
                       figure=scatter,
                       config = list('displaylogo' = FALSE))

bar <- barplot()
graph_bar <- dccGraph(id='bar_plot',
                      figure=bar,
                      config = list('displaylogo' = FALSE))

line <- lineplot()
graph_line <- dccGraph(id='line_plot',
                       figure=line,
                       config = list('displaylogo' = FALSE))



app$layout(htmlDiv(list(
  htmlDiv(
    list(
      htmlH2("Motor Trend Cars")
    ), style = list('columnCount'=1, 
                    'background-color'= 'lightgrey', 
                    'color'='white')
  ),# Main area, # DROPDOWNS
  htmlDiv(list(htmlDiv(
        list(
          htmlDiv(
            list(
              htmlP("Select a variable for the x-axis:"),
                       xaxis,
                       htmlP("Select a variable for the y-axis:"),
                       yaxis
              ), style = list('display'='flex',
                                            'justify-content'='flex-start',
                                            'white-space'='pre-line')),
          htmlDiv(list( ### PLOTS ###
            htmlDiv( ### TOP TWO PLOTS ###
                list(
                  htmlDiv(
                    list(
                      graph_hist
                    ), style=list('width'='90%')
                  ),
                  htmlDiv(
                    list(
                      graph_area
                    ), style=list('width'='90%')
                  )
                ), style = list('display'='flex')
              ),
              htmlDiv( ### BOTTOM TWO PLOTS ###
                list(
                  htmlDiv(
                    list(
                      graph_bar
                    ), style = list('width' = "90%")
                  ),
                  htmlDiv(
                    list(
                      graph_line
                    ), style=list('width'='90%')
                  )
                ), style = list('display'='flex')
              )
            )
          )
        )))))))



app$callback(
  output(id = 'histogram', property = 'figure'),
  params=list(input(id = 'xaxis', property = 'value')),
  function(xaxis) {
    histplot(xaxis)
  }
)

app$callback(
  output(id = 'scatter_plot', property = 'figure'),
  params=list(input(id = 'xaxis', property = 'value'), input(id = 'yaxis', property='value')),
  function(xaxis, yaxis) {
    scatterplot(xaxis, yaxis)
  }
)

app$callback(
  output(id = 'bar_plot', property = 'figure'),
  params=list(input(id = 'xaxis', property = 'value'), input(id = 'yaxis', property='value')),
  function(xaxis, yaxis) {
    barplot(xaxis, yaxis)
  }
)

app$callback(
  output(id = 'line_plot', property = 'figure'),
  params=list(input(id = 'xaxis', property = 'value'), input(id = 'yaxis', property='value')),
  function(xaxis, yaxis) {
    lineplot(xaxis, yaxis)
  }
)


app$run_server(debug = T)
