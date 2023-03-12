# Layout example for DashR
# This layout includes two basic plots next to each other, two dropdown boxes, a title bar, and a sidebar
# Author: Matthew Connell
# Date: February 2020

library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)

# Set an external stylesheet for CSS
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

df <- read.csv("ricky/data_extra.csv", header = TRUE, sep = ",")

# Set plot height and width
options(repr.plot.width = 10, repr.plot.height = 10)

### FUNCTIONS ###

scatterplot <- function(xaxis="meal_cheap", yaxis="meal_cheap") {
  #Return the ggplot
  scatter_plot <- df %>% 
    ggplot(aes(x= !!sym(xaxis), y = !!sym(yaxis))) +
    geom_point() + 
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  ggplotly(scatter_plot,
           width=500)}

barplot <- function(xaxis="meal_cheap") {
  options(repr.plot.width = 10, repr.plot.height = 20)
  bar_plot <- df %>% dplyr::filter(!!sym(xaxis) > 0)%>% 
    ggplot(aes(x=!!sym(xaxis), y =reorder(city, -!!sym(xaxis)))) +
    geom_col(position = "dodge") + 
    theme_bw(20) +
    labs(x=xaxis, y="")
  ggplotly(bar_plot, height = 2000, width = 700) 
}


lineplot <- function(xaxis="meal_cheap", yaxis="meal_cheap") {
  line_plot <- df %>% 
    ggplot(aes(x=!!sym(xaxis), y=!!sym(yaxis))) +
    geom_line() + 
    theme_bw(20) +
    labs(y=yaxis, x=xaxis)
  
  ggplotly(line_plot) %>% layout(showlegend = FALSE)
}

### INSTANCES ###
# Create instance of the histplot function and assign it to 'histogram'

scatter <- scatterplot()
graph_scatter <- dccGraph(id='scatter_plot',
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

# Create a dropdown box for the xaxis
xaxis <- dccDropdown(
  id = "xaxis",
  # Set the options for the dropdown (all the columns of the df)
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  
  # Assign a default value for the dropdown
  value = 'meal_cheap'
)

# Do the same for the y-axis
yaxis <- dccDropdown(
  id = "yaxis",
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  value = 'meal_cheap'
)

### THIS PART DOESN'T WORK ###
# Create a slider for number of bins
num_bins <- dccSlider(
  id="num_bins",
  min=1,
  max=30,
  value=3,
  step=1,
  marks = as.list(
    setNames(
      seq(2, 30, 3),
      seq(2, 30, 3)
    )
  )
)

## Attribution and more good slider stuff: https://github.com/plotly/dash-sample-apps/blob/639ebbb57df5d261ff28d92ad2edc9dc092aa7c7/apps/dashr-svm/app.R#L96


# Start the layout
app$layout(htmlDiv(list(
  # TITLE BAR
  htmlDiv(
    list(
      htmlH1("City test")
    ), style = list('columnCount'=1, 
                    'background-color'= 'black', 
                    'color'='white',
                    'text-align'='center')
  ),
  # SIDEBAR
  htmlDiv(list(
    htmlDiv(
        list(
          htmlDiv(
            list(
              # Dropdowns
              htmlP("Select a variable for the x-axis:"),
              xaxis,
              # Use htmlBr() for line breaks
              htmlBr(),
              htmlP("Select a variable for the y-axis:"),
              yaxis,
              htmlP("Choose the number of bins for your histogram:"),
              num_bins,
              htmlBr(),
              htmlP("placeholder text")
            ), style = list('background-color'='lightgrey', 
                            'columnCount'=1, 
                            'white-space'='pre-line',
                            'width' = '300px')
          ),
          htmlDiv(list(
            ### TOP TWO PLOTS ###
              htmlDiv( ## beside each other
                list(
                  htmlDiv(
                    list(htmlH2('plot 1 title'),
                      graph_bar
                    ), style=list('width'='100%')
                  ),
                  
                  htmlDiv(list(
                    htmlDiv(
                      list(htmlH2('plot 2 title'),
                        graph_scatter
                      ), style=list('width'='100%')
                    ),
                    htmlDiv(
                      list(htmlH2('plot 3 title'),
                        graph_line
                      ), style=list('width'='100%')
                    )))
                ), style = list('display'='flex')
              )))
        ), style=list('display'='flex'))
    ), style = list('display'='flex')
  ))))



# app$callback is what allows the graphs to update after the user changes the slider or dropdown
app$callback(

  # Update the 'figure' property of the object with id 'histogram'
  output(id = 'bar_plot', property = 'figure'),

  # with the 'value' property of the object with id 'xaxis' (the x-axis dropdown)
  params=list(input(id='xaxis', property = 'value')),

  # Update the histplot
  function(xaxis) {
    barplot(xaxis)
  }
)


app$callback(

  # Update the 'figure' property of the object with id 'scatter'
  output(id = 'scatter_plot', property = 'figure'),

  params=list(input(id='xaxis', property = 'value'),
              input(id='yaxis', property = 'value')),

  # Update the histplot
  function(xaxis, yaxis) {
    scatterplot(xaxis, yaxis)
  }
)

app$run_server(debug = T)
