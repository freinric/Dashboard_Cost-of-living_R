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

# -----------------------------------------------------------------------
scatterplot <- function(drop2a="meal_cheap", drop2b="meal_mid") {
  #Return the ggplot
  scatter_plot <- df %>% 
    ggplot(aes(x= !!sym(drop2a), y = !!sym(drop2b))) +
    geom_point() + 
    theme_bw(20) +
    labs(y=drop2b, x=drop2a)
  ggplotly(scatter_plot,
           width=500)}

barplot <- function(drop1="meal_cheap") {
  options(repr.plot.width = 10, repr.plot.height = 20)
  bar_plot <- df %>% dplyr::filter(!!sym(drop1) > 0)%>% 
    ggplot(aes(x=!!sym(drop1), y =reorder(city, -!!sym(drop1)))) +
    geom_col(position = "dodge") + 
    theme_bw(20) +
    labs(x=drop1, y="")
  ggplotly(bar_plot, height = 2000, width = 700) 
}


plot3 <- function(drop3a="meal_cheap", drop3b=list("Edmonton", "Kelowna")) {
  dff <- df %>% dplyr::filter(city %in% drop3b)
  plot3 <- dff %>% 
    ggplot(aes(x=!!sym(drop3a), y=reorder(city, -!!sym(drop3a)))) +
    geom_col() + 
    theme_bw(20) +
    labs(y='', x=drop3a)
  
  ggplotly(plot3, width = 700)
}


### INSTANCES ###

scatter <- scatterplot()
graph_scatter <- dccGraph(id='scatter_plot',
                          figure=scatter,
                          config = list('displaylogo' = FALSE))

bar <- barplot()
graph_bar <- dccGraph(id='bar_plot',
                      figure=bar,
                      config = list('displaylogo' = FALSE))

bar2 <- plot3()
graph_bar2 <- dccGraph(id='plot3',
                       figure=bar2,
                       config = list('displaylogo' = FALSE))

# -----------------------------------------------------------------------
### DROPDOWNS ###
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

yaxis <- dccDropdown(
  id = "yaxis",
  options = map(
    names(df), function(x){
      list(label=x, value=x)
    }),
  value = 'meal_cheap'
)

drop1 <- dccDropdown(
  id = "drop1",
  options = map(
    names(df)[3:57], function(x){
      list(label=x, value=x)
    }),
  value = 'meal_cheap'
)

drop2a <- dccDropdown(
  id = "drop2a",
  options = map(
    names(df)[3:57], function(x){
      list(label=x, value=x)
    }),
  value = 'meal_cheap'
)

drop2b <- dccDropdown(
  id = "drop2b",
  options = map(
    names(df)[3:57], function(x){
      list(label=x, value=x)
    }),
  value = 'meal_mid'
)

drop3a <- dccDropdown(
  id = "drop3a",
  options = map(
    names(df)[3:57], function(x){
      list(label=x, value=x)
    }),
  value = 'meal_mid'
)

drop3b <- dccDropdown(
  id = "drop3b",
  options = map(
    df$city, function(x){
      list(label=x, value=x)
    }),
  value = list('Edmonton', 'Kelowna'),
  multi = TRUE
)


# -----------------------------------------------------------------------

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
  # SIDEBAR
  htmlDiv(list(
    htmlDiv(
        list(
          htmlDiv(
            list(
              htmlH1("Where do you want to live?"),
              htmlH2("Cost of Living Dashboard"),
              htmlBr(),
              htmlH3("Select Provinces"),
              htmlP('checkboxes'),
              htmlBr(),
              htmlH3('Select City Population'),
              htmlP("slider")
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
                    list(htmlH2('plot 1 title'), drop1,
                      graph_bar
                    ), style=list('width'='100%')
                  ),
                  
                  htmlDiv(list(
                    htmlDiv(
                      list(htmlH2('plot 2 title'), drop2a, drop2b,
                        graph_scatter
                      ), style=list('width'='100%')
                    ),
                    htmlDiv(
                      list(htmlH2('plot 3 title'), drop3a, drop3b,
                        graph_bar2
                      ), style=list('width'='100%')
                    )))
                ), style = list('display'='flex')
              )))
        ), style=list('display'='flex'))
    ), style = list('display'='flex')
  ))))

# -----------------------------------------------------------------------
### CALLBACKS

### BARPLOT1 ###
app$callback(
  output(id = 'bar_plot', property = 'figure'),
  params=list(input(id='drop1', property = 'value')),
  function(drop1) {
    barplot(drop1)
  }
)

### PLOT 2 ###
app$callback(
  output(id = 'scatter_plot', property = 'figure'),
  params=list(input(id='drop2a', property = 'value'),
              input(id='drop2b', property = 'value')),
  function(drop2a, drop2b) {
    scatterplot(drop2a, drop2b)
  }
)

### PLOT 3 ###
app$callback(
  output(id = 'plot3', property = 'figure'),

  params=list(input(id='drop3a', property = 'value'),
              input(id='drop3b', property = 'value')),

  function(drop3a, drop3b) {
    plot3(drop3a, drop3b)
  }
)



app$run_server(debug = T)
