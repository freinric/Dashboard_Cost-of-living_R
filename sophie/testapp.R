library(dash)
library(dashCoreComponents)
#library(dashHtmlComponents)
#library(dashBootstrapComponents)
library(ggplot2)
library(plotly)


app = Dash$new()

# Set the layout of the app
app %>% set_layout(
  h1('Hello Dash'),
  div("Dash: A web application framework for your data."),
  dccGraph(
    figure = list(
      data = list(
        list(
          x = list(1, 2, 3),
          y = list(4, 1, 2),
          type = 'bar',
          name = 'SF'
        ),
        list(
          x = list(1, 2, 3),
          y = list(2, 4, 5),
          type = 'bar',
          name = 'Montr\U{00E9}al'
        )
      ),
      layout = list(title = 'Dash Data Visualization')
    )
  )
)

app$run_server(debug = T)
