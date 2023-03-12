# rplots
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)

df <- read.csv("ricky/data_extra.csv", header = TRUE, sep = ",")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    list(
      dccGraph(id='plot-area'),
      dccDropdown(
        id='col-select',
        options = df %>%
          colnames %>%
          purrr::map(function(col) list(label = col, value = col)), 
        value='meal_cheap')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol) {
    p <-  df %>% dplyr::filter(xcol > 0) %>% 
      ggplot(aes(x = xcol, y = reorder(city, -xcol))) + 
      geom_col() +
      scale_x_continuous(labels=scales::dollar_format())+
      labs(y = '')+
      ggthemes::scale_color_tableau()
    ggplotly(p)
  }
)

app$run_server(debug = T)

p <-  df %>% dplyr::filter(meal_cheap > 0) %>% 
  ggplot(aes(x = meal_cheap, y = reorder(city, -meal_cheap))) + 
  geom_col() +
  scale_x_continuous(position = "top",labels=scales::dollar_format())+
  labs(y = '')+
  ggthemes::scale_color_tableau()
p
ggplotly(p)
