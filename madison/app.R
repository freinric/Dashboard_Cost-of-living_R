library(dash)
library(dashHtmlComponents)
#library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(scales)




# Defining Things

#------------------------------------------------------------------------------

# Data Input
data <- read.csv("data_extra.csv", header = TRUE, sep = ",")

# Styles
colors <- list('background'='dark',
  'background_dropdown'='#DDDDDD',
  'H1'='#00BBFF',
  'H2'='#7FDBFF',
  'H3'='#005AB5')  # changed from Python dict to R dict with c('key'='value')

style_dropdown <- list('width'='100%', 'font-family'='arial', 
                    "font-size"="1.1em", 
                    "background-color"=colors['background_dropdown'], 
                    'font-weight'='bold') # changed from Python dict to R dict with c('key'='value')

style_H1 <- list('textAlign'= 'center', 'color'= colors['H1']) # Title
style_H2 <- list('textAlign'= 'center', 'color'= colors['H2']) # Subtitle
style_H3_c <- list('textAlign'='center', 'color'=colors['H3'], 
               'width'='100%') # For card
style_H3 <- list('color'=colors['H3'], 'width'='100%') # For Charts Title

style_plot1 <- list('border-width'= '0', 'width'='100%', 'height'='970px')
style_plot2 <- list('border-width'='0', 'width'= '100%', 'height'= '400px')
style_plot3 <- list('border-width'='0', 'width'='100%', 'height'='400px')

style_card <- list('border'='1px solid #d3d3d3', 'border-radius'='10px')

#------------------------------------------------------------------------------


# Plotting Functions

#------------------------------------------------------------------------------

# plot 1 in R
plot_1 <- function(dff, drop1_chosen){
  barchart <- ggplot(data = dff, aes(x = drop1_chosen, y = reorder(city, -drop1_chosen))) +
    geom_bar()+ labs(y = "", x = (paste('Cost of', drop1_chosen))) + 
    scale_x_continuous(labels=scales::dollar_format())
  barchart1 = ggplotly(barchart + aes(text = list(drop1_chosen, 'province')), tooltip = 'text')
  return(htmlTable(barchart1))}

# plot 2 in R
plot_2 <- function(dff, drop_a, drop_b){
  chart = ggplot(data = dff, aes(x = drop_a, y = drop_b)) + geom_point() + 
    scale_y_continuous(labels=scales::dollar_format()) + 
    scale_x_continuous(labels=scales::dollar_format())
  chart1 = ggplotly(chart + aes(text = list('city', drop_a, drop_b)), tooltip = 'text')
  return(htmlTable(chart1))
}

### PLOT 3 FUNCTION IN PYTHON###
def plot_altair3(dff, drop_a, drop_b):  
  chart = alt.Chart(dff).mark_bar().encode(
    x = alt.X(drop_a, axis=alt.Axis(format='$.0f', title = None)),
    y = alt.Y('city',sort='x', axis=alt.Axis(title = None))
  ).transform_filter(alt.FieldOneOfPredicate(field='city', oneOf=drop_b)
  ).configure_axis(labelFontSize = 16)
return chart.to_html()

# plot 3 in R
plot_3 <- function(dff, drop_a, drop_b){
  ch = ggplot(data = dff, aes(x = drop_a, y = reorder('city', -drop_a))) + geom_bar() + 
    labs(x = '', y = "") + 
    scale_x_continuous(labels=scales::dollar_format())
  # what is the purpose of transform_filter above???? that's the only thing missing from fn
  return(htmlTable(ch))
  
}


#------------------------------------------------------------------------------

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout = dbcContainer(
  dbcRow(
    list(
      dbcCol(
        dbcCard(dbcCardBody(htmlH1('Where do you want to live?', style = style_H1), 
                      htmlH3('Cost of Living Dashboard', style = style_H2)),
        color = colors['background']),
      htmlBr(),
      
      ### CHECKLIST ###
      htmlH3("Select the Province: ", style = style_H3_c),
      dccChecklist(id='prov_checklist',                
        options=list('label'='Select all', 'value'='all', 'disabled'= False),
        value=list('all'),    # values chosen by default
        
        ### STYLES IN CHECKLIST ###
        className='my_box_container', 
        inputClassName='my_box_input',         
        labelClassName='my_box_label', 
        inputStyle=list("margin-right"="3px", "margin-left"="20px"),         
      ),
      htmlBr(),
      
      ### SLIDER ###
      htmlH3("Select City Population: ", style = style_H3_c),
      dccRangeSlider(id="population", min=0, max=2800000, step = 1000, 
                      marks= list(100000='100k',
                        500000= '500k',
                        1000000='1M',
                        1500000='1.5M',
                        2000000= '2M',
                        2500000='2.5M',
                        3000000='3M'),
                      value=list(0,2800000))), 
      md = 3, style = style_card),
    
    ### PLOT 1 LAYOUT###    
    dbcCol([
      dbcCol([
        htmlH3('Rank Cities by', style = style_H3), 
        ### DROPDOWN 1 ###
        dccDropdown(
          id='drop1',
          placeholder="Variables",
          value='meal_cheap',  
          options=[{'label': col, 'value': col} for col in df.columns[2:55]], # only including actual variables
          style = style_dropdown)], 
        style = {'display': 'flex'}),
      htmlIframe(
        id='plot1',
        style = style_plot1)], 
      style={"height": "10%"}),
    
    ### PLOT 2  LAYOUT ###
    dbcCol([
      dbcCol([htmlH3('Compare ', style = {'color': colors['H3']}),
               dccDropdown(
                 id='drop2_a',
                 value='meal_cheap', 
                 options=[{'label': col, 'value': col} for col in df.columns[2:55]], 
                 style = style_dropdown),
               htmlH3('and ', style  = {'color': colors['H3']}),
               dccDropdown(
                 id='drop2_b',
                 value='meal_mid', 
                 options=[{'label': col, 'value': col} for col in df.columns[2:55]], 
                 style =style_dropdown)], 
              style={'display':'flex'}),
      htmlIframe(
        id='plot2',
        style = style_plot2),
      htmlBr(),
      
      ### PLOT 3 LAYOUT ###
      dbcCol([htmlH3('Compare', style = style_H3),
               dccDropdown(
                 id='drop3_a',
                 value='meal_mid', 
                 options=[{'label': col, 'value': col} for col in df.columns[2:55]], 
                 style=style_dropdown),
               htmlH3('among Cities', style = style_H3),
               dcc.Dropdown(
                 id='drop3_b',
                 value=list('Vancouver', 'Toronto'), 
                 options=[{'label': cities, 'value': cities} for cities in df['city']], multi = True)],
              style={'width': '100%', 'font-family': 'arial', "font-size": "1.1em", 'font-weight': 'bold'}),
      htmlIframe(
        id='plot3',
        style=style_plot3)
    ])
  ])
])

#------------------------------------------------------------------------------


app$run_server(debug = T)
