library(dash)
#library(dashCoreComponents)
#library(dashHtmlComponents)
library(ggplot2)
library(plotly)

### DEFINING ###
df <- read.csv("ricky/data_extra.csv", header = TRUE, sep = ",")
provs <- sort(unique(df$province))


### APP LAYOUT ###
#app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout = dbcContainer(
  dbcRow(
    list(
      dbcCol(
        dbcCard(dbcCardBody(htmlH1('Where do you want to live?'), 
                            htmlH3('Cost of Living Dashboard')),
                color = colors['background']),
        htmlBr(),
        
        ### CHECKLIST ###
        htmlH3("Select the Province: ", style = style_H3_c),
        dccChecklist(id='prov_checklist',                
                     options=list('label'='Select all', 'value'='all', 'disabled'= FALSE),  # modified by Sophie: false -> FALSE
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
                       marks= append(as.list(setNames(c(100000,500000), paste0(as.character(c(100000,500000)/1000), "k"))), 
                                     as.list(setNames(c(1000000, 1500000, 2000000, 2500000, 3000000), paste0(as.character(c(1000000, 1500000, 2000000, 2500000, 3000000)/1000000), "M")))),
                       value=list(0,2800000))), 
      md = 3, style = style_card),
    
    ### PLOT 1 LAYOUT###    
    dbcCol(list(
      dbcCol(list(
        htmlH3('Rank Cities by', style = style_H3), 
        ### DROPDOWN 1 ###
        dccDropdown(
          id='drop1',
          placeholder="Variables",
          value='meal_cheap',  
          options=for (col in colnames(data)[2:55]) {list('label' = col, 'value' = col)}, # only including actual variables
          style = style_dropdown), 
        style = list('display'= 'flex')),
        htmlIframe(
          id='plot1',
          style = style_plot1)), 
      style=list("height"= "10%"))),
    
    ### PLOT 2  LAYOUT ###
    dbcCol(list(
      dbcCol(list(
        htmlH3('Compare ', style = list('color'= colors['H3'])),
        dccDropdown(
          id='drop2_a',
          value='meal_cheap', 
          options=for (col in colnames(data)[2:55]) {list('label' = col, 'value' = col)}, 
          style = style_dropdown),
        htmlH3('and ', style  = list('color'= colors['H3'])),
        dccDropdown(
          id='drop2_b',
          value='meal_mid', 
          options=for (col in colnames(data)[2:55]) {list('label' = col, 'value' = col)}, 
          style =style_dropdown)), 
        style=list('display'= 'flex')),
      htmlIframe(
        id='plot2',
        style = style_plot2))),
    htmlBr(),
    
    ### PLOT 3 LAYOUT ###
    dbcCol(list(
      htmlH3('Compare', style = style_H3),
      dccDropdown(
        id='drop3_a',
        value='meal_mid', 
        options=for (col in colnames(data)[2:55]) {list('label' = col, 'value' = col)}, 
        style=style_dropdown),
      htmlH3('among Cities', style = style_H3),
      dccDropdown(
        id='drop3_b',
        value=list('Vancouver', 'Toronto'), 
        options=list(for (cities in data$city){list('label' = cities, 'value' = cities)}, multi = TRUE)),
      style=list('width'= '100%', 'font-family'= 'arial', "font-size"= "1.1em", 'font-weight'= 'bold')),
      htmlIframe(
        id='plot3',
        style=style_plot3)
    )
  )
)

app$run_server(debug = T)
