library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(ggplot2)
library(dplyr)


data <- read.csv("data_extra.csv", header = TRUE, sep = ",")
provs = as.list(unique(data['province']))

app <- Dash$new()
app$layout(
  dccChecklist(id='prov_checklist',                
               options = map(
                 append("all",provs$province), function(x){
                   list('label'=x, 'value'=x)
                 }),
               value=list('all'),    # values chosen by default
               
               ### STYLES IN CHECKLIST ###
               className='my_box_container', 
               inputClassName='my_box_input',         
               labelClassName='my_box_label', 
               inputStyle=list("margin-right"="3px", "margin-left"="20px"),         
  )
)

app$callback(
  list(output("prov_checklist", "value"),
  output("all_checklist", "value")),
  list(input("prov_chosen", "value"),
       input("all_chosen", "value")),
  
  function(prov_chosen, all_chosen){
    ctx <- app$callback_context()
    input_id <- unlist(strsplit(ctx$triggered$prop_id, "[.]"))[0]
    if (input_id == "prov_checklist"){
      
      ifelse(set(prov_chosen) == set(provs), all_chosen = list("Select all"), all_chosen = list())
    }
    else {
      ifelse(all_chosen, prov_chosen = provs, prov_chosen = list())
    }
    return (prov_chosen, all_chosen)
  }
)

app$run_server()



