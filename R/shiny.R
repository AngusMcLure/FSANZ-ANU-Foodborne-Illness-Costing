library(DT)
library(shiny)
library(tidyverse)

#setwd("R")

CostTable <- read.csv("CostTable.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~round(.x/10^6,2))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = '-')) %>%
  rename("Cost (millions AUD)" = "median") %>%
  select(-c(X5.,X95.))
CostTable
Measures <- unique(CostTable$CostItem)
PathogenNames <- unique(CostTable$Pathogen)
Diseases <- unique(CostTable$Disease)

AgeGroups <- c("<5", "5-64", "65+")


ui <- fluidPage(
  titlePanel("Cost of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel("Compare Years and Pathogens",
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenNames,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroups",
                                         "AgeGroups",
                                         AgeGroups,
                                         selected = AgeGroups,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measure",
                                         "Measure",
                                         Measures,
                                         selected = Measures,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease",
                                         "Disease",
                                         Diseases,
                                         selected = Diseases,
                                         multiple = TRUE
                                       ))
                       ),
                       DT::dataTableOutput("mytable1"))
  )
)

server <- function(input, output) {

  output$mytable1 = DT::renderDataTable(CostTable %>%
                                          subset(CostItem %in% input$Measure &
                                                   AgeGroup %in% input$AgeGroups &
                                                   Pathogen %in% input$Pathogen &
                                                   Disease %in% input$Disease))
}

shinyApp(ui, server)
