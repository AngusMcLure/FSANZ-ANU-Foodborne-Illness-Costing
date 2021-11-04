library(DT)
library(shiny)
library(tidyverse)

#setwd("R")

CostTable <- read.csv("CostTable.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~round(.x/10^6,2))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = '-')) %>%
  rename("Cost (millions AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostItem, `Cost (millions AUD)`, `90% CI`) %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature mortality",
                           WTPOngoing = 'WTP-Ongoing',
                           FrictionLow = 'Friction-Low',
                           FrictionHigh = 'Friction-High',
                           HumanCapital = 'Human Capital'))

CostTableSummaries <- read.csv("CostTableCategories.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~round(.x/10^6,2))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = '-')) %>%
  rename("Cost (millions AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostCategory, `Cost (millions AUD)`, `90% CI`) %>%
  mutate(CostCategory = recode(CostCategory,
                           Deaths = "Premature mortality",
                           FrictionLow = 'Friction-Low',
                           FrictionHigh = 'Friction-High',
                           HumanCapital = 'Human Capital'))



EpiTable <- read.csv('EpiTable.csv') %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~round(.x,2))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = '-')) %>%
  rename("Count" = "median") %>%
  select(Pathogen, Disease, AgeGroup, Measure, Count, `90% CI`)

ProductivityOptions <- c("Human Capital", "Friction-High", "Friction-Low")

EpiMeasures <- unique(EpiTable$Measure)
Measures <- c(setdiff(unique(CostTable$CostItem),ProductivityOptions), "Lost Productivity")
PathogenNames <- c(setdiff(unique(EpiTable$Pathogen), "All Pathogens"),"All Pathogens")
Diseases <- unique(EpiTable$Disease)
CostCategories <- c(setdiff(unique(CostTableSummaries$CostCategory), ProductivityOptions), "Lost Productivity")

AgeGroups <- unique(EpiTable$AgeGroup)


ui <- fluidPage(
  titlePanel("Cost of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel('Epi Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen1",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenNames,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease1",
                                         "Disease",
                                         Diseases,
                                         selected = Diseases,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroups1",
                                         "AgeGroups",
                                         AgeGroups,
                                         selected = AgeGroups,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "EpiMeasures",
                                         "Measure",
                                         EpiMeasures,
                                         selected = EpiMeasures,
                                         multiple = TRUE
                                       ))
                       ),
                       DT::dataTableOutput("EpiDT")),
              tabPanel('Cost Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Summary",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenNames[1],
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Summary",
                                         "Disease",
                                         Diseases,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Summary",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All Ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Summary",
                                         "Productivity Costs Method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("SummaryDT")),
              tabPanel("Cost Comparisons",
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen",
                                         "Pathogens",
                                         PathogenNames,
                                         selected = PathogenNames,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease",
                                         "Diseases",
                                         Diseases,
                                         selected = Diseases,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroups",
                                         "Age groups",
                                         AgeGroups,
                                         selected = AgeGroups,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measure",
                                         "Cost Items",
                                         Measures,
                                         selected = Measures,
                                         multiple = TRUE
                                       ))
                       ),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Productivity",
                                         "Productivity Costs Method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))),
                       DT::dataTableOutput("ComparisonDT"))
  )
)

server <- function(input, output) {

  output$EpiDT = DT::renderDataTable({
    EpiTable %>%
      subset(Measure %in% input$EpiMeasures &
               AgeGroup %in% input$AgeGroups1 &
               Pathogen %in% input$Pathogen1 &
               Disease %in% input$Disease1)
  }, rownames = FALSE)
  output$SummaryDT = DT::renderDataTable({
    CostTableSummaries %>%
      mutate(`Cost Category` = if_else(CostCategory == input$Productivity.Summary,'Lost Productivity',CostCategory)) %>%
      subset(`Cost Category` %in% CostCategories &
               AgeGroup == input$AgeGroup.Summary &
               Pathogen == input$Pathogen.Summary &
               Disease == input$Disease.Summary) %>%
      select(`Cost Category`, `Cost (millions AUD)`,`90% CI`)

  }, rownames = FALSE)
  output$ComparisonDT = DT::renderDataTable({
    CostTable %>%
      mutate(CostItem = if_else(CostItem == input$Productivity,'Lost Productivity',CostItem)) %>%
      subset(CostItem %in% input$Measure &
               AgeGroup %in% input$AgeGroups &
               Pathogen %in% input$Pathogen &
               Disease %in% input$Disease) %>%
      rename(`Age group` = AgeGroup,
             `Cost Item` = CostItem)
  }, rownames = FALSE)

}

shinyApp(ui, server)
