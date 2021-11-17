library(DT)
library(shiny)
library(tidyverse)

#setwd("R")
CostTable <- read.csv("CostTable.csv") %>%
  subset(!(Disease == "IBS" & Pathogen %in% c("STEC", "Yersinia Enterocolitica"))) %>%
  select(-X) %>%
  mutate(across(c(X5.,X95.),~format(.x/10^3,big.mark = ",", scientific = FALSE, digits = 0)),
         median = round(median/10^3, digits = 0)) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = ' - ')) %>%
  rename("Cost (thousands AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostItem, `Cost (thousands AUD)`, `90% CI`) %>%
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and Specialist Vists',
                           ED = 'ED Visits',
                           Deaths = "Premature Mortality",
                           WTPOngoing = 'WTP-Ongoing',
                           FrictionLow = 'Friction-Low',
                           FrictionHigh = 'Friction-High',
                           HumanCapital = 'Human Capital',
                           TotalFrictionHigh = 'Total.Friction-High',
                           TotalFrictionLow = 'Total.Friction-Low',
                           TotalHumanCapital = 'Total.Human Capital'))

CostTableSummaries <- read.csv("CostTableCategories.csv") %>%
  select(-X) %>%
  subset(!(Disease == "IBS" & Pathogen %in% c("STEC", "Yersinia Enterocolitica"))) %>%
  mutate(across(c(X5.,X95.),~format(.x/10^3,big.mark = ",", scientific = FALSE, digits = 0)),
         median = round(median/10^3, digits = 0)) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = ' - ')) %>%
  rename("Cost (thousands AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostCategory, `Cost (thousands AUD)`, `90% CI`) %>%
  mutate(CostCategory = recode(CostCategory,
                           Deaths = "Premature Mortality",
                           FrictionLow = 'Friction-Low',
                           FrictionHigh = 'Friction-High',
                           HumanCapital = 'Human Capital',
                           TotalFrictionHigh = 'Total.Friction-High',
                           TotalFrictionLow = 'Total.Friction-Low',
                           TotalHumanCapital = 'Total.Human Capital'))

EpiTable <- read.csv('EpiTable.csv') %>%
  select(-X) %>%
  subset(!(Disease == "IBS" & Pathogen %in% c("STEC", "Yersinia Enterocolitica"))) %>%
  mutate(across(c(median,X5.,X95.),~round(.x,2))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = '-')) %>%
  rename("Count" = "median") %>%
  select(Pathogen, Disease, AgeGroup, Measure, Count, `90% CI`)

ProductivityOptions <- c("Human Capital", "Friction-High", "Friction-Low")
TotalOptions <- paste0("Total.",ProductivityOptions)

EpiMeasures <- unique(EpiTable$Measure)
Measures <- c(setdiff(unique(CostTable$CostItem),c(ProductivityOptions,TotalOptions)), "Lost Productivity", "Total")
PathogenNames <- unique(EpiTable$Pathogen)
Diseases <- unique(EpiTable$Disease)
CostCategories <- c(setdiff(unique(CostTableSummaries$CostCategory), c(ProductivityOptions, paste0('Total.',ProductivityOptions))), "Lost Productivity", "Total")
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

  output$EpiDT = DT::renderDataTable(EpiTable %>%
                                       subset(Measure %in% input$EpiMeasures &
                                                AgeGroup %in% input$AgeGroups1 &
                                                Pathogen %in% input$Pathogen1 &
                                                Disease %in% input$Disease1),
                                     rownames = FALSE,
                                     server = FALSE,
                                     extensions = c("Buttons"),
                                     options = list(dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                                    )
                                     )
  output$SummaryDT = DT::renderDataTable({
    CostTableSummaries %>%
      mutate(`Cost Category` = if_else(CostCategory == input$Productivity.Summary,
                                       'Lost Productivity',
                                       if_else(CostCategory == paste0('Total.',input$Productivity.Summary),
                                               'Total', CostCategory))) %>%
      subset(`Cost Category` %in% CostCategories &
               AgeGroup == input$AgeGroup.Summary &
               Pathogen == input$Pathogen.Summary &
               Disease == input$Disease.Summary) %>%
      select(`Cost Category`, `Cost (thousands AUD)`,`90% CI`) %>%
      DT::datatable(rownames = FALSE,options = list(order = list(2, 'asc'))) %>%
      DT::formatCurrency(columns = "Cost (thousands AUD)", currency = "", interval = 3, mark = ",", digits = 0)
  })
  output$ComparisonDT = DT::renderDataTable({
    CostTable %>%
      mutate(CostItem = if_else(CostItem == input$Productivity,
                                'Lost Productivity',
                                if_else(CostItem == paste0('Total.',input$Productivity),
                                        'Total', CostItem))) %>%
      subset(CostItem %in% input$Measure &
               AgeGroup %in% input$AgeGroups &
               Pathogen %in% input$Pathogen &
               Disease %in% input$Disease) %>%
      rename(`Age group` = AgeGroup,
             `Cost Item` = CostItem) %>%
      DT::datatable(rownames = FALSE,
                    extensions = c("Buttons"),
                    options = list(dom = 'Bfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))
                    ) %>%
      DT::formatCurrency(columns = "Cost (thousands AUD)", currency = "", interval = 3, mark = ",", digits = 0)
  },
  server = FALSE
  )
}

shinyApp(ui, server)
