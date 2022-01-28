library(DT)
library(shiny)
library(tidyverse)
source("ClassDefinitions.R")
source("Distributions.R")
source("Diseases.R")
source("Outbreak.R")
source('estimationFunctions.R')



#setwd("R")
CostTable <- read.csv("CostTable.csv") %>%
  #subset(!(Disease == "IBS" & Pathogen %in% c("STEC", "Yersinia Enterocolitica"))) %>%
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
                       DT::dataTableOutput("ComparisonDT")),
              tabPanel('Outbreak',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Outbreak",
                                         "Pathogen",
                                         setdiff(PathogenNames,"All gastro pathogens"),
                                         selected = "Non-typhoidal salmonella",
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       checkboxInput(
                                         "DomesticMult",
                                         "Use domestic multiplier?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       checkboxInput(
                                         "UnderMult",
                                         "Use under-reporting multiplier for cases?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       checkboxInput(
                                         "UnderdiagMult",
                                         "Use under-reporting multiplier for deaths? [not implemented yet]",
                                         value = FALSE
                                       ))
                       ),
                       fluidRow(column(width = 3,
                                       textInput(
                                         "Notifications.Outbreak",
                                         "Confirmed Cases",
                                         '',
                                         placeholder = "e.g. 1,2,3 (musn't be left blank)"
                                       )),
                                column(width = 3,
                                       textInput(
                                         "Separations.Outbreak",
                                         "Hospitalisations",
                                         placeholder = 'e.g. 1,2,3 (or leave blank)'
                                       )),
                                column(width = 3,
                                       textInput(
                                         "Deaths.Outbreak",
                                         "Deaths",
                                         placeholder = 'e.g. 1,2,3 (or leave blank)'
                                       )),
                       ),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Disease.Outbreak",
                                         "Disease",
                                         Diseases,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Outbreak",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All Ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Outbreak",
                                         "Productivity Costs Method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("Outbreak")
              )
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
  output$SummaryDT = DT::renderDataTable({
    renderCostTableSummaries(input,CostTableSummaries,"Summary")
  })

  OutbreakCostSummary <- reactive({
    Outbreak <- PathogenAssumptions[[input$Pathogen.Outbreak]]
    if(!input$DomesticMult) Outbreak$domestic <- rdist('discrete', value = 1, continuous = FALSE)
    if(!input$UnderMult) Outbreak$underreporting <- rdist('discrete', value = 1, continuous = FALSE)
    separations <- list(read.input(input$Separations.Outbreak))
    names(separations) <- Outbreak$name
    deaths <- list(read.input(input$Deaths.Outbreak))
    names(deaths) <- Outbreak$name

    print('A')

    OutbreakCost <- costOutbreak(pathogen = Outbreak,
                                 notifications = read.input(input$Notifications.Outbreak),
                                 separations = separations,
                                 deaths = deaths,
                                 ndraws = 10^3)
    print('B')
    summariseCostList(list(Outbreak = OutbreakCost))$Categorised %>%
      mutate(across(c(`5%`,`95%`),~format(.x,big.mark = ",", scientific = FALSE, digits = 0)),
             median = round(median, digits = 0)) %>%
      mutate(`90% CI` = paste(`5%`,`95%`,sep = ' - ')) %>%
      rename("Cost (AUD)" = "median") %>%
      select(Pathogen, Disease, AgeGroup, CostCategory, `Cost (AUD)`, `90% CI`) %>%
      mutate(CostCategory = recode(CostCategory,
                                   Deaths = "Premature Mortality",
                                   FrictionLow = 'Friction-Low',
                                   FrictionHigh = 'Friction-High',
                                   HumanCapital = 'Human Capital',
                                   TotalFrictionHigh = 'Total.Friction-High',
                                   TotalFrictionLow = 'Total.Friction-Low',
                                   TotalHumanCapital = 'Total.Human Capital'))
  })
  output$Outbreak = DT::renderDataTable({
    renderCostTableSummaries(input,OutbreakCostSummary(),'Outbreak','Outbreak')
  })
}



read.input <- function(x){
  if(x == ''){out <- NULL}
  else{
    out <- as.list(as.numeric(strsplit(x, ',')[[1]]))
    names(out) <- c("<5","5-64","65+")
  }
  out
}

renderCostTableSummaries <- function(input,.CostTableSummaries,name,.Pathogen = NULL){
  .Productivity <- input[[paste0("Productivity.", name)]]
  .AgeGroup <- input[[paste0("AgeGroup.", name)]]
  if(is.null(.Pathogen)){
    .Pathogen <- input[[paste0("Pathogen.", name)]]
  }
  .Disease  <- input[[paste0("Disease.", name)]]
  cn <- colnames(.CostTableSummaries)
  CostCols <- cn[grep("Cost \\(",cn)]
  .CostTableSummaries %>%
    mutate(`Cost Category` = if_else(CostCategory == .Productivity,
                                     'Lost Productivity',
                                     if_else(CostCategory == paste0('Total.',.Productivity),
                                             'Total', CostCategory))) %>%
    subset(`Cost Category` %in% CostCategories &
             AgeGroup == .AgeGroup &
             Pathogen == .Pathogen &
             Disease == .Disease) %>%
    ungroup %>%
    select(-c(AgeGroup, Pathogen, Disease, CostCategory)) %>%
    DT::datatable(rownames = FALSE, options = list(order = list(2, 'asc'))) %>%
    DT::formatCurrency(columns = CostCols, currency = "", interval = 3, mark = ",", digits = 0)
}



shinyApp(ui, server)
