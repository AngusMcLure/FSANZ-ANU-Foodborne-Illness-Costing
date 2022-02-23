library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(mc2d) #for the standard PERT distribution parameterised by min, mode, max
load('Outputs/AusFBDiseaseImage-Light.RData', globalenv())
source("Outbreak.R")

CostTable <- read.csv("Outputs/CostTable.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x/1000<0.1, round(.x/1000,digits = 3),signif(.x/1000,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
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
                           TotalHumanCapital = 'Total.Human Capital'),
         Disease = ifelse(Disease == unlist(map(PathogenAssumptions, ~.x$name))[Pathogen],
                          'Initial Disease',
                          Disease))

CostTableSummaries <- read.csv("Outputs/CostTableCategories.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x/1000<0.1, round(.x/1000,digits = 3),signif(.x/1000,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
  mutate(`90% CI` = paste(X5.,X95.,sep = ' - ')) %>%
  rename("Cost (thousands AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostItem, `Cost (thousands AUD)`, `90% CI`) %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature Mortality",
                           FrictionLow = 'Friction-Low',
                           FrictionHigh = 'Friction-High',
                           HumanCapital = 'Human Capital',
                           TotalFrictionHigh = 'Total.Friction-High',
                           TotalFrictionLow = 'Total.Friction-Low',
                           TotalHumanCapital = 'Total.Human Capital'),
         Disease = ifelse(Disease == unlist(map(PathogenAssumptions, ~.x$name))[Pathogen],
                          'Initial Disease',
                          Disease))

EpiTable <- read.csv('Outputs/EpiTable.csv') %>%
  select(-X) %>%
  subset(!(Disease == "IBS" & Pathogen %in% c("STEC", "Yersinia Enterocolitica"))) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x<1000, round(.x,digits = 0),signif(.x,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%  mutate(`90% CI` = paste(X5.,X95.,sep = '-'),
         Disease = ifelse(Disease == unlist(map(PathogenAssumptions, ~.x$name))[Pathogen],
                          'Initial Disease',
                          Disease)) %>%
  rename("Count" = "median") %>%
  select(Pathogen, Disease, AgeGroup, Measure, Count, `90% CI`)

ProductivityOptions <- c("Human Capital", "Friction-High", "Friction-Low")
TotalOptions <- paste0("Total.",ProductivityOptions)

EpiMeasures <- unique(EpiTable$Measure)
Measures <- c(setdiff(unique(CostTable$CostItem),c(ProductivityOptions,TotalOptions)), "Lost Productivity", "Total")
PathogenNames <- unique(EpiTable$Pathogen)
Diseases <- unique(EpiTable$Disease)
CostCategories <- c(setdiff(unique(CostTableSummaries$CostItem), c(ProductivityOptions, paste0('Total.',ProductivityOptions))), "Lost Productivity", "Total")
AgeGroups <- unique(EpiTable$AgeGroup)

explainer_text <-
  paste0("The value of this multiplier is pathogen-specific, inlcudes uncertainty, ",
         "and is the same as in the FSANZ report \\'The annual cost of foodborne ",
         "illness in Australia\\' (2022). This multiplier will not be applied to ",
         "any death or hospitalisation figures supplied by the user.")

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Cost of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel('Epi Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Epi",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenNames,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Epi",
                                         "Disease",
                                         Diseases,
                                         selected = Diseases,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Epi",
                                         "AgeGroups",
                                         AgeGroups,
                                         selected = AgeGroups,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measures.Epi",
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
                                         "Pathogen.Detailed",
                                         "Pathogens",
                                         PathogenNames,
                                         selected = PathogenNames,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Detailed",
                                         "Diseases",
                                         Diseases,
                                         selected = Diseases,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Detailed",
                                         "Age groups",
                                         AgeGroups,
                                         selected = AgeGroups,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measure.Detailed",
                                         "Cost Items",
                                         Measures,
                                         selected = Measures,
                                         multiple = TRUE
                                       ))
                       ),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Productivity.Detailed",
                                         "Productivity Costs Method",
                                         ProductivityOptions,
                                         selected = "HumanCapital",
                                         multiple = FALSE
                                       ))),
                       DT::dataTableOutput("ComparisonDT")),
              tabPanel('Outbreak',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "PathogenOutbreak",
                                         "Pathogen",
                                         setdiff(PathogenNames,"All gastro pathogens"),
                                         selected = "Non-typhoidal salmonella",
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "UnderMult",
                                         "Use multiplier for under-reporting of cases?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "FoodborneMult",
                                         "Use multiplier for food-borne acquisition?",
                                         value = FALSE
                                       )),
                                column(width = 3,
                                       br(),
                                       checkboxInput(
                                         "DomesticMult",
                                         "Use multiplier for domestic acquisition?",
                                         value = FALSE
                                       ))
                       ),
                       bsPopover("DomesticMult",
                                 'Domestically Acquired Multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were acquired domestically and you want to exclude those acquired elsewhere.",
                                       explainer_text,
                                       "This option is only available for nationally notifiable diseases.")),
                       bsPopover("UnderMult",
                                 'Under-reporting multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were identified and you want to adjust estimates (up) to account for unidentified cases.",
                                       explainer_text,
                                       "This option is only available for nationally notifiable diseases.")),
                       bsPopover("FoodborneMult",'Foodborne multiplier',
                                 paste("Select this option if you think only a fraction of outbreak cases were acquired from food and you want to exclude those acquired via other routes.",
                                       explainer_text)),
                       fluidRow(column(width = 3,
                                       textInput(
                                         "NotificationsOutbreak",
                                         "Confirmed cases for ages <5, 5-64, 65+",
                                         '',
                                         placeholder = "e.g. 1,2,3 (musn't be left blank)"
                                       )),
                                column(width = 3,
                                       textInput(
                                         "SeparationsOutbreak",
                                         "Hospitalisations for ages <5, 5-64, 65+" ,
                                         placeholder = 'leave blank to estimate from cases'
                                       )),
                                column(width = 3,
                                       textInput(
                                         "DeathsOutbreak",
                                         "Deaths for ages <5, 5-64, 65+",
                                         placeholder = 'leave blank to estimate from cases'
                                       )),
                                column(width = 3,
                                       br(),
                                       actionButton("Trigger.Outbreak", "Estimate")
                                )
                       ),
                       bsPopover("NotificationsOutbreak",
                                 'Known number of infections associated with the outbreak',
                                 "The total number of cases will be adjusted up or down if the above multipliers are selected. The number of cases are used to estimate number of sequelae (if any) and number of hospitalisations and deaths due to the initial infection (if not provided)" ),
                       bsPopover("SeparationsOutbreak",
                                 "Known number of hospitalisations associated with outbreak",
                                 "Estimated from confirmed cases if left blank. Should only inlcude hospitalisations due to initial infections; hospitalisations due to sequelae are estimated from number of cases" ),
                       bsPopover("DeathsOutbreak",
                                 "Known number of deaths associated with outbreak",
                                 "Estimated from confirmed cases if left blank. Should only inlcude deaths due to initial infections; deaths due to sequelae are estimated from number of cases" ),
                       hr(),
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
                       DT::dataTableOutput("OutbreakDT")
              )
  )
)

server <- function(input, output) {

  observe({
    OutbreakNotifiableSelected <- PathogenAssumptions[[input$PathogenOutbreak]]$caseMethod == "Notifications"
    toggleState(id = "UnderMult", condition =  OutbreakNotifiableSelected)
    toggleState(id = "DomesticMult", condition =  OutbreakNotifiableSelected)
    if(!OutbreakNotifiableSelected){
      updateCheckboxInput(inputId = "UnderMult", value = F)
      updateCheckboxInput(inputId = 'DomesticMult', value = F)
    }
  })

  output$EpiDT = DT::renderDataTable(EpiTable %>%
                                       subset(Measure %in% input$Measures.Epi &
                                                AgeGroup %in% input$AgeGroup.Epi &
                                                Pathogen %in% input$Pathogen.Epi &
                                                Disease %in% input$Disease.Epi),
                                     rownames = FALSE,
                                     server = FALSE,
                                     extensions = c("Buttons"),
                                     options = list(dom = 'Bfrtip',
                                                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                                     )
  )

  output$ComparisonDT <- DT::renderDataTable({
    renderCostTableDetailed(input,CostTable,'Detailed')
  },
  server = FALSE)

  output$SummaryDT = DT::renderDataTable({
    renderCostTableSummaries(input,CostTableSummaries,"Summary")
  })

  OutbreakSummary <- reactive({
    Outbreak <- PathogenAssumptions[[input$PathogenOutbreak]]
    #All pathogens need to use the notification method of estimating case numbers for outbreak costing purposes
    #this requires domestic and undereporting multipliers which we set to 1 by default
    if(Outbreak$caseMethod != 'Notifications'){
      Outbreak$domestic <- rdist('discrete', value = 1, continuous = FALSE)
      Outbreak$underreporting <- rdist('discrete', value = 1, continuous = FALSE)
      Outbreak$caseMethod <- 'Notifications'
    }
    if(!input$DomesticMult) Outbreak$domestic <- rdist('discrete', value = 1, continuous = FALSE)
    if(!input$UnderMult) Outbreak$underreporting <- rdist('discrete', value = 1, continuous = FALSE)
    if(!input$FoodborneMult) Outbreak$foodborne <- rdist('discrete', value = 1, continuous = FALSE)
    separations <- list(read.input(input$SeparationsOutbreak))
    names(separations) <- Outbreak$name
    deaths <- list(read.input(input$DeathsOutbreak))
    names(deaths) <- Outbreak$name

    OutbreakCost <- costOutbreak(pathogen = Outbreak,
                                 notifications = read.input(input$NotificationsOutbreak),
                                 separations = separations,
                                 deaths = deaths,
                                 ndraws = 10^3)$costs

    summariseCostList(list(Outbreak = OutbreakCost))[['Categorised']] %>%
      mutate(across(c(median,`5%`,`95%`),~format(ifelse(.x<1000, round(.x,digits = 0),signif(.x,3)),
                                                 big.mark = ",",
                                                 scientific = FALSE,
                                                 trim = T,
                                                 drop0trailing = TRUE))) %>%
      mutate(`90% CI` = paste(`5%`,`95%`,sep = ' - ')) %>%
      rename("Cost (AUD)" = "median") %>%
      select(-c(`5%`, `95%`)) %>%
      mutate(CostItem = recode(CostItem,
                               Deaths = "Premature Mortality",
                               FrictionLow = 'Friction-Low',
                               FrictionHigh = 'Friction-High',
                               HumanCapital = 'Human Capital',
                               TotalFrictionHigh = 'Total.Friction-High',
                               TotalFrictionLow = 'Total.Friction-Low',
                               TotalHumanCapital = 'Total.Human Capital'),
             Disease = ifelse(Disease == Outbreak$name,
                              'Initial Disease',
                              Disease))
  })


  # Only display once users click the Estimate button AND clear display any time
  # the user changes the inputs defining the outbreak
  display <- reactiveValues(display = FALSE)

  observe({display$display <- FALSE}) %>%
    bindEvent(input$PathogenOutbreak,
              input$FoodborneMult,
              input$UnderMult,
              input$DomesticMult,
              input$DeathsOutbreak,
              input$SeparationsOutbreak,
              input$NotificationsOutbreak)
  observe({display$display <- TRUE}) %>%
    bindEvent(input$Trigger.Outbreak)


  output$OutbreakDT = DT::renderDataTable({
    if(!display$display){
      data.frame()
    }else{
      if(input$NotificationsOutbreak == ''){
        stop('Confirmed number of cases in each age group must be provided')
      }
      renderCostTableSummaries(input,
                               OutbreakSummary(),
                               'Outbreak','Outbreak')
    }
  })
}



read.input <- function(x){
  if(x == ''){out <- NULL}
  else{
    out <- as.list(as.numeric(strsplit(x, ',')[[1]]))
    if(length(out) != 3) stop('Number of cases, hospitalisations, and deaths must either be left blank or must exactly three numbers (one for each agegroup <5, 5-64 , 65+) seperated by commas')
    if(any(is.na(out))) stop('Numbers of cases, hospitalisation, and deaths must be numbers seperated by commas (or blank)')
    names(out) <- c("<5","5-64","65+")
  }
  out
}

renderCostTableSummaries <- function(input,.CostTableSummaries,tabname,.Pathogen = NULL){
  .Productivity <- input[[paste0("Productivity.", tabname)]]
  .AgeGroup <- input[[paste0("AgeGroup.", tabname)]]
  if(is.null(.Pathogen)){
    .Pathogen <- input[[paste0("Pathogen.", tabname)]]
  }
  .Disease  <- input[[paste0("Disease.", tabname)]]
  cn <- colnames(.CostTableSummaries)
  CostCols <- cn[grep("Cost \\(",cn)]


  .CostTableSummaries %>%
    mutate(`Cost Category` = if_else(CostItem == .Productivity,
                                     'Lost Productivity',
                                     if_else(CostItem == paste0('Total.',.Productivity),
                                             'Total', CostItem))) %>%
    subset(`Cost Category` %in% CostCategories &
             AgeGroup == .AgeGroup &
             Pathogen == .Pathogen &
             Disease == .Disease) %>%
    ungroup %>%
    select(-c(AgeGroup, Pathogen, Disease, CostItem)) %>%
    relocate(`Cost Category`) %>%
    DT::datatable(rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(searching = FALSE,
                                 order = list(2, 'asc'),
                                 dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) #%>%
    #DT::formatCurrency(columns = CostCols, currency = "", interval = 3, mark = ",", digits = 0)
}


renderCostTableDetailed <- function(input,.CostTableDetailed, tabname,.Pathogen = NULL){
  .Productivity <- input[[paste0("Productivity.", tabname)]]
  .AgeGroup <- input[[paste0("AgeGroup.", tabname)]]
  if(is.null(.Pathogen)){
    .Pathogen <- input[[paste0("Pathogen.", tabname)]]
  }
  .Disease  <- input[[paste0("Disease.", tabname)]]
  .Measure  <- input[[paste0("Measure.", tabname)]]

  CostTable %>%
    mutate(CostItem = if_else(CostItem == .Productivity,
                              'Lost Productivity',
                              if_else(CostItem == paste0('Total.',.Productivity),
                                      'Total', CostItem))) %>%
    subset(CostItem %in% .Measure &
             AgeGroup %in% .AgeGroup &
             Pathogen %in% .Pathogen &
             Disease %in% .Disease) %>%
    rename(`Age group` = AgeGroup,
           `Cost Item` = CostItem) %>%
    DT::datatable(rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) # %>%
    # DT::formatCurrency(columns = "Cost (thousands AUD)", currency = "", interval = 3, mark = ",", digits = 0)

}

shinyApp(ui, server)
