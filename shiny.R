library(DT)
library(shiny)
library(shinyBS)
library(shinyjs)
library(tidyverse)
library(stringr)
library(mc2d) #for the standard PERT distribution parameterised by min, mode, max
load('Outputs/AusFBDiseaseImage-Light.RData', globalenv())
source("Outbreak.R")

CPIData <- read.csv('./Data/CPI-ABS.csv',skip = 1,
                    col.names = c('Quarter', 'Change.Quarterly', 'Change.Annualised')) %>%
  drop_na() %>%
  mutate(Date = as_date(paste0('01-',Quarter),format = '%d-%m-%y')) %>%
  subset(Date > as_date('2019-12-01')) %>%
  arrange(Date) %>%
  mutate(Cumm.Inflation.Multiplier = cumprod(1+Change.Quarterly/100),
         CummCPI = 100*(Cumm.Inflation.Multiplier-1))
InitialDiseaseNames <- c(unlist(map(PathogenAssumptions, ~.x$name)), `All pathogens` = 'Initial')

CostTable <- read.csv("Outputs/CostTable.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x/1000<0.1, round(.x/1000,digits = 3),signif(.x/1000,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
  mutate(`90% UI` = paste(X5.,X95.,sep = ' - ')) %>%
  rename("Cost (thousands AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostItem, `Cost (thousands AUD)`, `90% UI`) %>%
  mutate(CostItem = recode(CostItem,
                           GPSpecialist = 'GP and specialist vists',
                           ED = 'Emergency department visits',
                           Deaths = "Premature mortality",
                           WTP = "Pain and suffering",
                           WTPOngoing = "Pain and suffering (ongoing illness)",
                           FrictionLow = 'Friction (low)',
                           FrictionHigh = 'Friction (high)',
                           HumanCapital = 'Human capital',
                           TotalFrictionHigh = 'Total.Friction (high)',
                           TotalFrictionLow = 'Total.Friction (low)',
                           TotalHumanCapital = 'Total.Human capital'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease))

CostTableSummaries <- read.csv("Outputs/CostTableCategories.csv") %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x/1000<0.1, round(.x/1000,digits = 3),signif(.x/1000,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
  mutate(`90% UI` = paste(X5.,X95.,sep = ' - ')) %>%
  rename("Cost (thousands AUD)" = "median") %>%
  select(Pathogen, Disease, AgeGroup, CostItem, `Cost (thousands AUD)`, `90% UI`) %>%
  mutate(CostItem = recode(CostItem,
                           Deaths = "Premature Mortality",
                           FrictionLow = 'Friction (low)',
                           FrictionHigh = 'Friction (high)',
                           HumanCapital = 'Human capital',
                           TotalFrictionHigh = 'Total.Friction (high)',
                           TotalFrictionLow = 'Total.Friction (low)',
                           TotalHumanCapital = 'Total.Human capital'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease))

EpiTable <- read.csv('Outputs/EpiTable.csv') %>%
  select(-X) %>%
  mutate(across(c(median,X5.,X95.),~format(ifelse(.x<1000, round(.x,digits = 0),signif(.x,3)),
                                           big.mark = ",",
                                           scientific = FALSE,
                                           trim = T,
                                           drop0trailing = TRUE))) %>%
  mutate(`90% UI` = paste(X5.,X95.,sep = '-'),
         Disease = ifelse(Disease == InitialDiseaseNames[Pathogen],
                          'Initial disease',
                          Disease)) %>%
  rename("Count" = "median") %>%
  select(Pathogen, Disease, AgeGroup, Measure, Count, `90% UI`)

ProductivityOptions <- c("Human capital", "Friction (high)", "Friction (low)")
TotalOptions <- paste0("Total.",ProductivityOptions)

EpiMeasures <- unique(EpiTable$Measure)
CostItems <- c(setdiff(unique(CostTable$CostItem),c(ProductivityOptions,TotalOptions)), "Non-fatal productivity losses", "Total")
PathogenNames <- unique(EpiTable$Pathogen) %>% setdiff(c('All pathogens', 'All gastro pathogens')) %>% sort %>% c('All pathogens', 'All gastro pathogens',.)
DiseaseNames <- unique(EpiTable$Disease) %>% setdiff(c('Initial disease', 'Initial and sequel disease')) %>% sort %>% c('Initial disease', 'Initial and sequel disease',.)
CostCategories <- c(setdiff(unique(CostTableSummaries$CostItem), c(ProductivityOptions, paste0('Total.',ProductivityOptions))), "Non-fatal productivity losses", "Total")
AgeGroups <- unique(EpiTable$AgeGroup)

PathogenSelect <- c('All pathogens','Norovirus','Campylobacter')
DiseaseSelect <- c('Initial disease','Initial and sequel disease')
MeasureSelect <- c('Hospitalisations','Cases')
CostItemSelect <- c('Total','Hospitalisation')
AgeGroupSelect <- 'All ages'

explainer_text <-
  paste0("The value of this multiplier is pathogen-specific, inlcudes uncertainty, ",
         "and is the same as in the FSANZ report \\'The annual cost of foodborne ",
         "illness in Australia\\' (2022). This multiplier will not be applied to ",
         "any death or hospitalisation figures supplied by the user.")

#render the starting page to html -- moved away from this approach as it was preventing the tables from displaying without an error that let me debug...
#rmarkdown::render('InfoText.Rmd')

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Cost of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel('Info',
                       #fluidRow(includeHTML('./InfoText.html')),
                       column(width = 1),
                       column(width = 10,
                              fluidRow(includeMarkdown('./InfoText.md')),
                              fluidRow(selectInput('Quarter.Inflation',
                                                   'Quarter for Inflation adjustment',
                                                   c('Dec-19 (Baseline - No adjustment)',CPIData$Quarter),
                                                   selected = 'Dec-19 (Baseline - No adjustment)',
                                                   multiple = FALSE)
                                       ),
                              fluidRow(includeMarkdown('./Whitespace.md')) ## adds whitespace after selection so that it doesn't run off screen. There is probably a better wat to do this but I couldn't find one easily!
                              )
                       ),
              tabPanel('Epi Summaries',
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Pathogen.Epi",
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Epi",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Epi",
                                         "Age group",
                                         AgeGroups,
                                         selected = AgeGroupSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Measures.Epi",
                                         "Measure",
                                         EpiMeasures,
                                         selected = MeasureSelect,
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
                                         DiseaseNames,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Summary",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Summary",
                                         "Non-fatal productivity losses method",
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
                                         "Pathogen",
                                         PathogenNames,
                                         selected = PathogenSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Disease.Detailed",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Detailed",
                                         "Age group",
                                         AgeGroups,
                                         selected = AgeGroupSelect,
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "CostItem.Detailed",
                                         "Cost items",
                                         CostItems,
                                         selected = CostItemSelect,
                                         multiple = TRUE
                                       ))
                       ),
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Productivity.Detailed",
                                         "Non-fatal productivity losses method",
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
                                         setdiff(PathogenNames,c("All gastro pathogens",'All pathogens')),
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
                                         DiseaseNames,
                                         selected = 'All Diseases',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "AgeGroup.Outbreak",
                                         "Age group",
                                         AgeGroups,
                                         selected = 'All ages',
                                         multiple = FALSE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Productivity.Outbreak",
                                         "Non-fatal productivity losses method",
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
  },
  server = FALSE)

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
      mutate(`90% UI` = paste(`5%`,`95%`,sep = ' - ')) %>%
      rename("Cost (AUD)" = "median") %>%
      select(-c(`5%`, `95%`)) %>%
      mutate(CostItem = recode(CostItem,
                               Deaths = "Premature Mortality",
                               FrictionLow = 'Friction (low)',
                               FrictionHigh = 'Friction (high)',
                               HumanCapital = 'Human capital',
                               TotalFrictionHigh = 'Total.Friction (high)',
                               TotalFrictionLow = 'Total.Friction (low)',
                               TotalHumanCapital = 'Total.Human capital'),
             Disease = ifelse(Disease == Outbreak$name,
                              'Initial disease',
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
  },
  server = FALSE)
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
                                     'Non-fatal productivity losses',
                                     if_else(CostItem == paste0('Total.',.Productivity),
                                             'Total', CostItem))) %>%
    subset(`Cost Category` %in% CostCategories &
             AgeGroup == .AgeGroup &
             Pathogen == .Pathogen &
             Disease == .Disease) %>%
    select(-c(AgeGroup, Pathogen, Disease, CostItem)) %>%
    relocate(`Cost Category`) %>%
    DT::datatable(rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(searching = FALSE,
                                 order = list(1, 'asc'),
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
  .CostItem  <- input[[paste0("CostItem.", tabname)]]

  CostTable %>%
    mutate(CostItem = if_else(CostItem == .Productivity,
                              'Non-fatal productivity losses',
                              if_else(CostItem == paste0('Total.',.Productivity),
                                      'Total', CostItem))) %>%
    subset(CostItem %in% .CostItem &
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
