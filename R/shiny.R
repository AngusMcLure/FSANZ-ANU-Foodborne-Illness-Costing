library(DT)
library(shiny)

source("./R/Trial Script.R")
DiseaseNames <- names(DiseaseAssumptions)

Years <- intersect(NNDSSIncidence$Year, AustralianPopulation$Year)
Measures <- c("Domestic", "Foodborne",
              "DomesticPerMillion", "FoodbornePerMillion")


ui <- fluidPage(
  titlePanel("Incidence of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel("Single Year and Pathogen",
                       fluidRow(column(width = 6,
                                       selectInput(
                                         "Disease",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseNames[1]
                                       )),
                                column(width = 6,
                                       selectInput(
                                         "Year",
                                         "Year",
                                         Years,
                                         selected = Years[length(Years)]
                                       )),
                       ),
                       tableOutput("mytable1")),
              tabPanel("Compare Years and Pathogens",
                       fluidRow(column(width = 4,
                                       selectInput(
                                         "Diseases",
                                         "Diseases",
                                         DiseaseNames,
                                         selected = DiseaseNames[1:3],
                                         multiple = TRUE
                                       )),
                                column(width = 4,
                                       selectInput(
                                         "Years",
                                         "Years",
                                         Years,
                                         selected = Years[length(Years)],
                                         multiple = TRUE
                                       )),
                                column(width = 4,
                                       selectInput(
                                         "Measure",
                                         "Measure",
                                         Measures,
                                         selected = Years[length(Years)],
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("mytable2"))
  )
)

server <- function(input, output) {

  output$mytable1 = shiny::renderTable({
    estimateIncidence(DiseaseAssumptions[[input$Disease]],
                      gastroRate = gastroRate,
                      population = AustralianPopulation[input$Year,2],
                      cases = NNDSSIncidence[input$Year, input$Disease],
                      ndraws = 10^6) %>%
      as.data.frame() %>%
      map(quantile, p = c(0.05,0.5,0.95)) %>%
      as.data.frame() %>%
      t()
  })

  output$mytable2 = DT::renderDataTable({
    Combinations <- expand.grid(Disease = sort(input$Diseases),
                                Year = sort(input$Years))
    cbind(Combinations,
          pmap_df(Combinations,
                  function(Disease, Year){
                    Disease <- as.character(Disease)
                    print(Disease)
                    Year <- as.character(Year)
                    print(Year)
                    estimateIncidence(DiseaseAssumptions[[Disease]],
                                      gastroRate = gastroRate,
                                      population = AustralianPopulation[Year,2],
                                      cases = NNDSSIncidence[Year, Disease],
                                      ndraws = 10^6)[[input$Measure]] %>%
                      quantile(p = c(0.05,0.5,0.95)) %>%
                      round
                  }))
  })
}

shinyApp(ui, server)
