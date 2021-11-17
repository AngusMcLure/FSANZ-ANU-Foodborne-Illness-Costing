library(DT)
library(shiny)

source("./R/Trial Script.R")
DiseaseNames <- names(PathogenAssumptions)

Years <- intersect(NNDSSIncidenceAgegroup$Year, AusPopAgegroup$Year)
Measures <- c("Domestic", "Foodborne",
              "DomesticPerMillion", "FoodbornePerMillion")
AgeGroups <- c("<5", "5-64", "65+")


ui <- fluidPage(
  titlePanel("Incidence of Foodborne Diseases in Australia"),
  tabsetPanel(type = "pills",
              tabPanel("Single Year and Pathogen",
                       fluidRow(column(width = 6,
                                       selectInput(
                                         "Disease",
                                         "Disease",
                                         DiseaseNames,
                                         selected = DiseaseNames[2]
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
                       fluidRow(column(width = 3,
                                       selectInput(
                                         "Diseases",
                                         "Diseases",
                                         DiseaseNames,
                                         selected = DiseaseNames[2],
                                         multiple = TRUE
                                       )),
                                column(width = 3,
                                       selectInput(
                                         "Years",
                                         "Years",
                                         Years,
                                         selected = Years[length(Years)],
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
                                         selected = Years[length(Years)],
                                         multiple = FALSE
                                       ))
                       ),
                       DT::dataTableOutput("mytable2"))
  )
)

server <- function(input, output) {

  output$mytable1 = shiny::renderTable({
    estimateIncidence(PathogenAssumptions[[input$Disease]],
                      gastroRate = gastroRate,
                      population = subset(AusPopAgegroup,
                                          Year == input$Year &
                                            AgeGroup == input$AgeGroup)$Persons,
                      notifications = subset(NNDSSIncidenceAgegroup,
                                             Year == input$Year &
                                               Disease == input$Disease)$Cases,
                      ndraws = 10^6) %>%
      as.data.frame() %>%
      map(quantile, p = c(0.05,0.5,0.95)) %>%
      as.data.frame() %>%
      t()
  })

  output$mytable2 = DT::renderDataTable({
    Combinations <- expand.grid(Disease = sort(input$Diseases),
                                Year = sort(input$Years),
                                AgeGroup = sort(input$AgeGroups))
    cbind(Combinations,
          pmap_df(Combinations,
                  function(Disease, Year, AgeGroup){
                    estimateIncidence(PathogenAssumptions[[Disease]],
                                      gastroRate = gastroRate,
                                      population = subset(AusPopAgegroup,
                                                          Year == Year &
                                                            AgeGroup == AgeGroup)$Persons,
                                      notifications = subset(NNDSSIncidenceAgegroup,
                                                             Year == Year &
                                                               Disease == Disease &
                                                               AgeGroup == AgeGroup)$Cases,
                                      ndraws = 10^6)[[input$Measure]] %>%
                      quantile(p = c(0.05,0.5,0.95)) %>%
                      round
                  }))
  })
}

shinyApp(ui, server)
