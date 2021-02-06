library(shiny)
library(ggplot2)
library(usmap)
library(usdata)

years <- c(1990:2019)
allStates <- sort(c(state.name, "Washington DC", "Total US"))
allStatesAbb <- sort(c(state.abb, "DC", "US-TOTAL"))
allStatesAbb3 <- sort(c(state.name = state.abb, "Washington DC" = "DC", "Total US" = "US-TOTAL"))
energySources <- sort(c("Coal", "Geothermal", "Hydro", "Natural Gas",
                        "Nuclear", "Petroleum", "Solar", "Wind", "Wood", "All"))
colorBlindPalette <- c("Coal" = "#000000", "Geothermal" = "#E69F00",
                       "Hydro" = "#56B4E9", "Natural Gas" = "#0072B2",
                       "Nuclear" = "#CC79A7", "Petroleum" = "#D55E00",
                       "Solar" = "#F0E442", "Wind" = "#999999", "Wood" = "#009E73")

generations <- read.csv("annual_generation_state.csv", header=TRUE, sep = ",")
class(generations)
generations$STATE <- toupper(generations$STATE)

#transforming the megawatthours into nums instead of strings
generations$GENERATION..Megawatthours.<- gsub(',', '', generations$GENERATION..Megawatthours.)
generations$GENERATION..Megawatthours. <- as.numeric(generations$GENERATION..Megawatthours.)

generations$STATE <- as.factor(generations$STATE)
generations$TYPE.OF.PRODUCER <- as.factor(generations$TYPE.OF.PRODUCER)
generations$ENERGY.SOURCE <- as.factor(generations$ENERGY.SOURCE)

#simplifying the dataset to remove negatives and unnecessary energy sources
newGenerations <- subset(generations, generations$GENERATION..Megawatthours. >= 0 & 
                           generations$ENERGY.SOURCE != "Other" & 
                           generations$ENERGY.SOURCE != "Other Gases" &
                           generations$ENERGY.SOURCE != "Other Biomass" &
                           generations$ENERGY.SOURCE != "Pumped Storage" &
                           generations$STATE != "  ")
#removes any unused factors such as "  " in STATE or "Other" in ENERGY.SOURCE
newGenerations$STATE <- factor(newGenerations$STATE)
newGenerations$ENERGY.SOURCE <- factor(newGenerations$ENERGY.SOURCE)

#rename Energy Sources to something more compact
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Wood and Wood Derived Fuels"] <- "Wood"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Hydroelectric Conventional"] <- "Hydro"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"

ui <- fluidPage(
  titlePanel("CS 424 Spring 2021 Project 1"),
  selectInput("State", "Select a state to view data", allStatesAbb, selected = "IL"),
  #selectInput("State2", "Select a state to view data", allStatesAbb, selected = "US-TOTAL"),
  plotOutput("hist0"),
  plotOutput("hist1"),
  checkboxGroupInput("Energy", "Select an energy source", 
                     energySources, selected = "All"),
  plotOutput("hist2")
)

server <- function(input, output) {
  #make the data reusable for multiple charts
  newGenerationReactive <- reactive({subset(newGenerations, newGenerations$STATE == input$State)})
  
  #stacked bar with total amount of each energy source
  output$hist0 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(subset(newGeneration, newGeneration$ENERGY.SOURCE != "Total"), 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity") + scale_fill_manual(values = colorBlindPalette)
  })
  
  #stacked bar with percent of total production for each energy source per year
  output$hist1 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(subset(newGeneration, newGeneration$ENERGY.SOURCE != "Total"), 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="fill", stat="identity") + scale_fill_manual(values = colorBlindPalette)
  })
  
  output$hist2 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(subset(newGeneration, newGeneration$ENERGY.SOURCE != "Total"), 
           aes(x=YEAR, y=GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "line") + scale_color_manual(values = colorBlindPalette)
  })
}

shinyApp(ui = ui, server = server)