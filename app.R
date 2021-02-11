library(shiny)
library(ggplot2)
library(usmap)
library(usdata)
library(DT)
library(dplyr)

years <- c(1990:2019)
allStates <- sort(c(state.name, "Washington DC", "Total US"))
allStatesAbb <- sort(c(state.abb, "DC", "US-TOTAL"))
energySources <- sort(c("Coal", "Geothermal", "Hydro", "Natural Gas",
                        "Nuclear", "Petroleum", "Solar", "Wind", "Wood"))
colorBlindPalette <- c("Coal" = "#000000", "Geothermal" = "#E69F00",
                       "Hydro" = "#56B4E9", "Natural Gas" = "#0072B2",
                       "Nuclear" = "#CC79A7", "Petroleum" = "#D55E00",
                       "Solar" = "#F0E442", "Wind" = "#999999", "Wood" = "#009E73")

generations <- read.csv("annual_generation_state.csv", header=TRUE, sep = ",")
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
                           generations$STATE != "  " & 
                           generations$ENERGY.SOURCE != "Total")
#removes any unused factors such as "  " in STATE or "Other" in ENERGY.SOURCE
newGenerations$STATE <- factor(newGenerations$STATE)
newGenerations$ENERGY.SOURCE <- factor(newGenerations$ENERGY.SOURCE)

#rename Energy Sources to something more compact
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Wood and Wood Derived Fuels"] <- "Wood"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Hydroelectric Conventional"] <- "Hydro"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"

newGenerations <- group_by(newGenerations, YEAR, STATE) %>% 
  mutate(PERCENT = GENERATION..Megawatthours./sum(GENERATION..Megawatthours.) * 100)
newGenerations$PERCENT <- round(newGenerations$PERCENT, digit = 2)

ui <- navbarPage("CS 424 Project One",
  tabPanel("Plots",
    fluidRow(
      column(1,
        selectInput("State", "Select a state to view data", 
                    allStatesAbb, selected = "IL"),
        checkboxInput('All', "All", value = TRUE),
        checkboxGroupInput("Energy", "Select the energy sources to plot on line chart",
                           energySources)
        ),
      column(5,
        plotOutput("hist10"),
        plotOutput("hist11"),
        plotOutput("hist12"),
        plotOutput("hist13"),
        dataTableOutput("table1")
        ),
      column(5,
        plotOutput("hist20"),
        plotOutput("hist21"),
        plotOutput("hist22"),
        plotOutput("hist23"),
        dataTableOutput("table2")
      ),
      column(1,
        selectInput("State2", "Select a state to view data",
                    allStatesAbb, selected = "US-TOTAL"),
        checkboxInput('All2', "All", value = TRUE),
        checkboxGroupInput("Energy2", "Select the energy sources to plot on line chart",
                           energySources)
      )
    )
  ),
  tabPanel("About",
    h1("Information about data"),
    p("The data used for the plots is from the US Energy Information Administration.
      It consists of electrical power generation throughout all 50 states, Washington DC,
      and the entire United States. Data was taken within the years 1990 to 2019."),
    br(),
    p("If you want to download the data for yourself to come up with
      all kinds of visualizations, it can be found ",
      a("here.",
        href = "https://www.evl.uic.edu/aej/424/annual_generation_state.csv")),
    br(),
    h2("Author of code"),
    p("The code for this Shiny App was written by Andres Tapia. At the time of this release,
      I am currently in my second semester of my third year at the University of Illinois
      at Chicago.")
  )
)

server <- function(input, output, session) {
  observe({
    updateCheckboxGroupInput(
      session, "Energy", choices = energySources,
      selected = if(input$All) energySources
    )
  })
  
  observe({
    updateCheckboxGroupInput(
      session, "Energy2", choices = energySources,
      selected = if(input$All2) energySources
    )
  })
  
  #make the data reusable for multiple charts
  newGenerationReactive <- reactive({subset(newGenerations, newGenerations$STATE == input$State)})
  newGenerationReactive2 <- reactive({subset(newGenerations, newGenerations$STATE == input$State2)})
  
  ### First half of the screen ###
  #stacked bar with total amount of each energy source
  output$hist10 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      ggtitle(paste("Data from", input$State)) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  })
  
  #stacked bar with percent of total production for each energy source per year
  output$hist11 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="fill", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation Percentage (%)") + 
      ggtitle(paste("Data from", input$State)) + 
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  #line chart with amount of each energy source per year
  output$hist12 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,], 
           aes(x=YEAR, y=GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "line", size=2) + 
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") + 
      xlab("Year") + ylab("Electricity Generation (MWh)") +
      scale_y_continuous(name = "Electricity Generation (MWh)", labels = scales::comma)
  })
  
  output$hist13 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,],
           aes(x=YEAR, y=PERCENT,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
    stat_summary(fun = sum, geom = "line", size=2) + 
      scale_color_manual(values = colorBlindPalette,name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation Percentage (%)")
  })
  
  #table outputs raw data. Need to adjust.
  output$table1 <- DT::renderDataTable(
    DT::datatable({
      newGeneration <- newGenerationReactive()
    },
    options = list(searching = TRUE, pageLength = 5, lengthChange = FALSE),
    rownames = FALSE
    )
  )
  
  ### the second half of the screen ###
  output$hist20 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name="Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      ggtitle(paste("Data from", input$State2)) + 
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  })
  
  output$hist21 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="fill", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Energy Generation Percentage (%)")
  })
  
  #line chart
  output$hist22 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,], 
           aes(x=YEAR, y=GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "line", size=2) + 
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  })
  
  output$hist23 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,],
           aes(x=YEAR, y=PERCENT,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "line", size=2) + 
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Energy Generation Percentage (%)")
  })
  
  output$table2 <- DT::renderDataTable(
    DT::datatable({
      newGeneration <- newGenerationReactive2()
    },
    options = list(searching = TRUE, pageLength = 5, lengthChange = FALSE),
    rownames = FALSE
    )
  )
}

shinyApp(ui = ui, server = server)