library(shiny)
library(ggplot2)
library(usmap)
library(usdata)
library(DT)
library(dplyr)

years <- c("All", 1990:2019)
allStates <- sort(c(state.name, "Washington DC", "Total US"))
allStatesAbb <- sort(c(state.abb, "DC", "US-TOTAL"))
states <- setNames(as.list(c(state.abb, "DC", "US-TOTAL")), 
                   c(state.name, "Washington DC", "Total US"))

energySources <- sort(c("Coal", "Geothermal", "Hydro", "Natural Gas",
                        "Nuclear", "Petroleum", "Solar", "Wind", "Wood"))

energySourcesPlusAll <- sort(c("All", "Coal", "Geothermal", "Hydro", "Natural Gas",
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
                           generations$ENERGY.SOURCE != "Total" &
                           generations$TYPE.OF.PRODUCER == "Total Electric Power Industry")
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
  navbarMenu("Summary of US Data",
    tabPanel("Bar Charts",
      fluidRow(
        column(12,
          plotOutput("histDefault0"),
          plotOutput("histDefault1")
        )
      )
    ),
    tabPanel("Line Charts",
      fluidRow(
        column(2,
          checkboxInput('All', "All", value = TRUE),
          checkboxGroupInput("Energy", "Select the energy sources to plot on line chart",
                             energySources)
          ),
        column(10,
          plotOutput("histDefault2"),
          plotOutput("histDefault3")
        )
      )
    ),
    tabPanel("Raw Data Table",
      dataTableOutput("dataTableDefault")
    )
  ),
  tabPanel("State, Energy, and Year Comparison",
    fluidRow(
      column(1,
        selectInput("State1", "Select a state to view data", 
                    allStates, selected = "Illinois"),
        selectInput("EnergyChoice1", "Select an energy source to view",
                    energySourcesPlusAll, selected = "All"),
        selectInput("Year1", "Select a year to view",
                    years, selected = "All")
        ),
      column(5,
          plotOutput("hist10", inline = TRUE),
          plotOutput("hist11", inline = TRUE),
          plotOutput("hist12", inline = TRUE)
        ),
      column(5,
        plotOutput("hist20", inline = TRUE),
        plotOutput("hist21", inline = TRUE)
      ),
      column(1,
        selectInput("State2", "Select a state to view data",
                    allStates, selected = "Total US"),
        selectInput("EnergyChoice2", "Select an energy source to view",
                    energySourcesPlusAll, selected = "All"),
        selectInput("Year2", "Select a year to view",
                    years, selected = "All")
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
  #checkbox input for summary of US
  observe({
    updateCheckboxGroupInput(
      session, "Energy", choices = energySources,
      selected = if(input$All) energySources
    )
  })
  
  #make the data reusable for multiple charts
  newGenerationReactive <- reactive({
    if(input$EnergyChoice1 == "All") 
    {
      if(input$Year1 == "All")
      {
        subset(newGenerations, newGenerations$STATE == states[[input$State1]])
      }
      else
      {
        newGenerations[newGenerations$ENERGY.SOURCE %in% energySourcesPlusAll[-1] &
                         newGenerations$STATE == states[[input$State1]] &
                         newGenerations$YEAR == input$Year1,]
      }
    }
    else if (input$Year1 == "All")
    {
      if(input$EnergyChoice1 == "All")
      {
        subset(newGenerations, newGenerations$STATE == states[[input$State1]])
      }
      else
      {
        newGenerations[newGenerations$YEAR %in% years[-1] &
                         newGenerations$STATE == states[[input$State1]] &
                         newGenerations$ENERGY.SOURCE == input$EnergyChoice1,]
      }
    }
    else {
      subset(newGenerations, newGenerations$ENERGY.SOURCE == input$EnergyChoice1
             & newGenerations$STATE == states[[input$State1]] &
               newGenerations$YEAR == input$Year1)
    }
  })
  
  newGenerationReactive2 <- reactive({
    if(input$EnergyChoice2 == "All") 
    {
      if(input$Year2 == "All")
      {
        subset(newGenerations, newGenerations$STATE == states[[input$State2]])
      }
      else
      {
        newGenerations[newGenerations$ENERGY.SOURCE %in% energySourcesPlusAll[-1] &
                         newGenerations$STATE == states[[input$State2]] &
                         newGenerations$YEAR == input$Year2,]
      }
    }
    else if (input$Year2 == "All")
    {
      if(input$EnergyChoice2 == "All")
      {
        subset(newGenerations, newGenerations$STATE == states[[input$State2]])
      }
      else
      {
        newGenerations[newGenerations$YEAR %in% years[-1] &
                         newGenerations$STATE == states[[input$State2]] &
                         newGenerations$ENERGY.SOURCE == input$EnergyChoice2,]
      }
    }
    else {
      subset(newGenerations, newGenerations$ENERGY.SOURCE == input$EnergyChoice2
             & newGenerations$STATE == states[[input$State2]] &
               newGenerations$YEAR == input$Year2)
    }
  })
  
  ### showing summary of overall data for US ###
  #bar chart with total amount of energy
  output$histDefault0 <- renderPlot({
    ggplot(subset(newGenerations, newGenerations$STATE == "US-TOTAL"),
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      ggtitle("Amount of each energy source from 1990 to 2019") + 
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 17),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 16, face = "bold")) +
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  })
  
  # bar chart with percentage
  output$histDefault1 <- renderPlot({
    ggplot(subset(newGenerations, newGenerations$STATE == "US-TOTAL"),
           aes(x=YEAR, y=PERCENT,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity") + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation Percentage (%)") + 
      ggtitle("Percent of production for each energy source from 1990 to 2019") + 
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 17),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 16, face = "bold"))
  })
  
  #line chart with total energy
  output$histDefault2 <- renderPlot({
    newGeneration <- subset(newGenerations, newGenerations$STATE == "US-TOTAL")
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,],
          aes(x=YEAR, y=GENERATION..Megawatthours.,
              group=ENERGY.SOURCE, color=ENERGY.SOURCE)) + 
        stat_summary(fun = sum, geom = "point", size=2) + 
        geom_line(size = 1.25) +
        scale_color_manual(values = colorBlindPalette, name = "Energy Source") + 
        xlab("Year") + ylab("Electricity Generation (MWh)") +
        ggtitle("Amount of each energy source from 1990 to 2019") + 
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
              legend.text = element_text(size = 14),
              legend.title = element_text(size = 17),
              axis.text = element_text(size = 15),
              axis.title = element_text(size = 16, face = "bold")) +
        scale_y_continuous(name="Electricity Generation (MWh)", 
                           labels = scales::comma)
  })
  
  #line chart with percent of energy production
  output$histDefault3 <- renderPlot({
    newGeneration <- subset(newGenerations, newGenerations$STATE == "US-TOTAL")
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy,],
           aes(x=YEAR, y=PERCENT, group=ENERGY.SOURCE, color=ENERGY.SOURCE)) + 
      stat_summary(fun = sum, geom = "point", size=2) +
      geom_line(size = 1.25) +
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") + 
      xlab("Year") + ylab("Electricity Generation Percentage (%)") +
      ggtitle("Percent of production for each energy source from 1990 to 2019") +
      theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), 
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 17),
            axis.text = element_text(size = 15),
            axis.title = element_text(size = 16, face = "bold"))
  })
  
  output$dataTableDefault <- DT::renderDataTable(
    DT::datatable({
      newGeneration <- subset(newGenerations, newGenerations$STATE == "US-TOTAL")
    }),
    options = list(searching = TRUE, pageLength = 6, lengthChange = FALSE),
    rownames = FALSE
  )
  
  #line chart with percent of total for each energy
  
  ### end of default screen ###
  
  ### First half of the screen ###
  #stacked bar with total amount of each energy source
  output$hist10 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity", width = 0.7) + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      ggtitle(paste("Data from", input$State1)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title = element_text(size = 11, face = "bold")) +
      scale_y_continuous(name="Electricity Generation (MWh)", 
                         labels = scales::comma)
  }, height = 235, width = 650)
  
  #stacked bar with percent of total production for each energy source per year
  output$hist11 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration, 
           aes(x=YEAR, y=PERCENT,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity", width = 0.7) + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation Percentage (%)") + 
      ggtitle(paste("Data from", input$State1)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.y = element_text(size = 11, face = "bold"))
  }, height = 235, width = 650)
  
  #line chart with amount of each energy source per year
  output$hist12 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration, 
           aes(x=YEAR, y=GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "point", size=2) +
      geom_line(size = 1.25) +
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") + 
      xlab("Year") + ylab("Electricity Generation (MWh)") +
      scale_y_continuous(name = "Electricity Generation (MWh)", 
                         labels = scales::comma) +
      ggtitle(paste("Data from", input$State1)) +
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.y = element_text(size = 11, face = "bold"))
  }, height = 235, width = 650)
  
  output$hist13 <- renderPlot({
    newGeneration <- newGenerationReactive()
    ggplot(newGeneration,
           aes(x=YEAR, y=PERCENT, group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
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
      geom_bar(position="stack", stat="identity", width = 0.7) + 
      scale_fill_manual(values = colorBlindPalette, name="Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      ggtitle(paste("Data from", input$State2)) + 
      theme(plot.title = element_text(hjust = 0.5, size = 15),
            axis.title.y = element_text(size = 11, face = "bold")) +
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  }, height = 250, width = 600)
  
  output$hist21 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration, 
           aes(x=YEAR, y=PERCENT,fill=ENERGY.SOURCE)) + 
      geom_bar(position="stack", stat="identity", width = 0.7) + 
      scale_fill_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Energy Generation Percentage (%)")
  }, height = 250, width = 600)
  
  #line chart
  output$hist22 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy2,], 
           aes(x=YEAR, y=GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) +
      stat_summary(fun = sum, geom = "line", size=2) + 
      scale_color_manual(values = colorBlindPalette, name = "Energy Source") +
      xlab("Year") + ylab("Electricity Generation (MWh)") + 
      scale_y_continuous(name="Electricity Generation (MWh)", labels = scales::comma)
  })
  
  output$hist23 <- renderPlot({
    newGeneration <- newGenerationReactive2()
    ggplot(newGeneration[newGeneration$ENERGY.SOURCE %in% input$Energy2,],
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