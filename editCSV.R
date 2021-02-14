library(ggplot2)
library(usmap)

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#000000 = black
#999999 = grey
#E69F00 = orange
#56B4E9 = sky blue
#009E73 = bluish green
#F0E442 = yellow
#0072B2 = blue
#D55E00 = vermillion (orange red)
#CC79A7 = reddish purple (pinkish color)
colorBlindPalette <- c("Coal" = "#000000", 
                       "Geothermal" = "#E69F00",
                       "Hydro" = "#56B4E9",
                       "Natural Gas" = "#0072B2",
                       "Nuclear" = "#CC79A7",
                       "Petroleum" = "#D55E00",
                       "Solar" = "#F0E442",
                       "Wind" = "#999999",
                       "Wood" = "#009E73") 

generations <- read.csv("annual_generation_state.csv", header=TRUE, sep = ",")
#class(generations)
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
                           generations$TYPE.OF.PRODUCER == "Total Electric Power Industry" &
                           generations$ENERGY.SOURCE != "Total")
#removes any unused factors such as "  " in STATE or "Other" in ENERGY.SOURCE
newGenerations$STATE <- factor(newGenerations$STATE)
newGenerations$ENERGY.SOURCE <- factor(newGenerations$ENERGY.SOURCE)

#rename Energy Sources to something more compact
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Wood and Wood Derived Fuels"] <- "Wood"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Hydroelectric Conventional"] <- "Hydro"
levels(newGenerations$ENERGY.SOURCE)[levels(newGenerations$ENERGY.SOURCE)=="Solar Thermal and Photovoltaic"] <- "Solar"

newGenerations <- 
        group_by(newGenerations, YEAR, STATE) %>% 
        mutate(PERCENT = GENERATION..Megawatthours./sum(GENERATION..Megawatthours.) * 100)

AKData <- subset(newGenerations, newGenerations$STATE == "AK" & newGenerations$ENERGY.SOURCE != "Total")
AKDataSmall <- subset(newGenerations, newGenerations$STATE == "AK" & newGenerations$ENERGY.SOURCE != "Total"
                      & newGenerations$ENERGY.SOURCE == "Coal")

#x-axis consists of all the years
#do not include total in the final results when graphing 
# work in progress chart
#ggplot(subset(AKData, AKData$ENERGY.SOURCE != "Total"), 
 #      aes(x=YEAR, y=GENERATION..Megawatthours.)) + geom_col(aes(fill=ENERGY.SOURCE))
#stacked bar chart
ggplot(AKData, 
       aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
       geom_bar(position="stack", stat="identity") +
       scale_fill_manual(values = colorBlindPalette)

#stacked bar chart with percentages
ggplot(AKData, 
       aes(x=YEAR, y=GENERATION..Megawatthours.,fill=ENERGY.SOURCE)) + 
       geom_bar(position="fill", stat="identity") +
       scale_fill_manual(values = colorBlindPalette)

#line chart with total amount of each energy source
#Need to figure out how to plot each energy
#ggplot(AKData, aes(x=YEAR, y=GENERATION..Megawatthours., color=ENERGY.SOURCE))
ggplot(AKData, aes(YEAR, GENERATION..Megawatthours.,group=ENERGY.SOURCE, color=ENERGY.SOURCE)) + 
        stat_summary(fun = sum, geom = "line", size=1) + 
        scale_color_manual(values=colorBlindPalette)