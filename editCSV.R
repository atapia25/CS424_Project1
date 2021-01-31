library(ggplot2)
library(usmap)

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
newGenerations$STATE <- factor(newGenerations$STATE)
newGenerations$ENERGY.SOURCE <- factor(newGenerations$ENERGY.SOURCE)
str(newGenerations)