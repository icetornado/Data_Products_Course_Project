#http://data.un.org/Data.aspx?d=EDATA&f=cmID%3aEC


library(ggplot2)
library(dplyr)
rawData <- read.csv(file.path("data", "UNdata_Export.csv"), stringsAsFactors=TRUE, na.strings = c("NA","#DIV/0!",""))

## data clean up
## remove last two lines 
d <- rawData[1:(nrow(rawData) -2), c(1,2,3,5)]
names(d) <- c("Country", "Commodity", "Year", "Quantity")
d$code = "G"
test <- grep("installed", d$Commodity)
d[test, "code"] = "I"
d$code = as.factor(d$code)

### clean up "Korea, Republic of", "United Rep. of Tanzania", "Central African Rep.", 
## "Congo", 
## "Iran (Islamic Rep. of)", "Lao People's Dem. Rep.", "Korea, Dem.Ppl's.Rep.", "Dem. Rep. of the Congo", 
## "Venezuela (Bolivar. Rep.)", "Bolivia (Plur. State of)" names
### to South Korea, Tanzania, DR Congo, Central African Republic, Iran, Laos, "North Korea", "Republic of the Congo"
### "Venezuela", "Bolivia"

d$Country <- gsub("^Korea, Republic of*", "South Korea", d$Country)
d$Country <- gsub("^United Rep. of Tanzania*", "Tanzania", d$Country)
d$Country <- gsub("^Central African Rep.*", "Central African Republic", d$Country)
d$Country <- gsub("^Congo*", "DR Congo", d$Country)

badNames <- c("Korea, Republic of", "United Rep. of Tanzania", "Central African Rep.", 
              "Congo", "Iran \\(Islamic Rep. of\\)", "Lao People's Dem. Rep.", "Korea, Dem.Ppl's.Rep.", 
              "Dem. Rep. of the Congo",  "Venezuela \\(Bolivar. Rep.\\)", "Bolivia \\(Plur. State of\\)")
goodNames <- c("South Korea", "Tanzania", "Central African Republic", "DR Congo", "Iran", "Laos", 
               "North Korea", "Republic of the Congo", "Venezuela", "Bolivia")

for(j in 1:length(badNames)) {
        #badNames[j]
        d$Country <- gsub(paste('^', badNames[j], '*', sep=""), goodNames[j], d$Country)
}
### functions
getTop10byYear <- function(df, year, type) {
        results <- df %>%
                filter(Year == year, code == type) %>%
                group_by(Country) %>%
                summarise(ttl = sum(Quantity)/1000) %>%
                arrange(desc(ttl))
        
        
        return(results[1:10, ])
}

plotTop10byYear <- function(df, year, type) {
        if(type == "I") {
                yText = "Total Installed Capacity (MW)"
        }
        else {
                yText = "Total Generating Capacity (MW)"
        }
                
        p <- ggplot(data = df, aes(x = Country, y = ttl, fill = Country))
        #p <- p + theme(plot.background = element_rect(fill = "red"))
        p <- p + geom_bar(stat="identity") + theme(axis.text.x = element_text(size  = 10, angle = 45, hjust = 1,vjust = 1))
        p <- p + theme(legend.position="none") 
        p <- p + labs(x = "Country", y = yText, title = paste("Top-10 Countries in ",year))
        
        return(p)
}

getG20Countries <- function() {
          countries <- c("Argentina", "Australia", "Brazil", "Canada", "China", "France", "Germany", "India", "Indonesia", "Italy", 
                         "Japan", "Mexico", "Russian Federation", "Saudi Arabia", "South Africa", "Korea, Republic of", "Turkey", "United Kingdom", 
                         "United States")
          
          return(countries)
}

getG20CountriesList <- function() {
        countries <- getG20Countries()
        cList <- list()
        
        for(i in 1:length(countries)) {
                cList[countries[i]] = countries[i] 
        }
        
        return(cList)
}

makeG20Checklist <- function(sel = list()) {
        if(length(sel) == 0) {
                sel = getG20Countries()
        }
        
       checkboxGroupInput("countriesCheck", label = h3("Countries"), 
                           choices = getG20CountriesList(),
                           selected = getG20Countries())
}

getG20Data <- function(df, type, countries) {
        if(length(countries) == 0) {
                results <- data.frame()
        }
        else {
                results <- df %>%
                        filter(Year > 2002) %>%
                        filter(code == type) %>%
                        filter(Country %in%  countries) %>%
                        group_by(Country, Year) %>%
                        summarise(ttl = sum(Quantity)/1000) 
        }
        
        return(results)
}

plotG20 <- function(df, type) {
        if(length(df) == 0) {
                return()
        }
        
        if(type == "I") {
                yText = "Total Installed Capacity (MW)"
        }
        else {
                yText = "Total Generating Capacity (MW)"
        }
        
        p <- ggplot(data = df, aes(x = factor(Year), y = ttl, color = Country))
        p <- p + geom_point() + aes(group = Country) + geom_line() 
        #p <- p + theme(legend.position="none")
        #p <- p + scale_x_discrete(labels=c("fatalities" = "Fatalities", "injuries" = "Injuries"))
        p <- p + labs(x = "Year", y = yText, title = "G20 Countries") 
        
        return(p)
}

# 
# [1] "Electricity - total net installed capacity of electric power plants, main activity & autoproducer"
# [2] "Electricity - total net installed capacity of electric power plants, hydro"                       
# [3] "Electricity - net installed capacity of electric power plants, public hydro"                      
# [4] "Electricity - net installed capacity of electric power plants, self-producer hydro"               
# [5] "Electricity - total net installed capacity of electric power plants, combustible fuels"           
# [6] "Electricity - net installed capacity of electric power plants, public combustible fuels"          
# [7] "Electricity - net installed capacity of electric power plants, self-producer combustible fuels"   
# [8] "Electricity - total net installed capacity of electric power plants, main activity"               
# [9] "Electricity - total net installed capacity of electric power plants, autoproducer"                
# [10] "Electricity - total net installed capacity of electric power plants, solar"                       
# [11] "Electricity - net installed capacity of electric power plants public solar"                       
# [12] "Electricity generating capacity - Solar Thermal - Total"                                          
# [13] "Electricity generating capacity - Solar Thermal - Main activity producers"                        
# [14] "Electricity generating capacity - Solar PV - Total"                                               
# [15] "Electricity generating capacity - Solar PV - Main activity producers"                             
# [16] "Electricity - total net installed capacity of electric power plants, nuclear"                     
# [17] "Electricity - net installed capacity of electric power plants, public nuclear"                    
# [18] "Electricity - total net installed capacity of electric power plants, geothermal"                  
# [19] "Electricity - net installed capacity of electric power plants, public geothermal"                 
# [20] "Electricity - total net installed capacity of electric power plants, wind"                        
# [21] "Electricity - net installed capacity of electric power plants, public wind"                       
# [22] "Electricity - net installed capacity of electric power plants, self-producer wind"                
# [23] "Electricity - net installed capacity of electric power plants, self-producer solar"               
# [24] "Electricity - total net installed capacity of electric power plants, tide, wave, marine"          
# [25] "Electricity - net installed capacity of electric power plants, public tide, wave, marine"         
# [26] "Electricity generating capacity - Pumped hydro - Total"                                           
# [27] "Electricity generating capacity - Pumped hydro - Main activity producers"                         
# [28] "Electricity generating capacity - Solar PV - Autoproducers"                                       
# [29] "Electricity generating capacity - From other sources - Total"                                     
# [30] "Electricity generating capacity - From other sources - Main activity producers"                   
# [31] "Electricity generating capacity - From other sources - Autoproducers"                             
# [32] "Electricity generating capacity - Solar Thermal - Autoproducers"                                  
# [33] "Electricity - net installed capacity of electric power plants, self-producer geothermal"          
# [34] "Electricity - net installed capacity of electric power plants, self-producer nuclear"             
# [35] "Electricity generating capacity - Pumped hydro - Autoproducers"                                   
# [36] "Electricity - net installed capacity of electric power plants, self-producer tide, wave, marine" 