## R Shiny app for visualization of CMIP6 global climate model simulations for BC and subregions
## author: Colin Mahony colin.mahony@gov.bc.ca

# Copyright 2021 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(RColorBrewer)
library(DT)
library(scales)
library(shinydashboard)
library(markdown)

# ----------------------------------------------
# Load the input data
# ----------------------------------------------

modelMetadata <- read.csv("data/ModelList.csv")

# Define ecoprovinces (subregions of BC) and climate elements
files <- list.files("data/", pattern="^summary.mean")
ecoprovs <- unique(sapply(strsplit(files, "[.]"), "[", 3))
ecoprov.names <- c("British Columbia", "Boreal Plains", "Central Interior", "Coast and Mountains", "Georgia Depression", "Northern Boreal Mountains", "Sub-Boreal Interior", "Southern Interior Mountains", "Southern Interior", "Taiga Plains")
elements <- c("Tave", "Tmax", "Tmin", "PPT")
element.names <- c("Mean temperature" , "Mean daily maximum temperature (Tmax)", "Mean daily minimum temperature (Tmin)", "Precipitation")
element.names.units <- c(bquote(Mean~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(maximum)~temperature~"("*degree*C*")"),bquote(Mean~daily~bold(minimum)~temperature~"("*degree*C*")"), "Precipitation (mm)")
variable.names <- read.csv("data/Variables_ClimateBC.csv")

funs <- c("bias", "sd")
fun.names <- c("Bias in", "Standard deviation of")

# extract the global climate models and scenarios from an arbitrary file. 
files <- list.files("data/", pattern="^ensmin.BC")
template <- read.csv(paste("data/", files[1], sep=""), stringsAsFactors = F)
gcms <- names(template)[-c(1:2, length(names(template)))]
scenarios <- unique(template[,1])
scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
gcm.names <- as.character(modelMetadata[,1])

# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

seasons <- c("wt", "sp", "sm", "at")
season.names <- c("Winter", "Spring", "Summer", "Autumn")

yeartimes <- c(seasons, monthcodes)
yeartime.names <- c(season.names, month.name)

ensstats <- c("ensmin", "ensmax", "ensmean")

# Define UI ----
ui <- fluidPage(
  navbarPage(title = "CMIP6 viewer for British Columbia", theme = "bcgov.css", 
             tabPanel("Time Series",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Compare CMIP6 climate model simulations to each other and to observations. Compile custom ensembles with and without bias correction. See projections for subregions (ecoprovinces) of BC."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxGroupInput("gcms1", "Choose global climate models:",
                                             choiceNames = gcm.names,
                                             choiceValues = gcms,
                                             selected = gcms[c(1,3,4,6,7,8,9,11,12,14,15,17,19)],
                                             inline = T
                          ),
                          
                          checkboxInput("compile", label = "Compile into ensemble projection", value = TRUE),
                          
                          checkboxInput("biascorrect", label = "Bias correction (match 1961-90 model climate to observations)", value = TRUE),
                          
                          checkboxGroupInput("scenarios1", "Choose emissions scenarios",
                                             choiceNames = scenario.names[-1],
                                             choiceValues = scenarios[-1],
                                             selected = scenarios[c(2,3,4)],
                                             inline = T
                          ),
                          
                          checkboxInput("showmean", label = "Show mean of projections", value = FALSE),
                          
                          checkboxInput("refline", label = "Show 1961-1990 baseline for models", value = T),
                          
                          checkboxInput("era5", label = "Show ERA5 reanalysis", value = F),
                          
                          # ELEMENT NAMES. THIS IS WHERE THE DERIVED VARIABLES WILL BE ADDED TO THE LIST
                          selectInput("element1",
                                      label = "Choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[1]),
                          
                          selectInput("yeartime1",
                                      label = "Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                          checkboxInput("compare", label = "Compare two variables", value = T),
                          
                          conditionalPanel(
                            condition = "input.compare == true",
                            
                            # ELEMENT NAMES. THIS IS WHERE THE DERIVED VARIABLES WILL BE ADDED TO THE LIST
                            selectInput("element2",
                                        label = "Choose a climate element for comparison",
                                        choices = as.list(element.names),
                                        selected = element.names[1]),
                            
                            selectInput("yeartime2",
                                        label = "Choose a month/season for comparison",
                                        choices = as.list(yeartime.names),
                                        selected = yeartime.names[1]),
                          ),
                          
                          selectInput("ecoprov.name",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          downloadButton(outputId = "downloadPlot", label = "Download plot"),
                          
                          img(src = "Ecoprovinces_Title.png", height = round(1861*1/5), width = round(1993*1/5))
                        ),
                        
                        mainPanel(
                          
                          plotOutput(outputId = "timeSeries")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("bias", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("compare bias and variance among models"),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxInput("ClimateBC", label = "Reduce to the ClimateBC ensemble", value = FALSE),
                          
                          radioButtons("xfun",
                                       label = "choose x-axis type",
                                       choices = list("bias" = 1, "st. dev." = 2),
                                       selected = 1),
                          
                          radioButtons("yfun",
                                       label = "Choose y-axis type",
                                       choices = list("bias" = 1, "st. dev." = 2),
                                       selected = 2),
                          
                          selectInput("element3",
                                      label = "Choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[1]),
                          
                          selectInput("yeartime3",
                                      label = "Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                          checkboxInput("compare2", label = "Compare two variables", value = F),
                          
                          conditionalPanel(
                            condition = "input.compare2 == true",
                            
                            # ELEMENT NAMES. THIS IS WHERE THE DERIVED VARIABLES WILL BE ADDED TO THE LIST
                            selectInput("element4",
                                        label = "Choose a climate element for comparison",
                                        choices = as.list(element.names),
                                        selected = element.names[1]),
                            
                            selectInput("yeartime4",
                                        label = "Choose a month/season for comparison",
                                        choices = as.list(yeartime.names),
                                        selected = yeartime.names[1]),
                          ),
                          
                          selectInput("ecoprov.name.II",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[9]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotOutput(outputId = "scatterPlot")
                          
                        )
                      ),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("About",
                      
                      includeMarkdown("about.Rmd"),
                      
                      img(src = "ModelRes.png", height = 4500/11, width = 12600/11),
                      
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             ),
             
             tabPanel("Model Info",
                      DT::dataTableOutput("table"),
                      column(width = 12,
                             style = "background-color:#003366; border-top:2px solid #fcba19;",
                             
                             tags$footer(class="footer",
                                         tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
                                                  tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
                                                          tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
                                                  )
                                         )
                             )
                      )
             )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  timeSeriesPlot <- function() {

    # user specificationS
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1)]
    yeartime2 <- if(input$compare==T) yeartimes[which(yeartime.names==input$yeartime2)] else yeartimes[which(yeartime.names==input$yeartime1)]
    element1 <- elements[which(element.names==input$element1)]
    element2 <- if(input$compare==T) elements[which(element.names==input$element2)] else elements[which(element.names==input$element1)]
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    gcms1 <- input$gcms1
    scenarios1 <- c("historical", input$scenarios1)
    nums <- if(input$compare==T) c(1,2) else c(1)
    
    ## Assemble the data that will be used in the plot
    alldata <- vector() # a vector of all data on the plot for setting the ylim (y axis range)
    num <- 1
    for(num in nums){
      
      # data for observations
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      obs.ts.mean <- read.csv(paste("data/ts.obs.mean.", ecoprov, ".csv", sep=""))
      
      x1 <- unique(obs.ts.mean[,1])
      y1 <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      baseline.obs <- mean(y1[which(x1%in%1961:1990)])
      recent.obs <- mean(y1[(length(y1)-10):(length(y1))])
      
      #data for GCMs
      # ensstat <- ensstats[1]
      for(ensstat in ensstats[c(3,1,2)]){ #need to reorder the enstats so that mean comes first, for bias correction
        # scenario <- scenarios[1]
        ## Note: rather than reading in a single large data file, this app stores data in many (~2000) small files, and reads them in on demand by user input
        data <- read.csv(paste("data/", paste(ensstat, ecoprov, get(paste("variable",num, sep="")), "csv", sep="."), sep=""))
        temp.historical <- data[which(data[,1]=="historical"),-1]
        for(scenario in scenarios1){
          temp <- data[which(data[,1]==scenario),-1]
          if(scenario != "historical"){
            temp <- rbind(temp.historical[dim(temp.historical)[1],match(names(temp), names(temp.historical))], temp) # add last year of historical runs
          }
          if(scenario == "historical") if(ensstat=="ensmean") baseline.mod <- apply(temp[which(temp[,1]%in%1961:1990),-1], 2, mean)
          
          alldata <- c(alldata, as.vector(unlist(temp[-1]))) #store values in a big vector for maintaining a constant ylim
          # optional bias correction
          if(input$biascorrect==T){
            if(element=="PPT"){
              delta <- baseline.obs/baseline.mod
              delta <- delta[which(names(delta)!="compile")]
              temp[,-1] <- sweep(temp[,-1], 2, delta, '*')
            } else {
              delta <- baseline.obs-baseline.mod
              delta <- delta[which(names(delta)!="compile")]
              temp[,-1] <- sweep(temp[,-1], 2, delta[match(names(temp[-1]), names(delta))], '+')
            }
          }
          temp$compile <- if(length(gcms1)==0) rep(NA, dim(temp)[1]) else if(length(gcms1)==1) temp[,which(names(temp)==gcms1)] else apply(temp[,which(names(temp)%in%gcms1)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
          assign(paste(ensstat, scenario, num, sep="."), temp)
        }
      }
    }
    
    # PLOT
    par(mfrow=c(1,1), mar=c(3,3,0.1,3), mgp=c(1.75, 0.25, 0), cex=1.4)
    if(element1==element2){
      ylab <- element.names.units[[which(elements==element1)]]
    } else {
      ylab <- if("PPT"%in%c(element1, element2)) bquote(Precipitation~"("*mm*")"~or~Mean ~ temperature ~ "(" * degree * C * ")") else element.names.units[[1]]
    }
    plot(0, col="white", xlim=c(1850, 2100), ylim=range(alldata, na.rm = T), xaxs="i", tck=0, xlab="", ylab=ylab)
    
    num <- 1
    for(num in nums){
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      # data for observations
      x1 <- unique(obs.ts.mean[,1])
      y1 <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      baseline.obs <- mean(y1[which(x1%in%1961:1990)])
      recent.obs <- mean(y1[(length(y1)-10):(length(y1))])
      
      # data for era5
      if(input$era5==T){
        era5.ts.mean <- read.csv(paste("data/ts.era5.mean.", ecoprov, ".csv", sep=""))
        x2 <- unique(era5.ts.mean[,1])
        y2 <- era5.ts.mean[,which(names(era5.ts.mean)==variable)]
      }
      
      if(input$compile==T) gcms1 <- "compile" #this prevents the plotting of individual GCM projections and plots a single envelope for the ensemble as a whole. 
      for(gcm in gcms1){
        # scenario <- scenarios1[1]
        for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
          
          for(ensstat in ensstats){
            temp <- get(paste(ensstat, scenario, num, sep="."))
            x <- temp[,1]
            temp <- temp[,which(names(temp)==gcm)]
            assign(ensstat, temp)
            if(scenario == "historical"){
              assign(paste(ensstat, scenario, sep="."), temp)
            }
          }
          
          # colScheme <- c("gray60", "seagreen", "goldenrod4", "darkorange3", "darkred")
          colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")
          # colScheme <- c("gray80", "#1d3354", "#e9dc3d", "#f11111", "#830b22")
          polygon(c(x, rev(x)), c(ensmin, rev(ensmax)), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.5 else 0.5), border=colScheme[which(scenarios==scenario)])
          if(input$showmean==T) lines(x, ensmean, col=colScheme[which(scenarios==scenario)], lwd=2)
          
          if(input$refline==T){
            ref.temp <- mean(ensmean.historical[111:140])
            lines(1961:1990, rep(ref.temp, 30), lwd=2)
            lines(c(1990,2100), rep(ref.temp, 2), lty=2)
          }
          
          if(scenario != "historical"){
            par(xpd=T)
            baseline <- mean(ensmean.historical[1:50])
            projected <- mean(ensmean[(length(x)-10):(length(x))])
            if(element=="PPT"){
              change <- round(projected/baseline-1,2)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
            } else {
              change <- round(projected-baseline,1)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
            }
            par(xpd=F)
          }
          
          print(scenario)
        }
        print(gcm)
      }
      
      # Text to identify the time of year
      # if(input$compare==T){
      if(element1==element2){
        label <- yeartime.names[which(yeartimes==yeartime)]
      } else {
        label <- paste(yeartime.names[which(yeartimes==yeartime)], get(paste("element", num, sep="")))
      }
      temp <- get(paste("ensmax.historical", num, sep="."))
      text(1915,mean(temp$compile[60:80]), label, col="black", pos=3, font=2, cex=1)
      # }
      
      # add in observations
      obs.color <- "blue"
      lines(x1[which(x1<1951)], y1[which(x1<1951)], lwd=3, lty=3, col=obs.color)
      lines(x1[which(x1>1949)], y1[which(x1>1949)], lwd=3, col=obs.color)
      if(element=="PPT"){
        change <- round(recent.obs/baseline.obs-1,2)
        text(2018,recent.obs, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=obs.color, pos=4, font=2, cex=1)
      } else {
        change <- round(recent.obs-baseline.obs,1)
        text(2018,recent.obs, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=obs.color, pos=4, font=2, cex=1)
      }
      lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=obs.color)
      lines(c(1990,2019), rep(baseline.obs, 2), lty=2, col=obs.color)
      
      era5.color <- "darkorange"
      if(input$era5==T){
        lines(x2, y2, col=era5.color, lwd=2)
      }
      
      #legend
      a <- 1
      b <- if(input$era5==T) 2 else NA
      c <- if(length(gcms1>0)) 3 else NA
      s <- !is.na(c(a,b,c))
      legend("topleft", title = "Historical Period", legend=c("Station observations", "ERA5 reanalysis", "GCM simulations (min & max)")[s], bty="n",
             lty=c(1,1,NA)[s], col=c(obs.color, era5.color, NA)[s], lwd=c(3,2,NA)[s], pch=c(NA,NA, 22)[s], pt.bg = c(NA, NA, colScheme[1])[s], pt.cex=c(NA,NA,2)[s])
      
      s <- which(scenarios[-1]%in%input$scenarios1)
      legend("top", title = "Future Scenarios", legend=scenario.names[-1][s], bty="n",
             lty=c(NA,NA,NA,NA)[s], col=c(NA,NA,NA,NA)[s], lwd=c(NA,NA,NA,NA)[s], pch=c(22, 22, 22, 22)[s], pt.bg = colScheme[-1][s], pt.cex=c(2,2,2,2)[s])
      
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      print(num)
    }
    box()
  }
  output$timeSeries <- renderPlot({ timeSeriesPlot() },
                                  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.5,0))
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename =  "Plot.png",
    
    content = function(file) {
      
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_timeSeries_width
      height <- session$clientData$output_timeSeries_height
      
      png(file, width = width*pixelratio, height = height*pixelratio, res = 72*pixelratio)
      timeSeriesPlot()
      dev.off()
    } 
  )
  
  
  output$scatterPlot <- renderPlot({
    
    # ecoprov <- ecoprovs[1]
    # yeartime1 <- yeartimes[3]
    # yeartime2 <- yeartimes[3]
    # element1 <- elements[4]
    # element2 <- elements[4]
    # xfun <- 1
    # yfun <- 2
    
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.II)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime3)]
    yeartime2 <- if(input$compare2==T) yeartimes[which(yeartime.names==input$yeartime4)] else yeartimes[which(yeartime.names==input$yeartime3)]
    element1 <- elements[which(element.names==input$element3)]
    element2 <- if(input$compare2==T) elements[which(element.names==input$element4)] else elements[which(element.names==input$element3)]
    xfun <- as.numeric(input$xfun)
    yfun <- as.numeric(input$yfun)
    
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    
    data.mean <- read.csv(paste("data/summary.mean", ecoprov, "csv", sep="."), stringsAsFactors = F)
    data.sd <- read.csv(paste("data/summary.sd", ecoprov, "csv", sep="."), stringsAsFactors = F)
    data.sd[,grep("PPT", names(data.bias))] <- data.sd[,grep("PPT", names(data.bias))]/data.mean[,grep("PPT", names(data.bias))] # convert sd to coefficient of variation for precipitation
    data.sd[,-c(1:2)] <- sweep(data.sd[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.sd[1,-c(1:2)])), "/")-1 # express sd relative to observational
    data.bias <- data.mean
    data.bias[,-c(1:2)] <- sweep(data.mean[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.mean[1,-c(1:2)])), "-")
    data.bias[,grep("PPT", names(data.bias))] <- sweep(data.mean[,grep("PPT", names(data.mean))], MARGIN=2, STATS=as.vector(unlist(data.mean[1,grep("PPT", names(data.mean))])), "/")-1 #express bias as relative for precipitation
    
    x <- get(paste("data", funs[xfun], sep="."))[, which(names(data.mean)==variable1)]
    y <- get(paste("data", funs[yfun], sep="."))[, which(names(data.mean)==variable2)]
    
    # xlim=if(variable.type1=="ratio") range(x) else if(min(x)<0) range(x) else c(0, max(x))
    # ylim=if(variable.type2=="ratio") range(y) else if(min(y)<0) range(y) else c(0, max(y))
    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
    plot(x,y,col="white", tck=0, xaxt="n", yaxt="n", xlim=xlim, ylim=ylim, ylab="",
         xlab=paste(fun.names[xfun], variable.names$Variable[which(variable.names$Code==variable1)]),
    )
    par(mgp=c(2.5,0.25, 0))
    title(ylab=paste(fun.names[yfun], variable.names$Variable[which(variable.names$Code==variable2)]))
    lines(c(0,0), c(-99,99), lty=2, col="gray")
    lines(c(-99,99), c(0,0), lty=2, col="gray")
    
    gcms <- unique(data.mean$gcm[!is.na(data.mean$run)])
    
    mods <- substr(gcms, 1, 3)
    mods[which(mods=="CNR")] <- paste("MIR", c("c", "e"), sep="")
    mods[which(mods=="MIR")] <- paste("MIR", c("e", "6"), sep="")
    mods[which(mods=="MPI")] <- paste("MPI", c("h", "l"), sep="")
    
    colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
    set.seed(2)
    ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(gcms)-12))
    ColScheme[11] <- "blue"
    
    for(gcm in gcms){
      i=which(gcms==gcm)
      s=which(data.mean$gcm==gcm)
      points(x[s],y[s], pch=21, bg=ColScheme[i], cex=1)
      points(mean(x[s]),mean(y[s]), pch=21, bg=ColScheme[i], cex=4)
      text(mean(x[s]),mean(y[s]), mods[i], cex=0.7, font=2)
    }
    
    if(element1=="PPT"){
      axis(1, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100), "%", sep=""), tck=0)
    } else axis(1, at=pretty(x), labels=pretty(x), tck=0)
    if(element2=="PPT"){
      axis(2, at=seq(-99,99,0.1), labels=paste(round(seq(-99,99,0.1)*100), "%", sep=""), las=2, tck=0)
    } else axis(2, at=pretty(y), labels=pretty(y), las=2, tck=0)
    
    # legend("bottomleft", legend = c("2001-2019", "2011-2019"), title = "Observed change", pch=c(16, 1), pt.cex=2, col="red", bty="n")
    
  },
  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.4,0))
  )
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = 'Model Metadata. Number of runs for each scenario are what we have downloaded, not necessarily what are currently available.'
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


