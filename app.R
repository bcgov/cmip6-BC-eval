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
library(plotly)
library(stinepack) # for interpolation splines

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
select <- c(1,3,4,6,7,8,9,11,12,14,15,17,19)
gcms.select = gcms[select]
scenarios <- unique(template[,1])
scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
gcm.names <- as.character(modelMetadata[,1])
gcm.names.select <- gcm.names[select]

mods <- substr(gcm.names, 1, 2)

colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
set.seed(2)
ColScheme <- c(brewer.pal(n=12, "Paired"),sample(colors,length(gcm.names)-12))
ColScheme[11] <- "blue"


# Other definitions
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)

proj.years <- c(2010, 2030, 2050, 2070, 2090)
proj.year.names <- c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

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
                          helpText("Compare CMIP6 climate model simulations to each other and to observations. Compile custom ensembles with and without bias correction. See projections for subregions (ecoprovinces) of BC. The 13-model ClimateBC/NA ensemble is the default selection."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          radioButtons("mode", "GCM selection mode",
                                       choiceNames = c("Single GCM", "Ensemble"),
                                       choiceValues = c("Single GCM", "Ensemble"),
                                       selected = "Ensemble",
                                       inline = T),
                          
                          conditionalPanel(
                            condition = "input.mode == 'Single GCM'",
                            
                            radioButtons("gcms.ts1", "Choose global climate models:",
                                         choiceNames = gcm.names,
                                         choiceValues = gcms,
                                         selected = gcms[4],
                                         inline = T
                            ),
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.mode == 'Ensemble'",
                            
                            checkboxGroupInput("gcms.ts2", "Choose global climate models:",
                                               choiceNames = gcm.names,
                                               choiceValues = gcms,
                                               selected = gcms[select],
                                               inline = T
                            ),
                            
                            checkboxInput("compile", label = "Compile into ensemble projection", value = TRUE),
                            
                          ),
                          
                          # checkboxInput("compile", label = "Compile into ensemble projection", value = TRUE),

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
             
             tabPanel("Change", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Compare amount of change among models, relative to the 1961-1990 period. Click on a legend item to hide it; double-click to isolate it. Drag a box on the plot to zoom in; double-click the plot to zoom back out."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxGroupInput("gcms.change", "Choose global climate models:",
                                             choiceNames = gcm.names[select],
                                             choiceValues = gcm.names[select],
                                             selected = gcm.names[select],
                                             inline = T
                          ),
                          
                          radioButtons("proj.year.change", inline = TRUE,
                                       label = "Choose a time slice",
                                       choiceNames = proj.year.names,
                                       choiceValues = proj.years,
                                       selected = proj.years[2]),
                          
                          radioButtons("scenario.change", "Choose emissions scenario",
                                             choiceNames = scenario.names[-1],
                                             choiceValues = scenarios[-1],
                                             selected = scenarios[3],
                                             inline = T),
                          
                          checkboxInput("trajectories", label = "Include model trajectories", value = T),
                          
                          selectInput("element1.change",
                                      label = "x-axis: choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[1]),
                          
                          selectInput("yeartime1.change",
                                      label = "x-axis: Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
                            selectInput("element2.change",
                                        label = "y-axis: choose the climate element",
                                        choices = as.list(element.names),
                                        selected = element.names[4]),
                            
                            selectInput("yeartime2.change",
                                        label = "y-axis: Choose the month/season",
                                        choices = as.list(yeartime.names),
                                        selected = yeartime.names[3]),

                          selectInput("ecoprov.name.change",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotlyOutput(outputId = "ChangePlot", height="600px")
                          
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
             
             tabPanel("Bias", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Compare bias among models, relative to observations. Bias is the difference between the observed 1961-1990 climate and the simulated 1961-1990 climate for each model run. The labelled point for each model is the mean of the biases for all of the historical runs of that model. "),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          checkboxInput("ClimateBC", label = "Reduce to the ClimateBC/NA ensemble", value = TRUE),
                          
                          checkboxInput("showruns", label = "Show scatter of model runs", value = TRUE),
                          
                          selectInput("element3",
                                      label = "Choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[2]),
                          
                          selectInput("yeartime3",
                                      label = "Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[3]),
                          
 
                            selectInput("element4",
                                        label = "Choose a climate element for comparison",
                                        choices = as.list(element.names),
                                        selected = element.names[3]),
                            
                            selectInput("yeartime4",
                                        label = "Choose a month/season for comparison",
                                        choices = as.list(yeartime.names),
                                        selected = yeartime.names[3]),

                          checkboxInput("equalscale", label = "Equal-scale axes", value = TRUE),
                          
                          selectInput("ecoprov.name.II",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotlyOutput(outputId = "BiasPlot", height="600px")
                          
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
             
             tabPanel("Resolution",
                      
                      includeMarkdown("resolution.Rmd"),
                      
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
    gcms.ts <- if(input$mode=="Ensemble") input$gcms.ts2 else input$gcms.ts1
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
          temp$compile <- if(length(gcms.ts)==0) rep(NA, dim(temp)[1]) else if(length(gcms.ts)==1) temp[,which(names(temp)==gcms.ts)] else apply(temp[,which(names(temp)%in%gcms.ts)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
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
    plot(0, col="white", xlim=c(1900, 2100), ylim=range(alldata, na.rm = T), xaxs="i", tck=0, xlab="", ylab=ylab)
    
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
      
      if(input$compile==T) gcms.ts <- "compile" #this prevents the plotting of individual GCM projections and plots a single envelope for the ensemble as a whole. 
      for(gcm in gcms.ts){
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
      c <- if(length(gcms.ts>0)) 3 else NA
      s <- !is.na(c(a,b,c))
      legend.GCM <- if(input$mode=="Ensemble") paste("GCM simulations (min & max);", length(input$gcms.ts2), "models")  else paste("GCM simulations (min & max);", input$gcms.ts1)
      legend("topleft", title = "Historical Period", legend=c("Station observations (ClimateBC)", "ERA5 reanalysis", legend.GCM)[s], bty="n",
             lty=c(1,1,NA)[s], col=c(obs.color, era5.color, NA)[s], lwd=c(3,2,NA)[s], pch=c(NA,NA, 22)[s], pt.bg = c(NA, NA, colScheme[1])[s], pt.cex=c(NA,NA,2)[s])
      
      s <- rev(which(scenarios[-1]%in%input$scenarios1))
      legend("top", title = "Future Scenarios", legend=scenario.names[-1][s], bty="n",
             lty=c(NA,NA,NA,NA)[s], col=c(NA,NA,NA,NA)[s], lwd=c(NA,NA,NA,NA)[s], pch=c(22, 22, 22, 22)[s], pt.bg = colScheme[-1][s], pt.cex=c(2,2,2,2)[s])
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      mtext("  Created using https://bcgov-env.shinyapps.io/cmip6-BC\n  Copyright 2021 Province of BC\n  Contact: Colin Mahony colin.mahony@gov.bc.ca", side=1, line=-1.35, adj=0.0, font=2, cex=1, col="gray")
      
      print(num)
    }
    box()
  }
  output$timeSeries <- renderPlot({ timeSeriesPlot() },
                                  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.45,0))
  )
  
  
  output$downloadPlot <- downloadHandler(
    filename =  "Plot.png",
    
    content = function(file) {
      
      pixelratio <- session$clientData$pixelratio
      width  <- session$clientData$output_timeSeries_width
      height <- session$clientData$output_timeSeries_height
      
      png(file, width = width*pixelratio*3/2, height = height*pixelratio*3, res = 120*pixelratio)
      timeSeriesPlot()
      dev.off()
    } 
  )
  
  output$ChangePlot <- renderPlotly({
    
    # ecoprov <- ecoprovs[1]
    # yeartime1 <- yeartimes[1]
    # yeartime2 <- yeartimes[1]
    # element1 <- elements[1]
    # element2 <- elements[4]
    # proj.year <- proj.years[3]
    # scenario <- scenarios[2]
    
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.change)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1.change)]
    yeartime2 <- yeartimes[which(yeartime.names==input$yeartime2.change)]
    element1 <- elements[which(element.names==input$element1.change)]
    element2 <- elements[which(element.names==input$element2.change)]
    proj.year <- input$proj.year.change
    scenario <- input$scenario.change
    gcms.change <- input$gcms.change
    
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    
    data <- read.csv(paste("data/change", ecoprov, "csv", sep="."))
    
    x <- data[, which(names(data)==variable1)]
    y <- data[, which(names(data)==variable2)]
    x0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable1)]
    y0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable2)]
    
    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    
    
    fig <- plot_ly(x=x,y=y, type = 'scatter', mode = 'markers', marker = list(color ="lightgrey", size=5), hoverinfo="none", color="All models/scenarios/times")

    fig <- fig %>% layout(xaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable1)]), 
                                       range=xlim), 
                          yaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable2)]),
                                       range=ylim)
                          )
    
    fig <- fig %>% add_markers(x=x0,y=y0, color="observed\n(2001-2020)", text="observed\n(2001-2020)", hoverinfo="text",
                               marker = list(size = 30,
                                             color = "lightgrey"))
    
        gcm=gcms.change[2]
    for(gcm in gcms.change){
      i=which(gcm.names==gcm)
      x1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable1)]
      y1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable2)]
      x2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable1)]
      y2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable2)]
      
      if(input$trajectories==T){
        if(length(unique(sign(diff(x2))))==1){
          x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
          y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
          s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/500)) # way better than interpSpline, not prone to oscillations
          fig <- fig %>% add_trace(x=s$x, y=s$y, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        } else fig <- fig %>% add_trace(x=x2, y=y2, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        
        fig <- fig %>% add_markers(x=x2,y=y2, color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
                                   marker = list(size = 8,
                                                 color = ColScheme[i]),
                                   legendgroup=paste("group", i, sep=""), showlegend = FALSE)
      }
      
      fig <- fig %>% add_markers(x=x1,y=y1, color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
                                 marker = list(size = 20,
                                               color = ColScheme[i],
                                               line = list(color = "black",
                                                           width = 1)),
                                 legendgroup=paste("group", i, sep=""))
      
      fig <- fig %>% add_annotations(x=x1,y=y1, text = sprintf("<b>%s</b>", mods[i]), xanchor = 'center', yanchor = 'center', showarrow = F,
                                     legendgroup=paste("group", i, sep="")    )
      
    }
    
    if(element1=="PPT") fig <- fig %>% layout(xaxis = list(tickformat = "%"))
    if(element2=="PPT") fig <- fig %>% layout(yaxis = list(tickformat = "%"))

        fig
 
    }  
  )
  
  
  output$BiasPlot <- renderPlotly({
    
    # ecoprov <- ecoprovs[1]
    # yeartime1 <- yeartimes[1]
    # yeartime2 <- yeartimes[3]
    # element1 <- elements[4]
    # element2 <- elements[4]
    # xfun <- 1
    # yfun <- 1
    # showruns <- T
    # ClimateBC <- T
    
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.II)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime3)]
    yeartime2 <- yeartimes[which(yeartime.names==input$yeartime4)] 
    element1 <- elements[which(element.names==input$element3)]
    element2 <- elements[which(element.names==input$element4)] 
    xfun <- 1
    yfun <- 1
    showruns <- input$showruns
    ClimateBC <- input$ClimateBC
    equalscale <- input$equalscale
    
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    
    data.mean <- read.csv(paste("data/summary.mean", ecoprov, "csv", sep="."), stringsAsFactors = F)
    # data.sd <- read.csv(paste("data/summary.sd", ecoprov, "csv", sep="."), stringsAsFactors = F)
    # data.sd[,grep("PPT", names(data.bias))] <- data.sd[,grep("PPT", names(data.bias))]/data.mean[,grep("PPT", names(data.bias))] # convert sd to coefficient of variation for precipitation
    # data.sd[,-c(1:2)] <- sweep(data.sd[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.sd[1,-c(1:2)])), "/")-1 # express sd relative to observational
    data.bias <- data.mean
    data.bias[,-c(1:2)] <- sweep(data.mean[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.mean[1,-c(1:2)])), "-")
    data.bias[,grep("PPT", names(data.bias))] <- sweep(data.mean[,grep("PPT", names(data.mean))], MARGIN=2, STATS=as.vector(unlist(data.mean[1,grep("PPT", names(data.mean))])), "/")-1 #express bias as relative for precipitation
    
    x <- get(paste("data", funs[xfun], sep="."))[, which(names(data.mean)==variable1)]
    y <- get(paste("data", funs[yfun], sep="."))[, which(names(data.mean)==variable2)]
    
    # xlim=if(variable.type1=="ratio") range(x) else if(min(x)<0) range(x) else c(0, max(x))
    # ylim=if(variable.type2=="ratio") range(y) else if(min(y)<0) range(y) else c(0, max(y))
    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    # fig <- plot_ly(x=x,y=y, type = 'scatter', mode = 'markers', marker = list(color ="white"), hoverinfo="none")
    fig <- plot_ly(x=x,y=y, type = NULL)
    
    fig <- fig %>% layout(xaxis = list(title=paste(fun.names[xfun], variable.names$Variable[which(variable.names$Code==variable1)]), 
                                       range=xlim), 
                          yaxis = list(title=paste(fun.names[yfun], variable.names$Variable[which(variable.names$Code==variable2)]),
                                       range=ylim)
    )

    gcms.bias <- if(ClimateBC==T) unique(data.mean$gcm[!is.na(data.mean$run)])[select] else unique(data.mean$gcm[!is.na(data.mean$run)])
    mods <- substr(gcm.names, 1, 2)
    
    for(gcm in gcms.bias){
      i=which(gcm.names==gcm)
      j=which(gcms.bias==gcm)
      s=which(data.mean$gcm==gcm)
      
      if(showruns==T){
        fig <- fig %>% add_markers(x=x[s],y=y[s], color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
                                   marker = list(size = 8,
                                                 color = ColScheme[i]),
                                   legendgroup=paste("group", i, sep=""), showlegend = FALSE)
      }
      
      fig <- fig %>% add_markers(x=mean(x[s]),y=mean(y[s]), color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
                                 marker = list(size = 20,
                                               color = ColScheme[i],
                                               line = list(color = "black",
                                                           width = 1)),
                                 legendgroup=paste("group", i, sep=""))
      
      fig <- fig %>% add_annotations(x=mean(x[s]),y=mean(y[s]), text = sprintf("<b>%s</b>", mods[i]), xanchor = 'center', yanchor = 'center', showarrow = F,
                                     legendgroup=paste("group", i, sep="")    )
      
      
    }
    
    if(equalscale==T) fig <- fig %>% layout(yaxis=list(scaleanchor="x", scaleratio=1))
    
    if(element1=="PPT") fig <- fig %>% layout(xaxis = list(tickformat = "%"))
    if(element2=="PPT") fig <- fig %>% layout(yaxis = list(tickformat = "%"))
    
    fig
    
  }
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


