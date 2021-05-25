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
library("shinyLP")
library("shinyBS")
library("shinythemes")
library("shinyWidgets")
# ----------------------------------------------
# Load the input data
# ----------------------------------------------

modelMetadata <- read.csv("data/ModelList.csv")
kkzRank.includeUKESM <- read.csv("data/kkzRank.includeUKESM.csv")
kkzRank.excludeUKESM <- read.csv("data/kkzRank.excludeUKESM.csv")

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

tab_block <- function(text, cor, icon, id){
  HTML(paste0('<a id="', id,'" href="#" class="action-button">
                  <div class = "voronoys-block" style = "background-color:', cor, ';"> 
                  <span class = "name">', texto, '</span>
                  <div class="img_block">
                    <div class="img_block_conteiner">
                      <img src="img/',icon,'">
                    </div>
                  </div>
              </div></a>'))
}

# Define UI ----
ui <- fluidPage(
  navbarPage(id="CMIP6-BC", title = "CMIP6 ensemble for ClimateBC", theme = "bcgov.css", 
             
             
             ## -----------------------------------------------------
             ## LANDING PAGE
             
             tabPanel(
               title = "Intro",
               value = "Intro",
               column(width = 12,
                      wellPanel(
                        HTML("<h1><b>CMIP6-BC</b> - The new climate model ensemble for ClimateBC</h1>"),
                        HTML("<h4>This tool provides visualizations and documentation of the global climate model ensemble featured in Version 7 
                                    of ClimateBC. The ensemble is from the new generation of global climate model simulations, the sixth Coupled Model 
                                    Intercomparison Project (CMIP6). Use this tool to learn about the model simulations in ClimateBC, choose a small 
                                    ensemble suited for your research, and export plots for your own use. </h4>")
                      )
               ),
               column(width = 2, align = "left",
                      wellPanel(
                        actionButton("link_to_timeSeries", HTML("<h4><b>Time series</b></h4>")),
                        HTML("<h5> Compare historical and future model projections against observations,
                                                  for individual models and customizable ensembles,
                                                  with and without bias correction.</h5 >")
                      )
               ),
               column(width = 2, align = "left",
                      wellPanel(
                        actionButton("link_to_Change", HTML("<h4><b>Choose models</b></h4>")),
                        HTML("<h5>Compare model projections in a two-variable climate space. 
                                    Create smaller ensembles based on predefined or custom criteria.</h5 >")
                      )
               ),
               column(width = 2, align = "left",
                      wellPanel(
                        actionButton("link_to_Bias", HTML("<h4><b>Assess Bias</b></h4>")),
                        HTML("<h5>Assess model biases relative to historical observations.</h5 >")
                      )
               ),
               column(width = 2, align = "left",
                      wellPanel(
                        actionButton("link_to_Maps", HTML("<h4><b>Maps</b></h4>")),
                        HTML("<h5>Compare spatial variation in climate change among models. </h5 >")
                      )
               ),
               column(width = 2, align = "left",
                      wellPanel(
                        actionButton("link_to_Guidance", HTML("<h4><b>Guidance</b></h4>")),
                        HTML("<h5>Guidance for selecting models, emissions scenarios, and time periods. </h5 >")
                      )
               ),
               column(width = 12,
                      br(), 
                      HTML("<h4><b>Contributors</b></h4>"),
                      HTML("<h5 >
                               App created by:<br>
                                 Colin Mahony<br>
                                 Research Climatologist<br>
                                 BC Ministry of Forests, Lands, Natural Resource Operations and Rural Development<br>
                                 colin.mahony@gov.bc.ca<br>
                               <br>
                               CMIP6 data downloaded and subsetted by Tongli Wang, Associate Professor at the UBC Department of Forest and Conservation Sciences.<br>
                               <br>
                               <b>Code: </b>The code and data for this tool are available at <a href='https://github.com/bcgov/cmip6-BC-eval'>https://github.com/bcgov/cmip6-BC-eval</a></h5>")
                      
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
             
             ## -----------------------------------------------------
             ## ABOUT
             
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
             
             ## -----------------------------------------------------
             ## Time Series
             
             tabPanel("Time Series",
                      sidebarLayout(
                        sidebarPanel(
                          helpText("Compare CMIP6 climate model simulations to each other and to observations. Compile custom ensembles with and without bias correction. See projections for subregions (ecoprovinces) of BC. The 13-model ClimateBC/NA ensemble is the default selection. Shaded areas are the minimum and maximum of the multiple simulation runs for each climate model; a line indicates there is only one simulation for that scenario. "),
                          
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
                          
                          img(src = "Ecoprovinces_Title.png", height = round(1861*1/5), width = round(1993*1/5))
                        ),
                        
                        mainPanel(
                          
                          downloadButton(outputId = "downloadPlot", label = "Download plot"),
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
             
             ## -----------------------------------------------------
             ## CHANGE
             
             tabPanel("Choose models", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("This tab shows the amount of change projected by each model, relative to the 1961-1990 period. 
                                   You can use this tab to reduce the ensemble size base on predefined or custom model selection methods; 
                                   see the 'About' tab for more information on the predefined ensembles. 
                                   Click on a legend item to hide it; double-click to isolate it. 
                                   Drag a box on the plot to zoom in; double-click the plot to zoom back out."),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                          
                          
                          radioButtons("modeChange", "Ensemble selection mode",
                                       choiceNames = c("Predefined", "Custom"),
                                       choiceValues = c("Predefined", "Custom"),
                                       selected = "Predefined",
                                       inline = T),
                          
                          conditionalPanel(
                            condition = "input.modeChange == 'Predefined'",
                            
                            checkboxInput("includeUKESM", label = "Include UKESM1 in small ensembles (<9 models)", value = T),
                            
                            sliderInput("kkzN", label = "Reduce ensemble size in predefined order", min = 1, max = 13, value = 13, step=1),
                          ),
                          
                          
                          conditionalPanel(
                            condition = "input.modeChange == 'Custom'",
                            
                            checkboxGroupInput("gcms.change", "Choose global climate models:",
                                               choiceNames = gcm.names[select],
                                               choiceValues = gcm.names[select],
                                               selected = gcm.names[select],
                                               inline = T
                            ),
                          ),
                          
                          radioButtons("proj.year.change", inline = TRUE,
                                       label = "Choose a time slice",
                                       choiceNames = proj.year.names,
                                       choiceValues = proj.years,
                                       selected = proj.years[3]),
                          
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
                                      selected = yeartime.names[1]),
                          
                          selectInput("element2.change",
                                      label = "y-axis: choose the climate element",
                                      choices = as.list(element.names),
                                      selected = element.names[4]),
                          
                          selectInput("yeartime2.change",
                                      label = "y-axis: Choose the month/season",
                                      choices = as.list(yeartime.names),
                                      selected = yeartime.names[1]),
                          
                          selectInput("ecoprov.name.change",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
                        ),    
                        
                        mainPanel(
                          
                          plotlyOutput(outputId = "ChangePlot", height="600px"),
                          downloadButton(outputId = "downloadData_change", label = "Download data")
                          
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
             
             ## -----------------------------------------------------
             ## BIAS TAB
             
             tabPanel("Assess bias", 
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
 
             ## -----------------------------------------------------
             ## Maps TAB
             
             tabPanel("Maps", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("These maps show the spatial pattern of simulated climate change for each model. 
                                   The change is the mean climate of the 2041-2070 period of the SSP2-4.5 simulations 
                                   relative to the 1961-1990 period of the model's historical simulations. 
                                   Maps for the Pacific Northwest are derived from raw GCM files; maps for North America are derived from ClimateNA output.
                                   "),
                          
                          tags$head(tags$script('$(document).on("shiny:connected", function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            $(window).resize(function(e) {
                            Shiny.onInputChange("innerWidth", window.innerWidth);
                            });
                            ')),
                          
                          radioButtons("areaMap", inline = F,
                                       label = "Choose the zoom level",
                                       choices = c("Pacific Northwest", "North America"),
                                       selected = "Pacific Northwest"),

                          radioButtons("elementMap", inline = F,
                                       label = "Choose the climate element",
                                       choiceNames = as.list(element.names)[-1],
                                       choiceValues = as.list(elements)[-1],
                                       selected = elements[4]),
                          
                          radioButtons("seasonsOrMonths", "Months or Seasons",
                                       choiceNames = c("Months", "Seasons"),
                                       choiceValues = c("Months", "Seasons"),
                                       selected = "Months",
                                       inline = T),
                          
                          conditionalPanel(
                            condition = "input.seasonsOrMonths == 'Seasons'",
                            
                            radioGroupButtons(
                              inputId = "seasonbuttons",
                              label = "Choose a season",
                              choices = season.names, 
                              selected = season.names[3]
                            )
                            
                          ),
                          
                          conditionalPanel(
                            condition = "input.seasonsOrMonths == 'Months'",
                            
                            sliderTextInput("monthslider", label = "Choose a month", choices = month.abb, selected = month.abb[7])
                          )
                          
                        ),    
                        
                        mainPanel(
                          
                          imageOutput("changeMap", width="100%", height="100%")
                          
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
             
             ## -----------------------------------------------------
             ## GUIDANCE
             
             tabPanel("Guidance",
                      
                      includeMarkdown("guidance.Rmd"),
                      
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
             
             ## -----------------------------------------------------
             ## RESOLUTION
             
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
             
             ## -----------------------------------------------------
             ## MODEL INFO
             
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
  
  observeEvent(input$link_to_timeSeries, {
    updateNavbarPage(session, "CMIP6-BC", selected="Time Series")
  })
  
  observeEvent(input$link_to_Change, {
    updateNavbarPage(session, "CMIP6-BC", selected="Choose models")
  })
  
  observeEvent(input$link_to_Bias, {
    updateNavbarPage(session, "CMIP6-BC", selected="Assess bias")
  })
  
  observeEvent(input$link_to_Maps, {
    updateNavbarPage(session, "CMIP6-BC", selected="Maps")
  })
  
  observeEvent(input$link_to_Guidance, {
    updateNavbarPage(session, "CMIP6-BC", selected="Guidance")
  })
  
  
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
            assign(paste("x", scenario, sep="."), x)
            assign(paste(ensstat, scenario, sep="."), temp)
          }
          
          # colScheme <- c("gray60", "seagreen", "goldenrod4", "darkorange3", "darkred")
          colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")
          # colScheme <- c("gray80", "#1d3354", "#e9dc3d", "#f11111", "#830b22")
          polygon(c(x, rev(x)), c(ensmin, rev(ensmax)), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=colScheme[which(scenarios==scenario)])

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
        
        # overlay the ensemble mean lines on top of all polygons
        for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
          if(input$showmean==T) lines(get(paste("x", scenario, sep=".")), get(paste("ensmean", scenario, sep=".")), col=colScheme[which(scenarios==scenario)], lwd=2)
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
      text(1925,mean(temp$compile[60:80]), label, col="black", pos=3, font=2, cex=1)
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
      legend.GCM <- if(input$mode=="Ensemble") paste("Simulations (", length(input$gcms.ts2), "GCMs)", sep="")  else paste("Simulations (", input$gcms.ts1, ")", sep="")
      legend("topleft", title = "Historical Period", legend=c("Station observations", "ERA5 reanalysis", legend.GCM)[s], bty="n",
             lty=c(1,1,NA)[s], col=c(obs.color, era5.color, NA)[s], lwd=c(3,2,NA)[s], pch=c(NA,NA, 22)[s], pt.bg = c(NA, NA, colScheme[1])[s], pt.cex=c(NA,NA,2)[s])
      
      s <- rev(which(scenarios[-1]%in%input$scenarios1))
      legend("top", title = "Future Scenarios", legend=scenario.names[-1][s], bty="n",
             lty=c(NA,NA,NA,NA)[s], col=colScheme[-1][s], lwd=c(NA,NA,NA,NA)[s], pch=c(22, 22, 22, 22)[s], pt.bg = alpha(colScheme[-1][s], 0.35), pt.cex=c(2,2,2,2)[s])
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      mtext("  Created using https://bcgov-env.shinyapps.io/cmip6-BC\n  Copyright 2021 Province of BC\n  Contact: Colin Mahony colin.mahony@gov.bc.ca", side=1, line=-1.35, adj=0.0, font=1, cex=1, col="gray")
      
      print(num)
    }
    box()
  }
  output$timeSeries <- renderPlot({ timeSeriesPlot() },
                                  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.425,0))
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
    # gcms.change <- gcm.names[select]
    
    # observe(updateCheckboxGroupInput(session, "gcms.change", selected = gcm.names[select][which(gcm.names[select]%in%kkzRank[1:input$kkzN,which(ecoprov.names==input$ecoprov.name.change)])]))
    
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.change)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1.change)]
    yeartime2 <- yeartimes[which(yeartime.names==input$yeartime2.change)]
    element1 <- elements[which(element.names==input$element1.change)]
    element2 <- elements[which(element.names==input$element2.change)]
    proj.year <- input$proj.year.change
    scenario <- input$scenario.change
    if(input$modeChange=="Predefined"){
      if(input$includeUKESM==T){
      gcms.change <- gcm.names[select][which(gcm.names[select]%in%kkzRank.includeUKESM[1:input$kkzN,which(ecoprov.names==input$ecoprov.name.change)])]
      } else {
        gcms.change <- gcm.names[select][which(gcm.names[select]%in%kkzRank.excludeUKESM[1:input$kkzN,which(ecoprov.names==input$ecoprov.name.change)])]
      }
      } else {
        gcms.change <- input$gcms.change
    }
    
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    
    data <- read.csv(paste("data/change", ecoprov, "csv", sep="."))
    
    x <- data[, which(names(data)==variable1)]
    y <- data[, which(names(data)==variable2)]
    x0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable1)]
    y0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable2)]
    
    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    #initiate the plot
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
          s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/1500)) # way better than interpSpline, not prone to oscillations
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
  
  # Downloadable csv of selected dataset ----
  data_change <- reactive(read.csv(paste("data/change", ecoprovs[which(ecoprov.names==input$ecoprov.name.change)], "csv", sep=".")))
  
  output$downloadData_change <- downloadHandler(

        filename = function() {
      paste("CMIP6BC.change", ecoprovs[which(ecoprov.names==input$ecoprov.name.change)], "csv", sep=".")
    },
    content = function(file) {
      write.csv(data_change(), file, row.names = FALSE)
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
  
  output$changeMap <- renderImage({

    yeartimeMap <- if(input$seasonsOrMonths=="Seasons") seasons[which(season.names==input$seasonbuttons)] else monthcodes[which(month.abb==input$monthslider)]
    
    if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("ChangeMap", input$elementMap, yeartimeMap, "png",sep=".")))
    } else {filename <- normalizePath(file.path('./www', paste("ChangeMap.NorAm", input$elementMap, yeartimeMap, "png",sep=".")))}
    
    list(src = filename, width="100%", height="100%")
    
  }, deleteFile = FALSE)
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = HTML("<p>Information about models featured in this app.
                                 ECS is equilibrium climate sensitivity (long-term temperature change in response to an instant doubling of CO2), and values are quoted from <a href='https://advances.sciencemag.org/content/6/26/eaba1981.abstract'>Meehl et al. (2020)</a>. 
                                 The last five columns are the number of model runs for each scenario that are included in ClimateBC/NA and this app</p>")
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


