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
library(cobs)
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
select8 <- c(1,6,7,8,9,14,15,17)
gcms.select = gcms[select]
gcms.select8 = gcms[select8]
scenarios <- unique(template[,1])
scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
gcm.names <- as.character(modelMetadata[,1])
gcm.names.select <- gcm.names[select]
gcms.names.select8 = gcm.names[select8]

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
                        HTML("<h2><b>CMIP6-BC</b> - The new climate model ensemble for ClimateBC</h2>"),
                        HTML("<h4>This tool provides visualizations and documentation of the global climate model ensemble featured in Version 7 
                                    of ClimateBC. The ensemble is from the new generation of global climate model simulations, the sixth Coupled Model 
                                    Intercomparison Project (CMIP6). Use this tool to learn about the model simulations in ClimateBC and choose a small 
                                    ensemble suited for your research. A similar app for North America is available at <a href='https://bcgov-env.shinyapps.io/cmip6-BC/' target='_blank'>https://bcgov-env.shinyapps.io/cmip6-NA/</a></h5></h4>")
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
               # column(width = 2, align = "left",
               #        wellPanel(
               #          actionButton("link_to_Bias", HTML("<h4><b>Assess Bias</b></h4>")),
               #          HTML("<h5>Assess model biases relative to historical observations.</h5 >")
               #        )
               # ),
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
                      HTML("<h4><b>Citation</b></h4>
                            <h5> <u>Please cite the contents of this app as:</u> <br>
                            Mahony, C.R., T. Wang, A. Hamann, and A.J. Cannon. 2022. <a href='https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.7566' target='_blank'>A global climate model ensemble for downscaled monthly climate normals over North America</a>. International Journal of Climatology. In press. <a href='https://doi.org/10.1002/joc.7566' target='_blank'>doi.org/10.1002/joc.7566</a> </h5>
                            <h4><b>Contributors</b></h4>
                            <h5> <u>App created by:</u><br>
                                 Colin Mahony<br>
                                 Research Climatologist<br>
                                 BC Ministry of Forests, Lands, Natural Resource Operations and Rural Development<br>
                                 colin.mahony@gov.bc.ca<br>
                               <br>
                               CMIP6 data downloaded and subsetted by Tongli Wang, Associate Professor at the UBC Department of Forest and Conservation Sciences.<br></h5>
                            <h4><b>Code</b></h4>
                            <h5> The code and data for this tool are available at <a href='https://github.com/bcgov/cmip6-BC-eval' target='_blank'>https://github.com/bcgov/cmip6-BC-eval</a></h5>
                               <br>")
                      
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
                          helpText("Compare CMIP6 climate model simulations to each other and to observations. Compile custom ensembles with and without bias correction. See projections for subregions (ecoprovinces) of BC. The 8-model subset of the ClimateBC/NA ensemble is the default selection. Shaded areas are the minimum and maximum of the multiple simulation runs for each climate model; a line indicates there is only one simulation for that scenario. "),
                          
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
                            condition = "input.mode == 'Ensemble'",
                            
                            fluidRow(
                              box(width = 12,
                                  splitLayout(cellWidths = c("75%", "25%"),
                                              
                                              radioButtons("default.ensemble", label="Default Ensemble",
                                                           choiceNames = c("13-model (ClimateBC/NA)", "8-model subset"),
                                                           choiceValues = c("13-model", "8-model"),
                                                           selected = "8-model",
                                                           inline = T
                                              ),
                                              
                                              actionButton("reset_input", "Reset")
                                              
                                  )
                              )
                            ),
                            
                            checkboxInput("compile", label = "Compile into ensemble projection", value = TRUE),
                            
                            uiOutput('reset_gcms'), # this is the radiobutton list of gcms but it is moved to the server side to allow the reset button 
                            

                          ),
                          
                          conditionalPanel(
                            condition = "input.mode == 'Single GCM'",
                            
                            radioButtons("gcms.ts1", "Choose a global climate model:",
                                         choiceNames = gcm.names,
                                         choiceValues = gcms,
                                         selected = gcms[4],
                                         inline = T
                            ),
                          ),
                          
                          checkboxInput("biascorrect", label = "Bias correction (match 1961-90 model climate to observations)", value = TRUE),
                          
                          checkboxGroupInput("scenarios1", "Choose emissions scenarios",
                                             choiceNames = scenario.names[-1],
                                             choiceValues = scenarios[-1],
                                             selected = scenarios[c(2,3,4)],
                                             inline = T
                          ),
                          
                          checkboxInput("refline", label = "Show 1961-1990 baseline for models", value = T),
                          
                          fluidRow(
                            box(width = 12, 
                                splitLayout(
                                  checkboxInput("showrange", label = "Show range of projections", value = T),
                                  checkboxInput("showmean", label = "Show mean of projections", value = T),
                                  checkboxInput("simplify", label = "Simplify", value = F)
                                )
                            )
                          ),
                          
                          fluidRow(
                            box(width = 12, 
                                splitLayout(
                                  checkboxInput("yearlines", label = "Show 5-year gridlines", value = F),
                                  
                                  checkboxInput("yfit", label = "fit y axis to visible data", value = T)
                                )
                            )
                          ),
                          
                                  

                          fluidRow(
                            box(width = 12,
                                splitLayout(cellWidths = c("75%", "25%"),
                                            
                                            radioButtons("compare.ensemble", label="Compare to a predefined ensemble",
                                                         choiceNames = c("None", "13-model (ClimateNA)", "8-model subset"),
                                                         choiceValues = c("None", "13-model", "8-model"),
                                                         selected = "None",
                                                         inline = T
                                            )
                                )
                            )
                          ),
                          
                          checkboxGroupInput("observations", "Choose observational datasets",
                                             choiceNames = c("Stations (PCIC)", "Stations (ClimateBC)", "ERA5 reanalysis"),
                                             choiceValues = c("pcic", "climatebc", "era5"),
                                             selected = "pcic",
                                             inline = T
                          ),
                          
                          div(style="display:inline-block; width: 290px",selectInput("element1",
                                                                       label = "Choose the climate element",
                                                                       choices = as.list(element.names),
                                                                       selected = element.names[1])),
                          div(style="display:inline-block; width: 200px",selectInput("yeartime1",
                                                                       label = "Choose the month/season",
                                                                       choices = as.list(yeartime.names),
                                                                       selected = yeartime.names[3])),
                          
                          checkboxInput("compare", label = "Compare two variables", value = F),
                          
                          conditionalPanel(
                            condition = "input.compare == true",
                            
                            div(style="display:inline-block; width: 290px",selectInput("element2",
                                                                                       label = "Choose the climate element",
                                                                                       choices = as.list(element.names),
                                                                                       selected = element.names[1])),
                            div(style="display:inline-block; width: 200px", selectInput("yeartime2",
                                                                                        label = "Choose the month/season",
                                                                                        choices = as.list(yeartime.names),
                                                                                        selected = yeartime.names[1])),
                          ),
                          
                          selectInput("ecoprov.name",
                                      label = "Choose an ecoprovince",
                                      choices = as.list(ecoprov.names),
                                      selected = ecoprov.names[1]),
                          
                          img(src = "Ecoprovinces_Title.png", height = round(1861*1/5), width = round(1993*1/5)),
                          
                          sliderInput("cex", label = "font size", min = 0.8, max = 1.4, step=0.1, value=1)
                        ),
                        
                        mainPanel(

                          # div(style="display:inline-block; width: 450px",downloadButton(outputId = "downloadPlot", label = "Download plot")),
                          # div(style="display:inline-block; width: 200px", sliderInput("cex", label = "font size", min = 0.8, max = 1.4, step=0.1, value=1)),
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
                          
                          fluidRow(
                            box(width = 12, 
                                splitLayout(
                                  checkboxInput("trajectories", label = "Show model trajectories", value = T),                                  
                                  checkboxInput("runs", label = "Show model runs", value = F)
                                )
                            )
                          ),
                          
                          div(style="display:inline-block; width: 290px",selectInput("element1.change",
                                                                                     label = "x-axis element",
                                                                                     choices = as.list(element.names),
                                                                                     selected = element.names[1])),
                          div(style="display:inline-block; width: 200px",selectInput("yeartime1.change",
                                                                                     label = "x-axis month/season",
                                                                                     choices = as.list(yeartime.names),
                                                                                     selected = yeartime.names[3])),
                          
                          
                          div(style="display:inline-block; width: 290px",selectInput("element2.change",
                                                                                     label = "y-axis element",
                                                                                     choices = as.list(element.names),
                                                                                     selected = element.names[4])),
                          div(style="display:inline-block; width: 200px",selectInput("yeartime2.change",
                                                                                     label = "y-axis month/season",
                                                                                     choices = as.list(yeartime.names),
                                                                                     selected = yeartime.names[3])),

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
             
             # ## -----------------------------------------------------
             # ## BIAS TAB
             # 
             # tabPanel("Assess bias", 
             #          sidebarLayout(
             #            sidebarPanel(
             #              helpText("Compare bias among models, relative to observations. Bias is the difference between the observed 1961-1990 climate (sourced from ClimateBC) and the simulated 1961-1990 climate for each model run. The labelled point for each model is the mean of the biases for all of the historical runs of that model. "),
             #              
             #              tags$head(tags$script('$(document).on("shiny:connected", function(e) {
             #                Shiny.onInputChange("innerWidth", window.innerWidth);
             #                });
             #                $(window).resize(function(e) {
             #                Shiny.onInputChange("innerWidth", window.innerWidth);
             #                });
             #                ')),
             #              
             #              checkboxInput("ClimateBC", label = "Reduce to the ClimateBC/NA ensemble", value = TRUE),
             #              
             #              checkboxInput("showruns", label = "Show scatter of model runs", value = TRUE),
             #              
             #              selectInput("element3",
             #                          label = "Choose the climate element",
             #                          choices = as.list(element.names),
             #                          selected = element.names[2]),
             #              
             #              selectInput("yeartime3",
             #                          label = "Choose the month/season",
             #                          choices = as.list(yeartime.names),
             #                          selected = yeartime.names[3]),
             #              
             #              
             #              selectInput("element4",
             #                          label = "Choose a climate element for comparison",
             #                          choices = as.list(element.names),
             #                          selected = element.names[3]),
             #              
             #              selectInput("yeartime4",
             #                          label = "Choose a month/season for comparison",
             #                          choices = as.list(yeartime.names),
             #                          selected = yeartime.names[3]),
             #              
             #              checkboxInput("equalscale", label = "Equal-scale axes", value = F),
             #              
             #              selectInput("ecoprov.name.II",
             #                          label = "Choose an ecoprovince",
             #                          choices = as.list(ecoprov.names),
             #                          selected = ecoprov.names[1]),
             #              
             #              img(src = "Ecoprovinces_Title.png", height = 1861*1/5, width = 1993*1/5)
             #            ),    
             #            
             #            mainPanel(
             #              
             #              plotlyOutput(outputId = "BiasPlot", height="600px")
             #              
             #            )
             #          ),
             #          column(width = 12,
             #                 style = "background-color:#003366; border-top:2px solid #fcba19;",
             #                 
             #                 tags$footer(class="footer",
             #                             tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
             #                                      tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
             #                                      )
             #                             )
             #                 )
             #          )
             # ),
             
             ## -----------------------------------------------------
             ## Maps TAB
             
             tabPanel("Maps", 
                      sidebarLayout(
                        sidebarPanel(
                          helpText("These maps show the spatial pattern of simulated climate change for each model relative to the 1961-1990 period. 
                                   All maps are derived from raw GCM files.
                                   Bias is the difference between the observed 1961-1990 climate (sourced from ClimateBC) and the simulated 1961-1990 climate for each model run.
                                   Temperature units (K) are Kelvins, which are equivalent to degrees Celsius. 
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
                                       choices = c("North America", "Pacific Northwest"),
                                       selected = "North America"),
                          
                          radioButtons("mapType", inline = F,
                                       label = "Choose the map type",
                                       choices = c("Climate change", "Bias", "Topography"),
                                       selected = "Climate change"),
                          
                          conditionalPanel(
                            condition = "input.mapType == 'Bias'",
                            
                            radioButtons("elementMap", inline = F,
                                         label = "Choose the climate element",
                                         choiceNames = as.list(element.names)[-1],
                                         choiceValues = as.list(elements)[-1],
                                         selected = elements[4]),
                            
                            radioButtons("seasonsOrMonths", "Months or Seasons",
                                         choiceNames = c("Months", "Seasons"),
                                         choiceValues = c("Months", "Seasons"),
                                         selected = "Seasons",
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
                          
                          conditionalPanel(
                            condition = "input.mapType == 'Climate change'",
                            
                            radioButtons("elementMap2", inline = F,
                                         label = "Choose the climate element",
                                         choiceNames = as.list(element.names[-1]),
                                         choiceValues = as.list(elements[-1]),
                                         selected = elements[4]),
                            
                            radioButtons("seasonsOrMonths2", "Months or Seasons",
                                         choiceNames = c("Months", "Seasons"),
                                         choiceValues = c("Months", "Seasons"),
                                         selected = "Seasons",
                                         inline = T),
                            
                            conditionalPanel(
                              condition = "input.seasonsOrMonths2 == 'Seasons'",
                              
                              radioGroupButtons(
                                inputId = "seasonbuttons2",
                                label = "Choose a season",
                                choices = season.names, 
                                selected = season.names[3]
                              ),
                              
                              conditionalPanel(
                                condition = "input.areaMap == 'North America'",
                                
                                sliderTextInput("proj.year.map2", 
                                                label = "Choose a time slice", 
                                                choices = proj.year.names, 
                                                selected = proj.year.names[3]),
                                
                                sliderTextInput("scenario.map2", 
                                                label = "Choose emissions scenario", 
                                                choices = scenario.names[-1], 
                                                selected = scenario.names[3]),
                                
                              ),
                              
                              conditionalPanel(
                                condition = "input.areaMap == 'Pacific Northwest'",
                                
                                radioButtons("proj.year.map.fixed1", inline = TRUE,
                                             label = "time slice",
                                             choiceNames = proj.year.names[3],
                                             choiceValues = proj.years[3],
                                             selected = proj.years[3]),
                                
                                radioButtons("scenario.map.fixed1", "emissions scenario",
                                             choiceNames = scenario.names[3],
                                             choiceValues = scenarios[3],
                                             selected = scenarios[3],
                                             inline = T)
                              ),
                            ),
                            
                            conditionalPanel(
                              condition = "input.seasonsOrMonths2 == 'Months'",
                              
                              sliderTextInput("monthslider2", 
                                              label = "Choose a month", 
                                              choices = month.abb, 
                                              selected = month.abb[7]),
                              
                              radioButtons("proj.year.map.fixed2", inline = TRUE,
                                           label = "time slice",
                                           choiceNames = proj.year.names[3],
                                           choiceValues = proj.years[3],
                                           selected = proj.years[3]),
                              
                              radioButtons("scenario.map.fixed2", "emissions scenario",
                                           choiceNames = scenario.names[3],
                                           choiceValues = scenarios[3],
                                           selected = scenarios[3],
                                           inline = T)
                              
                            )
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
             
             # ## -----------------------------------------------------
             # ## RESOLUTION
             # 
             # tabPanel("Resolution",
             #          
             #          includeMarkdown("resolution.Rmd"),
             #          
             #          img(src = "ModelRes.png", height = 4500/11, width = 12600/11),
             #          
             #          column(width = 12,
             #                 style = "background-color:#003366; border-top:2px solid #fcba19;",
             #                 
             #                 tags$footer(class="footer",
             #                             tags$div(class="container", style="display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
             #                                      tags$ul(style="display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home", "Home", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")),
             #                                              tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style="font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;"))
             #                                      )
             #                             )
             #                 )
             #          )
             # ),
             # 
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
  
  # observeEvent(input$link_to_Bias, {
  #   updateNavbarPage(session, "CMIP6-BC", selected="Assess bias")
  # })
  # 
  observeEvent(input$link_to_Maps, {
    updateNavbarPage(session, "CMIP6-BC", selected="Maps")
  })
  
  observeEvent(input$link_to_Guidance, {
    updateNavbarPage(session, "CMIP6-BC", selected="Guidance")
  })
  
  # This is the gcm selection for the time series plot. done as a renderUI to allow the reset button
  output$reset_gcms <- renderUI({
    times <- input$reset_input
    div(id=letters[(times %% length(letters)) + 1],
        checkboxGroupInput("gcms.ts2", "Choose global climate models:",
                           choiceNames = gcm.names,
                           choiceValues = gcms,
                           selected = if(input$default.ensemble=="13-model") gcms[select] else gcms[select8] ,
                           inline = T
        )
    )
  })
  
  timeSeriesPlot <- function() {
    
    # test specificationS
    observations <- "pcic"
    ecoprov <- ecoprovs[1]
    yeartime1 <- yeartimes[3]
    yeartime2 <- yeartimes[1]
    element1 <- elements[1]
    element2 <- elements[2]
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    gcms.ts <- gcms[select8]
    gcms.compare <- NA
    scenarios1 <- c(scenarios[1:4])
    nums <- c(1)
    biascorrect <- T
    showrange <- T
    yfit <- T
    cex <- 1
    compare.ensemble <- "None"
    showmean <- T
    compile <- T
    simplify <- T
    refline <- T
    yearlines <- T
    mode <- "Ensemble"
    gcms.ts1 <- gcms[4]
    gcms.ts2 <- gcms[select8]
    
    # user specificationS
    observations <- input$observations
    ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name)]
    yeartime1 <- yeartimes[which(yeartime.names==input$yeartime1)]
    yeartime2 <- if(input$compare==T) yeartimes[which(yeartime.names==input$yeartime2)] else yeartimes[which(yeartime.names==input$yeartime1)]
    element1 <- elements[which(element.names==input$element1)]
    element2 <- if(input$compare==T) elements[which(element.names==input$element2)] else elements[which(element.names==input$element1)]
    variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
    variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
    gcms.ts <- if(input$mode=="Ensemble") input$gcms.ts2 else input$gcms.ts1
    gcms.compare <- if(input$compare.ensemble=="13-model") gcms[select] else if(input$compare.ensemble=="8-model") gcms[select8] else NA
    scenarios1 <- c("historical", input$scenarios1)
    nums <- if(input$compare==T) c(1,2) else c(1)
    biascorrect <- input$biascorrect
    showrange <- input$showrange
    yfit <- input$yfit
    cex <- input$cex
    compare.ensemble <- input$compare.ensemble
    showmean <- input$showmean
    compile <- input$compile
    simplify <- input$simplify
    refline <- input$refline
    yearlines <- input$yearlines
    mode <- input$mode
    gcms.ts1 <- input$gcms.ts1
    gcms.ts2 <- input$gcms.ts2
    
    ## Assemble the data that will be used in the plot
    alldata <- vector() # a vector of all data on the plot for setting the ylim (y axis range)
    visibledata <- vector() # a vector of all visible data on the plot for setting the ylim (y axis range)
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
      alldata <- c(alldata, y1) #store values in a big vector for maintaining a constant ylim
      visibledata <- c(visibledata, y1) #store values in a big vector for maintaining a constant ylim
      
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
          if(biascorrect==T){
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
          if(is.na(gcms.compare)!=T) temp$compare <- apply(temp[,which(names(temp)%in%gcms.compare)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
          assign(paste(ensstat, scenario, num, sep="."), temp)
          if(showrange==T | ensstat==ensstats[3]) visibledata <- c(visibledata, temp$compile, if(is.na(gcms.compare)!=T) temp$compare ) #store values in a big vector for maintaining a constant ylim
        }
      }
    }
    
    # PLOT
    par(mfrow=c(1,1), mar=c(3,3,0.1,3), mgp=c(1.75, 0.25, 0), cex=1.5*cex)
    if(element1==element2){
      ylab <- element.names.units[[which(elements==element1)]]
    } else {
      ylab <- if("PPT"%in%c(element1, element2)) bquote(Precipitation~"("*mm*")"~or~Mean ~ temperature ~ "(" * degree * C * ")") else element.names.units[[1]]
    }
    plot(0, col="white", xlim=c(1900, 2100), ylim=range(if(yfit==T) visibledata else alldata, na.rm = T), xaxs="i", xaxt="n", tck=0, xlab="", ylab=ylab)
    axis(1, at=seq(1850,2100,25), labels = seq(1850,2100,25), tck=0)
    
    num <- 1
    for(num in nums){
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      # data for observations
      x1 <- unique(obs.ts.mean[,1])
      y1 <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      baseline.obs <- mean(y1[which(x1%in%1961:1990)])
      recent.climatebc <- mean(y1[which(x1%in%2011:2021)])
      
      # data for era5
      if("era5"%in%observations){
        era5.ts.mean <- read.csv(paste("data/ts.era5land.mean.", ecoprov, ".csv", sep=""))
        x2 <- unique(era5.ts.mean[,1])
        y2 <- era5.ts.mean[,which(names(era5.ts.mean)==variable)]
        baseline.era5 <- mean(y2[which(x2%in%1961:1990)])
        bias.era5 <- baseline.obs - baseline.era5
        if(biascorrect==T) y2 <- y2+bias.era5
        recent.era5 <- mean(y2[which(x2%in%2012:2021)], na.rm=T)
      }
      
      # data for pcic
      if("pcic"%in%observations){
        pcic.ts.mean <- read.csv(paste("data/ts.pcic.mean.", ecoprov, ".csv", sep=""))
      x3 <- unique(pcic.ts.mean[,1])
      y3 <- if(element=="PPT") pcic.ts.mean[,which(names(pcic.ts.mean)==variable)]*mean(y1[which(x1%in%1981:2010)]) + mean(y1[which(x1%in%1981:2010)]) else pcic.ts.mean[,which(names(pcic.ts.mean)==variable)] + mean(y1[which(x1%in%1981:2010)])  # apply faron's anomalies to the 1981-2010 absolute value of climatebc time series. 
      baseline.pcic <- mean(y3[which(x3%in%1961:1990)])
      y3 <- if(element=="PPT") y3*(baseline.obs/baseline.pcic) else y3+(baseline.obs-baseline.pcic)   # bias correct to 1961-1990 period
      recent.pcic <- mean(y3[which(x3%in%2012:2021)], na.rm=T)
      }
      
      # time series for the comparison ensemble
      colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")
      if(compare.ensemble!="None"){
        # scenario <- scenarios1[1]
        for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
          
          for(ensstat in ensstats){
            temp <- get(paste(ensstat, scenario, num, sep="."))
            x <- temp[,1]
            temp <- temp$compare
            assign(ensstat, temp)
            assign(paste("x", scenario, sep="."), x)
            assign(paste(ensstat, scenario, sep="."), temp)
          }
          
          if(showrange==T) polygon(c(x, rev(x)), c(ensmin, rev(ensmax)), col=alpha(colScheme[which(scenarios==scenario)], 0.25), border=colScheme[which(scenarios==scenario)], lty=2)
          
          if(scenario != "historical"){
            par(xpd=T)
            baseline <- mean(ensmean.historical[111:140])
            projected <- mean(ensmean[(length(x)-5):(length(x))])
            if(element=="PPT"){
              change <- round(projected/baseline-1,2)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=1, cex=0.8)
            } else {
              change <- round(projected-baseline,1)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=1, cex=0.8)
            }
            par(xpd=F)
          }
          
          print(scenario)
        }
        
        # overlay the ensemble mean lines on top of all polygons
        for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
          if(showmean==T) lines(get(paste("x", scenario, sep=".")), get(paste("ensmean", scenario, sep=".")), col=colScheme[which(scenarios==scenario)], lwd=2, lty=2)
        }
      }
      
      # time series for selected ensemble
      if(compile==T) gcms.ts <- "compile" #this prevents the plotting of individual GCM projections and plots a single envelope for the ensemble as a whole. 
      gcm <- gcms.ts[1]
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
          
          if(scenario != "historical"){
            par(xpd=T)
            baseline <- mean(ensmean.historical[111:140])
            projected <- mean(ensmean[(length(x)-5):(length(x))])
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
        
        if(input$showrange==T) {
          if(input$simplify==F){
            for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
              x <- get(paste("x", scenario, sep="."))
              polygon(c(x, rev(x)), c(get(paste("ensmin", scenario, sep=".")), rev(get(paste("ensmax", scenario, sep=".")))), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=colScheme[which(scenarios==scenario)])
            }
          } else {
            for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])][-1]){
              if(scenario==scenarios1[2]){ # need to run spline through the historical/projected transition
                x4 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin <- smooth.spline(x4,y.ensmin, df=8) 
                s.ensmax <- smooth.spline(x4,y.ensmax, df=8) 
                subset.hist <- which(x4%in%x.historical)
                subset.proj <- which(x4%in%get(paste("x", scenario, sep=".")))
                polygon(c(s.ensmin$x[subset.hist], rev(s.ensmax$x[subset.hist])), c(s.ensmin$y[subset.hist], rev(s.ensmax$y[subset.hist])), col=alpha(colScheme[which(scenarios=="historical")], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
                polygon(c(s.ensmin$x[subset.proj], rev(s.ensmax$x[subset.proj])), c(s.ensmin$y[subset.proj], rev(s.ensmax$y[subset.proj])), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
              } else { # this second routine uses interpolation splines so that the starting point for all scenarios is the same
                x5 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin2 <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax2 <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin2 <- smooth.spline(x5,y.ensmin2, df=8) 
                s.ensmax2 <- smooth.spline(x5,y.ensmax2, df=8) 
                knots.hist <- c(1, 20, 40, 80, 100, 120, 140, 165)
                knots.proj <- c(190, 210, 230, 250, length(x5))
                s.ensmin3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmin$y[knots.hist], s.ensmin2$y[knots.proj]), x5)
                s.ensmax3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmax$y[knots.hist], s.ensmax2$y[knots.proj]), x5)
                polygon(c(s.ensmin3$x[subset.proj], rev(s.ensmax3$x[subset.proj])), c(s.ensmin3$y[subset.proj], rev(s.ensmax3$y[subset.proj])), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
              }
            }
          }
        }

        if(refline==T){
          ref.temp <- mean(ensmean.historical[111:140])
          lines(1961:1990, rep(ref.temp, 30), lwd=2)
          lines(c(1990,2100), rep(ref.temp, 2), lty=2)
        }
        
        # overlay the ensemble mean lines on top of all polygons
        if(showmean==T){
          for(scenario in scenarios1[order(c(1,4,5,3,2)[which(scenarios%in%scenarios1)])]){
            if(simplify==F) lines(x=get(paste("x", scenario, sep=".")), y=get(paste("ensmean", scenario, sep=".")), col=colScheme[which(scenarios==scenario)], lwd=2)
            if(simplify==T){
              if(scenario=="historical"){ # need to run spline through the historical/projected transition
                x4 <- c(x.historical, get(paste("x", scenarios1[2], sep=".")))
                y4 <- c(ensmean.historical, get(paste("ensmean", scenarios1[2], sep=".")))
              } else {
                x4 <- c(x.historical, get(paste("x", scenario, sep=".")))
                y4 <- c(ensmean.historical, get(paste("ensmean", scenario, sep=".")))
              }
              s4 <- smooth.spline(x4,y4, df=10) 
              subset <- which(x4%in%get(paste("x", scenario, sep=".")))
              lines(x=s4$x[subset], y=s4$y[subset], col=colScheme[which(scenarios==scenario)], lwd=2)
            }
          }
        }
        print(gcm)
      }
      
      # overlay the 5-year lines on top of all polygons
      if(yearlines==T){
        for(n in seq(1905, 2095, 5)){
          lines(c(n, n), c(-9999, 9999), col="grey", lty=2)
        }
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
      
      # add in PCIC observations
      pcic.color <- "blue"
      if("pcic"%in%observations){
        end <- max(which(!is.na(y3)))
        lines(x3[which(x3<1951)], y3[which(x3<1951)], lwd=3, lty=3, col=pcic.color)
        lines(x3[which(x3>1949)], y3[which(x3>1949)], lwd=3, col=pcic.color)
        points(x3[end], y3[end], pch=16, cex=1, col=pcic.color)
        text(x3[end], y3[end], x3[end], pos= 4, offset = 0.25, col=pcic.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.pcic/baseline.obs-1,2)
          # text(2021,recent.pcic, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=pcic.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.pcic-baseline.obs,1)
          # text(2021,recent.pcic, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=pcic.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=pcic.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=pcic.color)
        # lines(c(2012,2021), rep(recent.pcic, 2), lty=2, col=pcic.color)
      }
      
      # add in ClimateBC observations
      obs.color <- "black"
        if("climatebc"%in%observations){
          lines(x1[which(x1<1951)], y1[which(x1<1951)], lwd=1.5, lty=3, col=obs.color)
          lines(x1[which(x1>1949)], y1[which(x1>1949)], lwd=1.5, col=obs.color)
          if(!("pcic"%in%observations)){
            if(element=="PPT"){
            change <- round(recent.climatebc/baseline.obs-1,2)
            text(2019,recent.climatebc, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=obs.color, pos=4, font=2, cex=1)
          } else {
            change <- round(recent.climatebc-baseline.obs,1)
            text(2019,recent.climatebc, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=obs.color, pos=4, font=2, cex=1)
          }
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=obs.color)
        lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=obs.color)
        lines(c(2012,2021), rep(recent.climatebc, 2), lty=2, col=obs.color)
      }
      
      # add in era5 observations
      era5.color <- "darkorange"
      if("era5"%in%observations){
        end <- max(which(!is.na(y2)))
        lines(x2, y2, col=era5.color, lwd=2)
        points(x2[end], y2[end], pch=16, cex=1, col=era5.color)
        text(x2[end], y2[end], x2[end], pos= 4, offset = 0.25, col=era5.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.era5/baseline.obs-1,2)
          # text(2021,recent.era5, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=era5.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.era5-baseline.obs,1)
          # text(2021,recent.era5, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=era5.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=era5.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=era5.color)
        # lines(c(2012,2021), rep(recent.era5, 2), lty=2, col=era5.color)
      }
      
      #legend
      a <- if("pcic"%in%observations) 1 else NA
      b <- if("climatebc"%in%observations) 2 else NA
      c <- if("era5"%in%observations) 3 else NA
      d <- if(length(gcms.ts>0)) 4 else NA
      e <- if(compare.ensemble!="None") 5 else NA
      s <- !is.na(c(a,b,c,d,e))
      legend.GCM <- if(mode=="Ensemble") paste("Simulated (", length(gcms.ts2), " GCMs)", sep="")  else paste("Simulated (", gcms.ts1, ")", sep="")
      legend.compare <- paste("Simulated (", length(gcms.compare), " GCMs)", sep="")  
      legend("topleft", title = "", legend=c("Observed (PCIC)", "Observed (ClimateNA)", "ERA5 reanalysis", legend.GCM, legend.compare)[s], bty="n",
             lty=c(1,1,1,1 ,2)[s], 
             col=c(pcic.color, obs.color, era5.color, "gray", "gray")[s], 
             lwd=c(3,1.5,2, 2 ,2)[s], 
             pch=c(NA,NA,NA, NA , NA)[s], 
             pt.bg = c(NA, NA,NA, NA , NA)[s], 
             pt.cex=c(NA,NA,NA,NA ,NA)[s])
      
      s <- rev(which(scenarios[-1]%in%scenarios1))
      legend("top", title = "Scenarios", legend=c("Historical", scenario.names[-1][s]), bty="n",
             lty=c(NA,NA,NA,NA,NA)[c(1,s+1)], col=colScheme[c(1,s+1)], lwd=c(NA,NA,NA,NA,NA)[c(1,s+1)], pch=c(22, 22,22,22,22)[c(1,s+1)], pt.bg = alpha(colScheme[c(1,s+1)], 0.35), pt.cex=c(2,2,2,2,2)[c(1,s+1)])
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      mtext(paste(" Created using https://bcgov-env.shinyapps.io/cmip6-BC\n", if("pcic"%in%observations) "Observed anomalies calculated by Faron Anslow, Pacific Climate Impacts Consortium\n"  , "Contact: Colin Mahony colin.mahony@gov.bc.ca"), side=1, line=-1.35, adj=0.0, font=1, cex=1.1, col="gray")
      
      print(num)
    }
    box()
  }
  output$timeSeries <- renderPlot({ timeSeriesPlot() },
                                  height=reactive(ifelse(!is.null(input$innerWidth),input$innerWidth*0.425,0))
  )
  
  # Plot download
  output$downloadPlot <- downloadHandler(
    filename =  function(){
      paste("cmip6BC", ecoprovs[which(ecoprov.names==input$ecoprov.name)], 
            elements[which(element.names==input$element1)], 
            if(input$compare==T){
              paste(yeartimes[which(yeartime.names==input$yeartime1)], 
                    elements[which(element.names==input$element2)] ,
                    yeartimes[which(yeartime.names==input$yeartime2)], sep=".")
            } else {yeartimes[which(yeartime.names==input$yeartime1)]},
            "png", sep=".")
    },
    
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
    data.runs <- read.csv(paste("data/change.runs", ecoprov, "csv", sep="."))
    
    x <- data[, which(names(data)==variable1)]
    y <- data[, which(names(data)==variable2)]
    x0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable1)]
    y0 <- data[which(data$proj.year==2010 & data$gcm=="obs"), which(names(data)==variable2)]
    x.mean <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms.change), which(names(data)==variable1)])
    y.mean <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcms.change), which(names(data)==variable2)])
    x.mean.ClimateBC <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcm.names[select]), which(names(data)==variable1)])
    y.mean.ClimateBC <- mean(data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm%in%gcm.names[select]), which(names(data)==variable2)])

    xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
    ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
    
    #initiate the plot
    fig <- plot_ly(x=x,y=y, type = 'scatter', mode = 'markers', marker = list(color ="lightgrey", size=5), hoverinfo="none", color="All models/scenarios/times")
    
    fig <- fig %>% layout(xaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable1)]), 
                                       range=xlim), 
                          yaxis = list(title=paste("Change in", variable.names$Variable[which(variable.names$Code==variable2)]),
                                       range=ylim)
    )
    
    fig <- fig %>% add_markers(x=x0,y=y0, color="observed (2001-2020)", text="observed\n(2001-2020)", hoverinfo="text",
                               marker = list(size = 25,
                                             color = "grey"))
    
    fig <- fig %>% add_markers(x=x.mean,y=y.mean, color="Custom ensemble mean", text="Custom ensemble mean", hoverinfo="text",
                               marker = list(size = 20,
                                             color = "grey", symbol = 3))
    
    fig <- fig %>% add_markers(x=x.mean.ClimateBC,y=y.mean.ClimateBC, color="ClimateBC 13-model mean", text="ClimateBC 13-model mean", hoverinfo="text",
                               marker = list(size = 20,
                                             color = "black", symbol = 103))
    
    gcm=gcms.change[3]
    for(gcm in gcms.change){
      i=which(gcm.names==gcm)
      x1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable1)]
      y1 <- data[which(data$scenario==scenario & data$proj.year==proj.year & data$gcm==gcm), which(names(data)==variable2)]
      x2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable1)]
      y2 <- data[c(1, which(data$scenario==scenario & data$gcm==gcm)), which(names(data)==variable2)]
      
      #data to produce convex hull of individual runs
      x.runs <- data.runs[which(data.runs$scenario==scenario & data.runs$proj.year==proj.year & data.runs$gcm==gcm), which(names(data.runs)==variable1)]
      y.runs <- data.runs[which(data.runs$scenario==scenario & data.runs$proj.year==proj.year & data.runs$gcm==gcm), which(names(data.runs)==variable2)]
      runs <- data.runs$run[which(data.runs$scenario==scenario & data.runs$proj.year==proj.year & data.runs$gcm==gcm)]
      
      if(input$trajectories==T){
        if(length(unique(sign(diff(x2))))==1){
          x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
          y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
          s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(xlim)/1500)) # way better than interpSpline, not prone to oscillations
          fig <- fig %>% add_trace(x=s$x, y=s$y, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i], width = 2, dash = 'dash'), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
          limit <- if(unique(sign(diff(x2)))==-1) which(s$x>x1) else which(s$x<x1)
          fig <- fig %>% add_trace(x=s$x[limit], y=s$y[limit], type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        } else {
          fig <- fig %>% add_trace(x=x2, y=y2, type = 'scatter', mode = 'lines', line = list(color=ColScheme[i], width = 2, dash = 'dash'), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
          limit <- c(1, (which(proj.years <= proj.year)+1))
          fig <- fig %>% add_trace(x=x2[limit], y=y2[limit], type = 'scatter', mode = 'lines', line = list(color=ColScheme[i]), marker=NULL, legendgroup=paste("group", i, sep=""), showlegend = FALSE)
        }
        fig <- fig %>% add_markers(x=x2,y=y2, color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
                                   marker = list(size = 8,
                                                 color = ColScheme[i]),
                                   legendgroup=paste("group", i, sep=""), showlegend = FALSE)
      }
      
      if(input$runs==T){
        fig <- fig %>% add_markers(x=x.runs,y=y.runs, text=paste(gcm.names[i], runs), text="runs", hoverinfo="text", showlegend = F,
                                 marker = list(size = 7,
                                               color = ColScheme[i],
                                               line = list(color = "black",
                                                           width = 1)))
      }
      
      fig <- fig %>% add_markers(x=x1,y=y1, color=gcm.names[i],
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
  
  # output$BiasPlot <- renderPlotly({
  #   
  #   # ecoprov <- ecoprovs[1]
  #   # yeartime1 <- yeartimes[1]
  #   # yeartime2 <- yeartimes[3]
  #   # element1 <- elements[4]
  #   # element2 <- elements[4]
  #   # xfun <- 1
  #   # yfun <- 1
  #   # showruns <- T
  #   # ClimateBC <- T
  #   
  #   ecoprov <- ecoprovs[which(ecoprov.names==input$ecoprov.name.II)]
  #   yeartime1 <- yeartimes[which(yeartime.names==input$yeartime3)]
  #   yeartime2 <- yeartimes[which(yeartime.names==input$yeartime4)] 
  #   element1 <- elements[which(element.names==input$element3)]
  #   element2 <- elements[which(element.names==input$element4)] 
  #   xfun <- 1
  #   yfun <- 1
  #   showruns <- input$showruns
  #   ClimateBC <- input$ClimateBC
  #   equalscale <- input$equalscale
  #   
  #   variable1 <- paste(element1, yeartime1, sep= if(yeartime1%in%seasons) "_" else "")
  #   variable2 <- paste(element2, yeartime2, sep= if(yeartime2%in%seasons) "_" else "")
  #   
  #   data.mean <- read.csv(paste("data/summary.mean", ecoprov, "csv", sep="."), stringsAsFactors = F)
  #   # data.sd <- read.csv(paste("data/summary.sd", ecoprov, "csv", sep="."), stringsAsFactors = F)
  #   # data.sd[,grep("PPT", names(data.bias))] <- data.sd[,grep("PPT", names(data.bias))]/data.mean[,grep("PPT", names(data.bias))] # convert sd to coefficient of variation for precipitation
  #   # data.sd[,-c(1:2)] <- sweep(data.sd[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.sd[1,-c(1:2)])), "/")-1 # express sd relative to observational
  #   data.bias <- data.mean
  #   data.bias[,-c(1:2)] <- sweep(data.mean[,-c(1:2)], MARGIN=2, STATS=as.vector(unlist(data.mean[1,-c(1:2)])), "-")
  #   data.bias[,grep("PPT", names(data.bias))] <- sweep(data.mean[,grep("PPT", names(data.mean))], MARGIN=2, STATS=as.vector(unlist(data.mean[1,grep("PPT", names(data.mean))])), "/")-1 #express bias as relative for precipitation
  #   
  #   x <- get(paste("data", funs[xfun], sep="."))[, which(names(data.mean)==variable1)]
  #   y <- get(paste("data", funs[yfun], sep="."))[, which(names(data.mean)==variable2)]
  #   
  #   # xlim=if(variable.type1=="ratio") range(x) else if(min(x)<0) range(x) else c(0, max(x))
  #   # ylim=if(variable.type2=="ratio") range(y) else if(min(y)<0) range(y) else c(0, max(y))
  #   xlim=range(x)*c(if(min(x)<0) 1.1 else 0.9, if(max(x)>0) 1.1 else 0.9)
  #   ylim=range(y)*c(if(min(y)<0) 1.1 else 0.9, if(max(y)>0) 1.1 else 0.9)
  #   
  #   # fig <- plot_ly(x=x,y=y, type = 'scatter', mode = 'markers', marker = list(color ="white"), hoverinfo="none")
  #   fig <- plot_ly(x=x,y=y, type = NULL)
  #   
  #   fig <- fig %>% layout(xaxis = list(title=paste(fun.names[xfun], variable.names$Variable[which(variable.names$Code==variable1)]), 
  #                                      range=xlim), 
  #                         yaxis = list(title=paste(fun.names[yfun], variable.names$Variable[which(variable.names$Code==variable2)]),
  #                                      range=ylim)
  #   )
  #   
  #   gcms.bias <- if(ClimateBC==T) unique(data.mean$gcm[!is.na(data.mean$run)])[select] else unique(data.mean$gcm[!is.na(data.mean$run)])
  #   mods <- substr(gcm.names, 1, 2)
  #   
  #   for(gcm in gcms.bias){
  #     i=which(gcm.names==gcm)
  #     j=which(gcms.bias==gcm)
  #     s=which(data.mean$gcm==gcm)
  #     
  #     if(showruns==T){
  #       fig <- fig %>% add_markers(x=x[s],y=y[s], color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
  #                                  marker = list(size = 8,
  #                                                color = ColScheme[i]),
  #                                  legendgroup=paste("group", i, sep=""), showlegend = FALSE)
  #     }
  #     
  #     fig <- fig %>% add_markers(x=mean(x[s]),y=mean(y[s]), color=gcm.names[i], text=gcm.names[i], hoverinfo="text",
  #                                marker = list(size = 20,
  #                                              color = ColScheme[i],
  #                                              line = list(color = "black",
  #                                                          width = 1)),
  #                                legendgroup=paste("group", i, sep=""))
  #     
  #     fig <- fig %>% add_annotations(x=mean(x[s]),y=mean(y[s]), text = sprintf("<b>%s</b>", mods[i]), xanchor = 'center', yanchor = 'center', showarrow = F,
  #                                    legendgroup=paste("group", i, sep="")    )
  #     
  #     
  #   }
  #   
  #   if(equalscale==T) fig <- fig %>% layout(yaxis=list(scaleanchor="x", scaleratio=1))
  #   
  #   if(element1=="PPT") fig <- fig %>% layout(xaxis = list(tickformat = "%"))
  #   if(element2=="PPT") fig <- fig %>% layout(yaxis = list(tickformat = "%"))
  #   
  #   fig
  #   
  # }
  # )
  

  output$changeMap <- renderImage({
    
    if(input$mapType=="Topography"){
      if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("Orography.PNW.png",sep=".")))
      } else {filename <- normalizePath(file.path('./www', paste("Orography.NorthAmerica.png",sep=".")))}
    }
    
    if(input$mapType=="Climate change"){
      yeartimeMap <- if(input$seasonsOrMonths2=="Seasons") seasons[which(season.names==input$seasonbuttons2)] else monthcodes[which(month.abb==input$monthslider2)]
      
      if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("changeMap", input$elementMap2, yeartimeMap, "png",sep=".")))
      } else { if(input$seasonsOrMonths2 == "Seasons"){
        filename <- normalizePath(file.path('./www', paste("changeMap.NorAm", input$elementMap2, yeartimeMap, scenarios[which(scenario.names==input$scenario.map2)], c(2001, 2021, 2041, 2061, 2081)[which(proj.year.names==input$proj.year.map2)] , "png",sep=".")))
      } else {
        filename <- normalizePath(file.path('./www', paste("changeMap.NorAm", input$elementMap2, yeartimeMap, "png",sep=".")))
      }
      }
    }
    
    if(input$mapType=="Bias"){
      yeartimeMap <- if(input$seasonsOrMonths=="Seasons") seasons[which(season.names==input$seasonbuttons)] else monthcodes[which(month.abb==input$monthslider)]
      
      if(input$areaMap=="Pacific Northwest"){
        filename <- normalizePath(file.path('./www', paste("biasMap", input$elementMap, yeartimeMap, "png",sep=".")))
      } else {filename <- normalizePath(file.path('./www', paste("biasMap.NorAm", input$elementMap, yeartimeMap, "png",sep=".")))}
    }
    
    list(src = filename, width="100%", height="100%")
    
  }, deleteFile = FALSE)
  
  output$table <- DT::renderDataTable({
    DT::datatable(modelMetadata, 
                  options = list(pageLength = dim(modelMetadata)[1]), 
                  rownames= FALSE, 
                  caption = HTML("<p><h4><b>Information about models featured in this app.</b> 
                                 ECS is equilibrium climate sensitivity (long-term temperature change in response to an instant doubling of CO2), and values are quoted from <a href='https://advances.sciencemag.org/content/6/26/eaba1981.abstract' target='_blank'>Meehl et al. (2020)</a>. 
                                 The last five columns are the number of model runs for each scenario that are included in ClimateBC/NA and this app</p></h4>")
    )
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)






