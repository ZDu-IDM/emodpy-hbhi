library(shiny)

#Define UI
#ui <- fluidPage(theme = shinytheme("lumen"),
createTabs <- function(barplot_start_year, barplot_end_year) {
  
   appendTab("tabs", 
    #  =======  barplots showing burden relative to BAU  =======  #
    tabPanel("Burden relative to BAU",
             #titlePanel(paste0("Burundi: comparison of malaria-control packages with business as usual (BAU) between ", barplot_start_year, " and ", barplot_end_year)),
             sidebarLayout(
               sidebarPanel(
                 class="sidePanel",
                 
                 h4('Health Districts plotted'),
                 # Select which health districts should be included in plot
                 selectInput(inputId = "district_subset_name0", label = strong("Select the set of health districts to include in plots"),
                             choices = c('all health districts', 'only IRS districts', 'only districts receiving PBO/IG2 in GF plan','only districts that receive community LLIN distribution'),
                             selected = "all health districts"),
                 
                 tags$br(),
                 h4('Simulation assumptions'),
                 # Select CHW coverage
                 selectInput(inputId = "chw_cov0", label = strong("CHW coverage level"),
                             choices = c('higher','lower'),
                             selected = "higher"),
                 
                 # Select how long bednets are retained
                 selectInput(inputId = "itn_halflife0", label = strong("Bednet retention half-life (years)"),
                             choices = c('1.31','2'),
                             selected = "1.31"),
                 
                 # Select level of insecticide resistance
                 selectInput(inputId = "pyr0", label = strong("Permethrin bioassay mortality"),
                             choices = c('64','40'),
                             selected = "64"),
                 
                 # Select LLIN-efficacy assumption (converting bioassay to hut mortality)
                 selectInput(inputId = "seed_subset0", label = strong("Impact of resistance on LLIN efficacy"),
                             choices = c('all', 'more sensitive', 'less sensitive'),
                             selected = "all"),
                 
               ),
               
               mainPanel(
                 class="mainPanel",
                 h3(paste0("Burundi: comparison of malaria-control packages with business as usual (BAU) between ", barplot_start_year, " and ", barplot_end_year)),
                 plotOutput(outputId = "barplot_allBurdenMetrics", height = "600px"),
               )
             ),

             select = TRUE
    ))

    #  =======  timeseries of all burden metrics  =======  #
    appendTab("tabs",
    tabPanel("Timeseries of burden (all metrics)",
             #titlePanel("Burundi: comparison of malaria-control packages"),
             sidebarLayout(
               sidebarPanel(
                 class="sidePanel",
                 h4('Health Districts plotted'),
                 # Select which health districts should be included in plot
                 selectInput(inputId = "district_subset_name1", label = strong("Select the set of health districts to include in plots"),
                             choices = c('all health districts', 'only IRS districts', 'only districts receiving PBO/IG2 in GF plan', 'only districts that receive community LLIN distribution'),
                             selected = "all health districts"),
                 
                 tags$br(),
                 h4('Simulation assumptions'),
                 # Select CHW coverage
                 selectInput(inputId = "chw_cov1", label = strong("CHW coverage level"),
                             choices = c('higher','lower'),
                             selected = "higher"),
                 
                 # Select how long bednets are retained
                 selectInput(inputId = "itn_halflife1", label = strong("Bednet retention half-life (years)"),
                             choices = c('1.31','2'),
                             selected = "1.31"),
                 
                 # Select level of insecticide resistance
                 selectInput(inputId = "pyr1", label = strong("Permethrin bioassay mortality"),
                             choices = c('64','40'),
                             selected = "64"),
                 
                 # Select LLIN-efficacy assumption (converting bioassay to hut mortality)
                 selectInput(inputId = "seed_subset1", label = strong("Impact of resistance on LLIN efficacy"),
                             choices = c('all', 'more sensitive', 'less sensitive'),
                             selected = "all"),
                 
               ),
               
               mainPanel(
                 class="mainPanel",
                 h3("Burundi: comparison of malaria-control packages"),
                 plotOutput(outputId = "timeseriesplot_allBurdenMetrics", height = "600px"),
               )
             )
    ))

    #  =======  timeseries of burden metric and intervention coverage   =======  #
    appendTab("tabs", tabPanel("Timeseries of burden and interventions", 
             #titlePanel("Burundi: comparison of malaria-control packages"),
             sidebarLayout(
               sidebarPanel(
                 class="sidePanel",
                 h4('Malaria burden indicator'),
                 fluidRow(
                   column(6,style=list("padding-right: 5px;"),
                          # Select burden indicator
                          selectInput(inputId = "burden_metric", label = strong("Select metric to be plotted"),
                                      choices = c('PfPR', 'incidence', 'mortality'),
                                      selected = "PfPR"),
                   ),
                   column(6,style=list("padding-left: 5px;"),
                          # Select burden indicator
                          selectInput(inputId = "age_plotted", label = strong("Select age group to be plotted"),
                                      choices = c('U5', 'all'),
                                      selected = "U5"),
                   )
                 ),
                 
                 tags$br(),
                 h4('Health Districts plotted'),
                 # Select which health districts should be included in plot
                 selectInput(inputId = "district_subset_name", label = strong("Select the set of health districts to include in plots"),
                             choices = c('all health districts', 'only IRS districts', 'only districts receiving PBO/IG2 in GF plan','only districts that receive community LLIN distribution'),
                             selected = "all health districts"),
                 
                 tags$br(),
                 h4('Simulation assumptions'),
                 # Select CHW coverage
                 selectInput(inputId = "chw_cov", label = strong("CHW coverage level"),
                             choices = c('higher','lower'),
                             selected = "higher"),
                 
                 # Select how long bednets are retained
                 selectInput(inputId = "itn_halflife", label = strong("Bednet retention half-life (years)"),
                             choices = c('1.31','2'),
                             selected = "1.31"),
                 
                 # Select level of insecticide resistance
                 selectInput(inputId = "pyr", label = strong("Permethrin bioassay mortality"),
                             choices = c('64','40'),
                             selected = "64"),
                 
                 # Select LLIN-efficacy assumption (converting bioassay to hut mortality)
                 selectInput(inputId = "seed_subset", label = strong("Impact of resistance on LLIN efficacy"),
                             choices = c('all', 'more sensitive', 'less sensitive'),
                             selected = "all"),
                 
                 tags$br(),
                 h4('Time resolution for plot'),
                 # Select yearly or monthly output
                 radioButtons(inputId = "time_res", label = strong("Set whether to plot annual or monthly averages"),
                              choices = c('annual average','monthly average'),
                              selected = "annual average"),
               ),
               
               mainPanel(
                 class="mainPanel",
                 h3("Burundi: comparison of malaria-control packages"),
                 plotOutput(outputId = "timeseriesplot_burdenIntervention", height = "800px"),
               )
             )
    ))

    #  =======  barplot and maps of IPTi impact   =======  #
    
    appendTab("tabs", tabPanel("Impact of IPTi",
             #titlePanel(paste0("Burundi: comparison of U1 burden with and without IPTi between ", barplot_start_year, " and ", barplot_end_year)),
             sidebarLayout(
               sidebarPanel(
                 class="sidePanel",
                 h4('Simulation assumptions'),
                 # Select CHW coverage
                 selectInput(inputId = "chw_cov3", label = strong("CHW coverage level"),
                             choices = c('higher','lower'),
                             selected = "higher"),
                 
                 # Select how long bednets are retained
                 selectInput(inputId = "itn_halflife3", label = strong("Bednet retention half-life (years)"),
                             choices = c('1.31','2'),
                             selected = "1.31"),
                 
                 # Select level of insecticide resistance
                 selectInput(inputId = "pyr3", label = strong("Permethrin bioassay mortality"),
                             choices = c('64','40'),
                             selected = "64"),
                 
                 # Select LLIN-efficacy assumption (converting bioassay to hut mortality)
                 selectInput(inputId = "seed_subset3", label = strong("Impact of resistance on LLIN efficacy"),
                             choices = c('all', 'more sensitive', 'less sensitive'),
                             selected = "all"),
                 
               ),
               mainPanel(
                 class="mainPanel",
                 h3(paste0("Burundi: comparison of U1 burden with and without IPTi between ", barplot_start_year, " and ", barplot_end_year)),
                 br(),
                 plotOutput(outputId = "barplot_IPTi_allBurdenMetrics", height = "300px"),
                 br(),
                 br(),
                 br(),
                 br(),
                 plotOutput(outputId = "map_comparisons_IPTi", height = "800px"),
               )

             )
    ))
  
}
