# Load packages
library(shiny)
library(shinythemes)
if("plyr" %in% search()) detach("plyr", unload=TRUE) 
library(dplyr)
library(readr)
library(ggplot2)
library(here)
library(rjson)

# install.packages("dplyr")
# install.packages("here")
# install.packages("sf")
# install.packages("reshape2")
# install.packages("remotes")
# install.packages("sodium")
# install.packages('rjson')

###################################################################################
########## specify filepaths and basic information on simulation set #############
###################################################################################

bdi_shiny_path <- '.'

hbhi_dir = here(bdi_shiny_path, 'data')
script_base_filepath = here(bdi_shiny_path, 'scripts')
configFile = here(bdi_shiny_path, 'config.json')
configJSON <- fromJSON(file = configFile)

source(paste0(script_base_filepath, '/process_sim_output_functions.R'))
source(paste0(script_base_filepath, '/plot_sim_output_functions.R'))
source(here(bdi_shiny_path, 'ui_fns.R'))

# directory where scenario output is located
sim_output_dir = paste0(hbhi_dir, '/simulation_output')
pop_filepath = paste0(hbhi_dir, '/admin_pop_archetype.csv')
admin_shapefile_filepath = paste0(hbhi_dir, '/SpatialClustering/reference_rasters_shapefiles/bdi_adm2.shp')
shapefile_admin_colname = 'NOMDEP'

# which experiments/scenarios should be plotted?
scenario_names = c('to-present', 'BAU', 'GF', 'GF with PBO', 'GF at 80%', 'GF at 80% with PBO')
scenario_input_references = paste0(hbhi_dir, '/simulation_inputs/_intervention_file_references/', c('Interventions_for_2010_2020', rep('Interventions_for_projections',5)), '.csv')
sim_end_years = c(2020, rep(2030, 5))
# set colors for each scenario
scenario_palette = c('to-present'=rgb(0,0,0), 'BAU'=rgb(0/255,120/255,0/255), 
                     'GF'=rgb(55/255,80/255,220/255),  'GF with PBO'=rgb(90/255,180/255,238/255), 
                     'GF at 80%'=rgb(170/255,51/255,119/255), 'GF at 80% with PBO'=rgb(255/255,140/255,175/255))

# set minimum and maximum years
min_year = 2012
max_year = 2030
barplot_start_year=2022
barplot_end_year=2024

# which admin will be plotted?
irs_districts = c('Buye', 'Gashoho', 'Kiremba','Muyinga')
comm_llin_districts = c('Giteranyi','Ngozi')
llin_gf_districts = c("Bubanza", "Bugarama", "Buhiga", "Bururi", "Busoni", "Butezi",   "Cankuzo", "Cibitoke", "Fota", "Gahombo","Gihofi",  "Gitega", "Giteranyi", "Isale" ,  "Kabezi",  "Kayanza",     
                     "Kibumbu",  "Kibuye" , "Kiganda", "Kinyinya", "Kirundo",  "Mabayi",  "Makamba",  "Matana", "Mpanda",  "Mukenke",  "Muramvya", "Murore" ,  "Musema" ,  "Mutaho", "Ngozi",  "Nyabikere" ,  
                     "Nyanza-Lac", "Rumonge", "Rutana", "Ruyigi", "Rwibaga", "Ryansoro", "Vumbi", "Zone Centre", "Zone Nord", "Zone Sud")

pbo_gf_districts = c("Bubanza", "Bugarama", "Buhiga", "Busoni", "Butezi",   "Cankuzo", "Cibitoke",  "Gahombo","Gihofi",  "Gitega",   "Kayanza",     
                      "Kibuye" ,"Kinyinya", "Kirundo",  "Mabayi",  "Makamba", "Mpanda",  "Mukenke", "Murore" ,  "Musema" ,  "Mutaho",  "Nyabikere" ,  
                     "Nyanza-Lac", "Rumonge", "Rutana", "Ruyigi", "Ryansoro", "Vumbi")
other_districts = 'Bubanza'
ipti_districts = c('Busoni', 'Buye', 'Gashoho', 'Gihofi', 'Kinyinya', 'Kiremba', 'Kirundo', 'Mukenke', 'Murore', 'Muyinga', 'Vumbi')

# fraction of indoor time protected under net - use same value as in simulation. Use this value to back-calculate the use rate from effective use rate.
indoor_protection_fraction = 0.75
overwrite_files = FALSE

# login tab ui to be rendered on launch
login_tab <- tabPanel(
  title = icon("lock"), 
  value = "login", 
  shinyauthr::loginUI("authr_login")
)


################################################################
##########           create Shiny output           #############
################################################################
# icon("info-circle", class = "infoIcon"),
# actionButton(label = "Update View", inputId = "infoIcon", class="infoIcon", icon = icon("refresh", class = "infoIcon")),


ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel="shortcut icon", href="favicon.ico"),
    actionButton(label="", inputId = "infobutton",  width="30px" ,icon = icon("info-circle", class="infoIcon"), class = "infoIconButton"),
    tags$script(type="text/javascript", src = "layout.js")
  ),
  tags$footer(
    class="footerSection",
    htmlTemplate("template.html"),
  ),
  navbarPage(
    collapsible = FALSE,
    fluid = FALSE,
    title="Predicted impact of malaria intervention packages in Burundi",
    windowTitle="IDM",
    id = "tabs",
    theme = shinytheme("cerulean"),
    login_tab
))


# Define server function
server <- function(input, output, session) {
  #------------------------------------
  # for login
  #------------------------------------

  user_base_authr_tbl <- tibble(user_name=character(), password=character())
  
  #add user creds to tibble
  for (row in configJSON$credentials) {
    new <- tibble_row(user_name=row$user_name, password=sodium::password_store(row$password))
    user_base_authr_tbl<-bind_rows(user_base_authr_tbl, new)
  }
  
  
  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )
  
  # call the shinyauthr login and logout server modules
  credentials <- shinyauthr::loginServer(
    id = "authr_login",
    data = user_base_authr_tbl,
    user_col = user_name,
    pwd_col = password,
    sodium_hashed = TRUE,
    reload_on_logout = TRUE,
    log_out = reactive(logout_init())
  )
  
  observeEvent(credentials()$user_auth, {
    if (credentials()$user_auth) { 
      removeTab("tabs", "login")
      removeTab("tabs", "")
      createTabs(barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year)
      updateNavbarPage(session, "tabs", selected = "Burden relative to BAU")
      
    }
  })
  # 
  # $(document).on('shiny:connected', function(event) {
  #   alert('Connected to the server');
  # });
  
  
  #  =======  barplots showing burden relative to BAU  =======  #
  
  # Create relative burden barplots with all burden metrics
  get_relative_barplot = reactive({
    seed_subset = input$seed_subset0
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
    }

    pyr = input$pyr0
    chw_cov = input$chw_cov0
    experiment_names = c(paste0('BDI_2010_2020_allInter'),
                         paste0('BDI_projection_', pyr,'pyr'),  # BAU
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_PBO', pyr,'pyr'),  # GF with PBO nets
                         paste0('BDI_projection_GF_80Coverage_', pyr,'pyr'),  # GF at 80% coverage
                         paste0('BDI_projection_GF_80CoveragePBO_', pyr,'pyr')
    )
    if(input$itn_halflife0 == '2'){
      experiment_names = paste0(experiment_names, '_2yLLIN')
    }

    if(input$district_subset_name0 == 'all health districts'){
      cur_admins = 'all'
      district_subset = 'districtsAll'
    } else if(input$district_subset_name0 == 'only IRS districts'){
      cur_admins = irs_districts
      district_subset = 'districtsIRS'
    } else if(input$district_subset_name0 == 'only districts that receive community LLIN distribution'){
      cur_admins = comm_llin_districts
      district_subset = 'districtsCommLLIN'
    } else if(input$district_subset_name0 == 'only districts receiving PBO/IG2 in GF plan'){
      cur_admins = pbo_gf_districts
      district_subset = 'districtsPBO'
    } else if(input$district_subset_name0 == 'DistrictsOther'){
      cur_admins = other_districts
      district_subset = 'DistrictsOther'
    } else{
      warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
      cur_admins = 'all'
      district_subset = 'districtsAll'
    }
    

    gg = plot_relative_burden_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                       barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                       pyr=pyr, chw_cov=chw_cov,
                                       scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, overwrite_files=overwrite_files)
    
  })
  
  # Create relative burden barplots with all burden metrics
  output$barplot_allBurdenMetrics <- renderPlot({
    print(get_relative_barplot())
  })
  
  
  
  #  =======  timeseries of all burden metrics  =======  #
  
  # Create timeseries plot with all burden metrics
  get_plot_all_burden_metrics = reactive({
    seed_subset = input$seed_subset1
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLogLog')
    }
    scenario_base_dirs = c(sim_past_output_dir, rep(sim_future_output_dir,5))
    
    
    pyr = input$pyr1
    chw_cov = input$chw_cov1
    experiment_names = c(paste0('BDI_2010_2020_allInter'),
                         paste0('BDI_projection_', pyr,'pyr'),  # BAU
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_PBO', pyr,'pyr'),  # GF with PBO nets
                         paste0('BDI_projection_GF_80Coverage_', pyr,'pyr'),  # GF at 80% coverage
                         paste0('BDI_projection_GF_80CoveragePBO_', pyr,'pyr')
    )
    if(input$itn_halflife1 == '2'){
      experiment_names = paste0(experiment_names, '_2yLLIN')
      LLIN2y_flag = TRUE
    } else LLIN2y_flag = FALSE
    scenario_filepaths = paste0(scenario_base_dirs, '/', experiment_names)
    
    if(input$district_subset_name1 == 'all health districts'){
      cur_admins = 'all'
      district_subset = 'districtsAll'
    } else if(input$district_subset_name1 == 'only IRS districts'){
      cur_admins = irs_districts
      district_subset = 'districtsIRS'
    } else if(input$district_subset_name1 == 'only districts that receive community LLIN distribution'){
      cur_admins = comm_llin_districts
      district_subset = 'districtsCommLLIN'
    } else if(input$district_subset_name1 == 'only districts receiving PBO/IG2 in GF plan'){
      cur_admins = pbo_gf_districts
      district_subset = 'districtsPBO'
    } else if(input$district_subset_name1 == 'DistrictsOther'){
      cur_admins = other_districts
      district_subset = 'DistrictsOther'
    } else{
      warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
      cur_admins = 'all'
      district_subset = 'districtsAll'
    }
    
    # if(input$time_res == 'annual average'){
    plot_by_month = FALSE
    # } else plot_by_month = TRUE
    
    gg = plot_simulation_output_burden_all(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                           plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                           pyr=pyr, chw_cov=chw_cov,
                                           scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, experiment_names=experiment_names, scenario_palette=scenario_palette, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
    
  })
  
  # Create timeseries plot with all burden metrics
  output$timeseriesplot_allBurdenMetrics <- renderPlot({
    print(get_plot_all_burden_metrics())
  })
  
  
  
  #  =======  timeseries of burden metric and intervention coverage   =======  #
  
  # Create plot with burden and intervention timeseries
  get_plot_burden_intervention = reactive({
    seed_subset = input$seed_subset
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
      sim_past_output_dir = paste0(sim_output_dir, '/simulations_to_present/simulation_subsets/mortLogLog')
    }
    scenario_base_dirs = c(sim_past_output_dir, rep(sim_future_output_dir,5))
    
    
    pyr = input$pyr
    chw_cov = input$chw_cov
    experiment_names = c(paste0('BDI_2010_2020_allInter'),
                         paste0('BDI_projection_', pyr,'pyr'),  # BAU
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF
                         paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_PBO', pyr,'pyr'),  # GF with PBO nets
                         paste0('BDI_projection_GF_80Coverage_', pyr,'pyr'),  # GF at 80% coverage
                         paste0('BDI_projection_GF_80CoveragePBO_', pyr,'pyr')
    )
    
    if(input$itn_halflife == '2'){
      experiment_names = paste0(experiment_names, '_2yLLIN')
      LLIN2y_flag = TRUE
    } else LLIN2y_flag = FALSE
    scenario_filepaths = paste0(scenario_base_dirs, '/', experiment_names)
    
    if(input$district_subset_name == 'all health districts'){
      cur_admins = 'all'
      district_subset = 'districtsAll'
    } else if(input$district_subset_name == 'only IRS districts'){
      cur_admins = irs_districts
      district_subset = 'districtsIRS'
    } else if(input$district_subset_name == 'only districts that receive community LLIN distribution'){
      cur_admins = comm_llin_districts
      district_subset = 'districtsCommLLIN'
    } else if(input$district_subset_name == 'only districts receiving PBO/IG2 in GF plan'){
      cur_admins = pbo_gf_districts
      district_subset = 'districtsPBO'
    } else if(input$district_subset_name == 'DistrictsOther'){
      cur_admins = other_districts
      district_subset = 'DistrictsOther'
    } else{
      warning('Name for the subset of districts to plot not recognized; results for all districts will be shown')
      cur_admins = 'all'
      district_subset = 'districtsAll'
    }
    
    if(input$time_res == 'annual average'){
      plot_by_month = FALSE
    } else plot_by_month = TRUE

    gg = plot_simulation_intervention_output(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset=district_subset, cur_admins=cur_admins, 
                                             plot_by_month=plot_by_month, min_year=min_year, max_year=max_year, sim_end_years=sim_end_years, 
                                             burden_metric=input$burden_metric, age_plotted=input$age_plotted, 
                                             pyr=pyr, chw_cov=chw_cov,
                                             scenario_filepaths=scenario_filepaths, scenario_names=scenario_names, scenario_input_references=scenario_input_references, experiment_names=experiment_names, scenario_palette=scenario_palette, 
                                             indoor_protection_fraction=indoor_protection_fraction, LLIN2y_flag=LLIN2y_flag, overwrite_files=overwrite_files)
    
  })

  # Create plot with burden and intervention timeseries
  output$timeseriesplot_burdenIntervention <- renderPlot({
    print(get_plot_burden_intervention())
  })
  

  
  
  #  =======  barplot of IPTi impact   =======  #
  
  # Plot burden reduction with IPTi
  get_relative_IPTi_barplot = reactive({
    seed_subset = input$seed_subset3
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
    }
    
    pyr = input$pyr3
    chw_cov = input$chw_cov3

    if(input$itn_halflife3 == '2'){
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_IPTi'))  # GF with IPTi
    } else{
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_IPTi'))  # GF with IPTi
    }
    
    cur_admins = ipti_districts
    gg = plot_relative_burden_IPTi_barplots(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset='districtsIPTi', cur_admins=cur_admins, 
                                            barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                                            pyr=pyr, chw_cov=chw_cov,
                                            experiment_names=experiment_names ,overwrite_files=overwrite_files)
    
  })
  
  # Create relative burden barplots with all burden metrics
  output$barplot_IPTi_allBurdenMetrics <- renderPlot({
    print(get_relative_IPTi_barplot())
  })
  
  
  
  
  
  #  =======  maps of IPTi impact   =======  #
  
  
  # Plot burden reduction with IPTi
  get_IPTi_maps = reactive({
    seed_subset = input$seed_subset3
    if(seed_subset == 'all'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future')
    } else if(seed_subset == 'more sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLog')
    } else if(seed_subset == 'less sensitive'){
      sim_future_output_dir = paste0(sim_output_dir, '/simulations_future/simulation_subsets/mortLogLog')
    }
    
    pyr = input$pyr3
    chw_cov = input$chw_cov3
    
    if(input$itn_halflife3 == '2'){
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_2yLLIN_IPTi'))  # GF with IPTi
    } else{
      experiment_names = c(paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr'),  # GF without IPTi
                           paste0('BDI_projection_GF_', chw_cov, 'CHWCoverage_', pyr,'pyr_IPTi'))  # GF with IPTi
    }
    
    cur_admins = ipti_districts
    plot_IPTi_burden_maps(sim_future_output_dir=sim_future_output_dir, pop_filepath=pop_filepath, district_subset='districtsIPTi', cur_admins=cur_admins, 
                          barplot_start_year=barplot_start_year, barplot_end_year=barplot_end_year, 
                          experiment_names=experiment_names, admin_shapefile_filepath=admin_shapefile_filepath, shapefile_admin_colname=shapefile_admin_colname)

  })
  
  output$map_comparisons_IPTi <- renderPlot({
    get_IPTi_maps()
  })
  
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
