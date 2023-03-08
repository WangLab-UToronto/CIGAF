# Libraries --------------------

require(collapsibleTree)
library(colorspace)
library(dashboardthemes)
library(dplyr)
library(DT)
library(forcats)
library(ggplot2)
library(ggbeeswarm)
library(heatmaply)
library(leaflet)
library(leaflet.extras)
library(lubridate)
library(pals)
library(plotly)
library(plyr)
library(Polychrome)
library(RColorBrewer)
library(reshape2)
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(tidyr)




# Data Wrangling ---------------------------------------

species <- read.csv("www/clim_out_with_tax_main_Oct_2022.csv")
#subset and rename the relevant columns in the data after running the GBIF taxonomy script
species <- species[1:43]
colnames(species)[36] <- "Genus"
colnames(species)[35] <- "Family"
colnames(species)[34] <- "Order"
verify_fa = FALSE
#remove Ichthyophonida
species = species %>%
  filter(Order != "Ichthyophonida")

species$Latitude <- as.numeric(species$Latitude)
species$Longitude <- as.numeric(species$Longitude)

species$Year.Collected <- as.integer(species$Year.Collected)
species$Date <- lubridate::dmy(species$Date)
species$Complete_date <- lubridate::dmy(species$Complete_date)


# support structures
speciesStudies <-
  sort(as.character(unique(species[, c("Citation.Number")])))
speciesLocation <-
  sort(as.character(unique(species[, c("Country")])))

#Add a column with data for the map popups
species <-
  mutate(
    species,
    popupdata = paste0(
      '<strong>Name: </strong>',
      '<i>' ,
      Species,
      '</i>',
      '<br><strong>Year Collected:</strong> ',
      Year.Collected,
      '<br><strong>Month Collected:</strong> ',
      Letter.Month,
      '<br><strong>Site Description:</strong> ',
      Site.Description,
      '<br><strong>Site Host_family:</strong> ',
      Host_family,
      '<br><strong>Lat:</strong> ',
      Latitude,
      '<br><strong>Long:</strong> ',
      Longitude,
      '<br><strong>Citation:</strong> ',
      Citation.Number,
      '. ',
      Citation,
      ', ',
      DOI,
      '<br><strong>CIGAF ID:</strong> ',
      CIGAF_ID
      
    )
  )

col_data <- species
col_data$Date <- as.character(as.Date(col_data$Date))
col_data$Year.Collected <-
  as.character(as.Date(ISOdate(col_data$Year.Collected, 01, 01)))
col_data$Complete_date <-
  as.character(as.Date((col_data$Complete_date)))

pub_data <- species
pub_data$Year.of.Publication <-
  as.character(as.Date(ISOdate(col_data$Year.of.Publication, 01, 01)))
pub_hist_data <- pub_data


#colors for plots --------------------------------------------------------------

host_pal <-
  c(
    "#FA9016",
    "#0D8749",
    "#A9F681" ,
    "#0DFFAA",
    "#EC16A6",
    "#7A16C3",
    "#1660B7",
    "#878B7D",
    "#6C7600",
    "#93F1BB",
    "#832E26",
    "#C022B6",
    "#009992",
    "#DFAF60",
    "#FB0D7A",
    "#B6E3E5",
    "#FE9590",
    "#CF3B0D",
    "#914D6C",
    "#CCDCA5",
    "#DE87FF",
    "#FA85D5",
    "#A60051",
    "#731C86",
    "#624200",
    "#F6001C",
    "#0DFC0D",
    "#160DFA",
    "#FDD1D2",
    "#FE00E2",
    "#0DD4FF",
    "#E7E300"
  )

mypal <-
  colorRampPalette(c("#FFFFFF", "#3690c0", "#0570b0", "#045a8d", "#023858"), bias = 3)


#download data
download_data <- species[c(1,
                           
                           10,
                           11,
                           13,
                           
                           34,
                           35,
                           36,
                           6,
                           
                           26,
                           27,
                           28,
                           
                           2,
                           3,
                           
                           14,
                           7,
                           8,
                           
                           9,
                           20,
                           21,
                           22, 
                           
                           37, 
                           38, 
                           39,
                           
                           41,
                           42,
                           43
                           )]

colnames(download_data) <- c(
  'CIGAF_ID',
  
  'Collection_year',
  'Collection_month',
  'Collection_day',
  
  'Order',
  'Family',
  'Genus',
  'Species',
  
  'Host_common_name',
  'Host_family',
  'Host_species',
  
  'Site_name',
  'Site_description',
  
  'Country',
  'Latitude' ,
  'Longitude',
  
  'Citation_number',
  'Year_published',
  'Citation',
  'DOI', 
  
  'Min_month_temp', 
  'Max_month_temp',
  'Ave_month_precipitation',
  
  'Eco_name',
  'Biome_name', 
  'Realm'
)

#custom theme -------------------------------------------------------------------

uoft_blue <- "#25355A"
boundless_blue <- "#4a6ab4"
light_gray <- "#F2F4F7"
black <- "#000000"

customTheme <- shinyDashboardThemeDIY(
  ### general
  appFontFamily = "Arial",
  appFontColor = uoft_blue,
  primaryFontColor = light_gray,
  infoFontColor = light_gray,
  successFontColor = light_gray,
  warningFontColor = light_gray,
  dangerFontColor = light_gray,
  bodyBackColor = light_gray,
  
 ### header 
  logoBackColor = uoft_blue,
  headerButtonBackColor = uoft_blue,
  headerButtonIconColor = light_gray,
  headerButtonBackColorHover = boundless_blue,
  headerButtonIconColorHover = light_gray,
  headerBackColor = uoft_blue,
  headerBoxShadowColor = boundless_blue,
  headerBoxShadowSize = "0px 0px 0px 0px",
  sidebarBackColor = uoft_blue,
  sidebarShadowRadius = "30px",
  sidebarPadding = 2,
  sidebarShadowColor = boundless_blue,
  sidebarMenuBackColor = uoft_blue,
  sidebarMenuPadding = 5,
  sidebarMenuBorderRadius = 30,
  sidebarUserTextColor = light_gray,
  sidebarSearchBackColor = boundless_blue,
  sidebarSearchIconColor =  boundless_blue,
  sidebarSearchBorderColor = boundless_blue,
  sidebarTabTextColor = light_gray,
  sidebarTabTextSize = 14,
  sidebarTabBorderStyle = "line",
  sidebarTabBorderColor = uoft_blue,
  sidebarTabBorderWidth = 1,
  sidebarTabBackColorSelected = boundless_blue,
  sidebarTabTextColorSelected = black,
  sidebarTabRadiusSelected = "30px",
  sidebarTabBackColorHover = boundless_blue,
  sidebarTabTextColorHover = black,
  sidebarTabBorderStyleHover = "none",
  sidebarTabBorderColorHover = "none",
  sidebarTabBorderWidthHover = 0,
  sidebarTabRadiusHover = "30px",
  
  ### boxes
  
  boxBackColor = light_gray,
  boxBorderRadius = 30,
  boxShadowSize = "1px 1px 1px",
  boxShadowColor = "",
  boxTitleSize = 16,
  boxDefaultColor = boundless_blue,
  boxPrimaryColor = boundless_blue,
  boxInfoColor = boundless_blue,
  boxSuccessColor = uoft_blue,
  boxWarningColor = uoft_blue,
  boxDangerColor = boundless_blue,
  tabBoxTabColor = uoft_blue,
  tabBoxTabTextSize = 14,
  tabBoxTabTextColor = boundless_blue,
  tabBoxTabTextColorSelected = light_gray,
  tabBoxBackColor = boundless_blue,
  tabBoxHighlightColor = uoft_blue,
  tabBoxBorderRadius = 15,
  
  ### inputs
  
  buttonBackColor = boundless_blue,
  buttonTextColor = black,
  buttonBorderColor = black,
  buttonBorderRadius = 20,
  buttonBackColorHover = boundless_blue,
  buttonTextColorHover = black,
  buttonBorderColorHover = boundless_blue,
  textboxBackColor = light_gray,
  textboxBorderColor = boundless_blue,
  textboxBorderRadius = 20,
  textboxBackColorSelect = light_gray,
  textboxBorderColorSelect = boundless_blue,
 
  ### tables
  
  tableBackColor = light_gray,
  tableBorderColor = light_gray,
  tableBorderTopSize = 1,
  tableBorderRowSize = 0.5
  
)



ui <- (fluidPage(
  
  #The browser tab icon
  tags$head(
    tags$link(rel = "shortcut icon",
              href = "favicon.ico"),
    HTML("<title>CIGAF</title>")
  ),
  
  
  # load page layout
  dashboardPage(
    dashboardHeader(
      
      # Title format and making the icon clickable to the home page
      title = tags$header(

        tags$b("CIGAF"),
        tags$a(href = 'http://cigaf.eeb.utoronto.ca/',
               tags$img(src = 'favicon.ico')),
        style = "position:absolute;left:10px;z-index:1000000;font-size: 50px"),
      titleWidth = 249,
      tags$li(class = "dropdown",
              tags$style(".main-header {height: 60px; padding-top: 2px !important;}"),
              tags$style(".main-header .logo {height: 60px; padding-top: 2px !important;}"),
              tags$style(".sidebar-toggle {height: 60px; padding-top: 20px !important;}"),
              tags$style(".navbar {min-height:60px !important}")
              )
    ),
    
    
    dashboardSidebar(
      tags$style(".left-side, .main-sidebar {padding-top: 61px}"),
      width = 250,
      sidebarMenu(
        
        menuItem(
          "Home", 
          tabName = "home",
          icon = icon("home")),
        
        menuItem(
          "Collections by Geography",
          tabName = "choro",
          icon = icon("globe-americas"),
          menuSubItem(
            "Collection Sites",
            tabName = "map",
            icon = icon("fa-fw", verify_fa = FALSE)),
          menuSubItem(
            "Diversity",
              tabName = "diversity",
              icon = icon("fa-fw", verify_fa = FALSE)),
          menuSubItem(
            "Range",
              tabName = "specrange",
              icon = icon("fa-fw", verify_fa = FALSE))
        ),
        menuItem(
          "Collections by Ecoregion",
          tabName = "eco_region",
          icon = icon("tree")),
        
        
        menuItem(
          "Collections by Climate",
          tabName = "weather",
          icon = icon("cloud-sun"),
          menuSubItem(
            "Average Monthly Temperature",
            tabName = "clim",
            icon = icon("fa-fw", verify_fa = FALSE)),
          menuSubItem(
            "Average Monthly Precipitation",
            tabName = "rain",
            icon = icon("fa-fw", verify_fa = FALSE))
        ),
        
        menuItem(
          "Collections Through Time",
          tabName = "time",
          icon = icon("clock"),
          menuSubItem(
            "Collection Date",
            tabName = "col_time",
            icon = icon("fa-fw", verify_fa = FALSE)),
          menuSubItem(
            "Publication Date",
            tabName = "pub_time",
            icon = icon("fa-fw", verify_fa = FALSE))
        ),
        
        menuItem(
          "Collections by Taxonomy",
          tabName = "taxon",
          icon = icon("sitemap")
        ),
        
        menuItem(
          "Collections by Host Insect",
          tabName = "host",
          icon = icon("bug")
        ),
        
        menuItem(
          "Downloadable Data Tables",
          tabName = "table",
          icon = icon("table")
        ),
        
        menuItem(
          "Contact",
          tabName = "contact",
          icon = icon("envelope-open-text")
        ),
        
        menuItem(
          "Works Cited",
          tabName = "workscited",
          icon = icon("tasks")
          )
        
        )
    ),
    
    # Dash borad body tabs UI ------------------------------------------------
    dashboardBody(
      shinyjs::useShinyjs(),
      customTheme,
      
      # Home UI -------------------------------------------------------------
      tabItems(
        tabItem(
          tabName = "home",
          fluidRow(
            box(
              width = 12,
              collapsible = FALSE,
              status = "warning",
              includeMarkdown("www/home1.md"),
              plotlyOutput("specimen_choro_map") %>%
                withSpinner(color = boundless_blue),
              includeMarkdown("www/home2.md")
              
            )
          )),
        # Map UI -------------------------------------------------------------
        tabItem(
          tabName = "map",
          
          fluidRow(
            box(
              width = 12,
              includeMarkdown("www/map1.md"),
              uiOutput("map_order_op", ),
              uiOutput("map_family_op"),
              uiOutput("map_genus_op"),
              uiOutput("map_species_op"),
              

              radioButtons(
                "marker_type",
                "Markers Options:",
                c("Collapse Markers", "Individual Points")),
              
              leafletOutput("sitesMap", height = 700) %>%
                withSpinner(color = boundless_blue),
              
              includeMarkdown("www/map2.md"),
              
              dataTableOutput("map_datatable") %>%
                withSpinner(color = boundless_blue)
            )
          )), 
        #Choro UI ------------------------------------------------------------
        tabItem(
          tabName = "diversity",
          box(
            width = 15,
            collapsible = FALSE,
            
            includeMarkdown("www/choro1.md"),
            
            selectInput(
              "div_choro_input",
              h4("Select a taxonomic level"),
              c("Order", "Family", "Genus",
                "Species", "Specimen"),
              selected = "Species"),
            
            plotlyOutput("div_choro_map") %>%
              withSpinner(color = boundless_blue),
            
            includeMarkdown("www/choro2.md"),
            
            dataTableOutput("div_choro_datatable") %>%
              withSpinner(color = boundless_blue)
          ),
        ),
        
        tabItem(
          tabName = "specrange",
          box(
            width = 15,
            collapsible = FALSE,
            
            includeMarkdown("www/specrange1.md"),
            uiOutput("range_order_op"),
            uiOutput("range_family_op"),
            uiOutput("range_genus_op"),
            uiOutput("range_species_op"),

            plotlyOutput("spe_choro_map"),
            
            includeMarkdown("www/specrange2.md"),
            
            dataTableOutput("spe_choro_datatable")
          ),
        ),
        
        # ECO UI--------------------------------------------------------------
        
        tabItem(
          tabName = "eco_region",
          fluidRow(
          box(
            width = 15,
            collapsible = FALSE,
            
            includeMarkdown("www/eco_region1.md"),
            uiOutput("eco_order_op"),
            uiOutput("eco_family_op"),
            uiOutput("eco_genus_op"),
            uiOutput("eco_species_op"),
           
            selectInput(
              "eco_x_axis",
              h4("Select an Ecoregion Descriptor for the X axis"),
              c("Realm",
                "Biome_name",
                "Eco_name")),
          div(
            style = 'overflow-x: scroll',
            plotlyOutput("eco_plot", height = 700 )%>%
              withSpinner(color = boundless_blue)),
          includeMarkdown("www/eco_region2.md"))
          )),
    
        
        
        #  Clim UI ------------------------------------------------------------
        tabItem(
          tabName = "clim",
          fluidRow(
            box(
              width = 15,
              collapsible = FALSE,
              
              includeMarkdown("www/clim1.md"),
              
              selectInput(
                "climxAxis",
                h4("Select an X Axis"),
                c("Order","Family","Genus","Hemisphere","Country")),
              selectInput(
                "climmeasure",
                h4("Select a Temperature Measure"),
                c("Average Monthly Temp (Calculated from WorldClim recorded Min and Max)",
                  "Minimum Monthly Average Temp",
                  "Maximum Monthly Average Temp" )),
              
              radioButtons(
                "show_points_temp",
                "Plot Options:",
                c("Only Boxplot", "Only Points", "Points and Boxplots")),
              
              plotlyOutput("climviolplot") %>%
                withSpinner(color = boundless_blue),
              
              includeMarkdown("www/clim1.5.md"),
              includeMarkdown("www/clim2.md"),
              
              dataTableOutput("clim_datatable")
            ),
          )),
        
        tabItem(
          tabName = "rain",
          fluidRow(
            box(
              width = 15,
              collapsible = FALSE,
              
              includeMarkdown("www/rain1.md"),
              
              selectInput(
                "rainxAxis",
                h4("Select an X Axis"),
                c("Order","Family", "Genus", "Hemisphere", "Country")),
              radioButtons(
                "show_points_rain",
                "Plot Options:",
                c("Only Boxplots", "Only Points", "Points and Boxplots")),
              
              plotlyOutput("rainviolplot") %>%
                withSpinner(color = boundless_blue),
              
              includeMarkdown("www/rain2.md"),
              
              dataTableOutput("rain_datatable")
            ),
          )),
        
        
        
        #Pub  Time UI -------------------------------------------------------------
        tabItem(
          tabName = "pub_time",
          
          box(
            width = 15,
            collapsible = FALSE,
            
            includeMarkdown("www/pub_time1.md"),
            
            dateRangeInput(
              'pub_date_range',
              label = h4('Select a range on the slider to view collections from that 
                period in the histogram, map and datatable below'),
              start = "1930-01-01",
              end = Sys.Date()),
            
            selectInput(
              "pub_colour_by",
              h4("Colour Chart By:"),
              c("Order",
                "Family",
                "Genus",
                "Country",
                "Citation",
                "Hemisphere")),
            
            plotlyOutput("pub_time_hist", height = 300) %>%
              withSpinner(color = boundless_blue),
            includeMarkdown("www/pub_time2.5.md"),
            
            fluidRow(
              column(width = 6,
                     
                     includeMarkdown("www/pub_time2.md"),
                     
                     leafletOutput("pub_timeMap") %>%
                       withSpinner(color = boundless_blue)
              ),
              
              #empty column for spacing
              column(width = 1),
              
              column( width = 6,
                      
                      includeMarkdown("www/pub_time3.md"),
                      
                      dataTableOutput("pub_time_datatable") %>%
                        withSpinner(color = boundless_blue)
              )
            )
          )
        ),
        
        # Col Time UI -------------------------------------------------------------
        
        tabItem(
          tabName = "col_time",
          
          box(
            width = 15,
            collapsible = FALSE,
            
            includeMarkdown("www/time1.md"),
            
            fluidRow(
              column(width = 6,
                     selectInput(
                       "col_date_resolution",
                       h4("Select a time-frame resolution - 
                     Note: A higher resolution has fewer data points
                          \n  "),
                       c("Year", "Year-Month", "Year-Month-Day"))
              ),
              
              column(width = 1),
              column(width = 6,
                     dateRangeInput(
                       'col_date_range',
                       label = h4('Select a range on the slider to view collections
                             from that period in the histogram, map and
                             datatable below'),
                       start = "1920-01-01",
                       end = Sys.Date()
                     )
              )
            ),
            
            includeMarkdown("www/time2.md"),
            
            selectInput(
              "colour_by",
              h4("Colour Chart By:"),
              c("Order",
                "Family",
                "Genus",
                "Country",
                "Citation",
                "Hemisphere")
            ),
            
            plotlyOutput("col_time_hist", height = 300) %>%
              withSpinner(color = boundless_blue),
            
            includeMarkdown("www/time2.5.md"),
            
            
            fluidRow(
              column(width = 6,
                     
                     includeMarkdown("www/time3.md"),
                     
                     leafletOutput("col_timeMap") %>%
                       withSpinner(color = boundless_blue)
              ),
              
              column(width = 1),
              
              column(width = 6,
                     
                     includeMarkdown("www/time4.md"),
                     
                     dataTableOutput("col_time_datatable") %>%
                       withSpinner(color = boundless_blue)
              )
            )
          )
        ),
        
        #Taxon UI ------------------------------------------------------------
        tabItem(
          tabName = "taxon",
          fluidRow(
            box(
              width = 15,
              collapsible = FALSE,
              
              includeMarkdown("www/taxon1.md"),
              
              box(
                width = 15,
                collapsible = TRUE,
                status = "warning",
                solidHeader = TRUE,
                title =  ("Link out to Lucid Server -
                                Select a species to activate"),
                h4(textOutput("str")),
                h3(uiOutput("link"))
              ),
              
              includeMarkdown("www/taxon2.md"),
              
              div(
                style = 'overflow-x: scroll',
                collapsibleTreeOutput("taxon_tree",
                                      width = "1000",
                                      height = "1000")%>%
                  withSpinner(color = boundless_blue)
              ) 
            ),
          )),
        
        #Host UI ------------------------------------------------------------
        tabItem(
          tabName = "host",
          fluidRow(
            box(
              width = 15,
              collapsible = FALSE,
              
              includeMarkdown("www/host.md"),
              
              fluidRow(
                column(
                  width = 12,
                  plotlyOutput("host_heatmap", height = "900px") %>%
                    withSpinner(color = boundless_blue),
                  
                  includeMarkdown("www/host1.5.md"),
                  
                  selectInput(
                    "host_x_input",
                    h3("Bar Plot Option - Select a Fungal
                       Taxonomic Level (X axis) :"),
                    c("Order", "Family", "Genus"),
                    selected = "Family"
                  ),
                  
                  fluidRow(
                    column(width = 6, 
                           
                           plotlyOutput("host_charts_count", height = "500px") %>%
                             withSpinner(color = boundless_blue)
                    ),
                    column(width = 1),
                    column(width = 6,
                           
                           plotlyOutput("host_charts_percent", height = "500px") %>%
                             withSpinner(color = boundless_blue)
                    ),
                    
                    includeMarkdown("www/host2.5.md")
                  )
                )
              )
            )
          )),
        
        # Table UI -------------------------------------------------------------
        tabItem(
          tabName = "table",
          includeMarkdown("www/table.md"),
          dataTableOutput("speciesDataTable") %>%
            withSpinner(color = boundless_blue),
          downloadButton("downloadData", "Download")
        ),
        
        # Contact UI -------------------------------------------------------------
        tabItem(
          tabName = "contact",
          includeMarkdown("www/contact.md")),
        
        
        # Workscited UI -------------------------------------------------------------
        tabItem(
          tabName = "workscited",
          includeMarkdown("www/workscited.md"))
      )
      
    ) # end dashboard Body
  )# end dashboard Page
  )# end fluid page)# end UI
)

# SERVER -------------------------------------------------------

server <- function(input, output) {

  #Home server -----------------------------------------------------
  
  #Choro diversity overview map output
  # settings
  g <- list(
    showframe = TRUE,
    showcoastlines = TRUE,
    projection = list(type = "Mercator"),
    bgcolor = toRGB("white", alpha = 0.7)
  )
  
  #plotly margins
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 0,
    pad = 0
  )
  
  l <- list(color = toRGB("black"), width = 1)
  
  output$specimen_choro_map <- renderPlotly ({
    #select a species and only display where that species is located
    specimen_map_data <- species
    
    #summarize data by specimen numbers and location
    specimen_map_data <-
      dplyr::count(specimen_map_data, ISO.code, Country)
    specimen_df <- specimen_map_data
    
    specimen_map <- plot_ly(
      specimen_df,
      type = 'choropleth',
      locations = specimen_df$ISO.code,
      z = specimen_df$n,
      text = specimen_df$Country,
      colors = "BuPu",
      marker = list(line = l)
    ) %>%
      
      colorbar(
        thickness = "0",
        len = "1",
        x = 0.01,
        y = 0.3,
        bgcolor = "transparent"
      ) %>%
      
      layout(plot_bgcolor = light_gray,
             margin = m, 
             paper_bgcolor = light_gray,
             geo = g) %>% 
      hide_colorbar()
    
    specimen_map
  })
  
  # Map server -----------------------------------------------------------
  
  #convert user input plotting variable 
    cluster_toggle <- reactive({
    switch(
      input$clust_tog_input,
      "Order" = Order,
      "Family" = Family,
      "Genus" = Genus,
      "Species" = Species,
      "Specimen" = Specimen
    )
  })
  
  
  marker_toggle <- reactive({
    
    #convert user input map point style toggle 
    switch(
      input$marker_type,
      "Individual Points" = NULL,
      "Collapse Markers" = markerClusterOptions(
        showCoverageOnHover = TRUE,
        iconCreateFunction = JS(
          "function (cluster) {
    var childCount = cluster.getChildCount();
    var c = ' marker-cluster-';
    if (childCount < 100) {
      c += 'large';
    } else if (childCount < 1000) {
      c += 'large';
    } else {
      c += 'large';
    }
    return new L.DivIcon({ html: '<div><span>' + childCount + '</span></div>', className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) });

  }"
        )
      )
    )
  })
  
  
  
  
  output$map_order_op <- renderUI({
    
    main_map_data <- species
    selectInput("map_order", 
                   "Select Order", 
                   choices = c(
                     "All",
                     as.character(unique(main_map_data$Order))), 
                   selected = "All")

  })
  
  output$map_family_op <- renderUI({
    main_map_data <- species
    selectInput("map_family", 
                   "Select a Family", 
                   choices = c(
                     as.character(unique(sort(main_map_data$Family[main_map_data$Order == input$map_order]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$map_genus_op <- renderUI({
    main_map_data <- species
    selectInput("map_genus", 
                   "Select a Genus", 
                   choices = c(
                     as.character(unique(sort(main_map_data$Genus[main_map_data$Family == input$map_family]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$map_species_op <- renderUI({
    main_map_data <- species
    selectInput("map_species",
                   "Select a Species", 
                   choices = c(
                     as.character(unique(sort(main_map_data$Species[main_map_data$Genus == input$map_genus]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$sitesMap <- renderLeaflet({
    req(input$map_order)
    
    if (input$map_order == "All"){
      main_map_data <- species
      main_map_data <- main_map_data
    
    }else if (input$map_family == "All" || input$map_family == " "){
      main_map_data <- species
      main_map_data <- dplyr::filter(main_map_data, Order == input$map_order)

    } else if (input$map_genus== "All" || input$map_genus == " "){
      main_map_data <- species
      main_map_data <- dplyr::filter(main_map_data, Family == input$map_family)

    } else if (input$map_species == "All" || input$map_species == " "){
      main_map_data <- species
      main_map_data <- dplyr::filter(main_map_data, Genus == input$map_genus)

    } else {
      main_map_data <- species
      main_map_data <- dplyr::filter(main_map_data, Species == input$map_species)

    }

    
    leaflet(data = main_map_data) %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik,
        group = "Open Street Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Esri World Topo Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Esri World Imagery",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addFullscreenControl() %>%
      addCircleMarkers(
        ~ Longitude,
        ~ Latitude,
        popup = ~ as.character(popupdata),
        fillColor = "#b876cc",
        color = "black",
        fillOpacity  = 0.5,
        stroke = TRUE,
        radius = 8,
        clusterOptions =  marker_toggle()
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map", "Esri World Imagery", "Esri World Topo Map"),
        position = c("topright"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      setView(0, 0, zoom = 1.5)
    
  })
  
  
  # map data table of subset of data shown in map
  
  #Input can be lower or upper case
  selected_map_data <- reactive({
    if (is.null(input$map_spec_input) ||
        input$map_spec_input == "" ||
        !(as.character(tolower(input$map_spec_input)) %in% tolower(species$Species))) {
      species[, c(
        "Order",
        "Family",
        "Genus",
        "Species",
        "Country",
        "Citation.Number",
        "Citation",
        "CIGAF_ID"
      )]
    }
    else{
      selected_species <- tolower(input$map_spec_input)
      (species[tolower(species$Species) == selected_species,])[, c(
        "Order",
        "Family",
        "Genus",
        "Species",
        "Country",
        "Citation.Number",
        "Citation",
        "CIGAF_ID"
      )]
    }
  })
  
  output$map_datatable <- renderDataTable(
    selected_map_data(),
    filter = "top",
    options = list(scrollX = TRUE,
                   autoWidth = TRUE, 
                   columnDefs = list(list(
                     width = '300px', targets = c(7)
                   ))),
    class = c('compact', 'cell-border stripe'),
    rownames = ""
  )
  
  
  #Choro diversity map server --------------------------------------------------
  
  # Choro data structures
  #subset the data to remove unnecessary info and duplicates for each category
  specimen_count <- species[complete.cases(species[, 6]),]
  # Data to be displayed as data table if specimen is selected
  specimen_overview <-
    specimen_count[c("CIGAF_ID",
                     "Order",
                     "Family",
                     "Genus",
                     "Species",
                     "Citation",
                     "Country")]
  # Data to be plotted if specimen is selected
  Specimen <- dplyr::count(specimen_count, ISO.code, Country)
  
  species_count <- species[complete.cases(species[, 6]),]
  species_count <-
    species_count [!duplicated(species_count[c(6, 14)]),]
  species_overview <- species_count[c("Order", "Family", "Genus",
                                      "Species", "Country")]
  Species <- dplyr::count(species_count, ISO.code, Country)
  
  genus_count <- species[complete.cases(species[, 36]),]
  genus_count <- genus_count [!duplicated(genus_count[c(36, 14)]),]
  genus_overview <-
    genus_count[c("Order", "Family", "Genus", "Country")]
  Genus <- dplyr::count(genus_count,  ISO.code, Country)
  
  family_count <- species[complete.cases(species[, 35]),]
  family_count <-
    family_count [!duplicated(family_count[c(35, 14)]),]
  family_overview <- family_count[c("Order", "Family", "Country")]
  Family <- dplyr::count(family_count,  ISO.code, Country)
  
  order_count <- species[complete.cases(species[, 34]),]
  order_count <- order_count [!duplicated(order_count[c(34, 14)]),]
  order_overview <- order_count[c("Order", "Country")]
  Order <- dplyr::count(order_count,  ISO.code, Country)
  
  
  #choro diversity user input switch to variable input
  div_choro_map_Input <- reactive({
    switch(
      input$div_choro_input,
      "Order" = Order,
      "Family" = Family,
      "Genus" = Genus,
      "Species" = Species,
      "Specimen" = Specimen
    )
  })
  
  
  output$div_choro_map <- renderPlotly ({
    # set the dataframe to be plotted as the appropriate corresponding df from selection
    diversity_df <- div_choro_map_Input()
    diversity_map <- plot_geo(
      diversity_df,
      type = 'choropleth',
      locations = diversity_df$ISO.code,
      z = diversity_df$n,
      color = diversity_df$n,
      text = diversity_df$Country,
      colors = "BuPu",
      showscale = FALSE,
      marker = list(line = l)
    )
    
    
    diversity_map <-
      diversity_map %>% colorbar(
        thickness = "10",
        len = "0.1",
        x = 0.01,
        y = 0.3,
        bgcolor = "transparent"
      ) %>%
      layout(plot_bgcolor = light_gray, margin = m) %>%
      layout(paper_bgcolor = 'transparent',
             margin = m,
             geo = g)
    
    diversity_map
    
  })
  
  # diversity choro table output
  div_choro_table_Input <- reactive({
    switch(
      input$div_choro_input,
      "Order" = order_overview,
      "Family" = family_overview,
      "Genus" = genus_overview,
      "Species" = species_overview,
      "Specimen" = specimen_overview
    )
  })
  
  
  output$div_choro_datatable <-
    renderDataTable(
      div_choro_table_Input(),
      filter = "top",
      options = list(scrollX = TRUE),
      class = c('compact', 'cell-border stripe'),
      rownames = ""
    )
  
  
  #Choro  range Server  ---------------------------------------------
  

  output$range_order_op <- renderUI({
    species_map_data <- species
    selectizeInput("range_order", 
                   "Select Order", 
                   choices = c(
                     "All",
                     as.character(unique(species_map_data$Order))), 
                   selected = "All")
  })
  
  output$range_family_op <- renderUI({
    species_map_data <- species
    selectizeInput("range_family", 
                   "Select a Family", 
                   choices = c(
                     as.character(unique(sort(species_map_data$Family[species_map_data$Order == input$range_order]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$range_genus_op <- renderUI({
    species_map_data <- species
    selectizeInput("range_genus", 
                   "Select a Genus", 
                   choices = c(
                     as.character(unique(sort(species_map_data$Genus[species_map_data$Family == input$range_family]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$range_species_op <- renderUI({
    species_map_data <- species
    selectizeInput("range_species",
                   "Select a Species", 
                   choices = c(
                     as.character(unique(sort(species_map_data$Species[species_map_data$Genus == input$range_genus]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$spe_choro_map <- renderPlotly ({
    req(input$range_order)
    
    if (input$range_order == "All"){
      species_map_data <- species
      species_map_data <- species_map_data
      species_map_data <-
        dplyr::count(species_map_data, ISO.code, Country) 
    }else if (input$range_family == "All" || input$range_family == " "){
      species_map_data <- species
      species_map_data <- dplyr::filter(species_map_data, Order == input$range_order)
      species_map_data <-
        dplyr::count(species_map_data, ISO.code, Country) 
    } else if (input$range_genus== "All" || input$range_genus == " "){
      species_map_data <- species
      species_map_data <- dplyr::filter(species_map_data, Family == input$range_family)
      species_map_data <-
        dplyr::count(species_map_data, ISO.code, Country) 
    } else if (input$range_species == "All" || input$range_species == " "){
      species_map_data <- species
      species_map_data <- dplyr::filter(species_map_data, Genus == input$range_genus)
      species_map_data <-
        dplyr::count(species_map_data, ISO.code, Country) 
    } else {
      species_map_data <- species
      species_map_data <- dplyr::filter(species_map_data, Species == input$range_species)
      species_map_data <-
        dplyr::count(species_map_data, ISO.code, Country) 
    }
    
    
    species_df <- species_map_data
    
    species_map <- plot_ly(
      species_map_data,
      type = 'choropleth',
      locations = species_map_data$ISO.code,
      z = species_map_data$n,
      text = species_map_data$Country,
      colors = "BuPu",
      marker = list(line = l)
      
    ) %>%
      
      colorbar(
        thickness = "0",
        len = "1",
        x = 0.01,
        y = 0.3,
        bgcolor = "transparent"
      ) %>%
      
      layout(plot_bgcolor = light_gray,
             margin = m) %>%
      layout(paper_bgcolor = 'transparent',
             geo = g) %>% hide_colorbar()
    species_map
  })
  


  #output datatable that cooresponds to species range choropleth selection
  output$spe_choro_datatable <-
    renderDataTable({
      req(input$range_order)
      if (input$range_order == "All"){
        range_df_data <- species
        range_df_data <- range_df_data
        range_df_data <-
          dplyr::count(range_df_data, Country) 
        colnames(range_df_data) <- c("Country", "Count")
        
      }else if (input$range_family == "All" || input$range_family == " "){
        range_df_data <- species
        range_df_data <- dplyr::filter(range_df_data, Order == input$range_order)
        range_df_data <-
          dplyr::count(range_df_data, Country) 
        colnames(range_df_data) <- c("Country", "Count")
      } else if (input$range_genus== "All" || input$range_genus == " "){
        range_df_data <- species
        range_df_data <- dplyr::filter(range_df_data, Family == input$range_family)
        range_df_data <-
          dplyr::count(range_df_data, Country) 
        colnames(range_df_data) <- c("Country", "Count")
        
      } else if (input$range_species == "All" || input$range_species == " "){
        range_df_data <- species
        range_df_data <- dplyr::filter(range_df_data, Genus == input$range_genus)
        range_df_data <-
          dplyr::count(range_df_data, Country) 
        colnames(range_df_data) <- c("Country", "Count")
        
      } else {
        range_df_data <- species
        range_df_data <- dplyr::filter(range_df_data, Species == input$range_species)
        range_df_data <-
          dplyr::count(range_df_data,Country) 
        colnames(range_df_data) <- c("Country", "Count")
        
      }
    
      range_df_data
     } )
  
  # Ecoregion Server ----------------------------------------------------------------
  output$eco_order_op <- renderUI({
    eco_data <- species
    selectizeInput("eco_order", 
                   "Select Order", 
                   choices = c(
                     "All",
                     as.character(unique(eco_data$Order))), 
                   selected = "All")
  })
  
  output$eco_family_op <- renderUI({
    eco_data <- species
    selectizeInput("eco_family", 
                   "Select a Family", 
                   choices = c(
                     as.character(unique(sort(eco_data$Family[eco_data$Order == input$eco_order]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$eco_genus_op <- renderUI({
    eco_data <- species
    selectizeInput("eco_genus", 
                   "Select a Genus", 
                   choices = c(
                    as.character(unique(sort(eco_data$Genus[eco_data$Family == input$eco_family]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$eco_species_op <- renderUI({
    
    eco_data <- species
    
    selectizeInput("eco_species",
                   "Select a Species", 
                   choices = c(
                     as.character(unique(sort(eco_data$Species[eco_data$Genus == input$eco_genus]))), 
                     "All", 
                     " "), 
                   selected = " ")
  })
  
  output$eco_plot <- renderPlotly ({
    req(input$eco_order)
    
    if (input$eco_order == "All"){
      eco_data <- species
      eco_data <- eco_data

    }else if (input$eco_family == "All" || input$eco_family == " "){
      eco_data <- species
      eco_data <- dplyr::filter(eco_data, Order == input$eco_order)

    } else if (input$eco_genus== "All" || input$eco_genus == " "){
      eco_data <- species
      eco_data <- dplyr::filter(eco_data, Family == input$eco_family)

    } else if (input$eco_species == "All" || input$eco_species == " "){
      eco_data <- species
      eco_data <- dplyr::filter(eco_data, Genus == input$eco_genus)

    } else {
      eco_data <- species
      eco_data <- dplyr::filter(eco_data, Species == input$eco_species)

    }
    
    
    if (input$eco_x_axis == "Eco_name"){
      plot_height = 900
      plot_width = 1200
    } else if (input$eco_x_axis == "Biome_name"){
      plot_height = 700
      plot_width = 900
    } else {
      plot_height = 500
      plot_width = 500
    }
    eco_data <- filter(eco_data, Realm != "NA")
    ecoxaxis <- (input$eco_x_axis)
   p <- ggplot(eco_data, aes_string(x = paste0("fct_infreq(",ecoxaxis,")" )
                                               )) +
      geom_bar(stat = "count", 
               aes_string(fill = ecoxaxis),
               position = position_dodge2(width = 0.5, preserve = "single"),             
               width = 0.5,
               colour="black", 
               show.legend = FALSE
               ) +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, 
                                       vjust = 1,
                                       hjust = 1, 
                                       face = "bold", 
                                       size = 10), 
            axis.text.y = element_text(size =10, 
                                       face = "bold"),
            axis.title = element_text(size = 15, 
                                      face = "bold"),
            #strip.background = element_blank(),
            strip.text.x = element_blank())+
      labs(x = "Ecoregion",
           y = "Number of specimens")
    
    
    q <- ggplotly(p, tooltip = c( "text","y", "fill")) %>%
      layout(paper_bgcolor = 'transparent', 
             height = plot_height,
             width = plot_width,
             autosize=T) %>%
          layout(plot_bgcolor = light_gray, margin = m) %>%
      hide_legend()

      
   q
  })
  
 
  
  # # Climate Server-------------------------------------------------------------
  
  clim_data <- species
  clim_data <- filter(clim_data, clim_data$ave_temp != "NA")
  clim_data$min_temp <- as.numeric(clim_data$min_temp)
  clim_data$max_temp <- as.numeric(clim_data$max_temp)
  clim_data$ave_temp <- as.numeric(clim_data$ave_temp)
  clim_data$prec_avg <- as.numeric(clim_data$prec_avg)
  
  # climmeasure
  clim_measure_input <- reactive({
    switch(
      input$climmeasure,
      "Average Monthly Temp (Calculated from WorldClim recorded Min and Max)" = "ave_temp",
      "Minimum Monthly Average Temp" = "min_temp",
      "Maximum Monthly Average Temp" = "max_temp")
  })
  
  show_points_temp_toggle <- reactive({
    switch(
      input$show_points_temp,
      "Only Boxplot" = FALSE,
      "Show Points" = TRUE)
  })
  
  
  

  output$climviolplot <- renderPlotly({
    #reshape data to be in long format with max, min temp and variable selected
    species_melt <-
      melt(clim_data,
        id.vars = input$climxAxis,
        measure.vars = c(clim_measure_input()))
    species_melt <- species_melt[complete.cases(species_melt), ]
    

      #show only boxplot or also points
      if (input$show_points_temp == "Only Boxplot") {
        box_plot <- ggplot(species_melt, aes_string(input$climxAxis, "value")) +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5
          )) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = input$climmeasure , x = input$climxAxis) +
          lims(y = c(-30, 50)) +
          scale_y_continuous(breaks = seq(-30, 60, 5)) +
        geom_boxplot(fill = "#4a6ab4") 
      }else if (input$show_points_temp == "Only Points"){ 
        box_plot <- ggplot(species_melt, aes_string(input$climxAxis, "value")) +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5
          )) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = input$climmeasure , x = input$climxAxis) +
          lims(y = c(-30, 50)) +
          scale_y_continuous(breaks = seq(-30, 60, 5)) +
        geom_quasirandom(
          bandwidth = 0.01,
          aes(alpha = 0.2),
          groupOnX = TRUE,
          varwidth = TRUE)
      }else if (input$show_points_temp == "Points and Boxplots") {
        box_plot <- ggplot(species_melt, aes_string(input$climxAxis, "value")) +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5
          )) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = input$climmeasure , x = input$climxAxis) +
          lims(y = c(-30, 50)) +
          scale_y_continuous(breaks = seq(-30, 60, 5)) +
        geom_boxplot(fill = "#4a6ab4") +
          geom_quasirandom(
            bandwidth = 0.01,
            aes(alpha = 0.2),
            groupOnX = TRUE,
            varwidth = TRUE)
        }
          
    
    plotly_box_plot <- ggplotly(box_plot) %>%
      layout(plot_bgcolor = light_gray,
             margin = m) %>%
      layout(paper_bgcolor = 'transparent') %>%
      hide_legend()
  })
  
  output$clim_datatable <- renderDataTable(
    clim_data[c(1, 34, 35, 36, 6, 37, 38, 3, 7, 8, 10, 11, 13, 22, 21)],
    filter = "top",
    colnames = c(
      'CIGAF ID',
      'Order',
      'Family',
      'Genus',
      'Scientific name',
      'WorldClim Average minimum monthly temp (C)',
      'WorldClim Average maximum monthly temp (C)',
      'Site Description',
      'Latitude',
      'Longitude' ,
      'Year Collected',
      'Month Collected',
      'Day Collected',
      'Citation',
      'DOI'
    ),
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '300px', targets = c(8, 14)
      ))
    ),
    class = c('compact', 'cell-border stripe'),
    rownames = ""
  )

  output$rainviolplot <- renderPlotly({
    
    #reshape data to be in long format with max, min temp and variable selected
    species_melt <-
      melt(clim_data,
           id.vars = input$rainxAxis,
           measure.vars = c("prec_avg"))
    species_melt <- species_melt[complete.cases(species_melt), ]
    

      if (input$show_points_rain == "Only Boxplots") {
        box_plot <-
          ggplot(species_melt, aes_string(input$rainxAxis, "value")) +
          geom_boxplot(fill = "#4a6ab4") +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5)) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = "Average Monthly Precipitation (mm)", x = input$rainxAxis) +
          lims(y = c(-30, 2000)) +
          scale_y_continuous(breaks = seq(0, 550, 50))
      }else if ( input$show_points_rain == "Only Points") {
        box_plot <-
          ggplot(species_melt, aes_string(input$rainxAxis, "value")) +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5)) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = "Average Monthly Precipitation (mm)", x = input$rainxAxis) +
          lims(y = c(-30, 2000)) +
          scale_y_continuous(breaks = seq(0, 550, 50)) +
          geom_quasirandom(
            bandwidth = 0.01,
            aes(alpha = 0.2),
            groupOnX = TRUE,
            varwidth = TRUE)
      }else if(input$show_points_rain == "Points and Boxplots") {
        box_plot <-
          ggplot(species_melt, aes_string(input$rainxAxis, "value")) +
          geom_boxplot(fill = "#4a6ab4") +
          theme_classic() +
          theme(axis.text.x = element_text(
            angle = 45,
            vjust = 0,
            hjust = 0.5)) +
          theme(axis.title = element_text(size = 8)) +
          labs(y = "Average Monthly Precipitation (mm)", x = input$rainxAxis) +
          lims(y = c(-30, 2000)) +
          scale_y_continuous(breaks = seq(0, 550, 50)) +
          geom_quasirandom(
            bandwidth = 0.01,
            aes(alpha = 0.2),
            groupOnX = TRUE,
            varwidth = TRUE)
      }
    
    plotly_box_plot <- ggplotly(box_plot) %>%
      layout(plot_bgcolor = light_gray,
             margin = m) %>%
      layout(paper_bgcolor = 'transparent') %>%
      hide_legend()
  })
  #
  output$rain_datatable <-  renderDataTable(
    clim_data[c(1, 34, 35, 36, 6, 40, 3, 7, 8, 10, 11, 13, 22, 21)],
    filter = "top",
    colnames = c(
      'CIGAF ID',
      'Order',
      'Family',
      'Genus',
      'Scientific name',
      'WorldClim Average Precipitation (mm)',
      'Site Description',
      'Latitude',
      'Longitude' ,
      'Year Collected',
      'Month Collected',
      'Day Collected',
      'Citation',
      'DOI'
    ),
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '300px', targets = c(7, 13)
      ))
    ),
    class = c('compact', 'cell-border stripe'),
    rownames = ""
  )
  
  
  
  #Publication year server -----------------------------------------
  
  
  pub_filtering_string <- reactive ({
    paste0(
      "filter(pub_data, Year.of.Publication ",
      ">=",
      " ",
      "'",
      input$pub_date_range[1] ,
      "'",
      "& Year.of.Publication ",
      "<=",
      " ",
      "'",
      input$pub_date_range[2] ,
      "'",
      "& Year.of.Publication != 'NA'",
      ")"
    )
  })
  
  
  output$pub_time_hist <- renderPlotly({
    pub_hist_data <- eval(parse(text = pub_filtering_string()))
    colVar <- input$pub_colour_by
    p <- ggplot(pub_hist_data, aes(as.Date(Year.of.Publication), 
                                   text = Year.of.Publication)) +
      geom_histogram(stat = "count",
                     aes_string(fill = colVar)) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Date",
           y = "Number of publications"          ) +
      scale_x_date(date_breaks = "5 years") #+
    q <- ggplotly(p, tooltip = c( "text","y", "fill"))
  })
  
  output$pub_timeMap <- renderLeaflet({
    pub_hist_data <- eval(parse(text = pub_filtering_string()))
    colVar <- input$colour_by
    leaflet(data = pub_hist_data, height = ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Esri World Topo Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik,
        group = "Open Street Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Esri World Imagery",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addFullscreenControl() %>%
      addCircleMarkers(
        ~ Longitude,
        ~ Latitude,
        popup = ~ as.character(popupdata),
        fillColor = "#b876cc",
        color = "black",
        fillOpacity  = 0.5,
        stroke = TRUE,
        radius = 8
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map", "Esri World Imagery", "Esri World Topo Map"),
        position = c("topright"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  
  #Pub Time series datatable
  
  pub_time_table_data <- reactive ({
    pub_hist_data <- eval(parse(text = pub_filtering_string()))
  })
  
  output$pub_time_datatable <- renderDataTable(
    pub_time_table_data()[c(
      "CIGAF_ID",
      "Year.of.Publication",
      "Order",
      "Family",
      "Genus",
      "Species",
      "Country",
      "Citation",
      "DOI"
    )],
    filter = "top",
    colnames = c(
      'CIGAF ID',
      'Publication Date',
      'Order',
      'Family',
      'Genus',
      'Species',
      'Country',
      'Citation',
      'DOI'
    ),
    
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '300px', targets = c(8, 9)
      ))
    ),
    class = c('compact', 'cell-border stripe'),
    rownames = ""
  )
  
  #Collection date server ----------------------------------------------------------------


  hist_xaxis <- reactive({
    switch(
      input$col_date_resolution,
      "Year" = "Year.Collected",
      "Year-Month" = "Date",
      "Year-Month-Day" = "Complete_date")
  })

  filtering_string <- reactive ({
    paste0(
      "filter(col_data, ",
      hist_xaxis(),
      " ",
      ">=",
      " ",
      "'",
      input$col_date_range[1] ,
      "'",
      "&",
      hist_xaxis(),
      " ",
      "<=",
      " ",
      "'",
      input$col_date_range[2] ,
      "'",
      ")"
    )
  })
  

  output$col_time_hist <- renderPlotly({
    hist_data <- eval(parse(text = filtering_string()))
    colVar <- input$colour_by
    p <-
      ggplot(hist_data, aes_string(paste0("as.Date(",hist_xaxis(),")")))+
      geom_histogram(stat = "count", 
                     aes_string(fill = colVar, 
                                text = hist_xaxis())) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "Date",
           y = "Number of Collections") +
      scale_x_date(date_breaks = "5 years")
    
    q <- ggplotly(p, tooltip = c( "text","y", "fill"))
    
  })
  
  output$col_timeMap <- renderLeaflet({
    hist_data <- eval(parse(text = filtering_string()))
    colVar <- input$colour_by
    leaflet(data = hist_data, height = ) %>%
      addProviderTiles(
        providers$Esri.WorldTopoMap,
        group = "Esri World Topo Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$OpenStreetMap.Mapnik,
        group = "Open Street Map",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addProviderTiles(
        providers$Esri.WorldImagery,
        group = "Esri World Imagery",
        options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addFullscreenControl() %>%
      addCircleMarkers(
        ~ Longitude,
        ~ Latitude,
        popup = ~ as.character(popupdata),
        fillColor = "#b876cc",
        color = "black",
        fillOpacity  = 0.5,
        stroke = TRUE,
        radius = 8
      ) %>%
      addLayersControl(
        baseGroups = c("Open Street Map", "Esri World Imagery", "Esri World Topo Map"),
        position = c("topright"),
        options = layersControlOptions(collapsed = TRUE)
        
      )
  })
  
  
  #Col Time series datatable
  col_time_table_data <- reactive ({
    hist_data <- eval(parse(text = filtering_string()))
  })
  output$col_time_datatable <- renderDataTable(
    col_time_table_data()[c(
      "CIGAF_ID",
      as.character(hist_xaxis()),
      "Order",
      "Family",
      "Genus",
      "Species",
      "Country",
      "Citation",
      'DOI')],
    filter = "top",
    colnames = c(
      'CIGAF_ID',
      'Date',
      'Order',
      'Family',
      'Genus',
      'Species',
      'Country',
      'Citation',
      'DOI'),
    
    options = list(
      scrollX = TRUE,
      autoWidth = TRUE,
      columnDefs = list(list(
        width = '300px', targets = c(8, 9)))
    ),
    class = c('compact', 'cell-border stripe'),
    rownames = ""
  )
  
  
  # Host server ---------------------------------------------------------------------
  
  #  set variable to user input
  output$host_heatmap <- renderPlotly({
    #prepping data for heatmap
    
    species_for_host <-
      read.csv("www/clim_out_with_tax_main_Oct_2022.csv")
    #subset and rename the relevant columns in the data after running the GBIF taxonomy script
    species_for_host <- species_for_host[1:36]
    colnames(species_for_host)[36] <- "Genus"
    colnames(species_for_host)[35] <- "Family"
    colnames(species_for_host)[34] <- "Order"
    
    
    
    host_species <-
      separate_rows(species_for_host,
                    Host_family,
                    sep = ", ",
                    convert = FALSE)
    as.data.frame(host_species)
    host_species[host_species == ""] <- NA
    host_species[host_species == "Others"] <- NA
    host_species[host_species == "Ephemeroptera"] <- NA
    host_species[host_species == "\tEphemerellidae"] <-
      "Ephemerellidae"
    host_species <- drop_na(host_species, "Host_family")
    sort(unique(host_species$Host_family))
    
    host_species <- drop_na(host_species, "Genus")
    
    host_genus_count <- host_species[, c("Genus", "Host_family")] %>%
      count(vars = c("Genus", "Host_family"))
    
    host_genus_wide <-
      pivot_wider(host_genus_count,
                  names_from = Host_family,
                  values_from = freq)
    host_genus_wide[is.na(host_genus_wide)] <- 0
    host_genus_df <- as.data.frame(host_genus_wide)
    
    
    fungal_info <-
      unique(host_species[, c("Genus", "Family", "Order")])
    host_genus_info_matrix <-
      merge(host_genus_df, 
            fungal_info[, c("Genus", "Family", "Order")], by = "Genus")
    rownames(host_genus_info_matrix) <- host_genus_info_matrix[, 1]
    host_genus_info_matrix[, 1] <- NULL
    
    
    #transforming the matrix to have percent by row (fungal genus) as values
    host_genus_info_matrix[1:(ncol(host_genus_info_matrix) - 2)] <-
      host_genus_info_matrix[1:(ncol(host_genus_info_matrix) - 2)] / 
      rowSums(host_genus_info_matrix[1:(ncol(host_genus_info_matrix) - 2)])
    
    host_genus_info_matrix[1:(ncol(host_genus_info_matrix) - 2)] <-
      host_genus_info_matrix[1:(ncol(host_genus_info_matrix) - 2)] * 100
    
    
    #plotting heatmap
    
    h <-  heatmaply(
      host_genus_info_matrix,
      plot_method = "ggplot",
      node_type = "scatter",
      guides(scale = "none"),
      seriate = "mean",
      cluster_cols = TRUE,
      cluster_rows = TRUE,
      show_dendrogram = c(FALSE, FALSE),
      row_side_palette = stevens.greenblue,
      grid_color = "darkgray",
      custom_hovertext = NULL,
      cellnote = NULL,
      xlab = "Insect Family",
      ylab = "Fungal Genus ",
      grid_size = 2,
      hide_colorbar = TRUE,
      label_names = c(
        "Fungal genus",
        "Host insect family",
        "% of this genus found in this host"
      ),
      main = "Percent insect host families associated with each fungal genus",
      scale_fill_gradient_fun = ggplot2::scale_color_gradient2(
        high = "#081d58",
        mid = "#225ea8",
        low = "#41b6c4",
        midpoint = 50,
        limits = c(0.000001, 100),
        na.value = light_gray
      )
    )
    
    ph <-  plotly_build(h)
    ph %>%
      layout(plot_bgcolor = light_gray,
             margin(
               t = 0.1,
               r = 0,
               b = 0,
               l = 0.1
             )) %>%
      layout(paper_bgcolor = 'transparent') %>%
      hide_legend()
  })
  
  
  
  
  output$host_charts_percent <- renderPlotly({
    host_pal = polychrome()
    species_for_host <-
      read.csv("www/clim_out_with_tax_main_Oct_2022.csv")
    #subset and rename the relevant columns in the data after running the GBIF taxonomy script
    species_for_host <- species_for_host[1:36]
    colnames(species_for_host)[36] <- "Genus"
    colnames(species_for_host)[35] <- "Family"
    colnames(species_for_host)[34] <- "Order"
    
    host_species <-
      separate_rows(species_for_host, Host_family, sep = ", ")
    as.data.frame(host_species)
    host_species[host_species == ""] <- NA
    host_species[host_species == "Others"] <- NA
    host_species[host_species == "\tEphemerellidae"] <-
      "Ephemerellidae"
    host_species <- drop_na(host_species, "Host_family")
    sort(unique(host_species$Host_family))
    
    host_species <- drop_na(host_species, input$host_x_input)
    
    
    fungal_family_host_percent_barplot <-
      ggplot(host_species,
             aes_string(x = input$host_x_input, 
                        fill = "Host_family")) +
      geom_bar(position = "Fill") +
      theme_classic() +
      labs(x = paste0("Fungal ", input$host_x_input), 
           y = "Insect Host Percentage",
           fill = "Host Insect Family") +
      ggtitle(paste0("Host Family Fraction by Fungal ", input$host_x_input)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1.2
        ),
        legend.title = element_text(size = 8)
      ) +
      scale_fill_manual(values = as.vector(cubehelix(37)),
                        aesthetics = "fill")
    
    q <- ggplotly(fungal_family_host_percent_barplot)
    q %>%
      layout(plot_bgcolor = light_gray,
             margin(
               t = 0.1,
               r = 0,
               b = 0,
               l = 0.1
             )) %>%
      layout(paper_bgcolor = 'transparent')
    
    
  })
  
  output$host_charts_count <- renderPlotly({
    host_pal = polychrome()
    species_for_host <-
      read.csv("www/clim_out_with_tax_main_Oct_2022.csv")
    
    #subset and rename the relevant columns in the data after running the GBIF taxonomy script
    species_for_host <- species_for_host[1:36]
    colnames(species_for_host)[36] <- "Genus"
    colnames(species_for_host)[35] <- "Family"
    colnames(species_for_host)[34] <- "Order"
    
    host_species <-
      separate_rows(species_for_host, Host_family, sep = ", ")
    as.data.frame(host_species)
    host_species[host_species == ""] <- NA
    host_species[host_species == "Others"] <- NA
    host_species[host_species == "\tEphemerellidae"] <- "Ephemerellidae"
    host_species <- drop_na(host_species, "Host_family")
    sort(unique(host_species$Host_family))
    host_species <- drop_na(host_species, input$host_x_input)
    
    
    fungal_family_host_count_barplot <-
      ggplot(host_species,
             aes_string(x = input$host_x_input, 
                        fill = "Host_family")) +
      geom_bar(position = "Stack") +
      theme_classic() +
      labs(x = paste0("Fungal ", input$host_x_input),
           y = "Insect Host Count",
           fill = "Host Insect Family") +
      ggtitle(paste0("Host Family Count by Fungal  ", input$host_x_input)) +
      theme(
        axis.text.x = element_text(
          angle = 45,
          hjust = 1,
          vjust = 1.2
        ),
        legend.title = element_text(size = 8)
      ) +
      scale_fill_manual(values = as.vector(cubehelix(37)), 
                        aesthetics = "fill")
    
    q <- ggplotly(fungal_family_host_count_barplot)
    
    q %>%
      layout(plot_bgcolor = light_gray,
             margin(
               t = 0.1,
               r = 0,
               b = 0,
               l = 0.1
             )) %>%
      layout(paper_bgcolor = 'transparent')
    
    
  })
  
  
  # Taxonomy Server ---------------------------------------------------------
  output$taxon_tree <- renderCollapsibleTree({
    
    species_count <- species
    species_count <-
      species_count [!duplicated(species_count[c(6)]),]
    species_overview <-
      species_count[c("Order", "Family", "Genus", "Species")]
    p <-
      dplyr::count(species_overview, Order, Family, Genus, Species)
    
    collapsibleTreeSummary(
      p,
      hierarchy = c("Order", "Family", "Genus", "Species"),
      zoomable = FALSE,
      attribute = "n",
      root = "Insect-associated Fungi",
      tooltip = TRUE,
      fontSize = 12,
      percentOfParent = FALSE,
      fillFun = heat_hcl,
      maxPercent = 200,
      alpha = 0.7,
      inputId = "node", 
      linkLength = 150
    )
    
  })
  
  #only produce a link when a species level node is selected
  output$str <- renderText(paste0("You selected node: ",
                                  (input$node[1])))
  
  output$link <-  renderUI({
    req(input$node)
    if ( input$node != "" &&
         # ( !is.null(input$node[1])) && ( !is.na(input$node[1])) && () && 
         length(strsplit(x = paste0(input$node[1]), split = " " )[[1]]) == 2
    ){
      print("im here")
      p <- (strsplit(x = paste0(input$node[1]), split = " ")[[1]][1])
      q <- (strsplit(x = paste0(input$node[1]), split = " ")[[1]][2])
      
      
      #pasting user input into lucid central link format
      #different links for different subgroups
      
      if (p == "Smittium") {
        url <- a(
          "Link",
          href = paste0(
            "https://keys.lucidcentral.org/keys/v4/trichomycetes/keys/key/key_to_species_of_smittium/Media/Html/",
            p,
            "%20",
            q,
            ".htm"
          ),
          target = "_blank"
        )
        tagList(h3(paste0(
          "Link to ", p, "  ", q, " Lucid Server Entry :"
        )), url)
      }
      else if (p == "Baltomyces" |
               p == "Asellaria" | p == "Orchesellaria") {
        url <- a(
          "Link",
          href = paste0(
            "https://keys.lucidcentral.org/keys/v4/trichomycetes/keys/key/key_to_species_of_asellariales/Media/Html/",
            p,
            "%20",
            q,
            ".htm"
          ),
          target = "_blank"
        )
        tagList(h3(paste0(
          "Link to ", p, "  ", q, " Lucid Server Entry :"
        )), url )
      }
      
      
      else if (p == "Orphella") {
        url <- a(
          "Link",
          href = paste0(
            "https://keys.lucidcentral.org/keys/v4/trichomycetes/keys/key/key_to_species_of_orphallales/Media/Html/",
            p,
            "%20",
            q,
            ".htm"
          ),
          target = "_blank"
        )
        tagList(h3(paste0(
          "Link to ", p, "  ", q, " Lucid Server Entry :"
        )), url )
      }
      
      else if (p == "Stachylina") {
        url <- a(
          "Link",
          href = paste0(
            "https://keys.lucidcentral.org/keys/v4/trichomycetes/keys/key/key_to_species_of_stachylina/Media/Html/",
            p,
            "%20",
            q,
            ".htm"
          ),
          target = "_blank"
        )
        tagList(h3(paste0(
          "Link to ", p, "  ", q, " Lucid Server Entry :"
        )), url )
      }
      
      else if (p != "Smittium") {
        url <- h3(a(
          "Link",
          href = paste0(
            "https://keys.lucidcentral.org/keys/v4/trichomycetes/keys/key/key_to_species_of_harpellales__excludi/Media/Html/",
            p,
            "%20",
            q,
            ".htm"
          )
        ))
        tagList(paste0("Link to ", p, "  ", q, " Lucid Server Entry :"), url)
      }
      
    }
    else {
      print(h3("No Link available. Please select a species level node."))
    }})
  
  # Table Server-------------------------------------------------------------
  
  
  #download data
  final_dt <- species[c(1,
                             
                             10,
                             11,
                             13,
                             
                             34,
                             35,
                             36,
                             6,
                             
                             26,
                             27,
                             28,
                             
                             2,
                             3,
                             
                             14,
                             7,
                             8,
                             
                             9,
                             20,
                             21,
                             22, 
                             
                             37, 
                             38, 
                             39,
                             
                             41,
                             42,
                             43
  )]
  
  colnames(final_dt) <- c(
    'CIGAF_ID',
    
    'Collection_year',
    'Collection_month',
    'Collection_day',
    
    'Order',
    'Family',
    'Genus',
    'Species',
    
    'Host_common_name',
    'Host_family',
    'Host_species',
    
    'Site_name',
    'Site_description',
    
    'Country',
    'Latitude' ,
    'Longitude',
    
    'Citation_number',
    'Year_published',
    'Citation',
    'DOI', 
    
    'Min_month_temp', 
    'Max_month_temp',
    'Ave_month_precipitation',
    
    'Eco_name',
    'Biome_name', 
    'Realm'
  )
  
 
  
  
  output$speciesDataTable <- renderDataTable(
    final_dt,
    filter = "top",
    options = list(
      scrollX = TRUE, dom = 'ltipr',
                   autoWidth = TRUE,
                  columnDefs = list(
                    list(width = '300px', targets = c(13, 19, 20, 21)),
                  list(width = '100px', targets = c(5,6,7,8, 9, 10, 11))
        )),
      class = c('compact', 'cell-border stripe'),
      rownames = ""
  )
  
  #Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CIGAF.csv", sep = "")
    },
    
    content = function(file) {
      write.csv(download_data,
                file)
    }
  )
}


shinyApp(ui = ui, server = server)
