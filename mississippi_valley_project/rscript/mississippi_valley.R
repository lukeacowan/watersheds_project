library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(lubridate)
library(readxl)
library(tidyverse)
rm(list = ls())

#setting working directory
#setwd("C:/Users/lukec/OneDrive/Documents/data332/mississippi_valley_project/data")

#reading in the data and selecting important columns
watershed_data <- read_excel("Site Assessment Master with Meta data.xlsx", .name_repair = "universal") %>%
  dplyr::select("Watershed", "Site", "Year_full_assess", "Leaf_Dec", "Tot_inv_abun", "FBI", "pH", "Temp",
                "DO_per", "DO_ppm", "SPC", "TDS", "PO4", "NO3", "NH3", "BOD", "TSS", "Discharge", "Chloride",
                "Imper_Catch", "Imper_5m", "Arsenic", "Mercury", "Lead", "Wetland", "Forest", "Grass", "Agricultural") %>%
  #removing watersheds that contain mainly NA values
  dplyr::filter(Watershed != "Duck",
                Watershed != "Rock River")

#setting up dataframe for use in correlation matrix
relationship_testing <- watershed_data 
relationship_testing <- relationship_testing[, -c(1, 2)]
relationship_testing <- na.omit(relationship_testing)

#creating correlation matrix to analyze relationships in the data
cor(relationship_testing) 

#creating pivot table for phosphate and impervious catchment
phosphate_catchment <- watershed_data %>%
  dplyr::select("Watershed", "PO4", "Imper_Catch") %>%
  group_by(Watershed) %>%
  mutate(Average_PO4 = mean(PO4),
         Average_Imper_Catch = mean(Imper_Catch)) %>%
  distinct(Watershed, Average_PO4, Average_Imper_Catch)

#creating pivot table for abundance of macroinvertebrates and leaf decomposition
invertebrate_leaf_decomp <- watershed_data %>%
  dplyr::select("Watershed", "Tot_inv_abun", "Leaf_Dec") %>%
  na.omit() %>%
  group_by(Watershed) %>%
  mutate(Average_Tot_inv_abun = mean(Tot_inv_abun),
         Average_Leaf_Dec = mean(Leaf_Dec)) %>%
  distinct(Watershed, Average_Tot_inv_abun, Average_Leaf_Dec)

#creating pivot table for nitrate and agricultural catchment
nitrate_catchment <- watershed_data %>%
  dplyr::select("Watershed", "NO3", "Agricultural") %>%
  group_by(Watershed) %>%
  mutate(Average_NO3 = mean(NO3),
         Average_Agricultural = mean(Agricultural)) %>%
  distinct(Watershed, Average_NO3, Average_Agricultural)

#creating pivot table for oxygen and family biotic index
oxygen_fbi <- watershed_data %>%
  dplyr::select("Watershed", "FBI", "DO_ppm") %>%
  na.omit() %>%
  group_by(Watershed) %>%
  mutate(Average_DO_ppm = mean(DO_ppm),
         Average_FBI = mean(FBI)) %>%
  distinct(Watershed, Average_DO_ppm, Average_FBI)

#creating pivot table for temperature and forest catchment
forest_temp <- watershed_data %>%
  dplyr::select("Watershed", "Temp", "Forest") %>%
  group_by(Watershed) %>%
  mutate(Average_Temp = mean(Temp),
         Average_Forest = mean(Forest)) %>%
  distinct(Watershed, Average_Temp, Average_Forest)

#creating dataframe exclusively for watershed sites to more easily identify relationships
catchment_by_site <- watershed_data %>%
  dplyr::select("Watershed", "Site", "Wetland", "Forest", "Grass", "Agricultural")

#creating dataframe to analyze agricultural catchment by Blackhawk sites
blackhawk_agricultural <- catchment_by_site %>%
  dplyr::select("Watershed", "Site", "Agricultural") %>%
  dplyr::filter(Watershed == "Blackhawk")

#creating dataframe to analyze grass catchment by Kickapoo sites
kickapoo_grass <- catchment_by_site %>%
  dplyr::select("Watershed", "Site", "Grass") %>%
  dplyr::filter(Watershed == "Kickapoo")

#creating dataframe to analyze agricultural catchment by Kickapoo sites
kickapoo_agricultural <- catchment_by_site %>%
  dplyr::select("Watershed", "Site", "Agricultural") %>%
  dplyr::filter(Watershed == "Kickapoo")

#setting up shinyapp
ui<-fluidPage( 
  
  titlePanel(title = "Explore Mississippi Valley Watershed Site Assessment Data"),
  h4('The data was collected from Rock Island (IL) and Scott (IA) counties'),
  
  #setting up visualization selection area for user
  sidebarLayout(
    sidebarPanel(
           selectInput("visualization_choice", "Select Visualization",
                          choices = c("Phosphate Versus Impervious Surface Catchment Levels", "Macroinvertebrate Abundance Versus Leaf Decomposition Rate",
                                      "Nitrate Versus Catchment in Agricultural Areas", "Effect of Oxygen on Insects' Tolerance to Pollution",
                                      "Temperature Versus Catchment in Forest Areas", "Agricultural Catchment Levels by Blackhawk Watershed Sites",
                                      "Grass Catchment Levels by Kickapoo Watershed Sites", "Agricultural Catchment Levels by Kickapoo Watershed Sites")),
    ),
    mainPanel(
      plotOutput("myPlot", width = "1000px", height = "800px"),
      verbatimTextOutput("myText")
    )
  )
)


server <- function(input, output) {
  #rendering plot based on selected chart type
  output$myPlot <- renderPlot({
    if (input$visualization_choice == "Phosphate Versus Impervious Surface Catchment Levels") {
      #rendering phosphate and impervious catchment chart
      ggplot(phosphate_catchment, aes(x = reorder(Watershed, Average_Imper_Catch), y = Average_Imper_Catch, fill = Average_PO4)) +
        geom_col(show.legend = TRUE) +
        scale_fill_gradient(low="white", high="darkblue") +
        labs(title = "Phosphate's Impact on Impervious Surface Catchment Levels", x = "Watershed", y = "Impervious Catchment", fill = "Phosphate (mg/L)")
      
    } else if (input$visualization_choice == "Macroinvertebrate Abundance Versus Leaf Decomposition Rate") {
      #rendering abundance of macroinvertebrates and leaf decomposition chart
      ggplot(invertebrate_leaf_decomp, aes(x = reorder(Watershed, Average_Leaf_Dec), y = Average_Leaf_Dec, size = Average_Tot_inv_abun)) +
        geom_point(color = "orange", alpha=0.8) +
        scale_size(range = c(5, 20), name = "Abundance of macroinvertebrates") +
        labs(title = "Macroinvertebrate Abundance Versus Leaf Decomposition Rate", x = "Watershed", y = "Leaf Decomposition Rate (g/day)")
      
    } else if (input$visualization_choice == "Nitrate Versus Catchment in Agricultural Areas") {
      #rendering nitrate and agricultural catchment chart
      ggplot(nitrate_catchment, aes(x = reorder(Watershed, Average_Agricultural), y = Average_Agricultural, fill = Average_NO3)) +
        geom_col(show.legend = TRUE) +
        scale_fill_gradient(low="cyan", high="black") +
        labs(title = "Nitrate Versus Catchment in Agricultural Areas", x = "Watershed", y = "Agricultural Catchment", fill = "Nitrate/Nitrite (mg/L)")
      
    } else if (input$visualization_choice == "Effect of Oxygen on Insects' Tolerance to Pollution") {
      #rendering oxygen and family biotic index chart
      ggplot(oxygen_fbi, aes(x = reorder(Watershed, -Average_FBI), y = Average_FBI, size = Average_DO_ppm)) +
        geom_point(color = "#FF5733", alpha=1.0) +
        scale_size(range = c(10, 30), name = "DO_ppm") +
        labs(title = "Effect of Oxygen on Insects' Tolerance to Pollution", x = "Watershed", y = "Macroinvertebrate Family Biotic Index")
      
    } else if (input$visualization_choice == "Temperature Versus Catchment in Forest Areas") {
      #rendering temperature and forest catchment chart
      ggplot(forest_temp, aes(x = reorder(Watershed, -Average_Forest), y = Average_Forest, fill = Average_Temp)) +
        geom_col(show.legend = TRUE) +
        scale_fill_gradient(low = munsell::mnsl("5P 7/12"), high = munsell::mnsl("5P 2/12")) +
        labs(title = "Temperature Versus Catchment in Forest Areas", x = "Watershed", y = "Forest Catchment", fill = "Temperature (degrees C)")
      
    } else if (input$visualization_choice == "Agricultural Catchment Levels by Blackhawk Watershed Sites") {
      #rendering agricultural catchment by Blackhawk sites
      ggplot(blackhawk_agricultural, aes(x = reorder(Site, Agricultural), y = Agricultural, fill = as.factor(Agricultural))) +
        geom_col(show.legend = FALSE) +
        labs(title = "Agricultural Catchment Levels by Blackhawk Watershed Sites", x = "Sites", y = "Agricultural Catchment")
      
    } else if (input$visualization_choice == "Grass Catchment Levels by Kickapoo Watershed Sites") {
      #rendering grass catchment by Kickapoo sites
      ggplot(kickapoo_grass, aes(x = reorder(Site, -Grass), y = Grass, fill = as.factor(Grass))) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("#3498DB", "#E74CEC", "#009E73")) +
        labs(title = "Grass Catchment Levels by Kickapoo Watershed Sites", x = "Sites", y = "Grass Catchment")
      
    } else if (input$visualization_choice == "Agricultural Catchment Levels by Kickapoo Watershed Sites") {
      #rendering agricultural catchment by Kickapoo sites
      ggplot(kickapoo_agricultural, aes(x = reorder(Site, Agricultural), y = Agricultural, fill = as.factor(Agricultural))) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = c("#FF2D2D", "#1F4E79", "#4C2C69")) +
        labs(title = "Agricultural Catchment Levels by Kickapoo Watershed Sites", x = "Sites", y = "Agricultural Catchment")
    }
  })
  
  ##rendering text based on selected chart type
  output$myText <- renderText({
    
    #text for chart 1
    if (input$visualization_choice == "Phosphate Versus Impervious Surface Catchment Levels") {
      "There is strong positive correlation between phosphate concentration and catchment level measures for impervious surfaces. The impervious catchment level measured in 
  Rock Island sites is eight times greater than Kickapoo watershed sites, and the large contrast in phosphate concentration offers us ann explanation as to why this is."
  
      #text for chart 2
    } else if (input$visualization_choice == "Macroinvertebrate Abundance Versus Leaf Decomposition Rate") {
      "There is moderately weak positive correlation between abundance of macroinvertebrates and the rate of leaf decomposition at the tested sites. This informs
  us that changes in leaf decomposition rates from site to site cannot be fully accounted for or explained by changes in the macroinvertebrate population."
    
      #text for chart 3
    } else if (input$visualization_choice == "Nitrate Versus Catchment in Agricultural Areas") {
      "There is exceptionally strong positive correlation between nitrate/nitrite concentration and catchment level measures in agricultural areas, as can be seen in the
  consistent progression of nitrate levels as agricultural catchment rises. Rock Island sites all had 0 values for agricultural catchment, but I chose not to filter
  them out because NA values were present elsewhere in the data to indicate that data for that specific variable was not collected. Thus, I concluded that these are
  legitimate test values."
    
      #text for chart 4
    } else if (input$visualization_choice == "Effect of Oxygen on Insects' Tolerance to Pollution") {
      "There is strong negative correlation between oxygen concentration and the family biotic index of invertebrates, which measures insects' tolerance to pollution. 
  This trend is relatively consistent in the chart displayed, although Crow watershed sites are an exception to the negative relationship."
    
      #text for chart 5
    } else if (input$visualization_choice == "Temperature Versus Catchment in Forest Areas") { 
      "There is moderately strong negative correlation between temperature and catchment levels in forest areas."
    
      #text for chart 6  
    } else if (input$visualization_choice == "Agricultural Catchment Levels by Blackhawk Watershed Sites") {
      "Sites with higher numbers assigned to them indicate that they are further upstream. This means that measures of catchment levels in agricultural areas
  increase as you move further upstream."
   
      #text for chart 7   
    } else if (input$visualization_choice == "Grass Catchment Levels by Kickapoo Watershed Sites") {
      "Compared to the previous chart, the complete opposite trend can be seen here. As you progress further upstream on the Kickapoo watershed, the measures 
  of catchment levels in grass areas decrease, albeit not significantly."
    
      #text for chart 8  
    } else if (input$visualization_choice == "Agricultural Catchment Levels by Kickapoo Watershed Sites") {
      "Similar to the sites in Blackhawk, the measures of catchment levels upstream for the Kickapoo watershed are much higher than those from downstream. The
  difference here, however, is much more dramatic, rising from a catchment level of 26.3 at site 4 all the way to 82.7 at site 18."
    
    }
  })
}

shinyApp(ui, server)
