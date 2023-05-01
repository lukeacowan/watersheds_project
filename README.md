# **Watersheds Analysis** 
## Introduction
- This document will provide a data dictionary, describe the data preparation process, and provide analysis through visualizations for data collected from 5 watersheds in Scott and Rock Island counties. Note: There were 7 watersheds included in the data, but many of the columns deemed insightful had NA values for two of the watersheds so they were filtered out.
---
## Data Dictionary :orange_book:
The columns used from the initial dataset include the following:
- Watershed: Which of the 5 watersheds the data was collected from
- Site: The unique site codes for the 5 watersheds; higher numbers indicate that the sites are further upstream
- Leaf_Dec: The rate of leaf decomposition; units are g/day
- Tot_inv_abun: Total abundance of all macroinvertebrates divided by 15 
- FBI: The family biotic index of the macroinvertebrates at the site (weighted mean of tolerance X abundance); indicates tolerance to pollution
- Temp: The mean of all temperature values at the site in degrees C
- DO_ppm: The mean oxygen in mg/l or ppm at site
- PO4: The mean phospate concentration in mg/l at the site
- NO3: The mean nitrate/nitrite concentration in mg/l at the site
- Imper_Catch: Measurement of catchment levels in areas with impervious surfaces
- Forest: Measurement of catchment levels in forest areas
- Grass: Measurement of catchment levels in grass areas
- Agricultural: Measurement of catchment levels in agricultural areas
---

## Data Prep :hammer:
1. I read the excel file in, selected the columns I initially thought would be useful, and filtered out Duck and Rock River watersheds.
```
watershed_data <- read_excel("Site Assessment Master with Meta data.xlsx", .name_repair = "universal") %>%
  dplyr::select("Watershed", "Site", "Year_full_assess", "Leaf_Dec", "Tot_inv_abun", "FBI", "pH", "Temp",
                "DO_per", "DO_ppm", "SPC", "TDS", "PO4", "NO3", "NH3", "BOD", "TSS", "Discharge", "Chloride",
                "Imper_Catch", "Imper_5m", "Arsenic", "Mercury", "Lead", "Wetland", "Forest", "Grass", "Agricultural") %>%
  #removing watersheds that contain mainly NA values
  dplyr::filter(Watershed != "Duck",
                Watershed != "Rock River")
```

2. I created a separate dataframe to analyze variable relationships, and I made a correlation matrix to identify which relationships offered the most valuable insights.
```
relationship_testing <- watershed_data 
relationship_testing <- relationship_testing[, -c(1, 2)]
relationship_testing <- na.omit(relationship_testing)

cor(relationship_testing)
```

3. I created pivot tables for most of the relationships I wanted to analyze.
```
invertebrate_leaf_decomp <- watershed_data %>%
  dplyr::select("Watershed", "Tot_inv_abun", "Leaf_Dec") %>%
  na.omit() %>%
  group_by(Watershed) %>%
  mutate(Average_Tot_inv_abun = mean(Tot_inv_abun),
         Average_Leaf_Dec = mean(Leaf_Dec)) %>%
  distinct(Watershed, Average_Tot_inv_abun, Average_Leaf_Dec)
```

- This was done for four other pivot tables with minimal deviation from the code above.

4. I created a dataframe exclusively for watershed sites to more easily identify catchment by area relationships.
```
catchment_by_site <- watershed_data %>%
  dplyr::select("Watershed", "Site", "Wetland", "Forest", "Grass", "Agricultural")
```

5. I used the dataframe from above to generate three additional dataframes containing relationships that I found noteworthy.
```
blackhawk_agricultural <- catchment_by_site %>%
  dplyr::select("Watershed", "Site", "Agricultural") %>%
  dplyr::filter(Watershed == "Blackhawk")
```
---

## Data Analysis :mag:
1. Set up the UI configuration in the following manner.
```
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
```
- plotOutput and verbatimTextOutput functions with only one argument were used to display analysis for eight charts through the usage of if/else functions in the server code.

2. Rendered plots and text based on the chart type selected by the user.
```
output$myPlot <- renderPlot({

})
output$myText <- renderText({

})
```
- Code provided below in steps 3 and 4 was included in the blank lines left between the parentheses and squiggly brackets.

3. Created bar and bubble visualizations based on each of the pivot tables and dataframes created in the data prep steps covered previously.
```
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
        
}
```
- Similar code was written to display the six other visualizations.

4. Generated text analyses and assigned them to their respective visualization.
```
if (input$visualization_choice == "Phosphate Versus Impervious Surface Catchment Levels") {
  "There is strong positive correlation between phosphate concentration and catchment level measures for impervious surfaces. The impervious catchment level measured in 
   Rock Island sites is eight times greater than Kickapoo watershed sites, and the large contrast in phosphate concentration offers us ann explanation as to why this is."
  
} else if (input$visualization_choice == "Macroinvertebrate Abundance Versus Leaf Decomposition Rate") {
  "There is moderately weak positive correlation between abundance of macroinvertebrates and the rate of leaf decomposition at the tested sites. This informs
   us that changes in leaf decomposition rates from site to site cannot be fully accounted for or explained by changes in the macroinvertebrate population."

}    
```
- As with step 3, similar code was written for the remaining six visualization synopses/explanations.
---
## Shinyapp Link :link:
-  https://lukecowan.shinyapps.io/mississippi_valley_watersheds/

