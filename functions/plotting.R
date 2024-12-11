## Script name: plotting.R
##
## Purpose of script: 
##    A collection of functions for plotting figures for the Pygoscelis penguins bill
##    morphology analysis
##
## Author: Biology3579
##
## Date Created: 2024-10-01 
##
## ---------------------------

# Required libraries
library(ggplot2)
library(dplyr)
library(ggsignif)

# Defining custom color palettes and custom shapes for each species of penguin ----
bad_custom_colours <- c(
  "Adelie" = "#D0B5E2",        
  "Chinstrap" = "#D7A6D1",    
  "Gentoo" = "#E2A7B2"         
)

custom_colours <- c(
  "Adelie" = "#E7B800", 
  "Chinstrap" = "#FC4E07", 
  "Gentoo" = "#00AFBB"
)

custom_shapes <- c(
  "Adelie" = 16,    # Filled circle
  "Chinstrap" = 17, # Filled triangle
  "Gentoo" = 15     # Filled square
)

# Function to create the 'bad figure' ----
plot_bad_figure <- function(data) {
  
  # Calculate the mean bill length and bill depth for each species
  mean_values <- analysis_data %>%
    group_by(species) %>%  # Group by species
    summarize( 
      mean_bill_length = mean(bill_length_mm, na.rm = TRUE),  # Calculate mean bill length
      mean_bill_depth = mean(bill_depth_mm, na.rm = TRUE))    # Calculate mean bill depth

  # Define data and axes
  ggplot(
    data = mean_values,
    aes(
      x = mean_bill_length, # Length on x-axis
      y = mean_bill_depth,  # Depth on y-axis
      fill = species)) +    # Species-specific colour
  
  # Graph type and styling
  geom_bar(
    stat = "identity",      # Use identity to map data directly to height
    color = "black",        # Black borders for clarity
    width = 1) +            # Bar width
               
  # Apply custom colours
   scale_fill_manual(values = bad_custom_colours) +
  
  # Labels
  labs(   
    x = "Mean Depth", 
    y = "Mean Length") +
  
  # Theme, sizes and positioning
  theme_minimal() + 
  theme(
    axis.title.x = element_text(size = 10),        # x-axis title style
    axis.title.y = element_text(size = 10),        # y-axis title style
    legend.position = "right")                     # Legend position
}


# Function to create the exploratory scatterplot ----
plot_exploratory_figure <- function(data) {
  
  # Define data and axes
  ggplot(data = analysis_data, 
         aes(
           x = bill_length_mm, # Length on x-axis
           y = bill_depth_mm,  # Depth on y-axis
           color = species,    # Species-specific colour
          shape = species)) +  # Species-specific shape
  
  # Plot all individual data points 
  geom_point(
    size = 3,       # Point size
    alpha = 0.7) +  # Point transparency
  
  # Apply custom colours
  scale_color_manual(values = custom_colours) +
  # Apply custom shapes
  scale_shape_manual(values = custom_shapes) +
    
  # Axis and legend labels 
  labs(
    x = "Bill Length (mm)", 
    y = "Bill Depth (mm)", 
    color = "Species", 
    shape = "Species",) +
  
  # Themes, sizes and positioning 
  theme_minimal() +  
  theme(
    axis.title.x = element_text(size = 10),        # x-axis title style
    axis.title.y = element_text(size = 10),        # y-axis title style
    legend.position = "right")                     # Legend position
}

# Function to create the boxplot with significance annotations
results_plot_1 <- function(data) {
  
  # Define data and axes
  ggplot(data = analysis_data, 
    aes(x = species, 
        y = bill_morphology)) + 
    
    # Boxplot for 
    geom_boxplot(aes
                 (color = species),      # Species-specific colour
                 width = 0.5,            # Width of the boxplot
                 show.legend = FALSE) +  # Remove legend for color
    
    # Plot all individual data points
    geom_jitter(aes
                (color = species),    # Species-specific colour
                alpha = 0.3,          # Transparency of the points
                show.legend = FALSE,  # No legend for the points
                position = position_jitter(width = 0.2, seed = 0)) +  # Add jitter with width
    
    # Add significance levels
    geom_signif(comparisons = list(c("Adelie", "Chinstrap"), 
                                   c("Chinstrap", "Gentoo"), 
                                   c("Adelie", "Gentoo")),
                annotations = c("***", "***", "***"),  # Adding the significance stars
                test = "aov",  # Test used
                y_position = c(3.5,3.6,3.8),    # Position of significance stars
                tip_length = 0.01) +            # Short tip lines for neatness
    
    # Apply custom colours
    scale_color_manual(values = custom_colours) +
    
    # Axes labels 
    labs(x = "Species",
         y = "Bill Length-to-Depth Ratio") +
    
    # Themes, sizes and positioning
    theme_minimal() + 
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "none")
}


# Function to create the results plot
results_plot_2 <- function(data) {
  
  # Combine the mean and confidence interval data with the original dataframe to enable plotting together
  combined_data <- analysis_data %>%
    left_join(CIs_bill_depth, by = "species") %>%  # combine bill length CI data
    left_join(CIs_bill_length, by = "species")     # combine bill depth CI data

  # Results plot ...
  ggplot(data = combined_data,
    aes(x = bill_length_mm, # Length on x-axis
        y = bill_depth_mm,  # Depth on y-axis
        color = species,    # Species-specific colour
        shape = species)) + # Species-specific shape
    
    # Plot all individual data points 
    geom_jitter(
      size = 3,       # Point size
      alpha = 0.3) +  # Point transparency
    
    # Plot mean points for bill length and depth
    geom_point(aes
               (x = mean_length,  # Length on x axis
                 y = mean_depth,  # Depth on y axis
                 color = species, # Species-specific colour
                 shape = species),# Species-specific shape
               size = 4) + # Point size
    
    # Add CIs for mean length as error bars
    geom_errorbar(aes
                  (xmin = lower_length_ci,  #
                    xmax = upper_length_ci, #
                    y = mean_depth),        
                  width = 0.2,     #
                  color = "black", #
                  size = 1) +      #
    
    # Add CIs for mean depth as error bars
    geom_errorbar(aes
                  (ymin = lower_depth_ci,  #
                    ymax = upper_depth_ci, #
                    x = mean_length),      #
                  width = 0.4,     #
                  color = "black", #
                  size = 1) +      #
    
    # Axis and legend labels 
    labs(x = "Bill Length (mm)", y = "Bill Depth (mm)", # Axes 
         color = "Species", shape = "Species") +        # Legend
    
    # Apply custom colours
    scale_color_manual(values = custom_colours) +
    # Apply custom shapes
    scale_shape_manual(values = custom_shapes) +
    
    # Themes, sizes and positioning
    theme_minimal() + 
    theme(
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      legend.position = "right")
}

