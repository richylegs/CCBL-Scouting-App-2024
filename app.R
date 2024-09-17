##### Libraries #####
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(bslib)
library(htmltools)
library(mongolite)
library(shinyWidgets)
library(ggplot2)
library(grid)
library(gridExtra)
library(DT)
library(openxlsx)
library(readr)
library(stringr)
library(GeomMLBStadiums)
library(tidyverse)
library(akima)
library(reshape2)
library(patchwork)

##### Usage Tree Functions #####
# List of Colors
pitch_colors <- c(
  "CH/SP" = "#00FF00",            # Green
  Curveball = "#00FFFF",          # Cyan
  Cutter = "#8B4513",             # Brown
  FastSink = "#FF0000",           # Red
  Knuckleball = "#800080",        # Purple
  Other = "grey",                 # Grey
  Slider = "#FFDB58"              # Yellow
)

# Layout Matrix
layout_matrix <- matrix(c(NA, NA, "0-0", NA, "legend", 
                          NA, "1-0", NA, "0-1", NA, 
                          "2-0", NA, "1-1", NA, "0-2", 
                          "3-0", NA, "2-1", NA, "1-2", 
                          NA, "3-1", NA, "2-2", NA, 
                          NA, NA, "3-2", NA, NA), 
                        nrow = 6, ncol = 5, byrow = TRUE)
# Layout Vector
layout_vector <- as.vector(t(layout_matrix))

# Function to extract legend
extract_legend <- function(plot){
  g <- ggplotGrob(plot)
  legend <- g$grobs[which(sapply(g$grobs, function(x) x$name) == "guide-box")]
  return(legend[[1]])
}

# Usage Tree Function
usage_tree <- function(pitcher_name, batter, data_source){
  # Filter the data based on the inputs
  filtered_data <- if(data_source == "NCAA"){
    df_NCAA_tree %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  } else if(data_source == "CCBL"){
    df_CCBL_tree %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  }
  
  # If empty return statement
  if (nrow(filtered_data) == 0){
    plot.new()
    text(0.5, 0.55, "No data available for the selected filters", cex = 1.5, col = "black", font = 2)
    text(0.5, 0.45, "Please select a new combination of inputs", cex = 1.2, col = "black", font = 1)
    return()
  }
  
  # Count occurrences of each Pitch
  pitch_type_counts <- filtered_data %>%
    group_by(Pitch) %>%
    summarise(total = sum(n)) %>%
    arrange(desc(total))
  
  # Ordered Pitch levels
  ordered_pitch_types <- pitch_type_counts$Pitch
  
  # Unique counts
  unique_counts <- unique(filtered_data$Count)
  
  # Create a list to hold the plots
  plot_list <- list()
  
  # Generate a pie chart for each count
  for (count in unique_counts) {
    # Filter data for the specific count
    count_data <- filtered_data %>%
      filter(Count == count)
    
    # Create the pie chart
    plot <- ggplot(count_data, aes(x = "", y = n, fill = Pitch)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = pitch_colors) +
      coord_polar(theta = "y") +
      geom_text(aes(label = ifelse(n / sum(n) >= 0.1, paste0(round(n / sum(n) * 100), "%"), "")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = paste0("Count: ", count, " (n = ", sum(count_data$n), ")"),
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
    
    # Add the plot to the list
    plot_list[[count]] <- plot
  }
  
  # Extract all unique Pitch values from the filtered data
  all_pitch_types <- unique(filtered_data$Pitch)
  
  # Create a dummy plot with all pitch types to extract the legend
  dummy_data <- data.frame(Pitch = ordered_pitch_types, n = 1)
  dummy_plot <- ggplot(dummy_data, aes(x = "", y = n, fill = Pitch)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pitch_colors, breaks = ordered_pitch_types) +
    theme_void() +
    theme(legend.title = element_blank())
  legend <- extract_legend(dummy_plot)
  
  # Create a common title for all plots
  common_title <- textGrob(
    paste("Usage Tree:", pitcher_name, "vs", batter, "handed batters in the", data_source),
    gp = gpar(fontface = "bold", cex = 1.5)
  )
  
  # Convert the plot list to match the layout matrix
  arranged_plots <- lapply(layout_vector, function(position) {
    if (is.na(position)) {
      ggplot() + theme_void()
    } 
    else {
      if (position %in% names(plot_list)) {
        plot_list[[position]]
      } 
      else if(position == "legend"){
        legend
      }
      else {
        ggplot() + theme_void() + labs(title = paste0("Count: ", position," (n = 0)"))
      }
    }
  })
  
  # Combine plots into a grid with a common title
  plot_grid <- gridExtra::grid.arrange(grobs = arranged_plots, ncol = 5, top = common_title)
  
  # Return the combined plot
  return(plot_grid)
}

# Classify Counts
classify_count <- function(count) {
  situations <- list()
  if (count == "0-0") {
    situations <- c(situations, "Total", "0-0", "Even")
  } 
  if (count %in% c("1-1", "2-2")) {
    situations <- c(situations, "Total", "Even")
  } 
  if (count %in% c("1-0", "2-0", "3-0", "2-1", "3-1")) {
    situations <- c(situations, "Total", "Hitters Ahead")
  }
  if (count == "0-1"){
    situations <- c(situations, "Total", "Pitchers Ahead")
  }
  if (count %in% c("0-2", "1-2")) {
    situations <- c(situations, "Total", "Pitchers Ahead", "2 Strikes")
  } 
  if (count == "3-2") {
    situations <- c(situations, "Total", "2 Strikes", "3-2")
  }
  return(situations)
}

# Usage Tree Summary Table Function
usage_tree_summary <- function(pitcher_name, batter, data_source) {
  # Filter the data based on the inputs
  filtered_data <- if (data_source == "NCAA") {
    df_NCAA_tree %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  } else if (data_source == "CCBL") {
    df_CCBL_tree %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  }
  
  # Expand data frame
  expanded_df <- filtered_data %>%
    rowwise() %>%
    mutate(count_class = list(classify_count(Count))) %>%
    unnest(count_class)
  
  # Summary Stats
  summary_stats <- expanded_df %>%
    group_by(count_class) %>%
    summarise(
      P = sum(n),
      FastSinkPct = sum(ifelse(Pitch == "FastSink", n, 0)) / sum(n),
      SliderCutterPct = sum(ifelse(Pitch %in% c("Slider", "Cutter"), n, 0)) / sum(n),
      CurveballPct = sum(ifelse(Pitch == "Curveball", n, 0)) / sum(n),
      ChangeSplitPct = sum(ifelse(Pitch == "CH/SP", n, 0)) / sum(n)
    ) %>%
    rename(
      Situation = count_class,
      `Fastball %` = FastSinkPct,
      `Slider + Cutter %` = SliderCutterPct,
      `Curveball %` = CurveballPct,
      `Changeup %` = ChangeSplitPct
    ) %>%
    mutate(
      Situation = factor(Situation, levels = c("Total", "0-0", "Even", "Hitters Ahead", "Pitchers Ahead", "2 Strikes", "3-2"))
    ) %>%
    arrange(Situation)
  
  # Datatable
  dt <- datatable(summary_stats, options = list(pageLength = 7, autoWidth = TRUE, dom = 't'))
  
  # Colours
  color_intervals <- c(0.4, 0.6)
  colors <- c('white', '#FFEAEB', '#FFC1C3')
  
  # Conditional formatting
  dt <- dt %>%
    formatStyle(
      columns = c('Fastball %', 'Slider + Cutter %', 'Curveball %', 'Changeup %'),
      backgroundColor = styleInterval(color_intervals, colors)
    )
  
  # Percentage formatting
  dt <- dt %>%
    formatPercentage(
      columns = c('Fastball %', 'Slider + Cutter %', 'Curveball %', 'Changeup %'),
      digits = 0
    )
  
  return(dt)
}

##### Pitch Sequencing Function #####
sequencing <- function(pitcher_name, batter, data_source){
  
  # Filter the data based on the inputs
  filtered_data <- if(data_source == "NCAA"){
    df_NCAA_sequence %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  } else if(data_source == "CCBL"){
    df_CCBL_sequence %>%
      filter(Pitcher == pitcher_name, BatterSide == batter)
  }
  
  # If empty return statement
  if (nrow(filtered_data) == 0){
    plot.new()
    text(0.5, 0.55, "No data available for the selected filters", cex = 1.5, col = "black", font = 2)
    text(0.5, 0.45, "Please select a new combination of inputs", cex = 1.2, col = "black", font = 1)
    return()
  }
  
  # Ordered Pitch levels
  ordered_pitch_types <- filtered_data %>%
    group_by(Pitch) %>%
    summarise(total = sum(count)) %>%
    arrange(desc(total)) %>%
    pull(Pitch)
  
  # Unique PrevPitch
  unique_prev_pitch <- unique(filtered_data$PrevPitch)
  
  # Create a list to hold the plots
  plot_list <- list()
  
  # Generate a pie chart for each PrevPitch
  for (prev_pitch in unique_prev_pitch) {
    # Filter data for the specific pitch
    pitch_data <- filtered_data %>%
      filter(PrevPitch == prev_pitch)
    
    # Skip if no data
    if (nrow(pitch_data) == 0) {
      next
    }
    
    # Create the pie chart
    plot <- ggplot(pitch_data, aes(x = "", y = count, fill = Pitch)) +
      geom_bar(stat = "identity", width = 1) +
      scale_fill_manual(values = pitch_colors) +
      coord_polar(theta = "y") +
      geom_text(aes(label = ifelse(count / sum(count) >= 0.05, paste0(round(count / sum(count) * 100), "%"), "")), 
                position = position_stack(vjust = 0.5)) +
      labs(title = paste0("Previous Pitch ", prev_pitch, " (n = ", sum(pitch_data$count), ")"),
           x = NULL, y = NULL) +
      theme_void() +
      theme(legend.position = "none")
    
    # Add the plot to the list
    plot_list[[prev_pitch]] <- plot
  }
  
  # Create a dummy plot with all pitch types to extract the legend
  dummy_data <- data.frame(Pitch = ordered_pitch_types, n = 1)
  dummy_plot <- ggplot(dummy_data, aes(x = "", y = n, fill = Pitch)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = pitch_colors, breaks = ordered_pitch_types) +
    theme_void() +
    theme(legend.title = element_blank())
  
  legend <- cowplot::get_legend(dummy_plot)
  
  # Title for all plots
  common_title <- textGrob(
    paste("Pitch Sequencing: ", pitcher_name, "vs", batter, "handed batters in the", data_source),
    gp = gpar(fontface = "bold", cex = 1.5)
  )
  
  # Layout
  layout_order <- c(ordered_pitch_types, "legend")
  
  # Convert the plot list to match the layout
  arranged_plots <- lapply(layout_order, function(position) {
    if (position %in% names(plot_list)) {
      plot_list[[position]]
    } 
    else if(position == "legend"){
      legend
    }
    else{
      NULL
    }
  })

  # Remove Null elements
  arranged_plots <- arranged_plots[!sapply(arranged_plots, is.null)]
    
  # Combine plots into a grid with a common title
  combined_plot <- grid.arrange(grobs = arranged_plots, ncol = 3, top = common_title)
  
  # Return the combined plot
  return(combined_plot)
}

##### Pitch Breaks/Location Functions #####
# Pitch Breaks
pitch_breaks <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All") {
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher) %>%
    {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(HorzBreak, InducedVertBreak, color = RelSpeed)) +
    geom_point(na.rm = TRUE, alpha = 0.9, size = 2.5) +
    scale_x_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) +
    scale_y_continuous(limits = c(-30, 30), breaks = seq(-30, 30, 10)) +
    scale_color_gradient(low = "blue", high = "red") +
    geom_hline(yintercept = 0, linetype = 2) + 
    geom_vline(xintercept = 0, linetype = 2) + 
    theme_bw() + 
    labs(title = "Pitch Movement", 
         y = "Induced Vertical Break", 
         x = "Horizontal Break", 
         color = "Release Speed (mph)") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) + 
    theme(legend.position = "right",
          legend.text = element_text(size = 12)) +
    coord_fixed() +
    guides(color = guide_colorbar(barheight = unit(5, "cm"),
                                  barwidth = unit(1, "cm")))
  
  return(plot)
}

# Pitch Locations
pitch_locations <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All") {
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher) %>%
    {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  
  # Add batter box
  batter_stance <- if(BatterHand == "All") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(PlateLocSide, PlateLocHeight, color = RelSpeed)) +
    geom_point(na.rm = TRUE, alpha = 0.9, size = 2.5) +
    scale_color_gradient(low = "blue", high = "red") +
    theme_bw() + 
    labs(title = "Pitch Locations", 
         x = "Plate Location Side (pitcher view)",
         y = "Plate Location Height") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
          legend.position = "none") + 
    coord_fixed() +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = 1.535, ymax = 3.371),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5)
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

convert_tilt_to_degrees <- function(tilt) {
  if (is.na(tilt)) return(NA)
  
  parts <- str_split(tilt, ":")[[1]]
  hour <- as.numeric(parts[1])
  minute <- as.numeric(parts[2])
  
  # Convert to degrees
  degree <- ((hour %% 12) * 30) + (minute * 0.5)
  return(degree)
}

convert_degrees_to_clock <- function(degrees) {
  if (is.na(degrees)) return(NA)
  
  hour <- floor(degrees / 30)
  minute <- round((degrees %% 30) / 0.5)
  
  # Ensure minute is two digits
  minute <- str_pad(minute, 2, pad = "0")
  
  return(paste0(hour, ":", minute))
}

adjust_degrees <- function(degrees) {
  degrees <- na.omit(degrees)
  
  if (length(degrees) == 0) {
    return(NA)
  }
  
  if (any(degrees > 300) & any(degrees < 60)) {
    degrees <- ifelse(degrees < 60, degrees + 360, degrees)
  }
  mean_degrees <- mean(degrees, na.rm = TRUE)
  if (mean_degrees > 360) {
    mean_degrees <- mean_degrees - 360
  }
  return(mean_degrees)
}

pitch_breaks_summary <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher) %>%
    {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  
  if(nrow(filtered_df) == 0){
    return()
  }
  
  summary_stats <- filtered_df %>%
    group_by(Pitch) %>%
    summarise(
      Pitches = n(),
      `Velo Range` = paste0(round(quantile(RelSpeed, 0.25, na.rm = TRUE),0), "-", round(quantile(RelSpeed, 0.75, na.rm = TRUE),0)),
      `Max Velo` = round(quantile(RelSpeed, 0.95, na.rm = TRUE),0),
      MeanDegrees = adjust_degrees(sapply(Tilt, convert_tilt_to_degrees)),
      #Tilt = convert_degrees_to_clock(MeanDegrees),
      Spin = round(mean(SpinRate, na.rm = TRUE), 0),
      IVB = round(mean(InducedVertBreak, na.rm = TRUE),1),
      Horz = round(mean(HorzBreak, na.rm = TRUE),1),
      VAA = round(mean(VertApprAngle, na.rm = TRUE),2),
      `Release Height` = round(mean(RelHeight, na.rm = TRUE),2),
      Extension = round(mean(Extension, na.rm = TRUE),2)
    ) %>%
    select(-MeanDegrees) %>%
    arrange(desc(Pitches))
  
  datatable(summary_stats, options = list(pageLength = 8, autoWidth = TRUE, dom = 't'))
}

##### Damage Functions #####
stat_by_zone_pitcher <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All", Statistic) {
  # Filter Data
  if (Statistic == "OPS"){
    filtered_data <- df_CCBL_breaks %>%
      filter(Pitcher == !!Pitcher, !is.na(OBP_Value)) %>%
      {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  } else if (Statistic == "SLG"){
    filtered_data <- df_CCBL_breaks %>%
      filter(Pitcher == !!Pitcher, !is.na(SLG_Value)) %>%
      {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  } else if (Statistic == "wOBA"){
    filtered_data <- df_CCBL_breaks %>%
      filter(Pitcher == !!Pitcher, !is.na(wOBA_Value)) %>%
      {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  } else if (Statistic == "wOBAcon"){
    filtered_data <- df_CCBL_breaks %>%
      filter(Pitcher == !!Pitcher, !is.na(wOBAcon_Value)) %>%
      {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  } else if (Statistic == "AVG"){
    filtered_data <- df_CCBL_breaks %>%
      filter(Pitcher == !!Pitcher, !is.na(AVG_Value)) %>%
      {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  }
  
  # Strike Zone Height
  sz_bot <- 1.535
  sz_top <- 3.371
  top_third <- sz_top - ((sz_top - sz_bot)/3)
  bot_third <- sz_bot + ((sz_top - sz_bot)/3)
  mid_zone <- sz_bot + ((sz_top - sz_bot)/2)
  
  # Shadow Zone Height
  bot_shadow <- sz_bot - 5/12
  top_shadow <- sz_top + 5/12
  
  # Zones
  zones <- data.frame(
    xmin = c(-0.71, -0.237, 0.237, -0.71, -0.237, 0.237, -0.71, -0.237, 0.237,
             -1.13, -0.71, 0.71, 0, -1.13, -0.71, 0.71, 0),
    xmax = c(-0.237, 0.237, 0.71, -0.237, 0.237, 0.71, -0.237, 0.237, 0.71,
             -0.71, 0, 1.13, 0.71, -0.71, 0, 1.13, 0.71),
    ymin = c(top_third, top_third, top_third, bot_third, bot_third, bot_third, sz_bot, sz_bot, sz_bot,
             mid_zone, sz_top, mid_zone, sz_top, bot_shadow, bot_shadow, bot_shadow, bot_shadow),
    ymax = c(sz_top, sz_top, sz_top, top_third, top_third, top_third, bot_third, bot_third, bot_third,
             top_shadow, top_shadow, top_shadow, top_shadow, mid_zone, sz_bot, mid_zone,sz_bot),
    Zone = c(1:9, 11, 11, 12, 12, 13, 13, 14, 14)
  )
  
  # Segments
  segments <- data.frame(
    x = c(0, 0, -1.13, -1.13, -1.13, -1.13, 0, 0, 0, 1.13, 1.13, 1.13, 1.13, 0),
    y = c(sz_top, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_top, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow),
    xend = c(0, -1.13, -1.13, -0.71, -1.13, 0, 0, 0, 1.13, 1.13, 0.71, 1.13, 0, 0),
    yend = c(top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_bot, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_bot)
  )
  
  # Assign Zones to Data
  filtered_data$Zone <- apply(filtered_data, 1, function(row) {
    get_zone(as.numeric(row['PlateLocHeight']), as.numeric(row['PlateLocSide']), zones)
  })
  
  # Group Data
  if (Statistic == "OPS"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(OBP_Value, na.rm = T) + mean(SLG_Value, na.rm = T),
                count = n())
  } else if(Statistic == "SLG"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(SLG_Value, na.rm = T),
                count = n())
  } else if(Statistic == "wOBA"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(wOBA_Value, na.rm = T),
                count = n())
  } else if(Statistic == "wOBAcon"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(wOBAcon_Value, na.rm = T),
                count = n())
  } else if(Statistic == "AVG"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(AVG_Value, na.rm = T),
                count = n())
  }
  
  # Assign Stat to each Zone
  zones <- merge(zones, display_stat, by = "Zone", all.x = TRUE)
  
  # Assign count = 0 if NA
  zones$count[is.na(zones$count)] <- 0
  
  # Separate Strike and Shadow Zones
  strike_zones <- zones[zones$Zone <= 9, ]
  shadow_zones <- zones[zones$Zone > 9, ]
  
  # Chart
  ggplot() +
    geom_rect(data = strike_zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone_stat), alpha = 0.5) +
    geom_rect(data = shadow_zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone_stat), alpha = 0.5) +
    geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), color = "black") +
    scale_fill_gradient2(low = "#257ca3", mid = "white", high = "#db1414", midpoint = mean(display_stat$zone_stat, na.rm = TRUE)) +
    xlim(c(-1.5, 1.5)) +
    ylim(c(0, 5)) +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    labs(title = paste0(Statistic, " Against by Zone"),
         x = "Plate Location Side (pitcher view)",
         y = "Plate Location Height") +
    theme_classic() +
    theme(legend.position = "None",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_text(data = zones %>% distinct(Zone, .keep_all = TRUE), 
              aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 + 0.1, label = paste("n:", count)),
              vjust = 1.5, hjust = 0.5) +
    geom_text(data = zones %>% distinct(Zone, .keep_all = TRUE), 
              aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 - 0.1, label = paste(sprintf("%.3f", zone_stat))),
              vjust = 1.5, hjust = 0.5, fontface = "bold")
}

exit_velo_against <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher, !is.na(ExitSpeed), PitchCall == "InPlay") %>%
    {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  
  # Add batter box
  batter_stance <- if(BatterHand == "All") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- 1.535
  sz_top <- 3.371
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = ExitVeloBin)) +
    geom_point(size = 3) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Exit Velo") +
    theme_minimal() +
    ggtitle("Exit Velocity") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    scale_color_manual(
      values = c("< 85 MPH" = "blue", "85-95 MPH" = "orange", "95+ MPH" = "red"),
      limits = c("< 85 MPH", "85-95 MPH", "95+ MPH")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

batted_ball_plot_against <- function(Pitcher, BatterHand = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher, !is.na(ExitSpeed), PitchCall == "InPlay") %>%
    {if (BatterHand != "All") filter(., BatterSide == !!BatterHand) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .}
  
  # Add batter box
  batter_stance <- if(BatterHand == "All") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- 1.535
  sz_top <- 3.371
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = ContactType)) +
    geom_point(size = 3) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Contact Type") +
    theme_minimal() +
    ggtitle("Batted Ball Type") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    scale_color_manual(
      values = c("Pop Up" = "purple", "Fly Ball" = "red", "Line Drive" = "orange", "Ground Ball" = "green"),
      limits = c("Pop Up", "Fly Ball", "Line Drive", "Ground Ball")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

##### Pitcher Report Functions #####
pitch_locations_heat <- function(Pitcher, BatterHand, PitchType) {
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher) %>%
    filter(BatterSide == !!BatterHand) %>%
    filter(PitchSimple == !!PitchType)
  
  # Add batter box
  batter_stance <- if(BatterHand == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(BatterHand == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # IF empty
  if (nrow(filtered_df) == 0){
    plot <- ggplot() +
      ylim(0,5) +
      xlim(-3, 3) +
      annotate("rect", xmin = -1, xmax = 1,
               ymin = 1.6, ymax = 3.4,
               fill = NA, color = "black") +
      labs(title = paste0(PitchType, " Usage"), x = "Plate Location Side (pitcher view)", y = "Plate Location Height") + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
            strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
            strip.text = element_text(size = 15),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
            panel.background = element_rect(fill = "white")) +
      coord_fixed() +
      geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend), color = "black")
    
    # Add Batter Stance
    if (!is.null(batter_stance)) {
      plot <- plot + batter_stance
    }
    
    return(plot)
  }
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(PlateLocSide, PlateLocHeight)) +
    geom_point(na.rm = TRUE) +
    stat_density2d(geom = 'raster', aes(fill = after_stat(density)), contour = F, na.rm = TRUE) +
    ylim(0,5) +
    xlim(-3, 3) +
    scale_fill_gradientn(colours = c(
      "lightblue",
      "#f7f7f7",
      "#fff7bc",
      "#fee391",
      "#fec44f",
      "#fe9929",
      "#ec7014",
      "#cc4c02",
      "#993404",
      "#662506"
    )) + # change color scheme
    annotate("rect", xmin = -1, xmax = 1,
             ymin = 1.6, ymax = 3.4,
             fill = NA, color = "black") +
    labs(title = paste0(PitchType, " Usage"), x = "Plate Location Side (pitcher view)", y = "Plate Location Height") + 
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = "20"),
          strip.background = element_rect(fill = "white", color = "black", linewidth = 2),
          strip.text = element_text(size = 15),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 2),
          panel.background = element_rect(fill = "lightblue")
    ) +
    guides(fill = "none") +
    coord_fixed() +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black")
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

pitch_stat_summary <- function(Pitcher){
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher)
  
  summary_stats <- filtered_df %>%
    summarise(
      IP = round(sum(PlayResult %in% c("CaughtStealing","FieldersChoice","Out","Sacrifice Bunt","Sacrifice Fly","StrikeOut")/3),0),
      Hits = sum(PlayResult %in% c("Single","Double","Triple","HomeRun")),
      SO = sum(PlayResult %in% c("StrikeOut")),
      BB = sum(PlayResult %in% c("Walk")),
      `SO/BB` = ifelse(sum(PlayResult %in% c("Walk")) == 0,NA,round(sum(PlayResult %in% c("StrikeOut"))/sum(PlayResult %in% c("Walk")),1))
    )
  
  datatable(summary_stats, options = list(pageLength = 1, autoWidth = TRUE, dom = 't'))
}

velo_summary <- function(Pitcher){
  # Filter Data
  filtered_df <- df_CCBL_breaks %>%
    filter(Pitcher == !!Pitcher)
  
  summary_stats <- filtered_df %>%
    group_by(PitchSimple) %>%
    summarise(
      Range = paste0(round(quantile(RelSpeed, 0.25, na.rm = TRUE),0), "-", round(quantile(RelSpeed, 0.75, na.rm = TRUE),0)),
      Max = round(quantile(RelSpeed, 0.95, na.rm = TRUE),0),
      SLG = round(mean(SLG_Value, na.rm = TRUE),3)
    ) %>%
    mutate(
      SLG = sprintf("%.3f", SLG)
    ) %>%
    arrange(desc(Max))
  
  datatable(summary_stats, options = list(pageLength = 4, autoWidth = TRUE, dom = 't'))
}

##### Swing Decisions/Whiffs Functions #####
swing_decision <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_sd %>%
    filter(Batter == !!Batter) %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  
  # If empty return statement
  if (nrow(filtered_df) == 0){
    plot.new()
    text(0.5, 0.55, "No data available for the selected filters", cex = 1.5, col = "black", font = 2)
    text(0.5, 0.45, "Please select a new combination of inputs", cex = 1.2, col = "black", font = 1)
    return()
  }
  
  # Batter Side
  bat_side <- unique(filtered_df$BatterSide)
  
  # Add batter box
  batter_stance <- if(length(bat_side) == 2) {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- filtered_df$SZ_Bot[1]
  sz_top <- filtered_df$SZ_Top[1]
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = SwingDecision)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Take" = "blue", "Swing" = "red")) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Swing Decision") +
    theme_minimal() +
    ggtitle("Pitch Locations and Swing Decisions") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

# Whiffs Function
whiffs <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_sd %>%
    filter(Batter == !!Batter, SwingDecision == "Swing") %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .} %>%
    mutate(
      PitchCall = case_when(
        PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable") ~ "Foul Ball",
        PitchCall == "StrikeSwinging" ~ "Whiff",
        PitchCall == "InPlay" ~ "In Play",
        TRUE ~ PitchCall
      )
    )
  
  # Batter Side
  bat_side <- unique(filtered_df$BatterSide)
  
  # Add batter box
  batter_stance <- if(length(bat_side) == 2) {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- filtered_df$SZ_Bot[1]
  sz_top <- filtered_df$SZ_Top[1]
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Whiff" = "red", "Foul Ball" = "blue", "In Play" = "green")) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Swing Result") +
    theme_minimal() +
    ggtitle("Swing Results") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)  
}

# Swing Decision Summary Table Function
swing_decision_summary <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All") {
  # Filter Data
  filtered_data <- df_sd %>%
    filter(Batter == !!Batter) %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .} %>%
    mutate(
      PitchCall = case_when(
        PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable") ~ "Foul Ball",
        PitchCall == "StrikeSwinging" ~ "Whiff",
        PitchCall == "InPlay" ~ "In Play",
        TRUE ~ PitchCall
      )
    )
  
  # Strike Zone Height
  sz_bot <- filtered_data$SZ_Bot[1]
  sz_top <- filtered_data$SZ_Top[1]
  
  # Calculate Num PA (Plate Appearances with Balls == 0 and Strikes == 0)
  num_pa <- filtered_data %>%
    filter(Balls == 0, Strikes == 0) %>%
    nrow()
  
  # Summary Stats
  summary_stats <- filtered_data %>%
    summarise(
      `Num Pitches` = n(),
      `Num Swings` = sum(SwingDecision == "Swing"),
      `O-Swing %` = round(sum(SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(!(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100, 1),
      `Z-Swing %` = round(sum(SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100, 1),
      `Swing %` = round(sum(SwingDecision == "Swing", na.rm = TRUE) / n() * 100, 1),
      `O-Contact %` = round(sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100, 1),
      `Z-Contact %` = round(sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100, 1),
      `Contact %` = round(sum(PitchCall == "Foul Ball" | PitchCall == "In Play", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100, 1),
      `Whiff %` = round(sum(PitchCall == "Whiff", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100, 1),
      `First Pitch Swing %` = round(sum(SwingDecision == "Swing" & Balls == 0 & Strikes == 0, na.rm = TRUE) / sum(Balls == 0 & Strikes == 0, na.rm = TRUE) * 100, 1),
      `Zone %` = round(sum(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / n() * 100, 1),
      `SwStr %` = round(sum(PitchCall == "Whiff", na.rm = TRUE) / n() * 100, 1),
      `CStr %` = round(sum(PitchCall == "StrikeCalled", na.rm = TRUE) / n() * 100, 1),
      `CSW %` = round((sum(PitchCall == "Whiff", na.rm = TRUE) + sum(PitchCall == "StrikeCalled", na.rm = TRUE)) / n() * 100, 1)
    ) %>%
    mutate(
      `O-Swing %` = sprintf("%.1f%%", `O-Swing %`),
      `Z-Swing %` = sprintf("%.1f%%", `Z-Swing %`),
      `Swing %` = sprintf("%.1f%%", `Swing %`),
      `O-Contact %` = sprintf("%.1f%%", `O-Contact %`),
      `Z-Contact %` = sprintf("%.1f%%", `Z-Contact %`),
      `Contact %` = sprintf("%.1f%%", `Contact %`),
      `Whiff %` = sprintf("%.1f%%", `Whiff %`),
      `First Pitch Swing %` = sprintf("%.1f%%", `First Pitch Swing %`),
      `Zone %` = sprintf("%.1f%%", `Zone %`),
      `SwStr %` = sprintf("%.1f%%", `SwStr %`),
      `CStr %` = sprintf("%.1f%%", `CStr %`),
      `CSW %` = sprintf("%.1f%%", `CSW %`)
    )
  
  # If Empty
  if (nrow(summary_stats) == 0) {
    summary_stats <- data.frame(
      `Num Pitches` = 0,
      `Num Swings` = 0,
      `O-Swing %` = "0.0%",
      `Z-Swing %` = "0.0%",
      `Swing %` = "0.0%",
      `O-Contact %` = "0.0%",
      `Z-Contact %` = "0.0%",
      `Contact %` = "0.0%",
      `Whiff %` = "0.0%",
      `First Pitch Swing %` = "0.0%",
      `Zone %` = "0.0%"
    )
  }
  
  datatable(summary_stats, options = list(pageLength = 1, autoWidth = TRUE, dom = 't'))
}

##### Batter Zone by Zone Functions #####
# Assign Zones
get_zone <- function(height, side, zones) {
  
  primary_zones <- zones[zones$Zone <= 9, ]
  shadow_zones <- zones[zones$Zone > 9, ]
  
  match_primary <- primary_zones %>%
    filter(height > ymin & height <= ymax & side > xmin & side <= xmax)
  
  if (nrow(match_primary) == 1) {
    return(match_primary$Zone)
  } else {
    match_shadow <- shadow_zones %>%
      filter(height > ymin & height <= ymax & side > xmin & side <= xmax)
    
    if (nrow(match_shadow) == 1) {
      return(match_shadow$Zone)
    } else {
      return(NA)
    }
  }
}

stat_by_zone <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All", Statistic) {
  # Filter Data
  if (Statistic == "OPS"){
    filtered_data <- df_sd %>%
      filter(Batter == !!Batter, !is.na(OBP_Value)) %>%
      {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
      {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  } else if (Statistic == "SLG"){
    filtered_data <- df_sd %>%
      filter(Batter == !!Batter, !is.na(SLG_Value)) %>%
      {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
      {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  } else if (Statistic == "wOBA"){
    filtered_data <- df_sd %>%
      filter(Batter == !!Batter, !is.na(wOBA_Value)) %>%
      {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
      {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  } else if (Statistic == "wOBAcon"){
    filtered_data <- df_sd %>%
      filter(Batter == !!Batter, !is.na(wOBAcon_Value)) %>%
      {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
      {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  } else if (Statistic == "AVG"){
    filtered_data <- df_sd %>%
      filter(Batter == !!Batter, !is.na(AVG_Value)) %>%
      {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
      {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
      {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
      {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  }
  
  # Strike Zone Height
  sz_bot <- filtered_data$SZ_Bot[1]
  sz_top <- filtered_data$SZ_Top[1]
  top_third <- sz_top - ((sz_top - sz_bot)/3)
  bot_third <- sz_bot + ((sz_top - sz_bot)/3)
  mid_zone <- sz_bot + ((sz_top - sz_bot)/2)
  
  # Shadow Zone Height
  bot_shadow <- sz_bot - 5/12
  top_shadow <- sz_top + 5/12
  
  # Zones
  zones <- data.frame(
    xmin = c(-0.71, -0.237, 0.237, -0.71, -0.237, 0.237, -0.71, -0.237, 0.237,
             -1.13, -0.71, 0.71, 0, -1.13, -0.71, 0.71, 0),
    xmax = c(-0.237, 0.237, 0.71, -0.237, 0.237, 0.71, -0.237, 0.237, 0.71,
             -0.71, 0, 1.13, 0.71, -0.71, 0, 1.13, 0.71),
    ymin = c(top_third, top_third, top_third, bot_third, bot_third, bot_third, sz_bot, sz_bot, sz_bot,
             mid_zone, sz_top, mid_zone, sz_top, bot_shadow, bot_shadow, bot_shadow, bot_shadow),
    ymax = c(sz_top, sz_top, sz_top, top_third, top_third, top_third, bot_third, bot_third, bot_third,
             top_shadow, top_shadow, top_shadow, top_shadow, mid_zone, sz_bot, mid_zone,sz_bot),
    Zone = c(1:9, 11, 11, 12, 12, 13, 13, 14, 14)
  )
  
  # Segments
  segments <- data.frame(
    x = c(0, 0, -1.13, -1.13, -1.13, -1.13, 0, 0, 0, 1.13, 1.13, 1.13, 1.13, 0),
    y = c(sz_top, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_top, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow),
    xend = c(0, -1.13, -1.13, -0.71, -1.13, 0, 0, 0, 1.13, 1.13, 0.71, 1.13, 0, 0),
    yend = c(top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_bot, top_shadow, top_shadow, mid_zone, mid_zone, bot_shadow, bot_shadow, sz_bot)
  )
  
  # Assign Zones to Data
  filtered_data$Zone <- apply(filtered_data, 1, function(row) {
    get_zone(as.numeric(row['PlateLocHeight']), as.numeric(row['PlateLocSide']), zones)
  })
  
  # Group Data
  if (Statistic == "OPS"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(OBP_Value, na.rm = T) + mean(SLG_Value, na.rm = T),
                count = n())
  } else if(Statistic == "SLG"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(SLG_Value, na.rm = T),
                count = n())
  } else if(Statistic == "wOBA"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(wOBA_Value, na.rm = T),
                count = n())
  } else if(Statistic == "wOBAcon"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(wOBAcon_Value, na.rm = T),
                count = n())
  } else if(Statistic == "AVG"){
    display_stat <- filtered_data %>%
      group_by(Zone) %>%
      summarise(zone_stat = mean(AVG_Value, na.rm = T),
                count = n())
  }
  
  # Assign Stat to each Zone
  zones <- merge(zones, display_stat, by = "Zone", all.x = TRUE)
  
  # Assign count = 0 if NA
  zones$count[is.na(zones$count)] <- 0
  
  # Separate Strike and Shadow Zones
  strike_zones <- zones[zones$Zone <= 9, ]
  shadow_zones <- zones[zones$Zone > 9, ]
  
  # Chart
  ggplot() +
    geom_rect(data = strike_zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone_stat), alpha = 0.5) +
    geom_rect(data = shadow_zones, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = zone_stat), alpha = 0.5) +
    geom_segment(data = segments, aes(x = x, y = y, xend = xend, yend = yend), color = "black") +
    scale_fill_gradient2(low = "#257ca3", mid = "white", high = "#db1414", midpoint = mean(display_stat$zone_stat, na.rm = TRUE)) +
    xlim(c(-1.5, 1.5)) +
    ylim(c(0, 5)) +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    labs(title = paste0(Statistic, " by Zone"),
         x = "Plate Location Side (pitcher view)",
         y = "Plate Location Height") +
    theme_classic() +
    theme(legend.position = "None",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    geom_text(data = zones %>% distinct(Zone, .keep_all = TRUE), 
              aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 + 0.1, label = paste("n:", count)),
              vjust = 1.5, hjust = 0.5) +
    geom_text(data = zones %>% distinct(Zone, .keep_all = TRUE), 
              aes(x = (xmin + xmax) / 2, y = (ymin + ymax) / 2 - 0.1, label = paste(sprintf("%.3f", zone_stat))),
              vjust = 1.5, hjust = 0.5, fontface = "bold")
}
##### Batter Batted Ball Plot Functions #####
# Exit Velo Plot Function
exit_velo <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_sd %>%
    filter(Batter == !!Batter, !is.na(ExitSpeed), PitchCall == "InPlay") %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  
  # Batter Side
  bat_side <- unique(filtered_df$BatterSide)
  
  # Add batter box
  batter_stance <- if(length(bat_side) == 2) {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- filtered_df$SZ_Bot[1]
  sz_top <- filtered_df$SZ_Top[1]
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = ExitVeloBin)) +
    geom_point(size = 3) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Exit Velo") +
    theme_minimal() +
    ggtitle("Exit Velocity") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend),
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    scale_color_manual(
      values = c("< 85 MPH" = "blue", "85-95 MPH" = "orange", "95+ MPH" = "red"),
      limits = c("< 85 MPH", "85-95 MPH", "95+ MPH")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

# Batted Ball Type Function
batted_ball_plot <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All"){
  # Filter Data
  filtered_df <- df_sd %>%
    filter(Batter == !!Batter, !is.na(ContactType), PitchCall == "InPlay") %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  
  # Batter Side
  bat_side <- unique(filtered_df$BatterSide)
  
  # Add batter box
  batter_stance <- if(length(bat_side) == 2) {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold"),
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Right") {
    list(
      # Right-handed batter box
      annotate("rect", xmin = 1.5, xmax = 2, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = 1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  } else if(length(bat_side) == 1 & bat_side[1] == "Left") {
    list(
      # Left-handed batter box
      annotate("rect", xmin = -2, xmax = -1.5, ymin = 0, ymax = 4.5, alpha = 0.2, fill = "darkgrey"),
      annotate("text", x = -1.75, y = seq(4, 1, length.out = 6), label = strsplit("BATTER", "")[[1]], color = "black", angle = 0, size = 5, fontface = "bold")
    )
  }
  
  # Strike Zone Height
  sz_bot <- filtered_df$SZ_Bot[1]
  sz_top <- filtered_df$SZ_Top[1]
  
  # Plot Data
  plot <- ggplot(filtered_df, aes(x = PlateLocSide, y = PlateLocHeight, color = ContactType)) +
    geom_point(size = 3) +
    labs(x = "Plate Location Side (pitcher view)", y = "Plate Location Height", color = "Contact Type") +
    theme_minimal() +
    ggtitle("Batted Ball Type") +
    geom_rect(aes(xmin = -0.71, xmax = 0.71, ymin = sz_bot, ymax = sz_top),
              color = "black", fill = NA) +
    geom_segment(data = home_plate_segments, aes(x = x, y = y, xend = xend, yend = yend), 
                 color = "black") +
    xlim(-2,2) +
    ylim(-0.5,5) +
    scale_color_manual(
      values = c("Pop Up" = "purple", "Fly Ball" = "red", "Line Drive" = "orange", "Ground Ball" = "green"),
      limits = c("Pop Up", "Fly Ball", "Line Drive", "Ground Ball")) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  # Add Batter Stance
  if (!is.null(batter_stance)) {
    plot <- plot + batter_stance
  }
  
  return(plot)
}

# Infield Spray Segments
infield_segments <- data.frame(
  x = c(0, 0, 0, 0, 0, 0, -120, -85, -25, 25, 85),
  y = c(-20, -20, -20, -20, -20, -20, 120, 175, 210, 210, 175),
  xend = c(-120, 120, -85, -25, 85, 25, -85, -25, 25, 85, 120),
  yend = c(120, 120, 175, 210, 175, 210, 175, 210, 210, 175, 120)
)

# Infield Spray
infield_spray <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All") {
  # Filter Data
  filtered_data <- df_sd %>%
    filter(Batter == !!Batter, ContactType == "Ground Ball", PlayResult != "Sacrifice Bunt", Bearing < 100, Bearing > -100) %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == !!PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .}
  
  # Count
  count_observations <- nrow(filtered_data)
  
  # Prepare Data
  temp <- filtered_data %>%
    mutate(
      GB_Zone = case_when(
        between(Bearing, -51, -30) ~ 1,
        between(Bearing, -30, -10) ~ 2,
        between(Bearing, -10, 10) ~ 3,
        between(Bearing, 10, 30) ~ 4,
        between(Bearing, 30, 51) ~ 5
      )
    ) %>%
    group_by(GB_Zone) %>%
    summarise(
      zone_rate = round(n() / nrow(filtered_data) * 100)
    ) %>%
    as.data.frame()
  
  # Zones
  zone_data <- data.frame(
    GB_Zone = 1:5,
    x = c(-75, -45, 0, 45, 75),
    y = c(100, 150, 175, 150, 100)
  )
  
  # Merge
  zone_data <- merge(zone_data, temp, by = "GB_Zone", all.x = TRUE)
  zone_data$zone_rate[is.na(zone_data$zone_rate)] <- 0
  
  # Plot
  plot <- ggplot() +
    geom_segment(data = infield_segments, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_point(data = zone_data, aes(x = x, y = y, fill = zone_rate), size = 15, shape = 21) +
    geom_text(data = zone_data, aes(x = x, y = y, label = paste0(zone_rate, "%")), color = "black") +
    scale_fill_gradient(low = "white", high = "#8bc462", na.value = "white") +
    ggtitle(paste0("Infield Spray Chart\n", "n = ", count_observations)) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold"),
      axis.text.x = element_blank(), 
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none"
    ) +
    coord_fixed()
  
  return(plot)
}

# Outfield Spray
outfield_spray <- function(Batter, PitcherSide = "All", PitchType = "All", Count = "All") {
  # Filter Data
  temp <- df_sd %>%
    filter(Batter == !!Batter, Angle > 10, Distance >= 130, PitchCall == "InPlay", PlayResult != "Sacrifice Bunt", Bearing < 100, Bearing > -100) %>%
    {if (PitcherSide != "All") filter(., PitcherThrows == !!PitcherSide) else .} %>%
    {if (PitchType != "All") filter(., Pitch == !!PitchType) else .} %>%
    {if (Count == "2 Strikes") filter(., Strikes == 2) else .} %>%
    {if (Count == "First Pitch") filter(., Strikes == 0 & Balls == 0) else .} %>% 
    mutate(Bearing_rad = Bearing * pi / 180,
           hc_x = sin(Bearing_rad) * Distance,  
           hc_y = cos(Bearing_rad) * Distance)
  
  # Plot
  plot <- ggplot(temp, aes(x = hc_x, y = hc_y)) +
    geom_spraychart(stadium_transform_coords = TRUE, stadium_segments = "all") +
    coord_fixed() +
    ggtitle(paste0("Outfield Spray Chart\n", "n = ", nrow(temp))) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, vjust = 1, face = "bold"))
  
  return(plot)
}

##### Leaderboard Functions #####
# Hitter Leaderboard
hitter_leaderboard <- function(min_pa, Team = "All") {
  # Filter Data by Team
  filtered_data <- df_CCBL_leaderboards %>%
    {if (Team != "All") filter(., toupper(substr(Team, 1, 3)) == toupper(substr(BatterTeam, 1, 3))) else .}
  
  # Calculate Plate Appearances
  pa_data <- filtered_data %>%
    group_by(Batter) %>%
    summarise(`PA` = sum(Balls == 0 & Strikes == 0)) %>%
    filter(`PA` >= min_pa)
  
  # Join PAs
  filtered_data <- filtered_data %>%
    inner_join(pa_data, by = "Batter")
  
  # Summary Stats
  summary_stats <- filtered_data %>%
    group_by(Batter) %>%
    summarise(
      `PA` = first(PA),
      `AVG` = mean(AVG_Value, na.rm = TRUE),
      `OBP` = mean(OBP_Value, na.rm = TRUE),
      `SLG` = mean(SLG_Value, na.rm = TRUE),
      `OPS` = mean(OBP_Value, na.rm = TRUE) + mean(SLG_Value, na.rm = TRUE),
      `BABIP` = mean(BABIP_Value, na.rm = TRUE),
      `wOBA` = mean(wOBA_Value, na.rm = TRUE),
      `wOBAcon` = mean(wOBAcon_Value, na.rm = TRUE),
      `ISO` = mean(SLG_Value, na.rm = TRUE) - mean(AVG_Value, na.rm = TRUE),
      `Max EV` = if_else(sum(!is.na(ExitSpeed)) == 0, NA_real_, max(ExitSpeed, na.rm = TRUE)),
      `90th EV` = if_else(sum(!is.na(ExitSpeed)) == 0, NA_real_, quantile(ExitSpeed, 0.9, na.rm = TRUE)),
      `Hard Hit Rate (90+ MPH)` = sum(ExitSpeed > 90 & PitchCall == "In Play", na.rm = TRUE) /
        if_else(sum(PitchCall == "In Play", na.rm = TRUE) == 0, NA_real_, sum(PitchCall == "In Play", na.rm = TRUE)) * 100,
      `BB%` = sum(PlayResult == "Walk", na.rm = TRUE) / `PA` * 100,
      `SO%` = sum(PlayResult == "StrikeOut", na.rm = TRUE) / `PA` * 100,
      `BB/SO` = sum(PlayResult == "Walk", na.rm = TRUE) / sum(PlayResult == "StrikeOut", na.rm = TRUE),
      `HR%` = sum(PlayResult == "HomeRun", na.rm = TRUE) / `PA` * 100,
      `GB%` = sum(ContactType == "Ground Ball", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `LD%` = sum(ContactType == "Line Drive", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `FB%` = sum(ContactType == "Fly Ball", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `GB/FB` = sum(ContactType == "Ground Ball", na.rm = TRUE) / sum(ContactType == "Fly Ball", na.rm = TRUE),
      `HR/FB` = sum(PlayResult == "HomeRun", na.rm = TRUE) / sum(ContactType == "Fly Ball", na.rm = TRUE)
    ) %>%
    arrange(desc(`PA`))
  
  # Format
  summary_stats <- summary_stats %>%
    mutate(
      `AVG` = round(`AVG`, 3),
      `OBP` = round(`OBP`, 3),
      `SLG` = round(`SLG`, 3),
      `OPS` = round(`OPS`, 3),
      `BABIP` = round(`BABIP`, 3),
      `wOBA` = round(`wOBA`, 3),
      `wOBAcon` = round(`wOBAcon`, 3),
      `ISO` = round(`ISO`, 3),
      `Max EV` = round(`Max EV`, 1),
      `90th EV` = round(`90th EV`, 1),
      `Hard Hit Rate (90+ MPH)` = round(`Hard Hit Rate (90+ MPH)`, 1),
      `BB%` = round(`BB%`, 1),
      `SO%` = round(`SO%`, 1),
      `BB/SO` = round(`BB/SO`, 2),
      `HR%` = round(`HR%`, 1),
      `GB%` = round(`GB%`, 1),
      `LD%` = round(`LD%`, 1),
      `FB%` = round(`FB%`, 1),
      `GB/FB` = round(`GB/FB`, 2),
      `HR/FB` = round(`HR/FB`, 2)
    ) %>%
    mutate(
      `AVG` = format(`AVG`, nsmall = 3),
      `OBP` = format(`OBP`, nsmall = 3),
      `SLG` = format(`SLG`, nsmall = 3),
      `OPS` = format(`OPS`, nsmall = 3),
      `BABIP` = format(`BABIP`, nsmall = 3),
      `wOBA` = format(`wOBA`, nsmall = 3),
      `wOBAcon` = format(`wOBAcon`, nsmall = 3),
      `ISO` = format(`ISO`, nsmall = 3),
      `Max EV` = format(`Max EV`, nsmall = 1),
      `90th EV` = format(`90th EV`, nsmall = 1),
      `Hard Hit Rate (90+ MPH)` = format(`Hard Hit Rate (90+ MPH)`, nsmall = 1),
      `BB%` = format(`BB%`, nsmall = 1),
      `SO%` = format(`SO%`, nsmall = 1),
      `BB/SO` = format(`BB/SO`, nsmall = 2),
      `HR%` = format(`HR%`, nsmall = 1),
      `GB%` = format(`GB%`, nsmall = 1),
      `LD%` = format(`LD%`, nsmall = 1),
      `FB%` = format(`FB%`, nsmall = 1),
      `GB/FB` = format(`GB/FB`, nsmall = 2),
      `HR/FB` = format(`HR/FB`, nsmall = 2)
    )
  
  datatable(summary_stats, options = list(pageLength = 25, autoWidth = TRUE))
}

# Hitter Swing Decision Leaderboard
hitter_sd_leaderboard <- function(min_pa, Team = "All") {
  # Filter Data
  filtered_data <- df_CCBL_leaderboards %>%
    {if (Team != "All") filter(., toupper(substr(Team, 1, 3)) == toupper(substr(BatterTeam, 1, 3))) else .}
  
  # Calculate Plate Appearances
  pa_data <- filtered_data %>%
    group_by(Batter) %>%
    summarise(`PA` = sum(Balls == 0 & Strikes == 0),
              `sz_bot` = first(SZ_Bot),
              `sz_top` = first(SZ_Top)) %>%
    filter(`PA` >= min_pa)
  
  # Join PAs
  filtered_data <- filtered_data %>%
    inner_join(pa_data, by = "Batter")
  
  # Summary Stats
  summary_stats <- filtered_data %>%
    group_by(Batter) %>%
    summarise(
      `PA` = first(PA),
      `Num Pitches` = n(),
      `Num Swings` = sum(SwingDecision == "Swing", na.rm = TRUE),
      `O-Swing %` = sum(SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(!(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100,
      `Z-Swing %` = sum(SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100,
      `Swing %` = sum(SwingDecision == "Swing", na.rm = TRUE) / n() * 100,
      `O-Contact %` = sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(SwingDecision == "Swing" & !(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100,
      `Z-Contact %` = sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(SwingDecision == "Swing" & PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100,
      `Contact %` = sum(PitchCall == "Foul Ball" | PitchCall == "In Play", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100,
      `Whiff %` = sum(PitchCall == "Whiff", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100,
      `First Pitch Swing %` = sum(SwingDecision == "Swing" & Balls == 0 & Strikes == 0, na.rm = TRUE) / sum(Balls == 0 & Strikes == 0, na.rm = TRUE) * 100,
      `Zone %` = sum(PlateLocHeight >= sz_bot & PlateLocHeight <= sz_top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / n() * 100,
      `SwStr%` = sum(PitchCall == "Whiff", na.rm = TRUE) / n() * 100,
      `CStr%` = sum(PitchCall == "StrikeCalled", na.rm = TRUE) / n() * 100,
      `CSW%` = (sum(PitchCall == "Whiff", na.rm = TRUE) + sum(PitchCall == "StrikeCalled", na.rm = TRUE)) / n() * 100
    ) %>%
    mutate(
      `O-Swing %` = round(`O-Swing %`, 1),
      `Z-Swing %` = round(`Z-Swing %`, 1),
      `Swing %` = round(`Swing %`, 1),
      `O-Contact %` = round(`O-Contact %`, 1),
      `Z-Contact %` = round(`Z-Contact %`, 1),
      `Contact %` = round(`Contact %`, 1),
      `Whiff %` = round(`Whiff %`, 1),
      `First Pitch Swing %` = round(`First Pitch Swing %`, 1),
      `Zone %` = round(`Zone %`, 1),
      `SwStr%` = round(`SwStr%`, 1),
      `CStr%` = round(`CStr%`, 1),
      `CSW%` = round(`CSW%`, 1)
    ) %>%
    mutate(
      `O-Swing %` = format(`O-Swing %`, nsmall = 1),
      `Z-Swing %` = format(`Z-Swing %`, nsmall = 1),
      `Swing %` = format(`Swing %`, nsmall = 1),
      `O-Contact %` = format(`O-Contact %`, nsmall = 1),
      `Z-Contact %` = format(`Z-Contact %`, nsmall = 1),
      `Contact %` = format(`Contact %`, nsmall = 1),
      `Whiff %` = format(`Whiff %`, nsmall = 1),
      `First Pitch Swing %` = format(`First Pitch Swing %`, nsmall = 1),
      `Zone %` = format(`Zone %`, nsmall = 1),
      `SwStr%` = format(`SwStr%`, nsmall = 1),
      `CStr%` = format(`CStr%`, nsmall = 1),
      `CSW%` = format(`CSW%`, nsmall = 1)
    ) %>%
    arrange(desc(`PA`))
  
  datatable(summary_stats, options = list(pageLength = 25, autoWidth = TRUE))
}

# Pitcher Leaderboard
pitcher_leaderboard <- function(min_bf, Team = "All") {
  # Filter Data by Team
  filtered_data <- df_CCBL_leaderboards %>%
    {if (Team != "All") filter(., toupper(substr(Team, 1, 3)) == toupper(substr(PitcherTeam, 1, 3))) else .}
  
  # Calculate Plate Appearances
  bf_data <- filtered_data %>%
    group_by(Pitcher) %>%
    summarise(`BF` = sum(Balls == 0 & Strikes == 0)) %>%
    filter(`BF` >= min_bf)
  
  # Join PAs
  filtered_data <- filtered_data %>%
    inner_join(bf_data, by = "Pitcher")
  
  # Summary Stats
  summary_stats <- filtered_data %>%
    group_by(Pitcher) %>%
    summarise(
      `BF` = first(BF),
      `AVG Against` = mean(AVG_Value, na.rm = TRUE),
      `OBP` = mean(OBP_Value, na.rm = TRUE),
      `SLG` = mean(SLG_Value, na.rm = TRUE),
      `OPS` = mean(OBP_Value, na.rm = TRUE) + mean(SLG_Value, na.rm = TRUE),
      `BABIP` = mean(BABIP_Value, na.rm = TRUE),
      `wOBA` = mean(wOBA_Value, na.rm = TRUE),
      `wOBAcon` = mean(wOBAcon_Value, na.rm = TRUE),
      `ISO` = mean(SLG_Value, na.rm = TRUE) - mean(AVG_Value, na.rm = TRUE),
      `Max EV Against` = if_else(sum(!is.na(ExitSpeed)) == 0, NA_real_, max(ExitSpeed, na.rm = TRUE)),
      `90th EV` = if_else(sum(!is.na(ExitSpeed)) == 0, NA_real_, quantile(ExitSpeed, 0.9, na.rm = TRUE)),
      `Hard Hit % Against` = sum(ExitSpeed > 90 & PitchCall == "In Play", na.rm = TRUE) /
        if_else(sum(PitchCall == "In Play", na.rm = TRUE) == 0, NA_real_, sum(PitchCall == "In Play", na.rm = TRUE)) * 100,
      `BB%` = sum(PlayResult == "Walk", na.rm = TRUE) / `BF` * 100,
      `SO%` = sum(PlayResult == "StrikeOut", na.rm = TRUE) / `BF` * 100,
      `SO-BB%` = (sum(PlayResult == "StrikeOut", na.rm = TRUE) / `BF` * 100) - (sum(PlayResult == "Walk", na.rm = TRUE) / `BF` * 100),
      `HR%` = sum(PlayResult == "HomeRun", na.rm = TRUE) / `BF` * 100,
      `GB%` = sum(ContactType == "Ground Ball", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `LD%` = sum(ContactType == "Line Drive", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `FB%` = sum(ContactType == "Fly Ball", na.rm = TRUE) / sum(PitchCall == "In Play", na.rm = TRUE) * 100,
      `GB/FB` = sum(ContactType == "Ground Ball", na.rm = TRUE) / sum(ContactType == "Fly Ball", na.rm = TRUE),
      `HR/FB` = sum(PlayResult == "HomeRun", na.rm = TRUE) / sum(ContactType == "Fly Ball", na.rm = TRUE)
    ) %>%
    mutate(
      `AVG Against` = round(`AVG Against`, 3),
      `OBP` = round(`OBP`, 3),
      `SLG` = round(`SLG`, 3),
      `OPS` = round(`OPS`, 3),
      `BABIP` = round(`BABIP`, 3),
      `wOBA` = round(`wOBA`, 3),
      `wOBAcon` = round(`wOBAcon`, 3),
      `ISO` = round(`ISO`, 3),
      `Max EV Against` = round(`Max EV Against`, 1),
      `90th EV` = round(`90th EV`, 1),
      `Hard Hit % Against` = round(`Hard Hit % Against`, 1),
      `BB%` = round(`BB%`, 1),
      `SO%` = round(`SO%`, 1),
      `SO-BB%` = round(`SO-BB%`, 1),
      `HR%` = round(`HR%`, 1),
      `GB%` = round(`GB%`, 1),
      `LD%` = round(`LD%`, 1),
      `FB%` = round(`FB%`, 1),
      `GB/FB` = round(`GB/FB`, 2),
      `HR/FB` = round(`HR/FB`, 2)
    ) %>%
    mutate(
      `AVG Against` = format(`AVG Against`, nsmall = 3),
      `OBP` = format(`OBP`, nsmall = 3),
      `SLG` = format(`SLG`, nsmall = 3),
      `OPS` = format(`OPS`, nsmall = 3),
      `BABIP` = format(`BABIP`, nsmall = 3),
      `wOBA` = format(`wOBA`, nsmall = 3),
      `wOBAcon` = format(`wOBAcon`, nsmall = 3),
      `ISO` = format(`ISO`, nsmall = 3),
      `Max EV Against` = format(`Max EV Against`, nsmall = 1),
      `90th EV` = format(`90th EV`, nsmall = 1),
      `Hard Hit % Against` = format(`Hard Hit % Against`, nsmall = 1),
      `BB%` = format(`BB%`, nsmall = 1),
      `SO%` = format(`SO%`, nsmall = 1),
      `SO-BB%` = format(`SO-BB%`, nsmall = 1),
      `HR%` = format(`HR%`, nsmall = 1),
      `GB%` = format(`GB%`, nsmall = 1),
      `LD%` = format(`LD%`, nsmall = 1),
      `FB%` = format(`FB%`, nsmall = 1),
      `GB/FB` = format(`GB/FB`, nsmall = 2),
      `HR/FB` = format(`HR/FB`, nsmall = 2)
    ) %>%
    arrange(desc(`BF`))
  
  datatable(summary_stats, options = list(pageLength = 25, autoWidth = TRUE))
}

# Pitcher Plate Discipline Leaderboard
pitcher_sd_leaderboard <- function(min_bf, Team = "All") {
  # Filter Data
  filtered_data <- df_CCBL_leaderboards %>%
    {if (Team != "All") filter(., toupper(substr(Team, 1, 3)) == toupper(substr(PitcherTeam, 1, 3))) else .}
  
  # Calculate Plate Appearances
  bf_data <- filtered_data %>%
    group_by(Pitcher) %>%
    summarise(`BF` = sum(Balls == 0 & Strikes == 0)) %>%
    filter(`BF` >= min_bf)
  
  # Join PAs
  filtered_data <- filtered_data %>%
    inner_join(bf_data, by = "Pitcher")
  
  # Summary Stats
  summary_stats <- filtered_data %>%
    group_by(Pitcher) %>%
    summarise(
      `BF` = first(BF),
      `Num Pitches` = n(),
      `Num Swings Against` = sum(SwingDecision == "Swing", na.rm = TRUE),
      `O-Swing %` = sum(SwingDecision == "Swing" & !(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(!(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100,
      `Z-Swing %` = sum(SwingDecision == "Swing" & PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100,
      `Swing %` = sum(SwingDecision == "Swing", na.rm = TRUE) / n() * 100,
      `O-Contact %` = sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & !(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) / sum(SwingDecision == "Swing" & !(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide <= -0.71 & PlateLocSide <= 0.71), na.rm = TRUE) * 100,
      `Z-Contact %` = sum((PitchCall == "Foul Ball" | PitchCall == "In Play") & SwingDecision == "Swing" & PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / sum(SwingDecision == "Swing" & PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) * 100,
      `Contact %` = sum(PitchCall == "Foul Ball" | PitchCall == "In Play", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100,
      `Whiff %` = sum(PitchCall == "Whiff", na.rm = TRUE) / sum(SwingDecision == "Swing", na.rm = TRUE) * 100,
      `First Pitch Swing %` = sum(SwingDecision == "Swing" & Balls == 0 & Strikes == 0, na.rm = TRUE) / sum(Balls == 0 & Strikes == 0, na.rm = TRUE) * 100,
      `Zone %` = sum(PlateLocHeight >= SZ_Bot & PlateLocHeight <= SZ_Top & PlateLocSide >= -0.71 & PlateLocSide <= 0.71, na.rm = TRUE) / n() * 100,
      `SwStr%` = sum(PitchCall == "Whiff", na.rm = TRUE) / n() * 100,
      `CStr%` = sum(PitchCall == "StrikeCalled", na.rm = TRUE) / n() * 100,
      `CSW%` = (sum(PitchCall == "Whiff", na.rm = TRUE) + sum(PitchCall == "StrikeCalled", na.rm = TRUE)) / n() * 100
    ) %>%
    mutate(
      `O-Swing %` = round(`O-Swing %`, 1),
      `Z-Swing %` = round(`Z-Swing %`, 1),
      `Swing %` = round(`Swing %`, 1),
      `O-Contact %` = round(`O-Contact %`, 1),
      `Z-Contact %` = round(`Z-Contact %`, 1),
      `Contact %` = round(`Contact %`, 1),
      `Whiff %` = round(`Whiff %`, 1),
      `First Pitch Swing %` = round(`First Pitch Swing %`, 1),
      `Zone %` = round(`Zone %`, 1),
      `SwStr%` = round(`SwStr%`, 1),
      `CStr%` = round(`CStr%`, 1),
      `CSW%` = round(`CSW%`, 1)
    ) %>%
    mutate(
      `O-Swing %` = format(`O-Swing %`, nsmall = 1),
      `Z-Swing %` = format(`Z-Swing %`, nsmall = 1),
      `Swing %` = format(`Swing %`, nsmall = 1),
      `O-Contact %` = format(`O-Contact %`, nsmall = 1),
      `Z-Contact %` = format(`Z-Contact %`, nsmall = 1),
      `Contact %` = format(`Contact %`, nsmall = 1),
      `Whiff %` = format(`Whiff %`, nsmall = 1),
      `First Pitch Swing %` = format(`First Pitch Swing %`, nsmall = 1),
      `Zone %` = format(`Zone %`, nsmall = 1),
      `SwStr%` = format(`SwStr%`, nsmall = 1),
      `CStr%` = format(`CStr%`, nsmall = 1),
      `CSW%` = format(`CSW%`, nsmall = 1)
    ) %>%
    arrange(desc(`BF`))
  
  datatable(summary_stats, options = list(pageLength = 25, autoWidth = TRUE))
}

##### CCBL Trackman Data #####
# Pull CCBL Trackman Data
#connection_string = "mongodb+srv://HarborHawks:CCBL24champs!@harborhawks2024.yonksq3.mongodb.net/"
#connection = mongo(collection="Cape Trackman 24", db="Harbor_Hawks_24", url=connection_string)
#df_CCBL <- connection$find()
#connection$disconnect()

# Import Data
df_CCBL <- read_csv("CCBL Trackman 2024.csv", show_col_types = FALSE)

# Order Data
df_CCBL <- df_CCBL %>%
  arrange(GameID, PitchNo)

# Fix typo
df_CCBL <- df_CCBL %>%
  mutate(Pitcher = ifelse(Pitcher == "Dzierwa, Joesph", "Dzierwa, Joseph", Pitcher),
         Batter = ifelse(Batter == "Cavil, Blake", "Cavill, Blake", Batter),
         Pitcher = ifelse(Pitcher == "Nard, Gabriel", "Nard, Gabe", Pitcher),
         Pitcher = ifelse(Pitcher == "Nelson, Cameron", "Nelson, Cam", Pitcher)) %>%
  mutate(
    Pitcher = case_when(
      Date == "2024-06-21" & 
        Pitcher == "Dzierwa, Joseph" & 
        HorzBreak > 0 & 
        RelSpeed > 88 ~ "Savary, Aaron",
      
      Date == "2024-06-21" & 
        Pitcher == "Dzierwa, Joseph" & 
        InducedVertBreak < -10 ~ "Savary, Aaron",
      
      Date == "2024-06-29" &
        Pitcher == "Beard, Trey" &
        Inning == 3 ~ "Reitz, Jason",
      
      Date == "2024-06-29" &
        Pitcher == "Beard, Trey" &
        Inning == 4 ~ "Reitz, Jason",
      
      Date == "2024-07-18" &
        Pitcher == "Dzierwa, Joseph" ~ "Meeks, Drake",
      
      Pitcher == "Gazdar, Jon Jon" ~ "Steeber, Joe",
      
      TRUE ~ Pitcher
    )
  ) %>%
  mutate(PlayResult = case_when(
    PlayResult == "Sacrifice" &
      ExitSpeed <= 60 ~ "Sacrifice Bunt",
    
    PlayResult == "Sacrifice" &
      ExitSpeed > 60 ~ "Sacrifice Fly",
    
    PlayResult == "Sacrifice" &
      TaggedHitType == "Bunt" ~ "Sacrifice Bunt",
    
    PlayResult == "Sacrifice" &
      TaggedHitType == "FlyBall" ~ "Sacrifice Fly",
    
    TRUE ~ PlayResult
  ))

##### Transform CCBL Data for usage tree #####
# Keep Relevant Columns
df_CCBL_t <- df_CCBL[,c("Pitcher", "BatterSide", "Balls", "Strikes", "TaggedPitchType")]

# Removed undefined pitches
df_CCBL_t <- df_CCBL_t %>%
  filter(TaggedPitchType != "Undefined")

# Remove undefined batter sides
df_CCBL_t <- df_CCBL_t %>%
  filter(BatterSide != "Undefined")

# Max 2 strikes
df_CCBL_t <- df_CCBL_t %>%
  filter(Strikes <= 2)

# Max 3 balls
df_CCBL_t <- df_CCBL_t %>%
  filter(Balls <= 3)

# Add Count
df_CCBL_t <- df_CCBL_t %>%
  mutate(Count = paste(Balls, Strikes, sep = "-")) %>%
  select(-Balls, -Strikes)

# Simplify Pitch Groupings
df_CCBL_t <- df_CCBL_t %>%
  mutate(Pitch = case_when(
    TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "CH/SP",
    TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "OneSeamFastBall", "TwoSeamFastBall", "Sinker") ~ "FastSink",
    TRUE ~ TaggedPitchType
  ))

# Split data
split_df_CCBL_t <- split(df_CCBL_t, f=list(df_CCBL_t$Pitcher,df_CCBL_t$BatterSide))

summarize_split <- function(df) {
  df %>%
    group_by(Pitcher, BatterSide, Count, Pitch) %>%
    summarise(n = n(), .groups = 'drop') %>%
    group_by(Pitcher, BatterSide, Count) %>%
    mutate(total_pitches = sum(n),
           PitchTypePct = round((n / total_pitches),3)) %>%
    ungroup() %>%
    select(-total_pitches)
}

# Properly formatted data for usage tree
df_CCBL_tree <- bind_rows(lapply(split_df_CCBL_t, summarize_split))

##### Transform CCBL Data for sequencing #####
# Keep Relevant Columns
df_CCBL_s <- df_CCBL[,c("Pitcher", "BatterSide", "PitchofPA", "TaggedPitchType")]

# Removed undefined pitches
df_CCBL_s <- df_CCBL_s %>%
  filter(TaggedPitchType != "Undefined")

# Remove undefined batter sides
df_CCBL_s <- df_CCBL_s %>%
  filter(BatterSide != "Undefined")

# Simplify Pitch Groupings
df_CCBL_s <- df_CCBL_s %>%
  mutate(Pitch = case_when(
    TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "CH/SP",
    TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "OneSeamFastBall", "TwoSeamFastBall", "Sinker") ~ "FastSink",
    TRUE ~ TaggedPitchType
  ))

# Previous Pitch
df_CCBL_s <- df_CCBL_s %>%
  mutate(PrevPitch = ifelse(PitchofPA > 1 & lag(Pitcher) == Pitcher, lag(Pitch), "None"))

# Properly formatted data for sequencing
df_CCBL_sequence <- df_CCBL_s %>%
  group_by(Pitcher, BatterSide, PrevPitch, Pitch) %>%
  summarize(count = n(), .groups = 'drop')

# Cape Pitchers
pitcher_list <- unique(df_CCBL$Pitcher)

##### Pitch Data Modification #####
df_CCBL_breaks <- df_CCBL[,c("Date","Pitcher","BatterSide","Balls","Strikes","TaggedPitchType","PitchCall","PlayResult","PlateLocHeight","PlateLocSide","InducedVertBreak","HorzBreak","RelSpeed","ExitSpeed","Angle","SpinRate","Tilt","RelHeight","Extension","VertApprAngle")]

df_CCBL_breaks <- df_CCBL_breaks %>%
  filter(!is.na(PitchCall) & PitchCall != "Undefined" &
           !is.na(PlateLocHeight) & PlateLocHeight != "Undefined" &
           !is.na(PlateLocSide) & PlateLocSide != "Undefined") %>%
  mutate(Tilt = ifelse(Tilt == "", NA, Tilt))

df_CCBL_breaks <- df_CCBL_breaks %>%
  mutate(PlayResult = case_when(
    PitchCall == "HitByPitch" ~ "HitByPitch",
    Balls == 3 & (PitchCall == "BallCalled" | PitchCall == "BallinDirt") ~ "Walk",
    Strikes == 2 & (PitchCall == "StrikeCalled" | PitchCall == "StrikeSwinging") ~ "StrikeOut",
    TRUE ~ PlayResult
  ))

df_CCBL_breaks <- df_CCBL_breaks %>%
  mutate(Pitch = case_when(
    TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "CH/SP",
    TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "OneSeamFastBall", "TwoSeamFastBall", "Sinker") ~ "FastSink",
    TRUE ~ TaggedPitchType
  )) %>%
  mutate(PitchSimple = case_when(
    TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "Changeup",
    TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "OneSeamFastBall", "TwoSeamFastBall", "Sinker") ~ "Fastball",
    TaggedPitchType %in% c("Slider", "Cutter") ~ "SL/CT",
    TRUE ~ TaggedPitchType
  ))

# Play Values
play_values <- data.frame(
  PlayResult = c("Single", "Double", "Triple", "HomeRun", "Error", "FieldersChoice", "HitByPitch", "Out", "Sacrifice Bunt", "Sacrifice Fly", "Walk", "StrikeOut"),
  AVG_Value = c(1, 1, 1, 1, 0, 0, NA, 0, NA, NA, NA, 0),
  BABIP_Value = c(1, 1, 1, NA, 0, 0, NA, 0, NA, 0, NA, NA),
  OBP_Value = c(1, 1, 1, 1, 0, 0, 1, 0, NA, 0, 1, 0),
  SLG_Value = c(1, 2, 3, 4, 0, 0, NA, 0, NA, NA, NA, 0),
  wOBA_Value =c(0.89747458, 1.23257991, 1.59320360, 1.97344452, 0.01971104, 0.01971104, 0.76283477, 0.01971104, 0.01971104, 0.01971104, 0.73521541, 0),
  wOBAcon_Value =c(0.89747458, 1.23257991, 1.59320360, 1.97344452, 0.01971104, 0.01971104, NA, 0.01971104, 0.01971104, 0.01971104, NA, NA)
)

df_CCBL_breaks <- df_CCBL_breaks %>%
  left_join(play_values, by = "PlayResult")

# Exit Velo Data
df_CCBL_breaks <- df_CCBL_breaks %>%
  mutate(ExitVeloBin = case_when(
    PitchCall == "InPlay" & ExitSpeed < 85 ~ "< 85 MPH",
    PitchCall == "InPlay" & ExitSpeed >= 85 & ExitSpeed < 95 ~ "85-95 MPH",
    PitchCall == "InPlay" & ExitSpeed >= 95 ~ "95+ MPH",
    TRUE ~ NA_character_
  ))

# Batted Ball Type Data
df_CCBL_breaks <- df_CCBL_breaks %>%
  mutate(ContactType = case_when(
    PitchCall == "InPlay" & Angle < 10 ~ "Ground Ball",
    PitchCall == "InPlay" & Angle >= 10 & Angle <= 25 ~ "Line Drive",
    PitchCall == "InPlay" & Angle > 25 & Angle <= 50 ~ "Fly Ball",
    PitchCall == "InPlay" & Angle > 50 ~ "Pop Up",
    TRUE ~ NA_character_
  ))

##### Pre-formatted Data #####
# Load NCAA Usage Tree Data
df_NCAA_tree <- read_csv("Pitch_Tree_Data_NCAA_2024.csv", show_col_types = FALSE)

# Change Name
df_NCAA_tree <- df_NCAA_tree %>%
  mutate(Pitcher = ifelse(Pitcher == "Ariola, Joseph", "Ariola, Joe", Pitcher),
         Pitcher = ifelse(Pitcher == "Primeaux, DJ", "Primeaux, D.J.", Pitcher),
         Pitcher = ifelse(Pitcher == "Thompson, JD", "Thompson, J.D.", Pitcher))

# Keep CCBL Pitchers
df_NCAA_tree <- df_NCAA_tree %>%
  filter(Pitcher %in% pitcher_list)

# Load NCAA Pitch Sequencing Data
df_NCAA_sequence <- read_csv("Pitch_Sequencing_Data_NCAA_2024.csv", show_col_types = FALSE)

# Change Name
df_NCAA_sequence <- df_NCAA_sequence %>%
  mutate(Pitcher = ifelse(Pitcher == "Ariola, Joseph", "Ariola, Joe", Pitcher),
         Pitcher = ifelse(Pitcher == "Primeaux, DJ", "Primeaux, D.J.", Pitcher),
         Pitcher = ifelse(Pitcher == "Thompson, JD", "Thompson, J.D.", Pitcher))

# Keep CCBL Pitchers
df_NCAA_sequence <- df_NCAA_sequence %>%
  filter(Pitcher %in% pitcher_list)

# Load CCBL pbp Data for 2024
df_pbp_CCBL <- read_csv("CCBL pbp 2024.csv", show_col_types = FALSE)

##### Swing Decisions #####
# Filter Columns for Swing Decisions
df_pbp_CCBL_sd <- df_pbp_CCBL[,c("matchup.batter.id","pitchData.strikeZoneBottom","pitchData.strikeZoneTop")]

# Rename Columns for Swing Decisions
df_pbp_CCBL_sd <- df_pbp_CCBL_sd %>%
  rename(BatterId = `matchup.batter.id`,
         SZ_Bot = `pitchData.strikeZoneBottom`,
         SZ_Top = `pitchData.strikeZoneTop`)

# Keep Unique Rows for Swing Decisions
df_pbp_CCBL_sd <- df_pbp_CCBL_sd %>%
  drop_na() %>%
  distinct(BatterId, .keep_all = TRUE)

# Trackman Columns for Swing Decisions
df_CCBL_sd <- df_CCBL[,c("Date","PitcherThrows","BatterId","Batter","BatterSide","Balls","Strikes", "TaggedPitchType","PitchCall","PlayResult","PlateLocHeight","PlateLocSide","ExitSpeed","Angle","Bearing","Distance")]

# Merge Data for Swing Decisions
df_sd <- df_CCBL_sd %>%
  left_join(df_pbp_CCBL_sd, by = "BatterId")

# Remove Missing Data for Swing Decisions
df_sd <- df_sd %>%
  filter(!is.na(PitchCall) & PitchCall != "Undefined" &
           !is.na(PlateLocHeight) & PlateLocHeight != "Undefined" &
           !is.na(PlateLocSide) & PlateLocSide != "Undefined")

# Add Strike Zone if Missing for Swing Decisions
# Bottom
df_sd <- df_sd %>%
  mutate(SZ_Bot = ifelse(is.na(SZ_Bot), 1.535, SZ_Bot))
# Top
df_sd <- df_sd %>%
  mutate(SZ_Top = ifelse(is.na(SZ_Top), 3.371, SZ_Top))

df_sd <- df_sd %>%
  mutate(PlayResult = case_when(
    PitchCall == "HitByPitch" ~ "HitByPitch",
    Balls == 3 & (PitchCall == "BallCalled" | PitchCall == "BallinDirt") ~ "Walk",
    Strikes == 2 & (PitchCall == "StrikeCalled" | PitchCall == "StrikeSwinging") ~ "StrikeOut",
    TRUE ~ PlayResult
  ))

# Simplify Pitch Groupings for Swing Decisions
df_sd <- df_sd %>%
  mutate(Pitch = case_when(
    TaggedPitchType %in% c("ChangeUp", "Splitter") ~ "CH/SP",
    TaggedPitchType %in% c("Fastball", "FourSeamFastBall", "OneSeamFastBall", "TwoSeamFastBall", "Sinker") ~ "FastSink",
    TRUE ~ TaggedPitchType
  ))

# Swing or Take
df_sd <- df_sd %>%
  mutate(SwingDecision = ifelse(PitchCall %in% c("BallCalled","BallinDirt","HitByPitch","StrikeCalled"), "Take", "Swing"))

# Home Plate
home_plate_segments <- data.frame(
  x = c(0, 0.71, 0.71, 0, -0.71, -0.71),
  y = c(0.15, 0.15, 0.3, 0.5, 0.3, 0.15),
  xend = c(0.71, 0.71, 0, -0.71, -0.71, 0),
  yend = c(0.15, 0.3, 0.5, 0.3, 0.15, 0.15)
)

##### Batter Stats by Zone #####
# Add Values to Data
df_sd <- df_sd %>%
  left_join(play_values, by = "PlayResult")

##### Batted Ball Types #####
df_sd <- df_sd %>%
  mutate(ExitVeloBin = case_when(
    PitchCall == "InPlay" & ExitSpeed < 85 ~ "< 85 MPH",
    PitchCall == "InPlay" & ExitSpeed >= 85 & ExitSpeed < 95 ~ "85-95 MPH",
    PitchCall == "InPlay" & ExitSpeed >= 95 ~ "95+ MPH",
    TRUE ~ NA_character_
  ))

df_sd <- df_sd %>%
  mutate(ContactType = case_when(
    PitchCall == "InPlay" & Angle < 10 ~ "Ground Ball",
    PitchCall == "InPlay" & Angle >= 10 & Angle <= 25 ~ "Line Drive",
    PitchCall == "InPlay" & Angle > 25 & Angle <= 50 ~ "Fly Ball",
    PitchCall == "InPlay" & Angle > 50 ~ "Pop Up",
    TRUE ~ NA_character_
  ))

##### Leaderboards Data Manipulation #####
df_CCBL_leaderboards <- df_CCBL[,c("Date","Pitcher","PitcherTeam","BatterId","Batter","BatterTeam","Balls","Strikes","TaggedPitchType","PitchCall","PlayResult","PlateLocHeight","PlateLocSide","RelSpeed","ExitSpeed","Angle")]

# Modify Data
df_CCBL_leaderboards <- df_CCBL_leaderboards %>%
  mutate(PlayResult = case_when(
    PitchCall == "HitByPitch" ~ "HitByPitch",
    Balls == 3 & (PitchCall == "BallCalled" | PitchCall == "BallinDirt") ~ "Walk",
    Strikes == 2 & (PitchCall == "StrikeCalled" | PitchCall == "StrikeSwinging") ~ "StrikeOut",
    TRUE ~ PlayResult
  ),
  PitchCall = case_when(
    PitchCall %in% c("FoulBallFieldable", "FoulBallNotFieldable") ~ "Foul Ball",
    PitchCall == "StrikeSwinging" ~ "Whiff",
    PitchCall == "InPlay" ~ "In Play",
    TRUE ~ PitchCall
  )
  ) %>%
  mutate(ContactType = case_when(
    PitchCall == "In Play" & Angle < 10 ~ "Ground Ball",
    PitchCall == "In Play" & Angle >= 10 & Angle <= 25 ~ "Line Drive",
    PitchCall == "In Play" & Angle > 25 & Angle <= 50 ~ "Fly Ball",
    PitchCall == "In Play" & Angle > 50 ~ "Pop Up",
    TRUE ~ NA_character_
  ))

# Merge Strike Zones
df_CCBL_leaderboards <- df_CCBL_leaderboards %>%
  left_join(df_pbp_CCBL_sd, by = "BatterId")

df_CCBL_leaderboards <- df_CCBL_leaderboards %>%
  mutate(SZ_Bot = ifelse(is.na(SZ_Bot), 1.535, SZ_Bot),
         SZ_Top = ifelse(is.na(SZ_Top), 3.371, SZ_Top))

# Swing Decisions
df_CCBL_leaderboards <- df_CCBL_leaderboards %>%
  mutate(SwingDecision = ifelse(PitchCall %in% c("BallCalled","BallinDirt","HitByPitch","StrikeCalled"), "Take", "Swing"))

# Join Play Values
df_CCBL_leaderboards <- df_CCBL_leaderboards %>%
  left_join(play_values, by = "PlayResult")

##### Pitch Count Tracker #####
# Remove non-pitches
df_pbp_CCBL_pitches <- df_pbp_CCBL %>%
  filter(isPitch == TRUE) %>%
  rename(pitcher = matchup.pitcher.fullName)

# Set target date (5 days)
target_date <- Sys.Date() - 5

# Filter Date (within last 5 days)
df_pbp_CCBL_pitches <- df_pbp_CCBL_pitches %>%
  filter(game_date >= target_date)

# Filter Columns
df_pbp_CCBL_pitches <- df_pbp_CCBL_pitches[,c("game_date", "pitcher", "fielding_team")]

##### Match Up Tracker #####
# Filter for Hyannis Games
df_pbp_CCBL_HHH <- df_pbp_CCBL %>%
  filter(home_team %in% "Hyannis Harbor Hawks" | away_team %in% "Hyannis Harbor Hawks")

# Filter At Bats
df_pbp_CCBL_HHH <- df_pbp_CCBL_HHH %>%
  filter(!(lead(game_pk) == game_pk & lead(about.atBatIndex) == about.atBatIndex))

# Filter Columns
df_pbp_CCBL_HHH <- df_pbp_CCBL_HHH[,c("matchup.batter.fullName","matchup.pitcher.fullName","batting_team","fielding_team")]

# Separate Pitching and Hitting
df_pbp_CCBL_HHH_batting <- df_pbp_CCBL_HHH %>%
  filter(batting_team %in% "Hyannis Harbor Hawks")

df_pbp_CCBL_HHH_pitching <- df_pbp_CCBL_HHH %>%
  filter(fielding_team %in% "Hyannis Harbor Hawks")

# Batter Match up Data
batting_pa_count <- df_pbp_CCBL_HHH_batting %>%
  group_by(matchup.batter.fullName, matchup.pitcher.fullName, fielding_team) %>%
  summarize(count_pa = n(), .groups = 'drop') %>%
  rename(
    batter = matchup.batter.fullName,
    pitcher = matchup.pitcher.fullName,
    fielding_team = fielding_team
  )

# Pitcher Match up Data
pitching_pa_count <- df_pbp_CCBL_HHH_pitching %>%
  group_by(matchup.batter.fullName, matchup.pitcher.fullName, batting_team) %>%
  summarize(count_pa = n(), .groups = 'drop') %>%
  rename(
    batter = matchup.batter.fullName,
    pitcher = matchup.pitcher.fullName,
    batting_team = batting_team
  )

##### Run Expectancy/Probability #####
# Import Run Expectancy Data
df_CCBL_RE <- read_csv("Run Expectancy CCBL 2011-2023.csv", show_col_types = FALSE)

format_runners_situation <- function(runners) {
  if (runners == 0) {
    return("000")
  } else if (runners == 10) {
    return("010")
  } else if (runners == 1) {
    return("001")
  } else if (runners == 11) {
    return("011")
  } else {
    return(str_pad(runners, 3, pad = "0"))
  }
}

df_CCBL_RE <- df_CCBL_RE %>%
  mutate(Runners_Situation = as.character(Runners_Situation),
         Runners_Situation = sapply(Runners_Situation, format_runners_situation))

# Import Run Probability
df_CCBL_RP <- read_csv("Run Probability CCBL 2011-2023.csv", show_col_types = FALSE)

# Import Steal Breakeven Data
df_CCBL_steal <- read_csv("Steal Breakeven CCBL 2011-2023.csv", show_col_types = FALSE)

# Import Run Expectancy by Count Data
df_CCBL_RE_Count <- read_csv("RE_summary_by_count_2021x2023.csv", show_col_types = FALSE)

# Transform Run Expectancy by Count Data
df_CCBL_RE_Count <- df_CCBL_RE_Count %>%
  select(expected_runs, balls, strikes, outs, first, second, third) %>%
  mutate(Count = paste(balls, strikes, sep = "-"),  Runners_Situation = paste0(first, second, third),'Expected Runs' = round(expected_runs, 3)) %>%
  select(-balls, -strikes, -first, -second, -third, -expected_runs) %>%
  rename(Outs = outs)

##### UI #####
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Hyannis Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home Page", tabName = "dashboard", icon = icon("home")),
      menuItem("Pitchers", tabName = "pitchers", icon = icon("baseball"),
               menuSubItem("Usage",
                           tabName = "usage",
                           icon = icon("tree")),
               menuSubItem("Pitch Shapes",
                           tabName = "shapes",
                           icon = icon("shapes")),
               menuSubItem("Damage",
                           tabName = "damage",
                           icon = icon("triangle-exclamation")),
               menuSubItem("Report",
                           tabName = "report",
                           icon = icon("file-lines"))
               ),
      menuItem("Batters", tabName = "batters", icon = icon("baseball-bat-ball")),
      menuItem("Leaderboards", tabName = "leaderboards", icon = icon("list-ol"),
               menuSubItem("Hitter Leaderboard",
                           tabName = "hitterleaderboard",
                           icon = icon("baseball-bat-ball")),
               menuSubItem("Pitcher Leaderboard",
                           tabName = "pitcherleaderboard",
                           icon = icon("baseball"))),
      menuItem("Match Up Tracker", tabName = "matchup", icon = icon("thumbtack")),
      menuItem("Pitch Count Tracker", tabName = "pitchcount", icon = icon("stopwatch")),
      menuItem("Run Expectancy", tabName = "runexpectancy", icon = icon("table"),
               menuSubItem("24 Game States",
                           tabName = "24states",
                           icon = icon("person-running")),
               menuSubItem("By Count",
                           tabName = "288states",
                           icon = icon("table-list"))
               )
    )
  ),
  dashboardBody(
    tags$style(HTML("
      body {background-color: white;}
      .content-wrapper {background-color: white;}
      .top-content {
        width: 100%;
        text-align: center;
        font-size: 40px;
        padding-top: 20px;
      }
      .center-content {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        height: calc(100vh - 200px);
        text-align: center;
      }
      .bottom-text {
        width: 100%;
        text-align: center;
        font-size: 20px;
        position: absolute;
        bottom: 20px;
      }
      h1 {
        font-weight: bold;
      }
    ")),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidPage(
                tags$div(
                  class = "top-content",
                  tags$h1("Cape League Advanced Scouting and Strategy App"),
                ),
                tags$div(
                  class = "center-content",
                  tags$img(src = "CCBL logo.png", height = "300px")
                ),
                tags$div(
                  class = "bottom-text",
                  tags$p("By: Richard Legler")
                )
              )
      ),
      tabItem(tabName = "usage",
              fluidRow(
                column(3, offset = 1, selectInput("PitcherInput", "Type a Player (Last, First):",
                                                  choices = sort(unique(df_CCBL$Pitcher)),
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(3, offset = 1, selectInput("BatterSide", "Batter Side",
                                                  choices = c("Left", "Right"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(3, offset = 1, selectInput("DataSource", "Data Source",
                                                  choices = c("NCAA", "CCBL"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              tabsetPanel(
                tabPanel("Usage Tree",
                         fluidRow(
                           column(12, plotOutput("tree", height = "80vh"))
                         ),
                         fluidRow(
                           column(12, DTOutput("tree_summary"))
                         )
                ),
                tabPanel("Pitch Sequence",
                         fluidRow(
                           column(12, plotOutput("sequence", height = "80vh"))
                         )
                )
              )
      ),
      tabItem(tabName = "shapes",
              fluidRow(
                column(2, offset = 1, selectInput("ShapesPitcherInput", "Type a Player (Last, First):",
                                                  choices = sort(unique(df_CCBL$Pitcher)),
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("ShapesBatterSide", "Batter Side",
                                                  choices = c("All", "Left", "Right"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("ShapesPitchType", "Pitch Type",
                                                  choices = c("All", "CH/SP", "Curveball", "Cutter", "FastSink", "Knuckleball", "Other", "Slider"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("ShapesCount", "Count",
                                                  choices = c("All", "2 Strikes"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("shapes_title"))
              ),
              fluidRow(
                column(6, plotOutput("movement", height = "80vh")),
                column(6, plotOutput("location", height = "80vh"))
              ),
              fluidRow(
                column(12, DTOutput("pitch_shape_summary"))
              )
      ),
      tabItem(tabName = "damage",
              fluidRow(
                column(2, offset = 1, selectInput("DamagePitcherInput", "Type a Player (Last, First):",
                                                  choices = sort(unique(df_CCBL$Pitcher)),
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("DamageBatterSide", "Batter Side",
                                                  choices = c("All", "Left", "Right"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("DamagePitchType", "Pitch Type",
                                                  choices = c("All", "CH/SP", "Curveball", "Cutter", "FastSink", "Knuckleball", "Other", "Slider"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("DamageCount", "Count",
                                                  choices = c("All", "2 Strikes"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("damage_title"))
              ),
              fluidRow(
                column(2, offset = 2, selectInput("StatA", "Statistic 1",
                                                  choices = c("AVG", "SLG", "OPS", "wOBA", "wOBAcon"),
                                                  multiple = FALSE,
                                                  selectize = FALSE,
                                                  selected = "SLG"),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 4, selectInput("StatB", "Statistic 2",
                                                  choices = c("AVG", "SLG", "OPS", "wOBA", "wOBAcon"),
                                                  multiple = FALSE,
                                                  selectize = FALSE,
                                                  selected = "wOBA"),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(6, plotOutput("statAbyzoneagainst", height = "80vh")),
                column(6, plotOutput("statBbyzoneagainst", height = "80vh"))
              ),
              fluidRow(
                column(6, plotOutput("exitvelopitcher", height = "80vh")),
                column(6, plotOutput("contacttypepitcher", height = "80vh"))
              )
      ),
      tabItem(tabName = "report",
              fluidRow(
                column(2, offset = 1, selectInput("ReportPitcherInput", "Type a Player (Last, First):",
                                                  choices = sort(unique(df_CCBL$Pitcher)),
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("ReportBatterSide", "Batter Side",
                                                  choices = c("Left", "Right"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("report_title"))
              ),
              fluidRow(
                column(4, offset = 4, plotOutput("fb_usage", height = "30vh")),
                column(4, plotOutput("ch_usage", height = "30vh"))
              ),
              fluidRow(
                column(4, offset = 4, plotOutput("sl_usage", height = "30vh")),
                column(4, plotOutput("cb_usage", height = "30vh"))
              ),
              fluidRow(
                column(3, DTOutput("report_pitch_stat_summary")),
                column(3, DTOutput("report_pitch_velo")),
                column(6, DTOutput("report_pitch_usage_summary"))
              )
      ),
      tabItem(tabName = "batters",
              fluidRow(
                column(2, offset = 1, selectInput("BatterInput", "Type a Player (Last, First):",
                                                  choices = sort(unique(df_CCBL$Batter)),
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("PitcherSide", "Pitcher Hand",
                                                  choices = c("All", "Left", "Right"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("PitchType", "Pitch Type",
                                                  choices = c("All", "CH/SP", "Curveball", "Cutter", "FastSink", "Knuckleball", "Other", "Slider"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 1, selectInput("Count", "Count",
                                                  choices = c("All", "2 Strikes", "First Pitch"),
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("batters_title"))
              ),
              fluidRow(
                column(6, plotOutput("swingdecision", height = "80vh")),
                column(6, plotOutput("whiffs", height = "80vh"))
              ),
              fluidRow(
                column(12, DTOutput("sd_summary"), style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(2, offset = 2, selectInput("Stat1", "Statistic 1",
                                                  choices = c("AVG", "SLG", "OPS", "wOBA", "wOBAcon"),
                                                  multiple = FALSE,
                                                  selectize = FALSE,
                                                  selected = "SLG"),
                       style = "padding-bottom: 20px;"),
                column(2, offset = 4, selectInput("Stat2", "Statistic 2",
                                                  choices = c("AVG", "SLG", "OPS", "wOBA", "wOBAcon"),
                                                  multiple = FALSE,
                                                  selectize = FALSE,
                                                  selected = "wOBA"),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(6, plotOutput("stat1byzone", height = "80vh")),
                column(6, plotOutput("stat2byzone", height = "80vh"))
              ),
              fluidRow(
                column(6, plotOutput("exitvelohitter", height = "80vh")),
                column(6, plotOutput("contacttypehitter", height = "80vh"))
              ),
              fluidRow(
                column(6, plotOutput("infieldspray", height = "80vh")),
                column(6, plotOutput("outfieldspray", height = "80vh"))
              )
      ),
      tabItem(tabName = "hitterleaderboard",
              fluidRow(
                column(3, offset = 1, selectInput("Min_PA", "Minimum Plate Appearances:",
                                                  choices = c(10, 20, 50, 100),
                                                  selected = 50,
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(3, offset = 1, selectInput("Team_Leaderboard_Batter", "Select Team:",
                                                  choices = c("All", "Bourne Braves", "Brewster Whitecaps", "Chatham Anglers", "Cotuit Kettleers", "Falmouth Commodores", "Harwich Mariners", "Hyannis Harbor Hawks", "Orleans Firebirds", "Wareham Gatemen", "Yarmouth-Dennis Red Sox"),
                                                  selected = "All",
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("warning1"))
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("hitter_leaderboard_title"))
              ),
              tabsetPanel(
                tabPanel(
                  "General",
                  fluidRow(
                    column(12, DTOutput("hitter_general_leaderboard"))
                  )
                ),
                tabPanel(
                  "Swing Decisions",
                  fluidRow(
                    column(12, DTOutput("hitter_swing_leaderboard"))
                  )
                )
              )
      ),
      tabItem(tabName = "pitcherleaderboard",
              fluidRow(
                column(3, offset = 1, selectInput("Min_BF", "Minimum Batters Faced:",
                                                  choices = c(10, 20, 50, 100),
                                                  selected = 50,
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;"),
                column(3, offset = 1, selectInput("Team_Leaderboard_Pitcher", "Select Team:",
                                                  choices = c("All", "Bourne Braves", "Brewster Whitecaps", "Chatham Anglers", "Cotuit Kettleers", "Falmouth Commodores", "Harwich Mariners", "Hyannis Harbor Hawks", "Orleans Firebirds", "Wareham Gatemen", "Yarmouth-Dennis Red Sox"),
                                                  selected = "All",
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("warning2"))
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("pitcher_leaderboard_title"))
              ),
              tabsetPanel(
                tabPanel(
                  "General",
                  fluidRow(
                    column(12, DTOutput("pitcher_general_leaderboard"))
                  )
                ),
                tabPanel(
                  "Plate Discipline",
                  fluidRow(
                    column(12, DTOutput("pitcher_swing_leaderboard"))
                  )
                )
              )
      ),
      tabItem(tabName = "matchup",
              fluidRow(
                column(3, offset = 1, selectInput("Opponent", "Select Team:",
                                                  choices = c("Bourne Braves", "Brewster Whitecaps", "Chatham Anglers", "Cotuit Kettleers", "Falmouth Commodores", "Harwich Mariners", "Orleans Firebirds", "Wareham Gatemen", "Yarmouth-Dennis Red Sox"),
                                                  selected = "Bourne Braves",
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              tabsetPanel(
                tabPanel("Hitters",
                         fluidRow(
                           column(12, align = "center", htmlOutput("bat_title"))
                         ),
                         fluidRow(
                           column(12, DTOutput("bat_table"))
                         )
                ),
                tabPanel("Pitchers",
                         fluidRow(
                           column(12, align = "center", htmlOutput("pit_title"))
                         ),
                         fluidRow(
                           column(12, DTOutput("pit_table"))
                         )
                )
              )
      ),
      tabItem(tabName = "pitchcount",
              fluidRow(
                column(3, offset = 1, selectInput("Team", "Select Team:",
                                                  choices = c("Bourne Braves", "Brewster Whitecaps", "Chatham Anglers", "Cotuit Kettleers", "Falmouth Commodores", "Harwich Mariners", "Hyannis Harbor Hawks", "Orleans Firebirds", "Wareham Gatemen", "Yarmouth-Dennis Red Sox"),
                                                  selected = "Hyannis Harbor Hawks",
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
                ),
              fluidRow(
                column(12, align = "center", htmlOutput("pitchcount_title"))
                ),
              fluidRow(
                column(12, DTOutput("pitchcount_table"))
                )
      ),
      tabItem(tabName = "24states",
              fluidRow(
                column(3, offset = 1, selectInput("Year", "Select Year:",
                                                  choices = sort(unique(df_CCBL_RE$Year)),
                                                  selected = 2023,
                                                  multiple = FALSE,
                                                  selectize = FALSE),
                       style = "padding-bottom: 20px;")
              ),
              fluidRow(
                column(6, align = "center", htmlOutput("runexpectancy_title")),
                column(6, align = "center", htmlOutput("runprobability_title"))
              ),
              fluidRow(
                column(6, DTOutput("runexpectancy_table")),
                column(6, DTOutput("runprobability_table"))
              ),
              fluidRow(
                column(12, align = "center", htmlOutput("steal_breakeven_title"))
              ),
              fluidRow(
                column(12, align = "center", 
                       div(style = "width: 50%;",
                           DTOutput("steal_breakeven_table")))
              )
      ),
      tabItem(tabName = "288states",
              fluidRow(
                column(12, align = "center", htmlOutput("runexpectancy_288_title"))
              ),
              fluidRow(
                column(12, DTOutput("runexpectancy_288_table"))
              )
      )
    )
  )
)

##### Server #####
# Define server logic
server <- function(input, output, session) {
  
  # Pitchers
  # Usage Tree
  output$tree <- renderPlot({
    req(input$PitcherInput, input$BatterSide, input$DataSource)
    usage_tree(input$PitcherInput, input$BatterSide, input$DataSource)
  })
  
  # Usage Tree Summary
  output$tree_summary <- renderDT({
    req(input$PitcherInput, input$BatterSide, input$DataSource)
    usage_tree_summary(input$PitcherInput, input$BatterSide, input$DataSource)
  })
  
  # Sequencing
  output$sequence <- renderPlot({
    req(input$PitcherInput, input$BatterSide, input$DataSource)
    sequencing(input$PitcherInput, input$BatterSide, input$DataSource)
  })
  
  # Pitch Shapes Title
  output$shapes_title <- renderUI({
    h3(paste("2024 CCBL - ", input$ShapesPitcherInput, " vs ", input$ShapesBatterSide, " Handed Batters, ", input$ShapesPitchType, " Tagged Pitch Types in ", input$ShapesCount, " Counts"))})
  
  # Movement Plot
  output$movement <- renderPlot({
    req(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
    pitch_breaks(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
  })
  
  # Location Plot
  output$location <- renderPlot({
    req(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
    pitch_locations(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
  })
  
  # Pitch Shape Summary Table
  output$pitch_shape_summary <- renderDT({
    req(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
    pitch_breaks_summary(input$ShapesPitcherInput, input$ShapesBatterSide, input$ShapesPitchType, input$ShapesCount)
  })
  
  # Damage title
  output$damage_title <- renderUI({
    h3(paste("2024 CCBL - ", input$DamagePitcherInput, " vs ", input$DamageBatterSide, " Handed Batters, ", input$DamagePitchType, " Tagged Pitch Types in ", input$DamageCount, " Counts"))})
  
  # StatA by Zone Against
  output$statAbyzoneagainst <- renderPlot({
    req(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
    stat_by_zone_pitcher(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount, input$StatA)
  })
  
  # StatB by Zone Against
  output$statBbyzoneagainst <- renderPlot({
    req(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
    stat_by_zone_pitcher(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount, input$StatB)
  })
  
  # Exit Velo Against
  output$exitvelopitcher <- renderPlot({
    req(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
    exit_velo_against(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
  })
  
  # Contact Type Against
  output$contacttypepitcher <- renderPlot({
    req(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
    batted_ball_plot_against(input$DamagePitcherInput, input$DamageBatterSide, input$DamagePitchType, input$DamageCount)
  })
  
  # Match up Tables
  output$hitters_table <- renderText({
    paste("Hitters data for opponent:", input$Opponent)
  })
  
  output$pitchers_table <- renderText({
    paste("Pitchers data for opponent:", input$Opponent)
  })
  
  # Match up Tracker
  # Filter Batter Data
  bat_data <- reactive({
    req(input$Opponent)
    batting_pa_count %>%
      filter(fielding_team == input$Opponent)
  })
  
  # Render Batter Table
  output$bat_table <- renderDT({
    datatable({
      data <- bat_data()
      data %>%
        select(batter, pitcher, count_pa) %>%
        spread(pitcher, count_pa, fill = 0) %>%
        rename(Batter = batter)
    }, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  # Title
  output$bat_title <- renderUI({
    h3(paste(input$Opponent, "Pitchers Count of Plate Appearances vs Hyannis Batters"))})
  
  # Filter Pitcher Data
  pit_data <- reactive({
    req(input$Opponent)
    pitching_pa_count %>%
      filter(batting_team == input$Opponent)
  })
  
  # Render Pitcher Table
  output$pit_table <- renderDT({
    datatable({data <- pit_data()
    data %>%
      select(batter, pitcher, count_pa) %>%
      spread(pitcher, count_pa, fill = 0) %>%
      rename(Batter = batter)
    }, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  # Title
  output$pit_title <- renderUI({
    h3(paste(input$Opponent, "Batters Count of Plate Appearances vs Hyannis Pitchers"))})
  
  # Pitch Count Tracker
  # Filter Data
  pc_data <- reactive({
    req(input$Team)
    df_pbp_CCBL_pitches %>%
      filter(fielding_team == input$Team)
  })
  
  # Render Pitch Count Table
  output$pitchcount_table <- renderDT({
    data <- pc_data()
    datatable({
    data %>%
      count(pitcher, game_date) %>%
      spread(game_date, n, fill = 0) %>%
        rename(Pitcher = pitcher)
    }, options = list(pageLength = 25, autoWidth = TRUE))
  })
  
  # Title
  output$pitchcount_title <- renderUI({
    h3(paste(input$Team, "Pitch Counts for Past 5 Days"))})
    
  # Run Expectancy
  # Filter Data
  re_data <- reactive({
    req(input$Year)
    df_CCBL_RE %>%
      filter(Year == input$Year) %>%
      select(Runners_Situation, '0 outs', '1 out', '2 outs')
  })
  rp_data <- reactive({
    req(input$Year)
    df_CCBL_RP %>%
      filter(Year == input$Year) %>%
      select(Runners_Situation, '0 outs', '1 out', '2 outs')
  })
  sb_data <- reactive({
    req(input$Year)
    df_CCBL_steal %>%
      filter(Year == input$Year) %>%
      select(Runners, Outs, Stolen_Base, Breakeven) %>%
      mutate(Breakeven = paste0(format(Breakeven * 100, nsmall = 1), "%")) %>%
      rename(
        'Starting State' = Runners,
        'Starting Outs' = Outs,
        'Base to Steal' = Stolen_Base,
        'Breakeven Success Rate' = Breakeven
      )
  })
  
  # Render Run Expectancy/Probability Matrix
  output$runexpectancy_table <- renderDT({
    data <- re_data()
    datatable({
      data
    },options = list(pageLength = 8, autoWidth = TRUE, dom = 't'))
  })
  output$runprobability_table <- renderDT({
    data <- rp_data()
    datatable({
      data
    },options = list(pageLength = 8, autoWidth = TRUE, dom = 't'))
  })
  output$steal_breakeven_table <- renderDT({
    data <- sb_data()
    datatable({
      data
    },options = list(pageLength = 27, autoWidth = TRUE, dom = 't'))
  })
  
  # Title
  output$runexpectancy_title <- renderUI({
    h3(paste("Run Expectancy in CCBL - ", input$Year))
  })
  output$runprobability_title <- renderUI({
    h3(paste("Run Probability in CCBL - ", input$Year))
  })
  output$steal_breakeven_title <- renderUI({
    h3(paste("Steal Breakeven - ", input$Year))
  })
  
  # 288 Table
  output$runexpectancy_288_table <- renderDT({
    data <- df_CCBL_RE_Count %>%
      spread(key = Count, value = 'Expected Runs', fill = NA)
    datatable(data, options = list(pageLength = 24, autoWidth = TRUE, dom = 't'))
  })
  
  # 288 Title
  output$runexpectancy_288_title <- renderUI({
    h3("CCBL Run Expectancy by Count (2021-2023)")
  })
  
  # Swing Decisions
  output$swingdecision <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    swing_decision(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Swing Decisions Title
  output$batters_title <- renderUI({
    h3(paste("2024 CCBL - ", input$BatterInput, " vs ", input$PitchType, " Pitches from ", input$PitcherSide, " Handed Pitchers in ", input$Count, " Counts"))
  })
  
  # Whiffs
  output$whiffs <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    whiffs(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Swing Decision Summary Table
  output$sd_summary <- renderDT({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    swing_decision_summary(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Stat 1 by Zone
  output$stat1byzone <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    stat_by_zone(input$BatterInput, input$PitcherSide, input$PitchType, input$Count, input$Stat1)
  })
  
  # Stat 2 by Zone
  output$stat2byzone <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    stat_by_zone(input$BatterInput, input$PitcherSide, input$PitchType, input$Count, input$Stat2)
  })
  
  # Exit Velo for Hitters
  output$exitvelohitter <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    exit_velo(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Batted Ball Type for Hitters
  output$contacttypehitter <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    batted_ball_plot(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Infield Spray Hitters
  output$infieldspray <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    infield_spray(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Outfield Spray Hitters
  output$outfieldspray <- renderPlot({
    req(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
    outfield_spray(input$BatterInput, input$PitcherSide, input$PitchType, input$Count)
  })
  
  # Leaderboards
  ## Warning
  output$warning1 <- renderUI({
    h3("Warning: All stats obtained via TrackMan, which can lead to discrepencies with official statistics.")
  })
  output$warning2 <- renderUI({
    h3("Warning: All stats obtained via TrackMan, which can lead to discrepencies with official statistics.")
  })
  
  ## Titles
  output$hitter_leaderboard_title <- renderUI({
    h3(HTML(paste0("<b>Hitter Leaderboard: ", input$Team_Leaderboard_Batter, " with at least ", input$Min_PA, " Plate Appearances</b>")))
  })
  output$pitcher_leaderboard_title <- renderUI({
    h3(HTML(paste0("<b>Pitcher Leaderboard: ", input$Team_Leaderboard_Pitcher, " with at least ", input$Min_BF, " Batters Faced</b>")))
  })
  
  ## Hitter General Leaderboard
  output$hitter_general_leaderboard <- renderDT({
    req(input$Min_PA, input$Team_Leaderboard_Batter)
    hitter_leaderboard(as.numeric(input$Min_PA), input$Team_Leaderboard_Batter)
  })
  
  ## Hitter Swing Decision Leaderboard
  output$hitter_swing_leaderboard <- renderDT({
    req(input$Min_PA, input$Team_Leaderboard_Batter)
    hitter_sd_leaderboard(as.numeric(input$Min_PA), input$Team_Leaderboard_Batter)
  })
  
  ## Pitcher General Leaderboard
  output$pitcher_general_leaderboard <- renderDT({
    req(input$Min_BF, input$Team_Leaderboard_Pitcher)
    pitcher_leaderboard(as.numeric(input$Min_BF), input$Team_Leaderboard_Pitcher)
  })
  
  ## Pitcher Plate Discipline Leaderboard
  output$pitcher_swing_leaderboard <- renderDT({
    req(input$Min_BF, input$Team_Leaderboard_Pitcher)
    pitcher_sd_leaderboard(as.numeric(input$Min_BF), input$Team_Leaderboard_Pitcher)
  })
  
  # Report
  ## Title
  output$report_title <- renderUI({
    h3(paste0("CCBL: ", input$ReportPitcherInput, " vs ", input$ReportBatterSide, " Handed Batters"))
  })
  
  ## FB Location
  output$fb_usage <- renderPlot({
    req(input$ReportPitcherInput, input$ReportBatterSide)
    pitch_locations_heat(input$ReportPitcherInput, input$ReportBatterSide, "Fastball")
  })
  
  ## CH/SP Location
  output$ch_usage <- renderPlot({
    req(input$ReportPitcherInput, input$ReportBatterSide)
    pitch_locations_heat(input$ReportPitcherInput, input$ReportBatterSide, "Changeup")
  })
  
  ## SL/CT Location
  output$sl_usage <- renderPlot({
    req(input$ReportPitcherInput, input$ReportBatterSide)
    pitch_locations_heat(input$ReportPitcherInput, input$ReportBatterSide, "SL/CT")
  })
  
  ## CB Location
  output$cb_usage <- renderPlot({
    req(input$ReportPitcherInput, input$ReportBatterSide)
    pitch_locations_heat(input$ReportPitcherInput, input$ReportBatterSide, "Curveball")
  })
  
  ## Stats Summary
  output$report_pitch_stat_summary <- renderDT({
    req(input$ReportPitcherInput)
    pitch_stat_summary(input$ReportPitcherInput)
  })
  
  ## Velo Summary
  output$report_pitch_velo <- renderDT({
    req(input$ReportPitcherInput)
    velo_summary(input$ReportPitcherInput)
  })
  
  ## Usage Summary
  output$report_pitch_usage_summary <- renderDT({
    req(input$ReportPitcherInput, input$ReportBatterSide)
    usage_tree_summary(input$ReportPitcherInput, input$ReportBatterSide, "CCBL")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
