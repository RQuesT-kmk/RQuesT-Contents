
# Set up ------------------------------------------------------------------


# 1. Load the necessary library
library(ggplot2)

# 2. Define the data for the main line graph
# We'll approximate the curve with key points
line_data <- data.frame(
    time = c(1, 2, 3, 3.5, 4, 4.5, 5, 6, 7, 8, 9),
    interest = c(10, 15, 60, 100, 95, 60, 20, 15, 30, 20, 15)
)

# 3. Define the data for the labels and annotations
# We use a second data frame for precisely placing the text
# Note: Angles are used to rotate some text along the lines
labels_data <- data.frame(
    time = c(2.6, 3.5, 4.2, 7),
    interest = c(60, 100, 60, 30),
    label = c("CAPE VERDE!", "PEAK HAALAND\nMEMES", "NORWAY\nOUT", "FINAL"),
    vjust = c(0.5, 0.5, 0.5, 0.5), # Vertical alignment
    hjust = c(0.5, 0.5, 0.5, 0.5), # Horizontal alignment
    angle = c(65, 0, 0, 0),       # Rotation angle
    color = c("forestgreen", "gold", "red", "red") # Matching colors
)

# 4. Define data for the large text block at the bottom
text_block_data <- data.frame(
    x = 0.5,
    y = -35, # We'll extend the plot area down
    text = "WON'T SOMEBODY\nPLEASE THINK OF\nTHE MEMES?!"
)




# Plot --------------------------------------------------------------------


# 5. Initialize the ggplot object
# We map 'time' to the x-axis and 'interest' to the y-axis
p <- ggplot(data = line_data, aes(x = time, y = interest)) +
    
    # 6. Add the main line and smooth it slightly
    # 'geom_line' draws straight lines, so we'll use an aesthetic function to approximate the smooth curve.
    geom_line(linewidth = 1.2) +
    
    # 7. Add the main axes and title
    labs(title = "CASUAL INTEREST IN THE THE WORLD CUP", y = "INTEREST") +
    theme_minimal() + # Use a clean, minimal theme
    theme(plot.title = element_text(size = 16, hjust = 0.5, family = "mono")) + # Mono for a comic-like feel
    
    # 8. Customize axes
    scale_y_continuous(breaks = c(10, 100), labels = c("", "HIGH")) + # Remove numbers, add "HIGH"
    theme(axis.title.y = element_text(size = 14, face = "bold", angle = 0, vjust = 0.5, hjust = 0.5, family = "mono")) +
    
    # 9. Add the "CAPE VERDE!" label along the line
    # Using 'geom_text' with the specific label data frame
    geom_text(data = labels_data[1,], aes(label = label), vjust = 1.3, angle = 65, size = 5, color = "forestgreen", family = "mono", show.legend = FALSE) +
    
    # 10. Add the other annotations with arrows using 'annotate'
    # Arrows are a special type of layer
    # Haaland Memes
    annotate("text", x = 3.6, y = 105, label = "PEAK HAALAND\nMEMES", size = 5, color = "gold", family = "mono") +
    annotate("segment", x = 3.5, y = 100, xend = 3.55, yend = 103, arrow = arrow(length = unit(0.3, "cm"))) +
    
    # Norway Out
    annotate("text", x = 4.3, y = 65, label = "NORWAY\nOUT", size = 5, color = "red", family = "mono") +
    annotate("segment", x = 4.2, y = 60, xend = 4.25, yend = 63, arrow = arrow(length = unit(0.3, "cm"))) +
    
    # Final
    annotate("text", x = 7.1, y = 35, label = "FINAL", size = 5, color = "red", family = "mono") +
    annotate("segment", x = 7, y = 30, xend = 7.05, yend = 33, arrow = arrow(length = unit(0.3, "cm"))) +
    
    # 11. Add the main text block at the bottom left
    # We use 'coord_cartesian' to extend the plot area so we can draw outside the main axes.
    coord_cartesian(ylim = c(-50, 110), clip = "off") +
    geom_text(data = text_block_data, aes(x = x, y = y, label = text), hjust = 0, size = 6, family = "mono") +
    
    # 12. Remove default elements that don't fit the comic style
    theme(axis.text.x = element_blank(), # Remove x-axis tick labels
          axis.ticks = element_blank(), # Remove all ticks
          panel.grid = element_blank(), # Remove all gridlines
          axis.line = element_line(linewidth =  1.2), # Make the axes lines bold
          panel.border = element_blank(),
          plot.margin = unit(c(1,1,1,1), "cm")) # Add some margin all around

# 13. Display the final plot
print(p)




# Clean version -----------------------------------------------------------

# 1. Load the necessary library
library(ggplot2)

# 2. Define the base keyframes (peaks and valleys of the timeline)
time_points <- c(1, 2, 3, 3.5, 4.5, 6, 7.5, 8.5, 9)
interest_levels <- c(10, 20, 70, 95, 30, 15, 40, 25, 20)

# 3. Interpolate the data for a mathematically smooth curve
# This generates 200 points to create a perfectly smooth line
smooth_data <- as.data.frame(spline(time_points, interest_levels, n = 200))
colnames(smooth_data) <- c("time", "interest")

# 4. Build the ggplot
p <- ggplot(data = smooth_data, aes(x = time, y = interest)) +
    
    # Add the smoothed line (using linewidth instead of size for newer ggplot2 versions)
    geom_line(color = "navy", linewidth = 1.2) +
    
    # Add title and axis labels
    labs(
        title = "Casual Interest in the World Cup",
        x = "Timeline",
        y = "Interest"
    ) +
    
    # Customize the Y-axis to show "HIGH" at the top and limit the height
    scale_y_continuous(breaks = c(10, 100), labels = c("", "HIGH"), limits = c(0, 110)) +
    
    # 5. Add precise annotations and pointer arrows
    # Cape Verde (Angled text resting on the curve)
    annotate("text", x = 2.4, y = 50, label = "CAPE VERDE!", angle = 65, 
             color = "forestgreen", fontface = "bold", size = 4.5) +
    
    # Peak Haaland Memes
    annotate("text", x = 5.2, y = 95, label = "PEAK HAALAND\nMEMES", 
             color = "darkgoldenrod", fontface = "bold", size = 4.5, hjust = 0) +
    annotate("segment", x = 5, y = 95, xend = 3.65, yend = 95, 
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "black") +
    
    # Norway Out
    annotate("text", x = 5.7, y = 60, label = "NORWAY\nOUT", 
             color = "firebrick", fontface = "bold", size = 4.5, hjust = 0) +
    annotate("segment", x = 5.5, y = 60, xend = 4.3, yend = 60, 
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "black") +
    
    # Final
    annotate("text", x = 8.3, y = 50, label = "FINAL", 
             color = "firebrick", fontface = "bold", size = 4.5, hjust = 0) +
    annotate("segment", x = 8.1, y = 48, xend = 7.6, yend = 42, 
             arrow = arrow(length = unit(0.2, "cm"), type = "closed"), color = "black") +
    
    # 6. Apply a clean, classic theme suitable for precise plotting
    theme_classic() +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_blank(),  # Remove arbitrary x-axis numbers
        axis.ticks.x = element_blank()  # Remove x-axis tick marks
    )

# 7. Display the plot
print(p)
