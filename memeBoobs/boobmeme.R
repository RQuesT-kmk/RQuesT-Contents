library(ggplot2)



data <- data.frame(
    age = c(0,3,5,8,20),
    interest = c(1,1,0,1,1)
)
data
bp1 <- ggplot(data = data)+
    geom_line(mapping = aes(x = age,
                            y = interest),
              colour = "slateblue",
              linewidth = 1)+
    
    scale_x_continuous(breaks = c(0,5,20), 
                       limits = c(0,21),
                       name = "Men's age")+
    
    scale_y_continuous(limits = c(0,1), 
                       breaks = c(0,.5,1), 
                       labels = scales::percent_format(),
                       name = "Men's interest in boobs")+
    
    theme_classic()+
    theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                         type = "closed")))+
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 18))
png("boobinterest.png", width = 720, height = 720)
bp1
dev.off()

pacman::p_load(ggview)
bp1 + canvas(bg = "blue", width = 1080, height = 1080, units = "px")

pacman::p_load(ggsketch, showtext)
font_add_google("Architects Daughter", "handwritten")

showtext_auto()
showtext_opts(dpi = 96)

sketchb <- ggplot(data = data) +
    geom_sketch_line(mapping = aes(x = age, y = interest),
                     colour = "slateblue") +
    # Keep axes strictly starting at 0 and set limits
    scale_x_continuous(breaks = c(0, 5, 20), 
                       limits = c(0, 21),
                       expand = c(0.1, 0),
                       name = "Men's age") +
    scale_y_continuous(breaks = c(0, 0.5, 1), 
                       limits = c(0, 1), 
                       expand = c(0, 0),
                       labels = scales::percent_format(),
                       name = "Men's interest in boobs") +
    theme_sketch() +
    labs(title = "Graph showing men's interest in boobs", size =16)+
    # Clean gridlines and apply hand-written typography
    theme(
        # Remove all background grid lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Apply your casual font globally to all text elements
        title = element_text(family = "handwritten", size = 24),
        axis.text = element_text(family = "handwritten", size = 14),
        axis.title = element_text(family = "handwritten", size = 14),
        
        # X-axis arrow (with corrected length spelling)
        axis.line.x = element_line(
            arrow = grid::arrow(length = unit(0.3, "cm"), type = "closed")
        )
    )


sketchb
