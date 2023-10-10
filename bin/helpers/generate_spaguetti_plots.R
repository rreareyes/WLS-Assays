generate_spaguetti_plot <- function(dataset = NULL, response = replicate_conc, font_size = 12){
  
  ggplot(dataset |> filter(!is.na({{response}})),
         aes(x = hours_before_freeze,
             y = {{response}})) +
    
    geom_path(aes(group = identifier,
                  color = identifier),
              position = position_dodge(0.3),
              alpha    = 0.3) +
    
    geom_point(aes(group = identifier,
                   fill  = identifier),
               position = position_dodge(0.3),
               alpha    = 0.3,
               shape    = 21,
               color    = "black") +
    
    stat_summary(aes(group = 1),
                 geom      = "line",
                 fun       = "mean",
                 col       = "#c5050c",
                 linewidth = 1) +
    
    stat_summary(geom  = "point",
                 fun   = "mean",
                 col   = "black",
                 size  = 3,
                 shape = 22,
                 fill  = "#c5050c") +
    
    scale_y_continuous(name = "Concentration") +
    
    scale_x_discrete(name = "Hours before freeze") +
    
    scale_color_viridis(discrete = T, end = 0.7, option = "magma") +
    
    scale_fill_viridis(discrete = T, end = 0.7, option = "magma") +
    
    guides(color = "none", fill = "none") +
    
    theme_bw(base_size = font_size) +
    
    coord_flex_cart(bottom = capped_horizontal(capped = "both"), left = capped_vertical(capped = "both")) +
    
    background_grid(major = "y") +
    
    theme(axis.line        = element_line(),
          panel.border     = element_blank(),
          panel.grid.major = element_line(linewidth = 0.25, 
                                          linetype = "dotted"))
  
}