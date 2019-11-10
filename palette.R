
############################################################
#######################    PALETTE   ####################### 
############################################################


rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)

# colors like default ggplot2
ggcolors <- function(n, alfa) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100, alpha = alfa)[1:n]
}



colour_BKGD <- rgb2hex(39,43,48)         
colour_text_axis <- rgb2hex(229,229,229)   
colour_grid_minor <- rgb2hex(64,64,64)    
grey_96 <- rgb2hex(96,96,96)

colour_p1 <- rgb2hex(253,231,37)
colour_p2 <- rgb2hex(159,218,58)
colour_p3 <- rgb2hex(74,193,109)
colour_p4 <- rgb2hex(31,161,135)
colour_p5 <- rgb2hex(39,127,142)
colour_p6 <- rgb2hex(54,92,141)
colour_p7 <- rgb2hex(70,51,126)
colour_p8 <- rgb2hex(68,1,84)



sources <- c('Brak danych','Inne',
             'Geotermia','Slonce','Wiatr',
             'Woda','Atom','Konwencjonalne')

sources_ind <- c('Brak danych','Inne',
                 'Geotermia','Slonce','Wiatr',
                 'Woda','Atom','Konwencjonalne NA',
                 'Inne nieodnawialne','Biomasa',
                 'Gaz ziemny','Ropa naftowa','Wegiel')

my_palette <- c(grey_96,
                colour_p1, colour_p2, 
                colour_p3, colour_p4, 
                colour_p6, colour_p7, 
                colour_p8)

my_palette_ind <- magma(13)

my_palette_df <- data.frame(my_palette, sources)
my_palette_df_ind <- data.frame(my_palette_ind, sources_ind)





my_theme <- function() {
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
         
        axis.line = element_line(colour = colour_text_axis), 
        axis.ticks = element_line(colour = colour_text_axis),
        axis.text.x = element_text(angle = 45, hjust = 1, colour = colour_text_axis, size=11),
        axis.text.y = element_text(colour = colour_text_axis, size=11),
        axis.title.x = element_text(colour = colour_text_axis, size=11),
        axis.title.y = element_text(colour = colour_text_axis, size=11),
         
        plot.background = element_rect(fill = colour_BKGD, colour = colour_BKGD),
        panel.background = element_rect(fill = colour_BKGD, colour = colour_BKGD),
         
        legend.background = element_rect(fill = colour_BKGD),
        legend.title = element_text(colour = colour_text_axis, size=11),
        legend.text = element_text(colour = colour_text_axis, size=11))  
  }



my_theme_title <- function(my_margin_b, my_hjust) {
  theme(plot.title = element_text(color = colour_text_axis, size=16,
                                  margin = margin(b = my_margin_b), 
                                  hjust = my_hjust, face = 'bold'),
        plot.subtitle = element_text(color = colour_text_axis, size=11, 
                                     margin = margin(b = my_margin_b), 
                                     hjust = my_hjust))
  }



my_theme_legend_11 <- function() {
  theme(legend.justification = c(1, 1), 
        legend.position = c(1, 1),
        legend.direction = 'horizontal')
  }




