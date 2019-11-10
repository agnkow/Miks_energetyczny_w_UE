
#############################################################
#######################    GRAPHICS   ####################### 
#############################################################


###  Hierarchical clustering  ###   
# plot_hc_1, plot_hc_2, plot_hc_3

plot_hc_1 <- function(res.hc, year, cluster_n){
  
  plot_out <- fviz_dend(res.hc, 
                        type = 'rectangle',
                        k = cluster_n, 
                        cex = 0.8, # label size
                        k_colors = viridis(cluster_n),
                        color_labels_by_k = TRUE, # color labels by groups
                        rect = TRUE, # Add rectangle around groups
                        main = paste0('\n', as.character(year), '\n'),
                        
                        ggtheme = my_theme_title(my_margin_b = -20, my_hjust = 0.05) +
                          theme(plot.background = element_rect(fill = colour_BKGD, colour = colour_BKGD)
                                ,panel.background = element_rect(fill = colour_BKGD, colour = colour_BKGD)
                                ,axis.ticks = element_line(colour = colour_text_axis)
                                ,axis.text.y = element_text(colour = colour_text_axis)
                                ,axis.title.y = element_text(colour = colour_text_axis, size=11)) 
                        )
  return(plot_out)
}





plot_hc_2 <- function(hcluster, year, num_clusters = 2){
  
  # cut dendrogram in given number of clusters
  clusters = cutree(tree = hcluster, k = num_clusters)
  
  phylo_tree = as.phylo(hcluster)          # convert to phylo object
  graph_edges = phylo_tree$edge            # get edges
  graph_net = graph.edgelist(graph_edges)  # convert to graph
  graph_layout = layout.auto(graph_net)    # extract layout (x-y coords)
  
  # colors of labels and points
  txt_pal = viridis(num_clusters)
  txt_col = txt_pal[clusters]
  
  # additional params
  nobs = length(clusters)
  nedges = nrow(graph_edges)
  
  
  par(bg = colour_BKGD)
  # start plot
  plot_out <- plot(graph_layout[,1], 
                   graph_layout[,2], 
                   type = 'n', axes = FALSE,
                   xlab = '', ylab = ''
                   ) 
  mtext(paste0('\n', as.character(year), '\n'), 
        adj=0, line=1, cex=1.4, font=2,
        col = colour_text_axis)
  
  # draw tree branches
  segments(x0 = graph_layout[graph_edges[,1],1], 
           y0 = graph_layout[graph_edges[,1],2],
           x1 = graph_layout[graph_edges[,2],1],
           y1 = graph_layout[graph_edges[,2],2],
           col = '#dcdcdc55', lwd = 3.5)
  
  # add empty nodes
  points(graph_layout[(nobs+1):nedges,1], 
         graph_layout[(nobs+1):nedges,2], 
         col = colour_text_axis, pch = 19, cex = 0.5)
  
  # add node labels
  text(graph_layout[1:nobs,1], 
       graph_layout[1:nobs,2], 
       col = txt_col,
       phylo_tree$tip.label, 
       cex = 1, xpd = TRUE, font = 1)
  
  return(plot_out)
}





plot_hc_3 <- function(res.hc, year, cluster_n){
  
  colors = viridis(cluster_n) 
  clus3 = cutree(res.hc, cluster_n)
  
  par(bg = colour_BKGD)
  plot_out <- plot(as.phylo(res.hc), type = 'fan', 
                   tip.color = colors[clus3],
                   edge.color = colour_text_axis,
                   label.offset = 1, cex = 0.7
                   ) 
  mtext(paste0('\n', as.character(year), '\n'), 
        adj=0, line=1, cex=1.4, font=2,
        col = colour_text_axis)
  
  return(plot_out)
}





plot_k_means <- function(df, df_year, year, cluster_n){
  plot_out <- fviz_cluster(df, 
                           data = df_year,
                           ellipse.type = 'convex',
                           palette = viridis(cluster_n), 
                           main = paste0('\n', as.character(year), '\n'),
                           legend.title = toupper('Skupienia'),
                           
                           ggtheme = my_theme() +
                             my_theme_title(my_margin_b = -1, my_hjust = 0.05) +
                             my_theme_legend_11() +
                             theme(axis.text.x = element_text(angle = 0, colour = colour_text_axis, size=11))
                           ) 
  return(plot_out)
}





plot_means <- function(df_means, year){
  
  plot_out <- 
    ggplot(df_means,
           aes(x=geo, y=value/100,
               group=cluster,
               color=cluster)) + 
    
    geom_point(size=5, alpha=0.8) + 
    geom_line(size=1) +
    
    ggtitle('\n  \n') +
    ylab('Sredni % udzial \n') + 
    xlab('Zrodla energii') +
    
    scale_y_continuous(limits = c(0, 1), labels = percent) +
    scale_color_viridis(option = 'D', discrete = TRUE, 
                        name = toupper('Skupienia'))
  
  plot_out <- plot_out + 
    my_theme() +
    my_theme_title(my_margin_b = -1, my_hjust = 0.05) +
    my_theme_legend_11() +
    theme(panel.grid.minor = element_line(colour = colour_grid_minor))
  
  return(plot_out)
}





barplot_country <- function(df, var_group, year_p, 
                            my_palette_cut = my_palette,
                            width_bar = 0.9){
  
  plot_out <- 
    ggplot(data = df, 
           aes(y = value/100, 
               x = var_group, 
               group = variable)) +
    
    geom_col(aes(fill = variable), width = width_bar) +
    
    ggtitle(as.character(year_p)) +
    ylab('% udzial \n') + 
    xlab('') +
    
    scale_fill_manual(name = toupper('Zrodla energii'), 
                      values = my_palette_cut) + 
    scale_y_continuous(labels = percent) 
  
  plot_out <- plot_out + 
    my_theme() +
    my_theme_title(my_margin_b = 6, my_hjust = 0.01) +
    theme(legend.position = 'right')
  
  return(plot_out)
}





my_silhouette <- function(df_res, year_p, 
                          mean_Si = 0, label_TF = TRUE){
  
  plot_out <- fviz_silhouette(df_res, 
                              label = label_TF, #print.summary = TRUE,
                              legend.title = toupper('Skupienia'))
  
  plot_out <- plot_out + 
    ggtitle(paste0('\n', as.character(year_p), ',    ', 
                   'Si: ', as.character(round(mean_Si,2)))) +
    ylab('') + #ylab('Si') 
    
    scale_color_viridis(option = 'D', discrete = TRUE, 
                        name = toupper('Skupienia'), alpha = 0.8) +
    scale_fill_viridis(option = 'D', discrete = TRUE, 
                       name = toupper('Skupienia'), alpha = 0.8) 
  
  plot_out <- plot_out + 
    my_theme() +
    my_theme_title(my_margin_b = 6, my_hjust = 0.01) +
    theme(legend.position = 'right')
  
  return(plot_out)
}





my_scree_plot <- function(df, r1_1, year_p){
  
  plot_out <- 
    ggplot(r1_1, 
           aes(x=factor(names(df)[r1_1$V1], 
                        level = names(df)[r1_1$V1]), 
               y=r1_1$V2,
               group=1)) + 
    
    geom_point(size=5, colour=viridis(1, alpha=0.8), shape = 15) + 
    geom_line(size=2, colour=viridis(1, alpha=0.5)) +
    ggtitle(paste0('\n ', year_p, '\n')) +
    ylab('topri \n') + 
    xlab('Zrodla energii')
  
  plot_out <- plot_out + 
    my_theme() +
    my_theme_title(my_margin_b = -1, my_hjust = 0.05) +
    my_theme_legend_11() +
    theme(panel.grid.minor = element_line(colour = colour_grid_minor)) 
  
  return(plot_out)
}





my_optimal_nclust_plot <- function(df, gap_stat, 
                                   year_p, FUNcluster_my, my_k_max){
  
  plot1 <- fviz_nbclust(x = df, 
                        FUNcluster = FUNcluster_my, 
                        k.max = my_k_max,
                        method = 'wss',
                        linecolor = colour_p1) + 
    ggtitle(paste0('\n', year_p, '\n')) +
    ylab('wss \n') + 
    xlab('Liczba skupien') +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    my_theme_title(my_margin_b = -1, my_hjust = 0.05)
  
  
  plot2 <- fviz_nbclust(x = df, 
                        FUNcluster = FUNcluster_my, 
                        method = 'silhouette',
                        k.max = my_k_max,
                        linecolor = colour_p6) + 
    ggtitle('\n \n') +
    ylab('silhouette \n') + 
    xlab('Liczba skupien') +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    my_theme_title(my_margin_b = -1, my_hjust = 0.05) 
  
  
  plot3 <- fviz_gap_stat(gap_stat, 
                         maxSE = list(method = 'firstSEmax',
                                      SE.factor = 1),
                         linecolor = colour_p3) + 
    ggtitle('\n \n') +
    ylab('gap_stat \n') + 
    xlab('Liczba skupien') +
    my_theme() +
    theme(axis.text.x = element_text(angle = 0)) +
    my_theme_title(my_margin_b = -1, my_hjust = 0.05)
  
  
  plots <- grid.arrange(plot1, plot2, plot3, ncol = 3)
  return(plots)
  
}





plot_GWh <- function(df, y_var){
  
  plot_out <- 
    ggplot(df, 
           aes(x = as.character(year), 
               y = df[,y_var]/1000,
               group = GEO.TIME,
               color = GEO.TIME)) + 
    
    geom_point(size=5, alpha=0.8) + 
    geom_line(size=1) +
    ggtitle('\n  \n') +
    ylab('TWh \n') + 
    xlab('') +
    scale_color_viridis(option = 'D', discrete = TRUE, 
                        name = toupper('Panstwa'))
  
  plot_out <- plot_out + 
    my_theme() +
    my_theme_title(my_margin_b = -1, my_hjust = 0.01) +
    theme(panel.grid.minor = element_line(colour = colour_grid_minor),
          legend.position = 'right')
  
  return(plot_out)
}





my_DT_style_1 <- function(df){
  
  df_1 = df
  my.options <- list(autoWidth = FALSE
                     ,pageLength = FALSE
                     ,paging = FALSE
                     ,info = FALSE
                     ,orderClasses = TRUE
                     ,scrollY = 300
                     ,columnDefs = list(list(width = '30%', targets = list(0,1)))
                     ,dom = 't')
  
  header.style <- "th { font-family: 'Montserrat', sans-serif; 
  font-weight: bold; color: rgb(229,229,229); 
  background-color: rgb(68,1,84); }"
  header.names <- c(colnames(df_1))
  
  # design the header of the table using CSS
  my.container <-  withTags(table(
    style(type = 'text/css', header.style),
    thead(
      tr(
        lapply(header.names, th, style = "text-align: left;  
               border-right-width: 1px; border-right-style: solid; 
               border-right-color: rgb(64,64,64); 
               border-bottom-width: 1px; border-bottom-style: solid; 
               border-bottom-color: rgb(64,64,64);
               border-top-width: 1px; border-top-style: solid;
               border-top-color: rgb(64,64,64);")  
        ))))
  
  df_2 = DT::datatable(df_1,
                       filter = list(position = 'top', clear = TRUE, plain = FALSE),
                       options = my.options,
                       container = my.container,
                       rownames = FALSE, 
                       width = '100%', height = '100%'
                       ) 
  
  df_2 <- formatStyle(df_2,
                      columns = c('Rok', 'Skupienie', 'Panstwo'),
                      borderCollapse = 'collapse',  
                      borderTopColor = 'rgb(64,64,64)',
                      borderBottomColor = 'rgb(64,64,64)',
                      borderBottomStyle = 'solid',
                      borderBottomWidth = '0.1px',
                      borderRightColor = 'rgb(64,64,64)',
                      borderRightStyle = 'solid',
                      borderRightWidth = '1px',
                      
                      color = 'rgb(229,229,229)',
                      fontFamily = 'Arial',
                      fontSize = '14px',
                      fontWeight = 'normal',
                      
                      lineHeight = 'normal',
                      paddingBottom = '5.5px',
                      paddingLeft = '5.5px',
                      paddingRight = '5.5px',
                      paddingTop = '5.5px',
                      textAlign = 'left',
                      verticalAlign = 'middle',
                      backgroundColor = 'rgb(39, 43, 48)'
                      )
  return(df_2)
}




