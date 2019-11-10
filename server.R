
#############################################################
########################    SERVER   ######################## 
#############################################################


shinyServer(function(input, output, session) {
  

###### SKUPIENIA ######
  
  output$type_cluster_plot <- renderUI({
    if (is.null(input$type_cluster_method))
      return()
    
    # Depending on input$type_cluster_method, we'll generate a different
    # UI component and send it to the client.
    switch(input$type_cluster_method,
           'warda' = radioButtons('type_cluster_plot_1', 
                                  'Dendrogram:',
                                  choices = c('rectangle' = 'rectangle',
                                              'unrooted' = 'unrooted',
                                              'fan' = 'fan'),
                                  selected = 'unrooted'))
  })
  
  
  
  
  
  
  
  
  
  
  # Insert the right number of plot output objects into the web page
  output$plots_cluster <- renderUI({
    plot_output_list <- lapply(input$years_multi, function(p1) {
      plotname_p1 <- paste('plot_cluster', p1, sep='')
      plotOutput(plotname_p1)
    })
    
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  })


  output$plots_means <- renderUI({
    plot_output_list_2 <- lapply(input$years_multi, function(p1) {
      plotname_p2 <- paste('plot_means', p1, sep='')
      plotOutput(plotname_p2)
    })
    do.call(tagList, plot_output_list_2)
  })
  
  
  output$plots_silhouhette <- renderUI({
    plot_output_list_3 <- lapply(input$years_multi, function(p1) {
      silhouhette_p1 <- paste('plot_silhouhette', p1, sep='')
      plotOutput(silhouhette_p1)
    })
    do.call(tagList, plot_output_list_3)
  })

  
  output$plots_scree <- renderUI({
    if (input$type_cluster_method == 'warda'){
      plot_output_list_4 <- lapply(input$years_multi, function(p1) {
        scree_p1 <- paste('plot_scree', p1, sep='')
        plotOutput(scree_p1)
      })
      do.call(tagList, plot_output_list_4)
    }
  })

  
  output$plots_optimal_nclust <- renderUI({
    plot_output_list_5 <- lapply(input$years_multi, function(p1) {
      optimal_nclust_p1 <- paste('plot_optimal_nclust', p1, sep='')
      plotOutput(optimal_nclust_p1)
    })
    do.call(tagList, plot_output_list_5)
  })
      
    
  
  # Call renderPlot for each one. Plots are only actually generated when they
  # are visible on the web page.
  
  for (p1 in years) {
    # Need local so that each item gets its own number. Without it, the value
    # of i in the renderPlot() will be the same across all instances, because
    # of when the expression is evaluated.
    local({
      my_i <- p1
      plotname_p1 <- paste('plot_cluster', my_i, sep='')
      silhouhette_p1 <- paste('plot_silhouhette', my_i, sep='')
      plotname_p2 <- paste('plot_means', my_i, sep='')
      scree_p1 <- paste('plot_scree', my_i, sep='')
      optimal_nclust_p1 <- paste('plot_optimal_nclust', my_i, sep='')
      
     
       selectedData_year_1 <- reactive({
        df_1 <- df[df$year == my_i,]
        df_1 <- prepare_data(df = df_1, 
                             conventional = input$type_conventional_1)
        df_1 <- df_1[ , names(df_1) %in% input$vars]
        df_1 <- df_1[rownames(df_1) %in% input$countries, ]
      })
      
      
       
      selectedData <- reactive({
        if (input$type_cluster_method == 'warda'){
          res.hc <- hcut(selectedData_year_1(), 
                         k = input$clusters, 
                         hc_method = 'ward.D2',
                         hc_metric = 'euclidean')
        } else if (input$type_cluster_method == 'k-means'){
          km.res <- kmeans(selectedData_year_1(), 
                           input$clusters, 
                           nstart = 25)
        #} else if (input$type_cluster_method == 'k_medoids'){
        #  pam.res <- pam(selectedData_year_1(), 
        #                 input$clusters)
        }
      })
      
      
      
      m_1_func <- function(df, res.hc, cluster_n){
        memb <- cutree(res.hc, k = cluster_n)
        m_1 <- NULL
        for(k in 1:cluster_n){
          m_1 <- rbind(m_1, colMeans(df[memb == k, , drop = FALSE]))
        }
        return(m_1)
      }
      
      
      
      selectedData_mean <- reactive({
        
        if (input$type_cluster_method == 'warda'){
          m_1 <- m_1_func(selectedData_year_1(), selectedData(), input$clusters)
        } else if (input$type_cluster_method == 'k-means'){
          m_1 <- selectedData()$centers
        #} else if (input$type_cluster_method == 'k_medoids'){
        #  m_1 <- selectedData()$centers
        }
        
        df_1 <- as.data.frame(t(m_1))
        names(df_1) <- paste0(rep(''), seq(input$clusters))
        df_1 <- stack(df_1)
        names(df_1) <- c('value', 'cluster')
        df_1['geo'] <- rep(names(m_1[1,]),input$clusters)
        
        return(df_1)
      })
      
      

      HINoV_data <- reactive({
        
        if (input$type_cluster_method == 'warda'){
          r1 <- HINoV.Mod(selectedData_year_1(), 
                          type='metric',
                          s=1, u=input$clusters, 
                          distance='d2',   #'d4'-squared Euclidean,'d2'-Euclidean
                          method='ward.D2', 
                          Index='RAND')
          r1_1 <- as.data.frame(r1$stopri)
          return(r1_1)
        #} else if (input$type_cluster_method == 'k-means'){
        #  r1 <- HINoV.Mod(selectedData_year_1(), 
        #                  type='metric',
        #                  s=2, u=input$clusters,
        #                  method='kmeans', 
        #                  Index='RAND')
        #  r1_1 <- as.data.frame(r1$stopri)
        #  return(r1_1)
        #} else if (input$type_cluster_method == 'k_medoids'){
        #  r1 <- HINoV.Mod(selectedData_year_1(), 
        #                  type='metric',
        #                  s=1, u=input$clusters,
        #                  method='median', 
        #                  Index='RAND')  
        #  r1_1 <- as.data.frame(r1$stopri)
        #  return(r1_1)
        }
      })

      
      
      gap_stat_data <- reactive({
        
        if (input$type_cluster_method == 'warda'){
          gap_stat <- clusGap(x = selectedData_year_1(), 
                              FUNcluster = hcut, 
                              K.max = max_clusters, 
                              nstart = 25, B = 50)
        } else if (input$type_cluster_method == 'k-means'){
          gap_stat <- clusGap(x = selectedData_year_1(), 
                              FUNcluster = kmeans, 
                              K.max = max_clusters, 
                              nstart = 25, B = 50)
        }
        return(gap_stat)
      })
      
      
       
      output[[plotname_p1]] <- renderPlot({
        
        if (input$type_cluster_method == 'warda'){
          if (input$type_cluster_plot_1 == 'rectangle'){
            plot_hc_1(selectedData(), my_i, input$clusters)
          } else if (input$type_cluster_plot_1 == 'unrooted'){
            plot_hc_2(selectedData(), my_i, input$clusters)
          } else if (input$type_cluster_plot_1 == 'fan'){
            plot_hc_3(selectedData(), my_i, input$clusters)
          }
          
        } else if (input$type_cluster_method == 'k-means'){
          plot_k_means(selectedData(), selectedData_year_1(), 
                       my_i, input$clusters)
        } #else if (input$type_cluster_method == 'k_medoids'){}
      })
      
      
      
      output[[plotname_p2]] <- renderPlot({
        plot_out <- plot_means(selectedData_mean(), my_i)
        return(plot_out)
      })
      
      
      
      output[[silhouhette_p1]] <- renderPlot({
        if (input$type_cluster_method == 'warda'){
          ms_1 <- my_silhouette(selectedData(), my_i)
          my_silhouette(selectedData(), my_i, mean(ms_1$data$sil_width))
        } else if (input$type_cluster_method == 'k-means'){
          sil <- silhouette(selectedData()$cluster, 
                            dist(selectedData_year_1()))
          my_silhouette(sil, my_i, mean(sil[, 'sil_width']),
                        label_TF = FALSE)
        } #else if (input$type_cluster_method == 'k_medoids'){}
      })
      
      
      
      output[[scree_p1]] <- renderPlot({
        if (input$type_cluster_method == 'warda'){
          plot_out <- my_scree_plot(selectedData_year_1(), 
                                    HINoV_data(), my_i)
          return(plot_out)
        } else {
          return()
        }
      })
      
      
      output$HINoV_text <- renderUI({
        if (input$type_cluster_method == 'k-means'){
          t1 <- 'HINoV jest dostępna wyłącznie dla metody Warda.'
          t0 <- '\n'
          HTML(paste(t0, t1, sep = '<br/>'))
        }
      })
      
      
      
      output[[optimal_nclust_p1]] <- renderPlot({
        
        if (input$type_cluster_method == 'warda'){
          plot_out <- my_optimal_nclust_plot(selectedData_year_1(), 
                                             gap_stat_data(), my_i, 
                                             FUNcluster_my = hcut, 
                                             my_k_max = max_clusters)
        } else if (input$type_cluster_method == 'k-means'){
          plot_out <- my_optimal_nclust_plot(selectedData_year_1(), 
                                             gap_stat_data(), my_i, 
                                             FUNcluster_my = kmeans, 
                                             my_k_max = max_clusters)
        }
 

        return(plot_out)
      })
      
    })
  }  
  
  
  
  
  
  
  
  
  
  
  cluster_table <- reactive({
    
    if (input$type_cluster_method == 'k-means'){
    
      table_s <- data.frame()
      for (tab_1 in input$years_multi) {
          my_i <- tab_1
          plotname_p2 <- paste('plot_means', my_i, sep='')
          
          df_1 <- df[df$year == my_i,]
          df_1 <- prepare_data(df_1)
          df_1 <- df_1[ , names(df_1) %in% input$vars]
          df_1 <- df_1[rownames(df_1) %in% input$countries, ]
           
          km.res <- kmeans(df_1, input$clusters, nstart = 25)
  
          s_1 <- km.res$cluster
          df_s1 <- stack(s_1)
          names(df_s1) <- c('Skupienie', 'Panstwo')
          df_s1['Rok'] <- my_i
          df_s1 <- df_s1[,c('Rok', 'Skupienie', 'Panstwo')]
          df_s1 <- df_s1[order(df_s1$Rok,
                               df_s1$Skupienie, 
                               df_s1$Panstwo),]
          table_s <- rbind(table_s, df_s1)
      }
    }
    return(table_s)
  })
  
  
  
  output$cluster_table_3 <- renderDataTable({
    if (input$type_cluster_method == 'k-means'){
      my_DT_style_1(cluster_table())
    } 
  })


  output$table_text <- renderUI({
    if (input$type_cluster_method == 'warda'){
      t1 <- 'Tabela jest dostępna wyłącznie dla metody k-średnich.'
      t0 <- '\n'
      HTML(paste(t0, t1, sep = '<br/>'))
    }
  })
  
  
  
  
  
  
  
###### STRUKTURA ######
  
  output$plots <- renderUI({
    plot_output_list <- lapply(c(input$year_1B_country_1B), function(i) {
      plotname <- paste('plot', i, sep='')
      plotOutput(plotname)
    })
    do.call(tagList, plot_output_list)
  })
  
  
  
  for (i in c(years, countries)) {
    local({
      my_i <- i
      plotname <- paste('plot', my_i, sep='')
      
      
      selectedData_year_1B <- reactive({
        
        if (input$type_plot_B %in% c('czas_sum','czas_ind')){
          
          df_1 <- df[as.character(df$year) == my_i,]
          df_1 <- prepare_data(df_1, 
                               conventional = input$type_conventional_2)
          df_1$country <- row.names(df_1)   
          df_2 <- melt(df_1, id.vars = 'country')
          
        } else if (input$type_plot_B == 'panstwa') {
          
          df_1 <- df[as.character(df$'GEO.TIME') == my_i,]
          df_1 <- prepare_data(df_1, var_del='GEO.TIME', index_var='year',
                               conventional = input$type_conventional_2)
          df_1$year <- row.names(df_1)   
          df_2 <- melt(df_1, id.vars = 'year')
        }
        
        if (input$type_conventional_2 == 'sum'){
          target <- sources
        } else if (input$type_conventional_2 == 'ind'){
          target <- sources_ind         
        }

        df_2$variable <- reorder.factor(df_2$variable, new.order = target)
        return(df_2)
      }) 
      
      
      
      palette_cut <- reactive({
        if (input$type_conventional_2 == 'sum'){
          my_palette_df_cut <- my_palette_df %>% 
            filter(sources %in% unique(selectedData_year_1B()$variable))
          my_palette_cut <- as.character(my_palette_df_cut$my_palette)
        } else if (input$type_conventional_2 == 'ind'){
          my_palette_df_cut <- my_palette_df_ind %>% 
            filter(sources_ind %in% unique(selectedData_year_1B()$variable))
          my_palette_cut <- as.character(my_palette_df_cut$my_palette_ind)
        }
        return(my_palette_cut)
      }) 
      
      
      output[[plotname]] <- renderPlot({
        if (input$type_plot_B %in% c('czas_sum','czas_ind')){
          barplot_country(selectedData_year_1B(),
                          selectedData_year_1B()$country,
                          my_i, palette_cut())
        } else if (input$type_plot_B == 'panstwa') {
          if (input$type_conventional_2 == 'sum') {
            barplot_country(selectedData_year_1B(),
                            selectedData_year_1B()$year,
                            my_i, palette_cut()) 
          } else if (input$type_conventional_2 == 'ind'){
            df_cut <- selectedData_year_1B()
            df_cut <- df_cut[df_cut$year >= 2017, ]
            barplot_country(df_cut, df_cut$year,
                            my_i, palette_cut(),
                            width_bar = 0.3) 
          }
        }
      })
      
    })
  }  

  
  
  
  
  
  
  
  
  
  output$year_country_1B <- renderUI({
    if (is.null(input$type_plot_B))
      return()
    
    switch(input$type_plot_B,
           'czas_sum' = selectInput(
                                 inputId = 'year_1B_country_1B',
                                 label = 'Lata:',
                                 choices = years,
                                 multiple = TRUE,
                                 selected = years[1]
                                ),
           
           'panstwa' = selectInput(
                                 inputId = 'year_1B_country_1B',
                                 label = 'Państwa:',
                                 choices = countries,
                                 multiple = TRUE,
                                 selected = 'Polska'
                               ),
           
           'czas_ind' = selectInput(
                                 inputId = 'year_1B_country_1B',
                                 label = 'Lata:',
                                 choices = years[years >= 2017],
                                 multiple = TRUE,
                                 selected = years[years >= 2017][1]
                                 )
           )
  })

  
  
  
  
  
  output$type_plot_B_sum_ind <- renderUI({
    if (is.null(input$type_conventional_2))
      return()
    
    switch(input$type_conventional_2,
           'sum' = radioButtons('type_plot_B', 'Porównanie:',
                      choices = c('czas' = 'czas_sum', 
                                  'państwa' = 'panstwa'),
                          selected = 'panstwa'),
           
           'ind' = radioButtons('type_plot_B', 'Porównanie:',
                      choices = c('czas' = 'czas_ind', 
                                  'państwa' = 'panstwa'),
                          selected = 'panstwa')
           )
  })
  
  
  
  
  
  
  

  
  
  output$year_1C <- renderUI({
    if (is.null(input$type_conventional_1))
      return()

    switch(input$type_conventional_1,
           'sum' = selectInput(
             inputId = 'years_multi',
             label = 'Lata:',
             choices = years,
             multiple = TRUE,
             selected = c(years[1],years[2])
           ),
           
           'ind' = selectInput(
             inputId = 'years_multi',
             label = 'Lata:',
             choices = years[years >= 2017],
             multiple = TRUE,
             selected = c(years[years >= 2017][1], 
                          years[years >= 2017][2])
           ))
  })
  
  
  
  output$var_1C <- renderUI({
    if (is.null(input$type_conventional_1))
      return()
    
    switch(input$type_conventional_1,
           'sum' = selectInput(
             inputId = 'vars',
             label = 'Zmienne:',
             choices = vars[vars %!in% c('Brak danych')],
             multiple = TRUE,
             selected = vars[vars %!in% c('Inne','Brak danych')]
             ),
           
           'ind' = selectInput(
             inputId = 'vars',
             label = 'Zmienne:',
             choices = vars_ind[vars_ind %!in% c('Brak danych','Konwencjonalne NA')],
             multiple = TRUE
             ,selected = vars_ind[vars_ind %!in% c('Inne','Brak danych',
                                                   'Inne nieodnawialne',
                                                   'Konwencjonalne NA')]
             ))
  })

  
  
  
  
  
  
  
  
  
###### ILOŚCI (TWh) ######  
  
  selectedData_GWh <- reactive({
    df_1 <- df_total[df_total$GEO.TIME %in% input$countries_GWh, 
                     c('year', 'GEO.TIME', input$var_GWh)]
    if(input$var_GWh %in% c('Wegiel', 'Ropa naftowa', 'Gaz ziemny',  
                            'Biomasa', 'Inne nieodnawialne')){
      df_1 <- df_1[df_1$year > 2017, ]
    }
    return(df_1)
  })
  
  
  output$plot_GWh_Out <- renderPlot({
    plot_out <- plot_GWh(selectedData_GWh(), y_var = input$var_GWh)
    return(plot_out)
  })
  
  
    
  
  
})  # END


