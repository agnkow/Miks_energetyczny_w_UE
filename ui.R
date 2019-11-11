
#############################################################
########################      UI     ######################## 
#############################################################


navbarPage(
  theme = shinytheme('slate'),
  title = h4(toupper('Miks energetyczny w UE'),
             style = "font-family: 'Old Standard TT', serif;
             line-height: 0.3;"),
  
  

  
###### SKUPIENIA ######
  
 tabPanel(h4(toupper('Skupienia'),
             style = "font-family: 'Montserrat', sans-serif;
             line-height: 0.3; font-size: 14px;"),
          
   headerPanel(
     h1('Generacja energii elektrycznej netto ze względu na źródła', 
        style = "font-family: 'Old Standard TT', serif;
        font-weight: 500; line-height: 1.5;" 
       )),
  
  sidebarPanel(position = 'left', width = 2,
               
               radioButtons('type_conventional_1',
                            'Źródła konwencjonalne:',
                            choices = c('sumarycznie' = 'sum',
                                        'indywidualnie (od.2017r.)' = 'ind'),
                            selected = 'sum'),
               
               sliderInput('clusters', 
                           label = 'Liczba skupień:', 
                           min = 1, max = max_clusters, value = 4),
               
               radioButtons('type_cluster_method', 
                            'Metoda:',
                            choices = c('k-średnich' = 'k-means',
                                        'warda (odl.euklidesowa)' = 'warda'),
                            selected = 'k-means'),
               
               uiOutput('type_cluster_plot')
               ),
  
  sidebarPanel(position = 'left', width = 9,
               fluidRow(
                 column(2, 
                        uiOutput('year_1C')),
                 column(3,
                        uiOutput('var_1C')),
                 column(7,
                        selectInput(
                          inputId = 'countries',
                          label = 'Państwa:',
                          choices = countries,
                          multiple = TRUE,
                          selected = countries))
                 )),
  
  
  
  mainPanel(
    tabsetPanel(
      
      tabPanel(title = h4('Wykresy',
               style = "font-family: 'Montserrat', sans-serif;
               line-height: 0.3; font-size: 14px;"),
               
               h1(' '),
               fluidRow(column(12,
                               splitLayout(cellWidths = c('50%', '50%'), 
                                           uiOutput('plots_cluster', width='400px',height='350px'),
                                           uiOutput('plots_means', width='420px',height='350px')
                                           )))
               ),
      
      tabPanel(title = h4('Tabela',
               style = "font-family: 'Montserrat', sans-serif;
               line-height: 0.3; font-size: 14px;"),
               
               #tableOutput('cluster_table')
               htmlOutput('table_text'),
               fluidRow(column(
                 dataTableOutput('cluster_table_3'), width = 7))
               ),
      
      tabPanel(title = h4('Silhouhette',
               style = "font-family: 'Montserrat', sans-serif;
               line-height: 0.3; font-size: 14px;"),
               uiOutput('plots_silhouhette')
               ),
        
      tabPanel(title = h4('HINoV',
               style = "font-family: 'Montserrat', sans-serif;
               line-height: 0.3; font-size: 14px;"),
               htmlOutput('HINoV_text'),
               uiOutput('plots_scree')
               ),
      
      tabPanel(title = h4('N-Skupień',
               style = "font-family: 'Montserrat', sans-serif;
               line-height: 0.3; font-size: 14px;"),
               h4('Optymalna liczba skupień'),
               uiOutput('plots_optimal_nclust')
               )
      
      )
    )
  ),





###### STRUKTURA ######

  tabPanel(h4(toupper('Struktura'), 
              style = "font-family: 'Montserrat', sans-serif;
              line-height: 0.3; font-size: 14px;"),
           
           fluidRow(
             column(2, wellPanel(
               radioButtons('type_conventional_2',
                            'Źródła konwencjonalne:',
                            choices = c('sumarycznie' = 'sum',
                                        'indywidualnie (od.2017r.)' = 'ind'),
                            selected = 'sum'))),
             
             column(2, wellPanel(
               uiOutput('type_plot_B_sum_ind'))),
             
             column(3, wellPanel(
               uiOutput('year_country_1B')))
             ),


  mainPanel(
      fluidRow( uiOutput('plots') )
      )
  ),





###### ILOŚCI (TWh) ######

tabPanel(h4('ILOŚCI (TWh)',
            style = "font-family: 'Montserrat', sans-serif;
            line-height: 0.3; font-size: 14px;"),
         
         fluidRow(
           column(3, wellPanel(
             selectInput(
               inputId = 'var_GWh',
               label = 'Zmienna:',
               choices = names(df_total)[names(df_total) %!in% c('GEO.TIME', 'year', 'Wegiel', 
                                                                 'Ropa naftowa', 'Gaz ziemny',  
                                                                 'Biomasa', 'Inne nieodnawialne')],
               multiple = FALSE
               ,selected = 'Calkowita generacja energii netto'
             ))),
           
           column(4, wellPanel(
             selectInput(
               inputId = 'countries_GWh',
               label = 'Państwa:',
               choices = countries,
               multiple = TRUE
               ,selected = 'Polska'
             )))
           ),
         
         mainPanel(
           fluidRow( plotOutput('plot_GWh_Out') )
           )
         )



)  # END


