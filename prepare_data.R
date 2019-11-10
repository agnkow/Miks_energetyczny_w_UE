
############################################################
############    DATA PREPARATION - FUNCTIONS    ############ 
############################################################


'%!in%' <- function(x,y)!('%in%'(x,y))
replace_na_df <- function(x) { replace(x, is.na(x), 0) }



rename_geo_my <- function(df) {
  
  df <- df[complete.cases(df[ , 1]),]
  df <- merge(x = df, y = dict_geo, 
              by.x = 'GEO.TIME', by.y = 'EN',
              all.x = TRUE)
  df['GEO.TIME'] <- df['PL']
  df <- df[ , names(df) != 'PL']
  return(df)
}



rename_vars_my <- function(df){
  
  names(df)[names(df) == 'Hydro'] <- 'Woda'
  names(df)[names(df) == 'Geothermal'] <- 'Geotermia'
  names(df)[names(df) == 'Nuclear'] <- 'Atom'
  names(df)[names(df) == 'Coal...combustion'] <- 'Wegiel'
  names(df)[names(df) == 'Oil...combustion'] <- 'Ropa naftowa'
  names(df)[names(df) == 'Natural.Gas...combustion'] <- 'Gaz ziemny'
  names(df)[names(df) == 'Renewables...combustion'] <- 'Biomasa'
  names(df)[names(df) == 'Other.Non.Renewable.Fuels...combustion'] <- 'Inne nieodnawialne'
  names(df)[names(df) == 'Wind'] <- 'Wiatr'
  names(df)[names(df) == 'Solar'] <- 'Slonce'
  names(df)[names(df) == 'Tide..wave..ocean.and.other.non.combustible.sources'] <- 'Fale morskie'
  names(df)[names(df) == 'Others'] <- 'Inne'
  names(df)[names(df) == 'Conventional.Thermal'] <- 'Konwencjonalne'
  names(df)[names(df) == 'Conventional NA'] <- 'Konwencjonalne NA'
  
  names(df)[names(df) == 'Total'] <- 'Calkowita generacja energii netto'
  names(df)[names(df) == 'Source NA'] <- 'Brak danych'
  return(df)
}



create_vars_NA <- function(df){
  
  df['Source NA'] <- 100 - 
    df['Hydro'] - 
    df['Geothermal'] - 
    df['Nuclear'] - 
    df['Wind'] - 
    df['Solar'] - 
    df['Others'] - 
    df['Tide..wave..ocean.and.other.non.combustible.sources'] -
    df['Conventional.Thermal']
  
  df['Conventional NA'] <- 100 - 
    df['Hydro'] - 
    df['Geothermal'] - 
    df['Nuclear'] - 
    df['Wind'] - 
    df['Solar'] - 
    df['Others'] - 
    df['Tide..wave..ocean.and.other.non.combustible.sources'] -
    df['Coal...combustion'] - 
    df['Oil...combustion'] - 
    df['Natural.Gas...combustion'] - 
    df['Renewables...combustion'] -
    df['Other.Non.Renewable.Fuels...combustion'] - 
    df['Source NA']
  
  df['Source NA'] <- round(df['Source NA'],2)
  df['Conventional NA'] <- round(df['Conventional NA'],2)
  
  for (i in 1:nrow(df)) {
    if (df[i,'Source NA'] < 1){
      df[i,'Source NA'] = 0
    }
    if (df[i,'Conventional NA'] < 1){
      df[i,'Conventional NA'] = 0
    }
  }
  return(df)
}



prepare_data <- function(df, 
                         var_del='year',
                         index_var='GEO.TIME',
                         conventional='sum'){
  
  #df <- df[!df$GEO.TIME == 'Malta',]
  rownames(df) <- df[,index_var]
  df[,index_var] <- NULL
  
  df['Renewables'] = df['Hydro'] + df['Geothermal'] + 
    df['Wind'] + df['Solar'] + 
    df['Tide..wave..ocean.and.other.non.combustible.sources']
  
  drops <- c('Tide..wave..ocean.and.other.non.combustible.sources',
             'Conventional.Thermal',
             'Renewables')
  
  drops_2 <- c('Tide..wave..ocean.and.other.non.combustible.sources',
               'Coal...combustion',
               'Oil...combustion',
               'Natural.Gas...combustion',
               'Renewables...combustion',
               'Other.Non.Renewable.Fuels...combustion',
               'Conventional NA',
               'Renewables')
  
  
  if (conventional == 'sum'){
    df <- df[ , names(df) %!in% drops_2]
  } else if (conventional == 'ind'){
    df <- df[ , names(df) %!in% drops]
  }
  
  df <- df[ , names(df) %!in% c(var_del)]    #'year'
  
  col_zero <- c()
  for (i in names(df)){
    if(sum(df[i]) == 0){
      col_zero <- c(col_zero, i)
    }   
  }
  df <- df[ , names(df) %!in% col_zero]
  
  df <- rename_vars_my(df)
  return(df)
}













