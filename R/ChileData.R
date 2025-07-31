


##### Funciones ####

numero_a_mes <- function(numero, largo=TRUE) {
  if(largo){
    meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  } else{
    meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  }
  return(meses[numero])
}



NasaPower <- function(Inicio = '2010-01-01', Termino = '2025-01-31',
                      LonLat='',Temporal = 'daily', Comunidad='ag'){
  require(nasapower)
  if(Temporal == 'monthly'){
    pars = c("RH2M", "T2M", "PRECTOTCORR","PRECTOTCORR_SUM","WD2M","WS2M","ALLSKY_SFC_PAR_TOT",
             "ALLSKY_SFC_PAR_TOT_MAX","CLRSKY_SFC_PAR_TOT","CLRSKY_SFC_PAR_TOT_MAX")
  }

  if(Temporal == 'daily'){
    pars = c("RH2M", "T2M", "PRECTOTCORR","WD2M","WS2M","ALLSKY_SFC_PAR_TOT",
             "CLRSKY_SFC_PAR_TOT")
  }

  NASAPOWER <- get_power(
    community = Comunidad,
    lonlat = LonLat,
    pars = pars,
    dates = c(Inicio, Termino),
    temporal_api = Temporal
  )

  return(NASAPOWER)
}


get_ERDDAP <- function(Var='chlor_a', Dia='01', Mes='08', Año='2019',
                       area=NULL, imagen=TRUE, rango=TRUE, archivo='png'){
  if(is.null(area)){
    area <- data.frame(
      area = 'Chiloe',
      lat_min = -40.44792,
      lat_max = -44.69792,
      lon_min = -75.19791,
      lon_max = -70.94791,
      PSMB = NA
    )
  }
  require(dplyr)
  require(lubridate)

  if(rango == TRUE){
    fecha_inicio <- as.Date(paste0(Año, "-", Mes, "-", Dia))
    fecha_actual <- Sys.Date()
    fechas <- seq.Date(fecha_inicio, fecha_actual, by="day")
    df_acumulado <- data.frame()

    for(fecha in fechas){
      fecha <- as.Date(fecha, origin="1970-01-01")
      fecha_str <- format(fecha, "%Y-%m-%dT12:00:00Z")

      if(Var == 'chlor_a'){
        link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3ASCIDINEOF2kmDaily.csv?',
                       Var, '%5B(', fecha_str, ')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                       '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                       Var, '&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')
      }

      if(Var == 'kd_490'){
        link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3AkdSCIDINEOF2kmDaily.csv?',
                       Var,'%5B(',fecha_str,')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                       '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                       Var,'&.colorBar=%7C%7CLog%7C0.001%7C100%7C&.bgColor=0xffccccff')
      }

      if(Var == 'spm'){
        link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3AspmSCIDINEOF2kmDaily.csv?',
                       Var,'%5B(',fecha_str,')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                       '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                       Var,'&.colorBar=%7C%7CLog%7C0.01%7C100%7C&.bgColor=0xffccccff')
      }

      if(Var == 'sst'){
        link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwBLENDEDsstDNDaily.csv?analysed_sst%5B(',fecha_str,')%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7Canalysed_sst&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')
      }

      datos <- tryCatch({
        print(fecha)
        print(area$area)
        read.csv(url(link))
      }, error = function(e) NULL)

      if(!is.null(datos) && nrow(datos) > 0 && any(!is.na(datos))){
        datos <- datos %>% filter(!time == 'UTC') %>% mutate(area = area$area)
        df_acumulado <- rbind(df_acumulado, datos)
        write.csv(df_acumulado, paste0(area$area,'_',Var, '.csv'))
        print('Dato ingresado')

        if(imagen == TRUE){
          link_imagen <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3ASCIDINEOF2kmDaily.',archivo,'?',
                                Var, '%5B(', fecha_str, ')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                                '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                                Var, '&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')

          download.file(link_imagen, destfile = paste0(area$area, '_', Var,'_' ,fecha_str,'.',archivo))
        }
      }
    }
    datos_out <- df_acumulado
  } else {
    fecha <- as.Date(paste0(Año, "-", Mes, "-", Dia))
    fecha_str <- format(fecha, "%Y-%m-%dT12:00:00Z")

    if(Var == 'chlor_a'){
      link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3ASCIDINEOF2kmDaily.csv?',
                     Var, '%5B(', fecha_str, ')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                     '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                     Var, '&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')
    }

    if(Var == 'kd_490'){
      link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3AkdSCIDINEOF2kmDaily.csv?',
                     Var,'%5B(',fecha_str,')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                     '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                     Var,'&.colorBar=%7C%7CLog%7C0.001%7C100%7C&.bgColor=0xffccccff')
    }

    if(Var == 'spm'){
      link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3AspmSCIDINEOF2kmDaily.csv?',
                     Var,'%5B(',fecha_str,')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                     '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                     Var,'&.colorBar=%7C%7CLog%7C0.01%7C100%7C&.bgColor=0xffccccff')
    }

    if(Var == 'sst'){
      link <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwBLENDEDsstDNDaily.csv?analysed_sst%5B(',fecha_str,')%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7Canalysed_sst&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')
    }

    datos <- tryCatch({
      read.csv(url(link))
    }, error = function(e) NULL)

    if(!is.null(datos) && nrow(datos) > 0 && any(!is.na(datos))){
      datos_out <- datos %>% filter(!time == 'UTC') %>% mutate(area = area$area)

      if(imagen == TRUE){
        link_imagen <- paste0('https://coastwatch.noaa.gov/erddap/griddap/noaacwNPPN20S3ASCIDINEOF2kmDaily.',archivo,'?',
                              Var, '%5B(', fecha_str, ')%5D%5B(0.0)%5D%5B(',area$lat_min,'):(',area$lat_max,')%5D',
                              '%5B(',area$lon_max,'):(',area$lon_min,')%5D&.draw=surface&.vars=longitude%7Clatitude%7C',
                              Var, '&.colorBar=%7C%7C%7C%7C%7C&.bgColor=0xffccccff')

        download.file(link_imagen, destfile = paste0(area$area, '_', Var,'_' ,fecha_str,'.',archivo))
      }
    } else {
      datos_out <- data.frame()
    }
  }

  return(datos_out)
}



# ChileSat <- function(Inicio = '2010-01-01', Termino = '2025-01-31',
#                      Lat, Lon){
#   require(dplyr)
#   require(lubridate)
#
#   #Nasa
#
#   #ERDDAP
#
#   #Procesado
#
# }


iloc_slm <- function(operator=59, Time=0.5){
  require(dplyr)
  require(rvest)
  require(lubridate)

  url <- paste0('https://www.ioc-sealevelmonitoring.org/list.php?order=delay&dir=asc&showall=all&operator=', operator)


  estaciones <-read_html(url)
  estaciones_df <- estaciones %>%
  html_nodes("table")%>%
    html_table() %>% .[[7]]%>%
    .[-(1:2), ] %>% select(1:6|9:10)


  colnames(estaciones_df) <-as.character(as.vector(estaciones_df[1,]))
  estaciones_df = estaciones_df[-1,]
  estaciones_df <- estaciones_df %>%
    mutate( url = paste0('https://www.ioc-sealevelmonitoring.org/bgraph.php?code=',Code,'&output=tab&period=', Time))



  return(estaciones_df)

}


SeaLevel <-  function(Operator=59, Time=0.5, ID='',plot=FALSE){
  require(dplyr)
  require(rvest)
  require(lubridate)
  require(stringr)
  app_data <- list()

  url <- paste0('https://www.ioc-sealevelmonitoring.org/list.php?order=delay&dir=asc&showall=all&operator=', Operator)


  urls <-read_html(url)
  urls_df <- urls %>%
    html_nodes("table")%>%
    html_table() %>% .[[7]]%>%
    .[-(1:2), ] %>% select(1:6|9:10)


  colnames(urls_df) <-as.character(as.vector(urls_df[1,]))
  urls_df = urls_df[-1,]
  urls_df <- urls_df %>%
    mutate( url = paste0('https://www.ioc-sealevelmonitoring.org/bgraph.php?code=',Code,'&output=tab&period=', Time)) %>%
    filter(!grepl('d', Delay)) %>% filter(!Delay == "")

  if(!ID ==''){
    urls_df <- urls_df %>% filter(Code == ID)

  }



  app_data$df <- tibble()

  urls<- urls_df$url

  for (url in urls){

    estacion <-read_html(url)
    text_content <- estacion %>%
      html_node("th") %>%
      html_text()
    estacion_texto <- sub("Tide gauge at ", "", text_content)
    app_data$df_ <- estacion %>%
      html_nodes("table") %>%
      html_table() %>% .[[1]] %>%
      janitor::row_to_names(row_number = 1) %>%
      janitor::clean_names() %>%
      mutate(Estacion = estacion_texto,
             Codigo = code_value <- str_extract(url, "(?<=code=)[^&]+"))    %>%
      mutate(time_utc = ymd_hms(time_utc, tz = "UTC"),
             `Time (Chile)` = with_tz(time_utc, tzone = "America/Santiago"))

    if(ncol(app_data$df_) ==6){

      if(grepl('prs', colnames(app_data$df_[2]))){
        app_data$df <- rbind(app_data$df, app_data$df_)
      }

    }

  }
  if(!ID ==''){
    if(plot){
      SLM_plot<- app_data$df %>%
        dplyr::rename(Fecha = 6) %>%
        tidyr::gather(key=var, value=value, -time_utc, -Estacion, -Codigo, -Fecha)

      SLM_plot$var <- factor(SLM_plot$var, levels = c("rad_m", "prs_m"))


      # min_y <- min(SLM_plot$value)
      # max_y <- max(SLM_plot$value)
      # interval_size <- (max_y - min_y) / 5 # Example: 5 intervals


      app_data$plot<- ggplot(SLM_plot, aes(x=Fecha, y=value, color=var, shape=var)) +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values=c('#CC0000','#66FF00'))

    }

  }

  return(app_data)

}






