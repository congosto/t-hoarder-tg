# Functions shared by tm_viz_channels.Rmd

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# msgs_vs_reach
#
# Chart line de doble escala del total msgs vs alcance de los mismos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
msgs_vs_views <- function(df, periodo,  ini_date, end_date, min_suscribers, max_overlaps) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupar los msgs por hora y calculamos el alcance
  msgs_vs_reach_df <- df %>% 
    filter(relation == "original") %>% 
    group_by(slot_time) %>%
    summarise(
      num_msgs = n(), 
      reach = sum(views,na.rm=T),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Buscamos los canales con más subscriptores de cada una de las horas
  msgs_vs_influencer_df <- df %>% 
    filter(relation == "original") %>% 
    group_by(slot_time) %>%
    summarise(
      reach = sum(views, na.rm = T),
      influencer = ifelse(participants_count >= min_suscribers, channel_name, NA),
      .groups = 'drop'
    ) %>%
    filter (!is.na(influencer)) %>%
    ungroup() %>%
    group_by(slot_time, influencer) %>%
    slice(1) %>%
    ungroup() 
  # Calculamos las dos escalas
  max_msgs <- max(msgs_vs_reach_df$num_msgs,na.rm = TRUE)
  max_reach <- max(msgs_vs_reach_df$reach,na.rm = TRUE)
  ajuste_escala <- max_reach/max_msgs
  limit_y = max_msgs
  if (max_reach  == 0){
    return("There are no views")
  }
  p <- ggplot(data = msgs_vs_reach_df) + 
    # Pintar la evolución de los msgs
    geom_line(
      aes(x = slot_time, y = num_msgs),
      size =0.5,
      color ="steelblue4",
      show.legend = FALSE
    ) +
    geom_area(
      aes( x = slot_time, y= num_msgs),
      fill ="steelblue4",
      alpha = 0.3,
      show.legend = FALSE
    ) +
    # Pintar los la evolución del alcance  
    geom_line(
      aes( x=slot_time,  y= reach/ajuste_escala),
      color="red4",
      alpha = 1,
      size =1,
      show.legend = FALSE
    ) +
    # Pintar los influencers
    geom_text_repel(
      data = msgs_vs_influencer_df,
      aes(
        x = slot_time,
        y = reach/ajuste_escala,
        label = influencer
      ),
      color = "#5a5856",
      ylim = c(0, limit_y* 1.3),
      max.time = 10,
      max.overlaps = max_overlaps,
      size = 3.5,
      vjust = .5
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    #Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. of original msgs per day",slot_time), 
      labels = label_number(scale_cut = scales::cut_si("")),
      limits= c(0,limit_y * 1.2 ),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("Views per", slot_time),
        labels = label_number(scale_cut = scales::cut_si("")) 
      )
    ) +
    # Poner los títulos
    labs(
      title = paste(base_title, ": msgs per",slot_time, "vs. views"),
      subtitle = paste(
        "Tagged profiles with more than",
        format(min_suscribers,
                big.mark=".", scientific=FALSE),
        "subscribers"
      ),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      leyend = "None",
      axis.title.y = element_text(color = "steelblue4", size = 14),
      axis.title.y.right = element_text(color = "red4", size = 14),
      axis.text.y = element_text(color = "steelblue4"),
      axis.text.y.right = element_text(color = "red4")
    )
  
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# msgs_vs_forward_out
#
# Chart line de doble escala del total msgs vs forward realizados
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
msgs_vs_forward_out <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Miramos si hay forwards enviados
  msgs_forward_df <- df %>% 
    filter (relation == "forward")
  if (nrow(msgs_forward_df) == 0){
    return("There are no forwards send") 
  }
  # Agrupamos los msgs por hora y calculamos los forward/hora
  msgs_forward_df <- df %>% 
    filter (relation == "forward") %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = n(),
      .groups = 'drop'
    ) %>% 
    ungroup() 

  # Agrupamos los msgs por hora y calculamos los mensajes originales/hora
  msgs_original_df <- df %>% 
    filter (relation == "original") %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = n(),
      .groups = 'drop'
    ) %>% 
    ungroup()  
  # Calculamos las dos escalas
  max_msgs <- max(msgs_original_df$num_msgs,na.rm = TRUE)
  max_forward_out <- max(msgs_forward_df$num_msgs,na.rm = TRUE)
  ajuste_escala <- max_forward_out/max_msgs
  limit_y = max_msgs
  #definimos la paleta de color
  my_color = c("Num. original msgs"= "steelblue4", "forward" = "red4")
  p <- ggplot() + 
    # Pintamos la evolución de los msgs originales/hora
    geom_line(
      data = msgs_original_df,
      aes(x = slot_time, y = num_msgs, color = "Num. original msgs"),
      size = 0.5
    ) +
    geom_area(
      data = msgs_original_df,
      aes( x=slot_time, y= num_msgs),
      fill = "steelblue4", 
      alpha=0.3
    ) +
    # Pintamos los la evolución de los forward/hora
    geom_line(
      data = msgs_forward_df,
      aes( x = slot_time, y = num_msgs/ajuste_escala, color="forward"),
      size = 0.5)+
    # Anotamos el máximo de msgs originales/hora
    geom_text_repel(
      data = msgs_original_df %>% top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs * 1.1, 
        label = paste0(
          slot_time,
          "\n",
          "Max. original msgs = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Anotamos el máximo de forward/hora
    geom_text_repel(
      data = msgs_forward_df %>%  top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs/ajuste_escala *1.3, 
        label = paste0(
          slot_time,
          "\n",
          "Max.forward = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original msgs per",slot_time), 
      labels = label_number(scale_cut = scales::cut_si("")),
      limits= c(0,limit_y * 1.2),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("forward per",slot_time),
        labels = label_number(scale_cut = scales::cut_si("")) )
    ) +
    # Aplicamos color
    scale_color_manual(values = my_color) +
    # Ponemos los títulos
    labs(
      title =  paste(base_title, ": msgs per",slot_time,"vs. forward send"),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="top",
      axis.title.y = element_text(color = "steelblue4", size = 14),
      axis.title.y.right = element_text(color = "red4", size = 14),
      axis.text.y = element_text(color = "steelblue4"),
      axis.text.y.right = element_text(color = "red4")
    )
   return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# msgs_vs_forward_in
#
# Chart line de doble escala del total msgs vs forward recibidos
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
msgs_vs_forward_in <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los msgs por hora y calculamos los forward/hora
  msgs_forward_df <- df %>% 
    filter (relation == "original") %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = sum(number_forwards),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Agrupamos los msgs por hora y calculamos los mensajes originales/hora
  msgs_original_df <- df %>% 
    filter (relation == "original") %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = n(),
      .groups = 'drop'
    ) %>% 
    ungroup()  
  # Calculamos las dos escalas
  max_msgs <- max(msgs_original_df$num_msgs,na.rm = TRUE)
  max_forward_in <- max(msgs_forward_df$num_msgs,na.rm = TRUE)
  if (max_forward_in  == 0){
    return("There are no forwards received")
  }
  ajuste_escala <- max_forward_in/max_msgs
  limit_y = max_msgs
  #definimos la paleta de color
  my_color = c("Num. original msgs"= "steelblue4", "forward" = "red4")
  p <- ggplot() + 
    # Pintamos la evolución de los msgs originales/hora
    geom_line(
      data = msgs_original_df,
      aes(x = slot_time, y = num_msgs, color = "Num. original msgs"),
      size = 0.5
    ) +
    geom_area(
      data = msgs_original_df,
      aes( x=slot_time, y= num_msgs), fill = "steelblue4", 
      alpha=0.4
    ) +
    # Pintamos los la evolución de los forward/hora
    geom_line(
      data = msgs_forward_df,
      aes( x = slot_time, y = num_msgs/ajuste_escala, color="forward"),
      size = 0.5)+
    # Anotamos el máximo de msgs originales/hora
    geom_text_repel(
      data = msgs_original_df %>% top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs * 1.1, 
        label = paste0(
          slot_time,
          "\n",
          "Max. original msgs = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Anotamos el máximo de forward/hora
    geom_text_repel(
      data = msgs_forward_df %>%  top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs/ajuste_escala *1.3, 
        label = paste0(
          slot_time,
          "\n",
          "Max.forward = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original msgs per",slot_time), 
      labels = label_number(scale_cut = scales::cut_si("")),
      #limits= c(0,limit_y*1.4),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("forward per",slot_time),
        labels = label_number(scale_cut = scales::cut_si("")) )
    ) +
    # Aplicamos color
    scale_color_manual(values = my_color) +
    # Ponemos los títulos
    labs(
      title =  paste(base_title, ": msgs per",slot_time,"vs. forward received"),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="top",
      axis.title.y = element_text(color = "steelblue4", size = 14),
      axis.title.y.right = element_text(color = "red4", size = 14),
      axis.text.y = element_text(color = "steelblue4"),
      axis.text.y.right = element_text(color = "red4")
    )
  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# msgs_vs_replies
#
# Chart line de doble escala del total msgs vs replies
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
msgs_vs_replies <- function(df, periodo,  ini_date, end_date) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Agrupamos los msgs por hora y calculamos los forward/hora
  msgs_replies_df <- df %>% 
    filter (relation == "original") %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = sum(number_replies),
      .groups = 'drop'
    ) %>% 
    ungroup() 
  # Agrupamos los msgs por hora y calculamos los mensajes originales/hora
  msgs_original_df <- df %>% 
    filter (relation == "original") %>%
    filter (is_reply == 0) %>%
    group_by(slot_time) %>%
    summarise(
      num_msgs = n(),
      .groups = 'drop'
    ) %>% 
    ungroup()  
  # Calculamos las dos escalas
  max_msgs <- max(msgs_original_df$num_msgs,na.rm = TRUE)
  max_replies <- max(msgs_replies_df$num_msgs,na.rm = TRUE)
  ajuste_escala <- max_replies/max_msgs
  limit_y = max_msgs
  #definimos la paleta de color
  my_color = c("Num. original msgs"= "steelblue4", "replies" = "red4")
  p <- ggplot() + 
    # Pintamos la evolución de los msgs originales/hora
    geom_line(
      data = msgs_original_df,
      aes(x = slot_time, y = num_msgs, color = "Num. original msgs"),
      size = 0.5
    ) +
    geom_area(
      data = msgs_original_df,
      aes( x=slot_time, y= num_msgs), fill = "steelblue4", 
      alpha=0.4
    ) +
    # Pintamos los la evolución de los replies/hora
    geom_line(
      data = msgs_replies_df,
      aes( x = slot_time, y = num_msgs/ajuste_escala, color="replies"),
      size = 0.5)+
    # Anotamos el máximo de msgs originales/hora
    geom_text_repel(
      data = msgs_original_df %>% top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs * 1.1, 
        label = paste0(
          slot_time,
          "\n",
          "Max. original msgs = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Anotamos el máximo de replies/hora
    geom_text_repel(
      data = msgs_replies_df %>%  top_n(1, num_msgs),
      aes(
        x = slot_time, y = num_msgs/ajuste_escala *1.3, 
        label = paste0(
          slot_time,
          "\n",
          "Max.replies = ",scales::comma(num_msgs)
        )
      ),
      color = "grey50",
      size = 3.5,
      vjust = .5,
      show.legend = FALSE
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos la doble escala
    scale_y_continuous(
      name = paste("Num. Original msgs per",slot_time), 
      labels = label_number(scale_cut = scales::cut_si("")),
      #limits= c(0,limit_y*1.4),
      expand= c(0,0),
      sec.axis = sec_axis(
        trans=(~ . * ajuste_escala), 
        name = paste("replies per",slot_time),
        labels = label_number(scale_cut = scales::cut_si("")) )
    ) +
    # Aplicamos color
    scale_color_manual(values = my_color) +
    # Ponemos los títulos
    labs(
      title =  paste(base_title, ": msgs per",slot_time,"vs. replies"),
      x = "", 
      color=""
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="top",
      axis.title.y = element_text(color = "steelblue4", size = 14),
      axis.title.y.right = element_text(color = "red4", size = 14),
      axis.text.y = element_text(color = "steelblue4"),
      axis.text.y.right = element_text(color = "red4")
    )
  return(p)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# msgs_by_community 
#
# Bar line desglosado por grupos o comunidades
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
msgs_by_community <-  function(df, periodo,  ini_date, end_date, communities) {
  # Si es zoom, acotamos el tiempo
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  top_community <- communities$community
  top_community_names <- communities$name_community
  top_community_color <- communities$color
  # Seleccionamos las comunidades del top
  msgs_communities_df <- df %>% 
    filter(community %in%  top_community) %>%
    #agrupamos por comunidad y contamos cuantos msgs hay en cada una de ellas en cada hora
    group_by(slot_time,community) %>%
    summarise(
      num_msgs = n(),
      .groups ="drop" )%>%
    ungroup()
  # Ordenamos las comunidades como aparecen en el fichero
  msgs_communities_df$community <- factor(msgs_communities_df$community, levels = top_community)
  # Generamos la gráfica por comunidades
  p <- ggplot(msgs_communities_df) + 
    # Pintamos la evolución por comunidades
    geom_col(
      aes(x = slot_time, y = num_msgs, fill = community),
      alpha = 0.7
    )+
    # Ponemos los títulos
    labs(
      title =paste(base_title, ": msgs by group"),
      x = "",
      y = paste("Num msgs per",slot_time),
      fill = "Groups"
    ) +
    # Ajustamos la escala de tiempo
    scale_x_datetime(
      date_labels = format_time(ini_date, end_date),
      date_breaks = time_scale(ini_date, end_date)
    ) +
    # Ajustamos el eje y
    scale_y_continuous(
      limits= c(0,msg_peak*1.1),
      expand= c(0,0),
      labels = label_number(scale_cut = scales::cut_si(""))
    ) +
    #coloreamos según el fichero
    scale_fill_manual(
      values = top_community_color,
      labels = top_community_names
    ) +
    # Aplicamos template
    my_theme() +
    theme(
      legend.position="right",
      legend.key.size = unit(0.4, 'cm'),
      axis.title.y = element_text(vjust = +4),
      panel.grid.major.x = element_blank(),
      axis.ticks.x = element_line(),
      legend.text = element_text(size=11)
    ) 
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# accumulated_sites
#
# Char line acumulado de los sitios referenciados en los msgs
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
accumulated_sites <-  function(df, periodo, ini_date, end_date) {
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  df <- df %>%
    mutate(enlace = str_extract(message, "(?i)\\bhttps?://\\S+")) %>% # Extraer enlaces
    mutate(dominio = str_extract(enlace, "(?<=://)[^/]+")) %>% # Extraer dominios
    filter(
      dominio != "https://t.me"
      ) %>% 
    group_by(dominio,slot_time) %>%
    summarise(
     msgs_count = n()
    ) %>%
    mutate(cumulative_sum = cumsum(msgs_count)) 
  top_dominios <- df %>%
    group_by(dominio) %>%
    summarise(
      total = sum(msgs_count),
      .groups = "drop"
    ) %>%
    arrange (desc(total)) %>%
    head (15)
  visible_dates <- as.POSIXct(seq(min(msgs_df$slot_time),max(msgs_df$slot_time), by = time_scale(ini_date, end_date)) )
  limit_x = as.POSIXct(c(min(df$slot_time), max(df$slot_time)+( expand_time(ini_date, end_date, 50))))
  limit_y =  max(df$cumulative_sum) 
  offset <- 2
  max_date <- max(df$slot_time)
  p <- ggplot() + 
    geom_path(
      data = df %>% filter (dominio %in% top_dominios$dominio),  
      aes(x = slot_time, y = cumulative_sum, color = dominio),
      size =1.3,
      show.legend =FALSE
    ) +
    geom_text_repel(
      data = df  %>%
        top_n(1, cumulative_sum) %>%
        filter (dominio %in% top_dominios$dominio),
      aes(
        x = slot_time, y = cumulative_sum, color = dominio,
        label = paste0(
          dominio,
          "(",
          format(cumulative_sum, big.mark=".",decimal.mark=","),
          " ref.)")
      ), 
      vjust = 1,
      hjust = 0,
      size = 4,
      nudge_x =  expand_time(min_date,max_date, 2), # Ajuste eje x
      nudge_y = 0.005,  # Ajuste eje y
      direction="y",
      max.overlaps=30,
      segment.size = 0.5,
      segment.hjust = 0,
      segment.linetype = 2,
      show.legend = FALSE
    )+
    scale_x_datetime(
      limits=limit_x,
      breaks = visible_dates,
      date_labels = format_time(ini_date, end_date)
    ) +
    scale_y_continuous(
     labels = label_number(),
    limits= c(0,limit_y*1.3 ),
    expand= c(0,0)
    ) +
    labs(
      title = paste0(base_title, ": cumulative domain mentions per ",slot_time),
      x = "", 
      y = "cumulative referrals",
      color=""
    ) +
    guides(color=guide_legend(ncol=2)) +
    my_theme() +
    theme( legend.position="top")
  return(p)
}
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# words_frequency
#
# Word cloud de las palabras más frecuentes
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
words_frequency <- function(df, periodo, ini_date, end_date ) {
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  #Obtenemos las stop words en Inglés, español y catalán
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom"),
    data_frame(word = tm::stopwords("Russian"),lexicon = "custom")
  )
  # Extraemos el corpus de texto de la columna "message" para generar el wordcloud 
  corpus_text <- msgs_df %>%
    filter(relation == "original")  %>%
    # Extraemos las URL y los handles  de perfiles del texto del tweet
    mutate(text_plain = gsub('http\\S+\\s*',"",message)) %>% # Quitamos las URLs
    select(text_plain) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    group_by(word) %>%  # Agrupamos por palabras   
    summarise(
      freq = n(), # Calculamos la frecuencia de cada palabra
      .group = "drop"
    ) %>%  
    ungroup() %>%
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    arrange(desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    top_n(n = 50, freq)  # Obtenemos las 50 palabras más frecuentes de cada comunidad
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_text,
      aes(label = word, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(base_title,": Most frequent words"),
      x = "", y = "",
      color=""
    ) +
    # Aplicamos un template minimalista
    my_theme() 
  return(p)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# words_frequency_by_community
#
# Word cloud de las palabras más frecuentes por comunidad
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
words_frequency_by_community <- function(df, periodo, ini_date, end_date, communities ) {
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
  # Ordenamos por comunidad de más a menos miembros
  top_community <- communities$community
  top_community_color <- communities$color
  communities$name_community <- factor(communities$name_community,levels = communities$name_community )
  #Obtenemos las stop words en Inglés, español y catalán
  custom_stop_words <- bind_rows(
    stop_words,
    data_frame(word = tm::stopwords("spanish"),lexicon = "custom"),
    data_frame(word = tm::stopwords("catalan"),lexicon = "custom"),
    data_frame(word = tm::stopwords("Russian"),lexicon = "custom")
  )
  # Extraemos el corpus de texto de la columna "text" para generar el wordcloud 
  corpus_text <- msgs_df %>%
    filter(relation == "original")  %>%
    # Le añadimos el nombre de la comunidad
    left_join(communities, by  = "community") %>%
    # Dejamos solo las comunidades con más miembros
    filter(community %in%  top_community) %>%
    # Extraemos las URL y los handles  de perfiles del texto del tweet
    mutate(text_plain = gsub('http\\S+\\s*',"",message)) %>% # Quitamos las URLs
    select(text_plain,name_community) %>%
    unnest_tokens(word, text_plain) %>% # Convertimos las frases en un conjunto de palabras
    group_by(word,name_community) %>%  # Agrupamos por palabras   
    summarise(
      freq = n(), # Calculamos la frecuencia de cada palabra
     .group = "drop"
    ) %>%  
    ungroup() %>%
    anti_join(custom_stop_words) %>% # Eliminamos las stop words
    arrange(name_community,desc(freq)) %>% # Ordenamos de mayor a menor frecuencia de aparición
    group_by(name_community) %>% # Agrupamos por name_community
    top_n(n = 20, freq)  # Obtenemos las 20 palabras más frecuentes de cada comunidad
  # Generamos los colores para para que las facetas tengan el color de la comunidad
  strip <- strip_themed(
    background_x = elem_list_rect(
      fill = top_community_color,
      color  = top_community_color
    )
  )
  # Generamos los colores para las palabras según frecuencia
  paleta <- brewer.pal(8, "Dark2")
  # Pintamos la nube de palabras
  p <- ggplot() +
    # Dibujamos la nube de palabras
    geom_text_wordcloud_area(
      data = corpus_text,
      aes(label = word, group = name_community, size = freq, color = freq),
      angle = 0.35
    ) +
    # Definimos la proporción del tamaño de las letras según frecuencia
    geom_text_wordcloud_area(rm_outside = TRUE) +
    geom_text_wordcloud_area(area_corr_power = 1) +
    scale_radius(range = c(4, 20), limits = c(0, NA)) +
    # Aplicamos una paleta de color
    scale_color_gradientn(colors = paleta) +
    # Definimos el título principal y el de los ejes
    labs(
      title = paste(base_title,": Most frequent words by group"),
      x = "", y = "",
      color=""
    ) +
    # Desdoblamos la gráfica según el periodo previo o posterior de la compra
    facet_wrap2(~name_community, ncol = 2, strip = strip ) +
    # Aplicamos un template minimalista
    my_theme() +
    theme(strip.text = element_text(color  = "white"))
  return(p)
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# spread_topics_msgs
#
# line chart acumulativo
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

spread_topics_msgs <- function(df, periodo, ini_date, end_date, topics){
  if (periodo == "zoom"){
    df <- df %>% 
      filter(date >= ini_date & date <= end_date)
  }
   # extraemos los  mensajes con topics
  topics <- tolower(topics)
  list_topic <- as.list(topics)
  regex_topic <- paste(list_topic, collapse = "|") 
  df <- df %>%
    mutate(msg_clean = tolower(str_extract_all(message, "[:alnum:]+"))) %>%
    filter(str_detect ( msg_clean, regex_topic)) 
  first_time <- TRUE
  # extraemos cada topic por separado
  for (topic in topics) {
    aux_df <- df %>% 
    filter(str_detect (msg_clean, topic)) %>%
    mutate (topic = topic)
    if (first_time == TRUE){
      topics_df <- aux_df
      first_time <- FALSE
    }else{
      topics_df <- rbind (topics_df,aux_df)
    }
  } 
  topics_df <- topics_df %>%
    group_by(topic, slot_time) %>%
    summarise(nun_topics = n()) %>%
    mutate(cumulative_sum = cumsum(nun_topics))
  visible_dates <- as.POSIXct(seq(min(msgs_df$slot_time),max(msgs_df$slot_time), by = time_scale(ini_date, end_date)) )
  limit_x = as.POSIXct(c(min(df$slot_time), max(df$slot_time)+( expand_time(ini_date, end_date, 50))))
  limit_y =  max(topics_df$cumulative_sum) 
  p <- ggplot() + 
    geom_line(
      data = topics_df, 
      aes(
        x = slot_time,
        y = cumulative_sum,
        color = topic
      ),
      show.legend =FALSE,
      size =1, alpha  = 0.7)+
    geom_text_repel(
      data = topics_df %>% 
        top_n(1, cumulative_sum),
      aes(
        x = slot_time, y = cumulative_sum, color = topic,
        label = paste0(
          topic,
          "(",
          format(cumulative_sum, big.mark=".",decimal.mark=","),
          " ref.)")
      ), 
      vjust = 1,
      hjust = 0,
      size = 4,
      nudge_x =  expand_time(min_date,max_date, 2), # Ajuste eje x
      nudge_y = 0.005,  # Ajuste eje y
      direction="y",
      max.overlaps=30,
      segment.size = 0.5,
      segment.hjust = 0,
      segment.linetype = 2,
      show.legend = FALSE
    )+
    scale_x_datetime(
      limits=limit_x,
      breaks = visible_dates,
      date_labels = format_time(ini_date, end_date)
    ) +
    scale_y_continuous(
      labels = label_number(scale_cut = scales::cut_si("")),
      limits= c(0,limit_y*1.3 ),
      expand= c(0,0)
    ) +
    labs(
      title = paste0(base_title,": Topics"),
      x = "",
      y = "Num. accumulated topics per day",
      color = ""
    ) +
    my_theme() +
    theme(
      legend.position="top"
    )
  return (p)
}

