plot_mw <- function(mw, mw_info, input, agg_name){

  mw |>
    dplyr::arrange(ID_Messposition) |>
    dplyr::mutate(Messposition = factor(ID_Messposition,
                                        labels = paste("Messposition:",
                                                       unique(Messposition),
                                                       "cm")
                                        )
                  ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = Datum,
        y = Messwert,
        group = as.factor(paste0(ID_Spot, "_", ID_Wdh)),
        color = as.factor(paste0(ID_Spot)),
        shape = as.factor(ID_Wdh)
      )
    )+
    ggplot2::geom_point()+
    ggplot2::geom_line()+
    ggplot2::theme_bw()+
    ggplot2::theme(text = ggplot2::element_text(size = 16))+
    #ggplot2::scale_shape("Wiederholung")+
    ggplot2::scale_shape_manual("Wiederholung", values = 1:12)+
    #ggplot2::scale_shape_identity()+
    ggplot2::scale_color_viridis_d("Spot")+
    ggplot2::facet_wrap(~Messposition, ncol = 1)+
    ggplot2::labs(x = "Datum",
                  y = paste(stringr::str_trim(mw_info$Parameter[1]),
                            stringr::str_trim(mw_info$Einheit[1]),
                            agg_name))+
    ggplot2::guides(
      color = ifelse(length(input$Spot) == 1, "none", "legend"),
      shape = ifelse(length(unique(mw_info$ID_Wdh)) == 1, "none", "legend")
    )+
    ggplot2::scale_x_datetime(limits = as.POSIXct(input$Datum))
}


plot_health <- function(last_signal, stbls){

  color.df <- data.frame(
    breaks = c(0,1,2,4,7,14),
    last_signal_fct = c("0-1", "1-2", "2-4", "4-7", "7-14", ">14"),
    colors = c("black", "black", "black", "white", "white", "white"),
    fills = rev(viridis::viridis(6))
  )

  last_signal |>
    dplyr::mutate(last_signal = as.numeric(difftime(as.POSIXct(Sys.time(), tz = "Etc/GMT-1"),
                                                    lubridate::force_tz(Datum, tzone = "Etc/GMT-1"),
                                                    units = "days"))) |>
    dplyr::mutate(last_signal_fct = cut(last_signal,
                                    breaks = c(color.df$breaks, max(last_signal)+ 1),
                                    labels = color.df$last_signal_fct)) |>
    dplyr::left_join(color.df) |>
    dplyr::left_join(stbls$Tab_Para) |>
    dplyr::left_join(stbls$Tab_Messposition) |>
    dplyr::mutate(Spot = paste0("Spot: ", ID_Spot)) |>
    dplyr::mutate(Para = paste(stringr::str_trim(Parameter), stringr::str_trim(Einheit))) |>
  ggplot2::ggplot(ggplot2::aes(x = as.factor(ID_Wdh),
                               y = as.factor(Messposition),
                               fill = fills,
                               color = colors,
                               label = ifelse(last_signal > 14, "> 14", signif(last_signal, 2))))+
    ggplot2::labs(y = "Messposition [cm]",
                  x = "Wiederholung",
                  title = "Tage seit letztem Signal")+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::geom_tile()+
    ggplot2::geom_text()+
    ggplot2::facet_wrap(Spot~Para, scales = "free")+
    ggplot2::theme_classic()+
    ggplot2::theme(text = ggplot2::element_text(size = 14))
}

#' plot value of the last measurement for all active devices
#'
#' @param last_signal data.frame, download result by download_last_signal
#' @param stbls list, download result by download_static_tables
#'
#' @returns ggiraph object
#' @export
plot_last_value <- function(last_signal, stbls){
  (last_signal |>
     dplyr::left_join(stbls$Tab_Para) |>
     dplyr::filter(!is.na(Messwert)) |>
     dplyr::group_by(Parameter, ID_Spot) |>
     dplyr::mutate(text_size = 100 / (length(unique(last_signal$ID_Spot)) * length(unique(ID_Wdh)))) |>
     dplyr::ungroup() |>
     dplyr::left_join(stbls$Tab_Messposition) |>
     dplyr::group_by(Parameter) |>
     dplyr::mutate(Messposition = ifelse(is.na(Messposition), 0, Messposition)) |>
     dplyr::mutate(fill_value = rank(Messwert)/dplyr::n()) |>
     dplyr::mutate(Spot = paste0("Spot: ", ID_Spot)) |>
     dplyr::mutate(Para = paste(stringr::str_trim(Parameter), stringr::str_trim(Einheit))) |>
     ggplot2::ggplot(ggplot2::aes(x = as.factor(ID_Wdh),
                                  y = as.factor(Messposition),
                                  fill = fill_value,
                                  label = signif(Messwert,3)))+
     ggplot2::labs(y = "Messposition [cm]",
                   x = "Wiederholung",
                   title = "Aktuelle Messwerte",
                   caption = "Aktuelle Messwerte der ausgewählten Station nach Spot, Wiederholung sowie Parameter gruppiert.\n Weitere mögliche Gruppierungen wie Einzelbäume + Orientierung werden aktuell nicht einzeln dargestellt.")+
     ggplot2::scale_fill_viridis_c(
       "Relative Farbskala",
       breaks = c(.9, 0.5, 0.1),
       labels = c("hoch", "mittel", "niedrig"),
       option = "E",
       end = .9)+
     ggiraph::geom_tile_interactive(color = "gray99",
                                    ggplot2::aes(
                                      tooltip = paste(stringr::str_trim(Parameter), "\n",
                                                      signif(Messwert,3), stringr::str_trim(Einheit), "\n",
                                                      Datum)))+
     ggplot2::geom_text(color = "white", ggplot2::aes(size = text_size))+
     ggh4x::facet_grid2(Para~Spot, scales = "free", independent = "all")+
     ggplot2::scale_size_identity()+
     ggplot2::theme_minimal()+
     ggplot2::theme(text = ggplot2::element_text(size = 24),
                    legend.position = "top",
                    strip.text.y = ggplot2::element_text(angle = 0))) |>
    ggiraph::girafe(code = NULL, width_svg = 24, height_svg = 18, pointsize = 20)

}
