plot_mw <- function(mw, mw_info, input){
  mw |>
    dplyr::mutate(Messposition = paste("Messposition:", Messposition, "cm")) |>
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
    ggplot2::scale_shape("Wiederholung")+
    ggplot2::scale_color_viridis_d("Spot")+
    ggplot2::facet_wrap(~Messposition, ncol = 1)+
    ggplot2::labs(x = "Datum",
                  y = paste(stringr::str_trim(mw_info$Parameter[1]),
                            mw_info$Einheit[1]))+
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
  ggplot2::ggplot(ggplot2::aes(y = as.factor(ID_Wdh),
                               x = as.factor(Messposition),
                               fill = fills,
                               color = colors,
                               label = ifelse(last_signal > 14, "> 14", signif(last_signal, 2))))+
    ggplot2::labs(x = "Messposition [cm]",
                  y = "Wiederholung",
                  title = "Tage seit letztem Signal")+
    ggplot2::scale_fill_identity()+
    ggplot2::scale_color_identity()+
    ggplot2::geom_tile()+
    ggplot2::geom_text()+
    ggplot2::facet_wrap(Spot~Para, scales = "free")+
    ggplot2::theme_classic()+
    ggplot2::theme(text = ggplot2::element_text(size = 14))
}
