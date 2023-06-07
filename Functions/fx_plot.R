fx_plot <-
function (data, plotname = " ", variables_color = 12) {
  ggplot(
    pivot_longer(data,-Date, names_to = "Series", values_to = "Value"),
    aes(x = Date, y = Value, col = Series)
  ) +
    geom_line() +
    facet_wrap (. ~ Series, scale = "free") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    theme(
      text = element_text(size = 7),
      strip.background = element_rect(colour = "white", fill = "white"),
      axis.text.x = element_text(angle = 90),
      axis.title = element_text(size = 7),
      plot.tag = element_text(size = 7)
    ) +
    labs(x = "", y = plotname) +
    scale_color_manual(values = pnw_palette("Cascades", variables_color))
}


pivot <- function(data){
  result <- 
  pivot_longer(data,-Date, names_to = "Series", values_to = "Value") 
  result
}

fx_recode_plot <-
  function (data, plotname = " ", variables_color = 12, ncol = NULL, nrow = NULL) {
    ggplot(
      data,
      aes(x = Date, y = Value, color = Category, group = Category)
    ) +
      geom_line() +
      facet_wrap (. ~ Series, scale = "free", labeller = label_parsed, ncol, nrow) +
      theme_bw() +
      theme(
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 8),
        plot.tag = element_text(size = 8),
        legend.position = "bottom"
      ) +
      labs(x = "", y = plotname, color = NULL) +
      scale_color_manual(values = pnw_palette("Cascades", variables_color))
  }

fx_nopivot_plot <-
  function (data, plotname = " ", variables_color = 12, scale = "fixed") {
    ggplot(
      data = data,
      aes(x = Date, y = Value, fill = Series)
    ) +
      geom_area() +
      facet_wrap (. ~ Series, scale = scale) +
      theme_bw() +
      theme(
        legend.position = "none",
        legend.key.height= unit(0.5, 'cm'),
        legend.key.width= unit(0.5, 'cm'),
        legend.text = element_text(size=6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      theme(
        text = element_text(size = 8),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.x = element_text(angle = 90),
        axis.title = element_text(size = 5),
        plot.tag = element_text(size = 8)
      ) +
      labs(x = "", y = plotname) +
      scale_fill_manual(name = "", values = pnw_palette("Winter", variables_color))
  }
