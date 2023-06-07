ols_slidify_models <-
function(data, window = 250){
  rolling_reg_spec <-
    slidify(
      .f =  ~coeftest(lm(..1 ~ ..2 + ..3)),
      .period = window,
      .align = "right",
      .unlist = FALSE,
      .partial = FALSE
    )
  
  data %>% 
    group_by(Category) %>% 
    mutate(models = rolling_reg_spec(CSAD, abs(`Market Return`), I(`Market Return` ^ 2))) %>% 
    unnest_rol_col(rol_column = models)
}
unnest_rol_col <-
function(data, rol_column) {
  data %>% 
    mutate(tidy = map({{ rol_column }}, broom::tidy)) %>% 
    unnest(tidy) %>% 
    dplyr::select(Date, term:estimate, statistic) %>% 
    drop_na() %>% 
    pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
    dplyr::rename("a0" = `estimate_(Intercept)`,
                  "a1" = `estimate_..2`,
                  "a2" = `estimate_..3`,
                  "t-statistic a0" = `statistic_(Intercept)`,
                  "t-statistic a1" = `statistic_..2`,
                  "t-statistic a2" = `statistic_..3` ) # may delete depending on number of variables or focus
}
fx_recode_prep <-
function(data){
  data %>% 
    pivot_longer(c(-Date, -Category), names_to = "Series", values_to = "Value") %>% 
    mutate(Series = dplyr::recode(
      Series,
      "a0" = "gamma[0]",
      "a1" = "gamma[1]",
      "a2" = "gamma[2]",
      "t-statistic a0" = "t-statistic:gamma[0]",
      "t-statistic a1" = "t-statistic:gamma[1]",
      "t-statistic a2" = "t-statistic:gamma[2]"
    ))
}
fx_recode_plot <-
function (data, plotname = " ", variables_color = 12, col_pallet = "Cascades",  ncol = NULL, nrow = NULL) {
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
      scale_color_manual(values = pnw_palette(col_pallet, variables_color))
  }
slidyfy_gg_workflow <-
function(data_model_rol){
  data_model_rol %>% 
    fx_recode_prep() %>% 
    fx_recode_plot(variables_color = 6)
}
