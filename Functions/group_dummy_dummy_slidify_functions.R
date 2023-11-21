ols_slidify_models_dummy <-
  function(data, window = 250){
    rolling_reg_spec <-
      slidify(
        .f =  ~coeftest(lm(..1 ~ ..2)),
        .period = window,
        .align = "right",
        .unlist = FALSE,
        .partial = FALSE
      )
    
    data %>% 
      group_by(Category) %>% 
      mutate(models = rolling_reg_spec(a2_dummy, party_dummy)) 
  }

unnest_rol_col_dummy <-
  function(data, rol_column) {
    data %>% 
      mutate(tidy = map({{ rol_column }}, broom::tidy)) %>% 
      unnest(cols = tidy) %>% 
      dplyr::select(Date, Category, term:estimate, statistic) %>%
      drop_na() %>% 
      pivot_wider(names_from = term, values_from = c(estimate, statistic)) %>% 
      dplyr::rename("a0" = `estimate_(Intercept)`,
                    "a1" = `estimate_..2`,
                    # "a2" = `estimate_..3`,
                    "t-statistic a0" = `statistic_(Intercept)`,
                    "t-statistic a1" = `statistic_..2`
                    # "t-statistic a2" = `statistic_..3` 
                    ) 
  }

fx_recode_prep_dummy <-
  function(data){
    data %>% 
      pivot_longer(c(-Date, -Category), names_to = "Series", values_to = "Value") %>% 
      mutate(Series = dplyr::recode(
        Series,
        "a0" = "alpha",
        "a1" = "gamma[1]",
        # "a2" = "gamma[2]",
        "t-statistic a0" = "t-statistic:alpha",
        "t-statistic a1" = "t-statistic:gamma[1]"
        # "t-statistic a2" = "t-statistic:gamma[2]"
      ))
  }

fx_recode_prep_standard <-
  function(data){
    data %>% 
      pivot_longer(c(-Date, -Category), names_to = "Series", values_to = "Value") %>% 
      mutate(Series = dplyr::recode(
        Series,
        "a0" = "alpha",
        "a1" = "gamma[1]",
        "a2" = "gamma[2]",
        "t-statistic a0" = "t-statistic:alpha",
        "t-statistic a1" = "t-statistic:gamma[1]",
        "t-statistic a2" = "t-statistic:gamma[2]"
      ))
  }
fx_recode_prep_crisis <-
  function(data){
    data %>% 
      pivot_longer(c(-Date, -Category, -Crisis), names_to = "Series", values_to = "Value") %>% 
      mutate(Series = dplyr::recode(
        Series,
        "a0" = "alpha",
        "a1" = "gamma[1]",
        "a2" = "gamma[2]",
        "a3" = "gamma[3]",
        "a4" = "gamma[4]",
        "t-statistic a0" = "t-statistic:alpha",
        "t-statistic a1" = "t-statistic:gamma[1]",
        "t-statistic a2" = "t-statistic:gamma[2]",
        "t-statistic a3" = "t-statistic:gamma[3]",
        "t-statistic a4" = "t-statistic:gamma[4]"
      ))
  }
fx_recode_plot <-
  function (data, plotname = " ", 
            variables_color = 6, 
            col_pallet = "Shuksan2"
  ) {
    
    ggplot(
      data,
      aes(x = Date, y = Value, color = Category, group = Category)
    ) +
      # geom_rect(
      #   data = crisis_tbl,
      #   inherit.aes = F,
      #   aes(xmin=recession_start, xmax=recession_end, ymin=-Inf, ymax=Inf), 
      #   alpha=0.5, 
      #   fill = "grey70"
      # ) +
      geom_line() +
      facet_wrap(. ~ Series , scales = "free", labeller = label_parsed) +
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
