descr_stats = function(df, features) {
  if(sum(unique(features) %in% colnames(df)) <= 1) {
    stop("Error! Must supply at least two variables in data. Otherwise, use summary() function.")
  }
  df = suppressWarnings(
    df %>%
      dplyr::summarise_at(
        unique(features),
        list(
          sep_N = function(x, na.rm) { sum(!is.na(x)) },
          sep_mean = mean,
          sep_sd = sd,
          sep_p10 = function(x, na.rm) { quantile(x, 0.10, na.rm = na.rm) },
          sep_p25 = function(x, na.rm) { quantile(x, 0.25, na.rm = na.rm) },
          sep_p50 = function(x, na.rm) { quantile(x, 0.50, na.rm = na.rm) },
          sep_p75 = function(x, na.rm) { quantile(x, 0.75, na.rm = na.rm) },
          sep_p90 = function(x, na.rm) { quantile(x, 0.90, na.rm = na.rm) },
          sep_min = min,
          sep_max = max
        ),
        na.rm = TRUE
      ) %>%
      tidyr::gather(
        key = "temp",
        value = "value"
      ) %>%
      tidyr::separate(
        temp,
        sep = "_sep_",
        into = c("variable", "statistic")
      ) %>%
      tidyr::spread(
        statistic,
        value
      ) %>%
      dplyr::arrange(
        match(
          variable,
          unique(features)
        )
      ) %>%
      dplyr::select(
        variable,
        N,
        mean,
        sd,
        p10,
        p25,
        p50,
        p75,
        p90,
        min,
        max
      )
  )
  df
}

descr_stats_tc = function(df, features, cluster) {
  df = df %>% data.frame()
  if(sum(colnames(df) == "treated") != 1) {
    stop("Error! Must have column 'treated'!")
  }
  if(sum(sort(unique(df$treated)) == c(0, 1)) != 2) {
    stop("Error! 'treated' column must only have values 0 and 1!")
  }
  stat_df = dplyr::bind_cols(
    descr_stats(
      df = df,
      features = features
    ),
    descr_stats(
      df = df %>%
        dplyr::filter(
          treated == 1
        ),
      features = features
    ) %>%
      dplyr::rename(
        treated_mean = mean
      ) %>%
      dplyr::select(
        treated_mean
      ),
    descr_stats(
      df = df %>%
        dplyr::filter(
          treated == 0
        ),
      features = features
    ) %>%
      dplyr::rename(
        control_mean = mean
      ) %>%
      dplyr::select(
        control_mean
      )
  ) %>%
    dplyr::mutate(
      treated_minus_control = treated_mean - control_mean
    )
  t_stat_store = NULL
  for(x in features) {
    if(missing(cluster)) {
      form = paste(x, "~ treated")
    } else {
      form = paste(x, "~ treated | 0 | 0 |", cluster)
    }
    est = lfe::felm(
      formula = as.formula(form),
      data = df
    )
    t_stat_store = c(
      t_stat_store,
      coef(est)["treated"] / sqrt(diag(vcov(est)))["treated"]
    )
  }
  stat_df = stat_df %>% dplyr::left_join(
    y = data.frame(
      variable = features,
      t_stat = t_stat_store,
      stringsAsFactors = FALSE
    ),
    by = c("variable" = "variable")
  )
  stat_df
}

