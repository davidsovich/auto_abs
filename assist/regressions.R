reg_table = function(est_list, round_val = 3, coef_scale = 1, manual_var_names) {
  reg_df = lapply(
    est_list,
    broom::tidy,
    robust = TRUE
  )
  reg_df = lapply(
    seq_along(reg_df),
    function(x, i) {
      x[[i]] %>%
        dplyr::mutate(
          model_number = i
        )
    },
    x = reg_df
  ) 
  reg_df = reg_df %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      estimate = round(estimate * coef_scale, round_val),
      statistic = round(statistic, round_val)
    ) %>%
    dplyr::rename(
      coef = estimate,
      tstat = statistic
    ) %>%
    dplyr::select(
      term, 
      coef,
      tstat,
      model_number
    )
  temp_df = reg_df %>%
    dplyr::distinct(
      term
    ) %>%
    dplyr::mutate(
      var_order = dplyr::row_number()
    )
  reg_df = reg_df %>%
    tidyr::gather(
      coef,
      tstat,
      key = "statistic",
      value = "value"
    ) %>%
    tidyr::spread(
      key = "model_number",
      value = "value"
    ) %>%
    dplyr::left_join(
      y = temp_df,
      by = c("term" = "term")
    ) %>%
    dplyr::arrange(
      var_order,
      statistic
    ) %>%
    dplyr::mutate(
      term = ifelse(
        dplyr::row_number() %% 2 == 0,
        "",
        term
      )
    ) %>%
    dplyr::select(
      -var_order
    ) %>%
    rbind(
      c(
        "",
        "N",
        unlist(
          lapply(
            est_list,
            function(x) { summary(x)$N }
          )
        )
      )
    ) %>%
    rbind(
      c(
        "",
        "RSQ",
        unlist(
          lapply(
            est_list,
            function(x) { round(summary(x)$r.squared, 3) }
          )
        )
      )
    ) %>%
    rbind(
      c(
        "",
        "AdjRSQ",
        unlist(
          lapply(
            est_list,
            function(x) { round(summary(x)$adj.r.squared, 3) }
          )
        )
      )
    ) %>%
    rbind(
      c(
        "",
        "F",
        unlist(
          lapply(
            est_list,
            function(x) { round(summary(x)$fstat, 3) }
          )
        )
      )
    ) %>%
    rbind(
      c(
        "",
        "Outcome",
        unlist(
          lapply(
            est_list,
            function(x) { colnames(x$response)[1] }
          )
        )
      )
    )
  if(missing(manual_var_names)) {
    reg_df
  } else {
    reg_df %>%
      #slice(
      #  -6
      #) %>%
      rbind(
        c("", "", manual_var_names)
      )
  }
}

prep_dyn_dd_tbl = function(dd_df, ref_time, ctrls, copy_time = 1) {
  if(!missing(ctrls)) {
    dd_df = dd_df %>%
      dplyr::filter(
        !(coef_name %in% ctrls)
      )
  }
  dd_df = dd_df %>%
    dplyr::bind_rows(
      dd_df %>%
        dplyr::slice(
          copy_time
        ) %>%
        dplyr::mutate(
          time = as.numeric(ref_time),
          coef_name = ifelse(
            as.numeric(ref_time) < 0,
            paste0(
              "dd_n",
              abs(as.numeric(ref_time))
            ),
            paste0(
              "dd_",
              as.numeric(ref_time)
            )
          ),
          coef = 0,
          se = 0.001
        )
    ) %>%
    dplyr::arrange(
      time
    )
  dd_df
}

prep_joint_dd_tbl = function(dd_df, ctrls) {
  if(!missing(ctrls)) {
    dd_df = dd_df %>%
      dplyr::filter(
        !(term %in% ctrls)
      )
  }
  dd_df = dd_df %>%
    dplyr::mutate(
      wage_label = dplyr::case_when(
        group == 0  ~ "[0]",
        group == 1  ~ "(0, \u0394)",
        group == 20 ~ "[19\u0394 +)",
        TRUE           ~ paste0(
          "[",
          as.numeric(group) - 1,
          "\u0394", 
          ", ",
          as.numeric(group),
          "\u0394",
          ")"
        )
      )
    )
  dd_df
}

prep_cumul_dd_tbl = function(dd_df, beg, group = "group", coef = "coef", se = "se", level = 0.95) {
  z_stat = qnorm(1 - (1 - level) / 2)
  dd_df[, "group"] = dd_df[, group]
  dd_df[, "coef"] = dd_df[, coef]
  dd_df[, "se"] = dd_df[, se]
  dd_df[, "upper"] = dd_df[, "coef"] + z_stat * dd_df[, "se"]
  dd_df[, "lower"] = dd_df[, "coef"] - z_stat * dd_df[, "se"]
  dd_df = dd_df %>%
    dplyr::filter(
      !is.na(group)
    ) %>%
    dplyr::mutate(
      group = as.numeric(group)
    ) %>%
    dplyr::arrange(
      group
    ) %>%
    dplyr::mutate(
      t_index = dplyr::row_number()
    ) %>%
    dplyr::filter(
      group >= beg
    )
  dd_df %>%
    dplyr::select(
      group,
      coef,
      se,
      lower,
      upper,
      t_index
    )
}

plot_dd_jole = function(dd_df, time = "time", coef = "coef", se = "se", level = 0.95, ymin, ymax, xlab, ylab, title) {
  z_stat = qnorm(1 - (1 - level) / 2)
  dd_df[, "time"] = dd_df[, time]
  dd_df[, "coef"] = dd_df[, coef]
  dd_df[, "se"] = dd_df[, se]
  dd_df[, "upper"] = dd_df[, "coef"] + z_stat * dd_df[, "se"]
  dd_df[, "lower"] = dd_df[, "coef"] - z_stat * dd_df[, "se"]
  dd_df = dd_df %>%
    dplyr::filter(
      !is.na(time)
    ) %>%
    dplyr::arrange(
      time
    ) %>%
    dplyr::mutate(
      t_index = dplyr::row_number()
    )
  plot_out = ggplot2::ggplot(
    dd_df,
    ggplot2::aes(
      x = t_index,
      y = coef
    )
  ) + 
    ggplot2::geom_line(
      linetype = "solid"#,
      #size = 0.75
      #color = "blue"
    ) + 
    ggplot2::geom_point(
      #color = "blue",
      shape = 1,
      size = 3,
      color = "black"
    ) + 
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = lower,
        ymax = upper
      ),
      #color = "red",
      #size = 0.75,
      width = 0.2,
      position = ggplot2::position_dodge(0.05)
    ) + 
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "solid",
      color = "black",
      size = 0.5
    ) +
    ggplot2::scale_x_continuous(
      breaks = unique(dd_df$t_index),
      labels = unique(dd_df$time)
    ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1, 
        size = 12
      ),
      axis.text.y = ggplot2::element_text(
        size = 12
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
      #,
      #plot.title = ggplot2::element_text(
      #  size = 12,
      #  face = "bold"
      #)
    )
  if(!missing(ymin) & !missing(ymax)) {
    plot_out = plot_out + 
      ggplot2::ylim(ymin, ymax)
  } 
  if(!missing(xlab)) {
    plot_out = plot_out + 
      ggplot2::labs(
        x = xlab
      )
  }
  if(!missing(ylab)) {
    plot_out = plot_out + 
      ggplot2::labs(
        y = ylab
      )
  }
  if(!missing(title)) {
    plot_out = plot_out + 
      ggplot2::labs(
        title = title
      )
  }
  plot_out      
}

plot_split_dd_jole = function(dd_df, group = "group", coef = "coef", se = "se", cumul_line = TRUE,
                              level = 0.95, custom_break_labels, ymin, ymax, xlab, ylab, title) {
  z_stat = qnorm(1 - (1 - level) / 2)
  dd_df[, "group"] = dd_df[, group]
  dd_df[, "coef"] = dd_df[, coef]
  dd_df[, "se"] = dd_df[, se]
  dd_df[, "upper"] = dd_df[, "coef"] + z_stat * dd_df[, "se"]
  dd_df[, "lower"] = dd_df[, "coef"] - z_stat * dd_df[, "se"]
  dd_df = dd_df %>%
    dplyr::filter(
      !is.na(group)
    ) %>%
    dplyr::mutate(
      group = as.numeric(group)
    ) %>%
    dplyr::arrange(
      group
    ) %>%
    dplyr::mutate(
      t_index = dplyr::row_number()
    )
  if(cumul_line == TRUE) {
    dd_df = dd_df %>%
      dplyr::mutate(
        cum_sum = cumsum(coef)
      )
  }
  plot_out = ggplot2::ggplot(
    dd_df
  ) + 
    ggplot2::geom_bar(
      ggplot2::aes(
        x = t_index,
        y = coef
      ),
      position = "dodge",
      stat = "identity",
      color = "black",
      fill = "white"
      #fill = "gray",
      #alpha = 0.5
    ) + 
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = t_index,
        ymin = coef - z_stat * se,
        ymax = coef + z_stat * se
      ),
      color = "black",
      width = 0.2,
      position = ggplot2::position_dodge(0.5)
    )
  if(!missing(custom_break_labels)) {
    dd_df[, "break_vals"] = dd_df[, custom_break_labels]
    plot_out = plot_out + 
      ggplot2::scale_x_continuous(
        breaks = dd_df$t_index,
        labels = dd_df$break_vals
      ) + 
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          size = 12,
          angle = 45,
          hjust = 1
        ),
        axis.text.y = ggplot2::element_text(
          size = 12
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  } else {
    plot_out = plot_out + 
      ggplot2::scale_x_continuous(
        breaks = dd_df$t_index,
        labels = dd_df$group
      ) + 
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(
          angle = 45,
          hjust = 1
        ),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  if(cumul_line == TRUE) {
    plot_out = plot_out + 
      ggplot2::geom_line(
        data = dd_df,
        ggplot2::aes(
          x = t_index,
          y = cum_sum
        ),
        linetype = "twodash",
        color = "gray46"
      ) +
      ggplot2::geom_point(
        data = dd_df,
        ggplot2::aes(
          x = t_index,
          y = cum_sum
        ),
        shape = 5,
        size = 2,
        color = "black"
      )
  }
  if(!missing(ymin) & !missing(ymax)) {
    plot_out = plot_out + 
      ggplot2::ylim(ymin, ymax)
  } 
  if(!missing(xlab)) {
    plot_out = plot_out + 
      ggplot2::labs(
        x = xlab
      )
  }
  if(!missing(ylab)) {
    plot_out = plot_out + 
      ggplot2::labs(
        y = ylab
      )
  }
  if(!missing(title)) {
    plot_out = plot_out + 
      ggplot2::labs(
        title = title
      )
  }
  plot_out   
}

dynamic_dd = function(df, y, treated, time, excluded, fes, clusters, ctrls, output = "table") {
  df[, "treated"] = df[, treated]
  df[, "time"] = df[, time]
  df[, "y_var"] = df[, y]
  df = df %>%
    dplyr::mutate(
      dist_dd = ifelse(
        time >= 0,
        paste0("dd_", time),
        paste0("dd_n", abs(time))
      )
    ) %>%
    tidyr::spread(
      dist_dd,
      treated,
      fill = 0
    )
  form = paste(
    "y_var ~", 
    paste(
      paste0(
        "dd_",
        ifelse(
          unique(df$time)[unique(df$time) != excluded] >= 0,
          unique(df$time)[unique(df$time) != excluded],
          paste0("n", abs(unique(df$time)[unique(df$time) != excluded]))
        )
      ),
      sep = "", 
      collapse = " + "
    ),
    ifelse(
      missing(ctrls),
      "",
      paste0(
        " + ",
        paste(
          ctrls,
          sep = "",
          collapse = " + "
        )
      )
    ),
    "|",
    ifelse(
      missing(fes),
      "0",
      paste(
        fes,
        sep = "",
        collapse = " + "
      )
    ),
    "| 0 |",
    ifelse(
      missing(clusters),
      "0",
      paste(
        clusters,
        sep = "",
        collapse = " + "
      )
    )
  )
  est = lfe::felm(
    formula = as.formula(form),
    data = df
  )
  if(output == "model") {
    est
  }  else {
    data.frame(
      variable = rep(y, length(coef(est))),
      time = c(
        unique(df$time)[unique(df$time) != excluded],
        rep(
          NA, 
          length(coef(est)) - length(unique(df$time)[unique(df$time) != excluded])
        )
      ),
      coef_name = names(coef(est)),
      coef = coef(est),
      se = sqrt(diag(vcov(est))),
      tstat = coef(est) / sqrt(diag(vcov(est))),
      lower_95 = coef(est) - 1.96 * sqrt(diag(vcov(est))),
      upper_95 = coef(est) + 1.96 * sqrt(diag(vcov(est))),
      stringsAsFactors = FALSE
    ) %>%
      dplyr::as_tibble()
  }
}

static_dd = function(df, y, treated, post, fes, clusters, ctrls, output = "model") {
  df[, "treated"] = df[, treated]
  df[, "post"] = df[, post]
  df[, "y_var"] = df[, y]
  df = df %>%
    dplyr::mutate(
      diff_diff_var = treated * post
    ) 
  form = paste(
    "y_var ~ ",
    "diff_diff_var",
    ifelse(
      missing(ctrls),
      "",
      paste0(
        " + ",
        paste(
          ctrls,
          sep = "",
          collapse = " + "
        )
      )
    ),
    "|",
    ifelse(
      missing(fes),
      "0",
      paste(
        fes,
        sep = "",
        collapse = " + "
      )
    ),
    "| 0 |",
    ifelse(
      missing(clusters),
      "0",
      paste(
        clusters,
        sep = "",
        collapse = " + "
      )
    )
  )
  est = lfe::felm(
    formula = as.formula(form),
    data = df
  )
  if(output == "model") {
    est
  } else {
    broom::tidy(
      est
    ) %>%
      dplyr::mutate(
        variable = y,
        lower_95 = estimate - 1.96 * std.error,
        upper_95 = estimate + 1.96 * std.error
      ) %>%
      dplyr::rename(
        coef = estimate,
        se = std.error,
        tstat = statistic
      ) %>%
      dplyr::select(
        variable,
        coef,
        se,
        tstat,
        lower_95,
        upper_95
      )
  }
}

joint_dd = function(df, y, treated, post, grouper, fes, clusters, ctrls, output = "model") {
  df[, "treated"] = df[, treated]
  df[, "post"] = df[, post]
  df[, "y_var"] = df[, y]
  df[, "grouper"] = df[, grouper]
  df = df %>%
    dplyr::mutate(
      dd = treated * post,
      grouper_dd = paste0(
        "dd_",
        grouper
      )
    ) %>%
    tidyr::spread(
      grouper_dd,
      dd,
      fill = 0
    ) %>%
    mutate(
      diff_diff_var = treated * post
    )
  form = paste(
    "y_var ~ ",
    paste(
      paste0(
        "dd_",
        sort(unique(df$grouper))
      ),
      sep = "",
      collapse = " + "
    ),
    ifelse(
      missing(ctrls),
      "",
      paste0(
        " + ",
        paste(
          ctrls,
          sep = "",
          collapse = " + "
        )
      )
    ),
    "|",
    ifelse(
      missing(fes),
      "0",
      paste(
        fes,
        sep = "",
        collapse = " + "
      )
    ),
    "| 0 |",
    ifelse(
      missing(clusters),
      "0",
      paste(
        clusters,
        sep = "",
        collapse = " + "
      )
    )
  )
  est = lfe::felm(
    formula = as.formula(form),
    data = df
  )
  if(output == "model") {
    est
  } else {
    broom::tidy(
      est
    ) %>%
      dplyr::mutate(
        group = gsub("dd_", "", term),
        lower_95 = estimate - 1.96 * std.error,
        upper_95 = estimate + 1.96 * std.error
      ) %>%
      dplyr::rename(
        coef = estimate,
        se = std.error,
        tstat = statistic
      ) %>%
      dplyr::select(
        group,
        term,
        coef,
        se,
        tstat,
        lower_95,
        upper_95
      )
  }
}

descr_stats = function(df, features) {
  if(sum(unique(features) %in% colnames(df)) <= 1) {
    stop("Error! Must supply at least two variables in data. Otherwise, use summary() function.")
  }
  df = suppressWarnings(
    df %>%
      dplyr::summarise_at(
        unique(features),
        list(
          sepzzzz_N = function(x, na.rm) { sum(!is.na(x)) },
          sepzzzz_mean = mean,
          sepzzzz_sd = sd,
          sepzzzz_p10 = function(x, na.rm) { quantile(x, 0.10, na.rm = na.rm) },
          sepzzzz_p25 = function(x, na.rm) { quantile(x, 0.25, na.rm = na.rm) },
          sepzzzz_p50 = function(x, na.rm) { quantile(x, 0.50, na.rm = na.rm) },
          sepzzzz_p75 = function(x, na.rm) { quantile(x, 0.75, na.rm = na.rm) },
          sepzzzz_p90 = function(x, na.rm) { quantile(x, 0.90, na.rm = na.rm) },
          sepzzzz_min = min,
          sepzzzz_max = max
        ),
        na.rm = TRUE
      ) %>%
      tidyr::gather(
        key = "temp",
        value = "value"
      ) %>%
      tidyr::separate(
        temp,
        sep = "_sepzzzz_", 
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

fwinsor2 = function(x, lower_p, upper_p) {
  extrema = quantile(
    x[!is.na(x)],
    c((lower_p / 100), (upper_p / 100)),
    type = 5
  )
  x[(x < extrema[1]) & (!is.na(x))] = extrema[1]
  x[(x > extrema[2]) & (!is.na(x))] = extrema[2]
  x
}

add_months_to_archive = function(beg_archive, num_months) {
  temp_months = as.numeric(substr(beg_archive, 1, 4))*12 + as.numeric(substr(beg_archive, 5, 6))*1
  temp_months = temp_months + num_months
  temp_years = temp_months %/% 12 
  temp_months = temp_months %% 12
  temp_years[temp_months == 0] = temp_years[temp_months == 0] - 1
  temp_months[temp_months == 0] = 12
  temp_years*100 + temp_months
}
