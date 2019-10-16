# Revenue by day split by country
plot_ly(
    data = tidy_df,
    x =~ date,
    y =~ country_revenue,
    type = "scatter",
    mode = "lines+markers",
    split =~ country
)

# Downloads by day split by country
plot_ly(
    data = tidy_df,
    x =~ date,
    y =~ country_download,
    type = "scatter",
    mode = "lines+markers",
    split =~ country
)

# ARPU by day split by country
plot_ly(
    data = tidy_df,
    x =~ date,
    y =~ country_revenue/country_download,
    type = "scatter",
    mode = "lines+markers",
    split =~ country
)

# Downloads by day split by country
plot_ly(
    data = tidy_df %>% filter(
        country %in% c("CA", "US", "CH", "DE", "BG", "NL")
    ),
    x =~ date,
    y =~ country_download,
    type = "scatter",
    mode = "lines+markers",
    split =~ country
) %>%
    layout(
        title = "",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "Downloads", titlefont = list(size = 20), tickfont = list(size = 18))
    )


## Stages ----
plot_list <- list()

## Stage 1 ----
# Time-series for ARPU
plot_list$stage_1$time_arpu <- tidy_df %>%
    filter(date >= dates$start_traffic & date < dates$switch_1) %>%
    filter(country %in% countries_switch$start) %>%
    plot_ly(
        x =~ date,
        y =~ country_revenue/country_download,
        split =~ country,
        type = "scatter",
        mode = "lines+markers"
    ) %>%
    layout(
        title = "Stage 1",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "ARPU", titlefont = list(size = 20), tickfont = list(size = 18))
    )

# Violin subplot for total downloads and revenue results
plot_list$stage_1$violin_download <- tidy_df %>%
    filter(date >= dates$start_traffic & date < dates$switch_1) %>%
    filter(country %in% countries_switch$start) %>%
    plot_ly(
        x =~ country,
        y =~ country_download,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Downloads", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_1$violin_revenue <- tidy_df %>%
    filter(date >= dates$start_traffic & date < dates$switch_1) %>%
    filter(country %in% countries_switch$start) %>%
    plot_ly(
        x =~ country,
        y =~ country_revenue,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Revenue", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_1$subplot <- subplot(
    plot_list$stage_1$violin_download, 
    plot_list$stage_1$violin_revenue,
    nrows = 2, shareX = TRUE, titleY = TRUE
) %>% 
    layout(
        title = "Stage 1: Downloads & Revenue",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        showlegend = FALSE
    )

plot_list$stage_1$time_arpu
plot_list$stage_1$subplot

## Stage 2 ----
plot_list$stage_2$time_arpu <- tidy_df %>%
    filter(date >= dates$switch_1 & date < dates$switch_2) %>%
    filter(country %in% countries_switch$switch_1) %>%
    plot_ly(
        x =~ date,
        y =~ country_revenue/country_download,
        split =~ country,
        type = "scatter",
        mode = "lines+markers"
    ) %>%
    layout(
        title = "Stage 2",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "ARPU", titlefont = list(size = 20), tickfont = list(size = 18))
    )

# Violin subplot for total downloads and revenue results
plot_list$stage_2$violin_download <- tidy_df %>%
    filter(date >= dates$switch_1 & date < dates$switch_2) %>%
    filter(country %in% countries_switch$switch_1) %>%
    plot_ly(
        x =~ country,
        y =~ country_download,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Downloads", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_2$violin_revenue <- tidy_df %>%
    filter(date >= dates$switch_1 & date < dates$switch_2) %>%
    filter(country %in% countries_switch$switch_1) %>%
    plot_ly(
        x =~ country,
        y =~ country_revenue,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Revenue", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_2$subplot <- subplot(
    plot_list$stage_2$violin_download, 
    plot_list$stage_2$violin_revenue,
    nrows = 2, shareX = TRUE, titleY = TRUE
) %>% 
    layout(
        title = "Stage 2: Downloads & Revenue",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        showlegend = FALSE
    )

plot_list$stage_2$time_arpu
plot_list$stage_2$subplot


## Stage 3 ----
plot_list$stage_3$time_arpu <- tidy_df %>%
    filter(date >= dates$switch_2) %>%
    filter(country %in% countries_switch$switch_2) %>%
    plot_ly(
        x =~ date,
        y =~ country_revenue/country_download,
        split =~ country,
        type = "scatter",
        mode = "lines+markers"
    ) %>%
    layout(
        title = "Stage 3",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "ARPU", titlefont = list(size = 20), tickfont = list(size = 18))
    )

# Violin subplot for total downloads and revenue results
plot_list$stage_3$violin_download <- tidy_df %>%
    filter(date >= dates$switch_2) %>%
    filter(country %in% countries_switch$switch_2) %>%
    plot_ly(
        x =~ country,
        y =~ country_download,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Downloads", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_3$violin_revenue <- tidy_df %>%
    filter(date >= dates$switch_2) %>%
    filter(country %in% countries_switch$switch_2) %>%
    plot_ly(
        x =~ country,
        y =~ country_revenue,
        type = 'violin',
        box = list(visible = T),
        meanline = list(visible = T)
    ) %>%
    layout(yaxis = list(title = "Revenue", titlefont = list(size = 20), tickfont = list(size = 18)))

plot_list$stage_3$subplot <- subplot(
    plot_list$stage_3$violin_download, 
    plot_list$stage_3$violin_revenue,
    nrows = 2, shareX = TRUE, titleY = TRUE
) %>% 
    layout(
        title = "Stage 3: Downloads & Revenue",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        showlegend = FALSE
    )

plot_list$stage_3$time_arpu
plot_list$stage_3$subplot


## General metrics ----
# Boxplot with revenue by day by stages
tidy_df %>% 
    group_by(stage, date) %>%
    summarise(
        revenue = sum(country_revenue)
    ) %>% 
    plot_ly(
        y =~ revenue,
        split =~ stage,
        type = "box",
        boxpoints = "all"
    ) %>% 
    layout(
        title = "Revenue by stage",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "Revenue", titlefont = list(size = 20), tickfont = list(size = 18)),
        legend = list(font = list(size = 14))
    )

# Time series with 
tidy_df %>% 
    group_by(stage, date) %>%
    summarise(
        revenue = sum(country_revenue)
    ) %>% 
    plot_ly(
        x =~ date,
        y =~ revenue,
        split =~ stage,
        type = "scatter",
        mode = "lines+markers"
    ) %>% 
    layout(
        title = "Revenue by stage",
        titlefont = list(size = 18),
        xaxis = list(title = "", tickfont = list(size = 18)),
        yaxis = list(title = "Revenue", titlefont = list(size = 20), tickfont = list(size = 18)),
        legend = list(font = list(size = 14))
    )
