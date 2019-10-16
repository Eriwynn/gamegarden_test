## Libraries and environment setup ----
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)


## Editing DF due to tidy data manifest ----
df <- read.csv(file = "D:\\Downloads\\mail_test.csv")

df$date <- as.Date(as.character(df$date), "%d.%m.%Y")
clear_names <- names(df)[6:22]

tidy_df <- df[2:23] %>%
    gather(
        key = "country", value = "country_download", 
        -date, -Profit, -ADV.SPEND, -TOTAL.DOWNLOAD, -TOTAL.REVENUE
    )
temp <- df[c("date", names(df)[24:40])] %>%
    gather(key = "country", value = "country_revenue", -date) %>%
    mutate(country = substr(country, start = 1, stop = 2))

tidy_df <- tidy_df %>% 
    left_join(temp) %>% 
    mutate_if(is.factor, as.character) %>%
    mutate(
        Profit = as.numeric(gsub(" ", "", gsub(",", ".", Profit))),
        ADV.SPEND = as.numeric(gsub(" ", "", gsub(",", ".", ADV.SPEND)))
    ) %>%
    arrange(date) 
rm(temp)

head(tidy_df)


## Dates of switch ----
dates <- list(
    start_traffic = as.Date("2012-01-13"),
    switch_1 = as.Date("2012-02-04"),
    switch_2 = as.Date("2012-03-20")
)

dates_seq <- list(
    stage_1 = seq(from = dates$start_traffic, to = dates$switch_1 - 1, by = 'days'),
    stage_2 = seq(from = dates$switch_1, to = dates$switch_2 - 1, by = 'days')
)

stages <- list(
    pre_ad = "No traffic",
    stage_1 = "Stage 1",
    stage_2 = "Stage 2",
    stage_3 = "Stage 3"
)

tidy_df <- tidy_df %>% 
    mutate(
        stage = ifelse(
            date < dates$start_traffic, stages$pre_ad, 
            ifelse(
                date %in% dates_seq$stage_1, stages$stage_1, 
                ifelse(date %in% dates_seq$stage_2, stages$stage_2, stages$stage_3)
            )
        ),
    )


## Active countries during traffic switches ----
countries_switch <- list()
countries_switch$start <- unique(
    tidy_df %>% 
    filter(date >= dates$start_traffic & date < dates$switch_1) %>%
    filter(country_download > 290) %>%
    .$country
)

countries_switch$switch_1 <- unique(
    tidy_df %>% 
    filter(date >= dates$switch_1 & date < dates$switch_2) %>%
    filter(country_download > 200) %>%
    .$country
)

countries_switch$switch_2 <- unique(
    tidy_df %>% 
        filter(date >= dates$switch_2) %>%
        filter(country_download > 400) %>%
        .$country
)


## Efficiency metrics for active countries during traffic switches ----
# Whole experiment time
difftime(max(tidy_df$date), min(tidy_df$date))

# Mean revenue by stage
tidy_df %>% 
    group_by(stage, date) %>%
    summarise(revenue = sum(country_revenue)) %>%
    group_by(stage) %>%
    summarise(mean_revenu = mean(revenue))

# Countries efficiency by stage
metrics <- list()

for (i in stages){
    metrics[[i]] <- tidy_df %>% 
        filter(stage == i) %>%
        group_by(stage, country) %>%
        summarise(
            revenue = sum(country_revenue)
        ) %>%
        ungroup() %>%
        mutate(
            total_revenue = sum(revenue),
            percentage = revenue/total_revenue
        ) %>%
        arrange(-revenue) %>%
        mutate(cum_sum = cumsum(percentage))
}

metrics$`Stage 1`

# Average ARPU by country by stage
metrics_arpu <- list()
for (i in stages){
    metrics_arpu[[i]] <- tidy_df %>% 
        filter(stage == i) %>%
        group_by(stage, country) %>%
        summarise(
            ARPU = mean(country_revenue/country_download, )
        ) %>%
        arrange(-ARPU)
}


## Countries revenue analysis ----
tidy_df %>% 
    filter(country == "US" & dates > dates$start_traffic + 5) %>%
    plot_ly(x =~ country_revenue, type = "histogram", nbinsx = 50)
