library(ggplot2)
library(dplyr)
library(tidyr)
library(here)
library(naflex)

source(here("src", "helper_funs.R"))

mozambique_s_daily <- readRDS(here("data", "moz3_qc.RDS"))

# Figure 2: Mean Monthly Maximum and Minimum Temperatures
# Missing value rule taken from  WMO Guidelines on the Calculation of Climate Normals
# https://library.wmo.int/doc_num.php?explnum_id=4166 Section 4.4.1 Mean values
temperature_summaries <- mozambique_s_daily %>% 
  group_by(station, s_year, month_abbr) %>%
  summarise(mean_tmax = mean(Tmax %>% na_omit_if(n = 10, consec = 4)),
            mean_tmin = mean(Tmin %>% na_omit_if(n = 10, consec = 4))) %>% 
  pivot_longer(cols = 4:5, names_to = "variable", values_to = "values")

for (s in unique(temperature_summaries$station)) {
  g <- temperature_summaries %>%
    filter(station == s) %>%
    ggplot(aes(x = s_year, y = values, group = variable, color = variable)) +
    geom_line(linewidth = 0.3)+
    geom_point(size = 0.6, color = "black") +
    geom_smooth(method = lm, linewidth = 1) +
    labs(y = "Mean Temperature (°C)") +
    facet_wrap( ~ month_abbr, ncol = 4) +
    theme_bw() +
    xlab("") +
    theme(legend.position = "right") +
    scale_color_discrete(labels = c('Mean Tmax', 'Mean Tmin'), name = NULL) +
    scale_y_continuous(breaks = seq(10, 40, 10))
  ggsave(here("output", paste0("a.figure_2_mean_monthly_temp_", s, ".png")),
         g, width = 12, height = 8)
}

# Figure 3: Monthly rainfall totals over the course of 1 year (Aug-Jul): top 10%, median & bottom 10%
# Missing value rule taken from  WMO Guidelines on the Calculation of Climate Normals
# https://library.wmo.int/doc_num.php?explnum_id=4166 Section 4.4.1 Sum values
mz_s_sum_precipitaion_summaries <- mozambique_s_daily %>%
  group_by(station, s_year, month_abbr) %>%
  summarise(sum_Prec = sum(Prec, na.rm = FALSE)) %>%
  group_by(station, month_abbr) %>%
  summarise(`Bottom 10%` = quantile(sum_Prec, probs = 0.1, na.rm = TRUE),
            `Median` = median(sum_Prec, na.rm = TRUE),
            `Top 90%` = quantile(sum_Prec, probs = 0.9, na.rm = TRUE))

precip_sum_stacked <- mz_s_sum_precipitaion_summaries %>%
  pivot_longer(cols = 3:5, names_to = "variable", values_to = "values", 
               names_ptypes = factor(levels = c("Top 90%", "Median", "Bottom 10%")))

for (s in unique(mz_s_sum_precipitaion_summaries$station)){
  g <- precip_sum_stacked %>% filter(station == s) %>%
    ggplot2::ggplot(aes(x = month_abbr, y = values, group = variable, 
                        color = variable)) +
    ggplot2::geom_line()+
    ggplot2::geom_point()+
    theme_grey() +
    xlab("") +
    ylab("Monthly Total Rainfall (mm)")
  ggsave(here("output", paste0("b.figure_3_monthly_rainfall_totals_", s, ".png")), 
         g, width = 12, height = 8)
}

# Figure 4: Rainy season start dates

theme_annual <- theme(
  panel.grid.major = element_line(colour = "lightblue", linetype = "longdash", linewidth = 1),
  panel.grid.minor = element_line(colour = "lightblue", linetype = "longdash", linewidth = 1),
  panel.border = element_rect(colour = "black", fill = "NA", linewidth = 0),
  axis.text.x = element_text(angle = 90, size = 12, vjust = 0.4),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  title = element_text(size = 20),
  plot.subtitle = element_text(size = 15),
  plot.caption = element_text(size = 8),
  axis.text.y = element_text(size = 12),
  panel.background = element_rect(colour = "white", fill = "white", linewidth = 0.5, linetype = "solid")
  )

start_of_rains_dry <- mozambique_s_daily %>% 
  group_by(station) %>%
  mutate(rain_day = Prec >= 0.85,
         dry_spell = .spells(x = rain_day == 0),
         roll_max_dry_spell = dplyr::lead(x = RcppRoll::roll_maxl(x = dry_spell, n = 21, fill = NA)),
         roll_sum_rain = RcppRoll::roll_sumr(x = Prec, n = 3, fill = NA, na.rm = FALSE)) %>%
  filter((Prec >= 0.85 & roll_sum_rain > 20 & roll_max_dry_spell <= 9) | 
           is.na(Prec) | is.na(roll_sum_rain) | is.na(roll_max_dry_spell), 
         .preserve = TRUE) %>%
  group_by(s_year, .add = TRUE) %>%
  filter(s_doy >= 93 & s_doy <= 184, 
         .preserve = TRUE) %>%
  summarise(start_rain_dry = ifelse(test = is.na(dplyr::first(x=Prec)) | 
                                      is.na(dplyr::first(x=roll_sum_rain)) | 
                                      is.na(dplyr::first(x=roll_max_dry_spell)), 
                                    yes = NA, 
                                    no = dplyr::first(x = s_doy, default = NA)))

start_of_rains_dry_with_mean_y <- start_of_rains_dry %>% 
  group_by(station) %>%
  dplyr::mutate(.mean_y = as.Date(round(mean(start_rain_dry, na.rm = TRUE)), origin = "2015-07-31"))

for (s in unique(start_of_rains_dry_with_mean_y$station)) {
  g <- start_of_rains_dry_with_mean_y %>% filter(station == s) %>%
    ggplot(mapping = aes(x = s_year, y = as.Date(start_rain_dry, origin = "2015-07-31"))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y,
                             label = paste("Mean:", format(x = .mean_y, format = "%d %b"))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "Start date") + 
    labs(caption = "First occurance from 1 Nov with more than 20mm in 3 days and no 10 day dry spell in the next 21 days.") +
    scale_y_date(date_labels = "%d %b", date_breaks ="1 months") +
    scale_x_continuous(breaks = seq(1900, 2022, 4))
  ggsave(here("output", paste0("c.figure_4_start_rains_", s, ".png")), 
         g, width = 12, height = 8)
}

# Figure 4: Rainy season end date

end_of_rains <- mozambique_s_daily %>%
  group_by(station) %>%
  mutate(roll_sum_rain = RcppRoll::roll_sumr(x = Prec, n = 1, fill = NA, na.rm = FALSE)) %>%
  filter((roll_sum_rain > 10) | is.na(roll_sum_rain)) %>% 
  group_by(s_year, .add = TRUE) %>%
  filter(s_doy >= 185 & s_doy <= 289, .preserve = TRUE) %>% 
  summarise(end_rains = ifelse(test = is.na(dplyr::last(roll_sum_rain)), 
                               yes = NA, 
                               no = dplyr::last(s_doy)))

end_of_season <- mozambique_s_daily %>% 
  group_by(station) %>%
  mutate(rain_min = ifelse(test = is.na(Prec), yes = 0, no = Prec),
         wb_min = Reduce(f = function(x, y) pmin(pmax(x + y, 0), 120), x = tail(x = rain_min - 5, n = -1), 
                         init = 0, accumulate = TRUE),
         rain_max = ifelse(test = is.na(Prec), yes = 120, no = Prec),
         wb_max = Reduce(f = function(x, y) pmin(pmax(x + y, 0), 120), x = tail(x = rain_max - 5, n = -1), 
                         init = 0, accumulate = TRUE),
         wb = ifelse(test = (wb_min != wb_max) | is.na(Prec), yes = NA, no = wb_min)) %>%
  filter((wb <= 0) | is.na(Prec)) %>% 
  group_by(s_year, .add = TRUE) %>%
  left_join(end_of_rains, by = c("station", "s_year"), suffix = c("", "")) %>% 
  filter(s_doy >= end_rains & s_doy <= 366, .preserve = TRUE) %>%
  summarise(end_season = ifelse(test = is.na(dplyr::first(x=wb)), yes = NA, no = dplyr::first(s_doy)),
            end_season_date = dplyr::if_else(condition = is.na(dplyr::first(wb)), 
                                             true = as.Date(NA), false = dplyr::first(date)))

end_season_with_mean_y <- end_of_season %>% 
  group_by(station) %>%
  dplyr::mutate(.mean_y = as.Date(round(mean(end_season, na.rm = TRUE)), origin = "2015-07-31"))

for (s in unique(end_season_with_mean_y$station)) {
  g <- end_season_with_mean_y %>% filter(station == s) %>%
    ggplot(mapping = aes(x = s_year, y = as.Date(end_season, origin = "2015-07-31"))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept=.mean_y), linewidth=1.5) +  
    geom_label(mapping = aes(x = -Inf, y = .mean_y,
                             label = paste("Mean:", format(x = .mean_y, format = "%d %b"))),
               hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "End date") + 
    labs(caption = "First occasion from the last rainfall of more than 10mm with empty water balance.\n 
         Capacity is 120mm and evaporation is taken as 5mm per day.") +
    scale_y_date(date_labels = "%d %b", date_breaks = "1 months") + 
    scale_x_continuous(breaks = seq(1950, 2022, 4))
  ggsave(here("output", paste0("d.figure_4_end_season_", s, ".png")), 
         g, width = 12, height = 8)
}

# Figure 4: Length of season

combined_rain_summaries <- start_of_rains_dry %>% 
  left_join(end_of_rains, by = c("station", "s_year"), suffix = c("", "")) %>%
  left_join(end_of_season, by = c("station", "s_year"), suffix = c("", "")) %>% 
  mutate(length_season = end_season - start_rain_dry)

length_season_with_mean_y <- combined_rain_summaries %>% 
  group_by(station) %>%
  dplyr::mutate(.mean_y = mean(length_season, na.rm = TRUE))

for (s in unique(length_season_with_mean_y$station)) {
  g <- length_season_with_mean_y %>% filter(station == s) %>%
    ggplot(mapping=aes(x = s_year, y = as.numeric(length_season))) + 
    geom_line(colour = "blue", linewidth = 0.8) + geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y, label = paste("Mean:", round(.mean_y))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "Length of Season") + 
    labs(caption = "Season length: Number of days from start of rains date to end of season date.") +
    scale_x_continuous(breaks = seq(1970, 2018, 4)) + 
    scale_y_continuous(limits = c(0, 240), expand = expansion(mult = c(0,0)), 
                                breaks = seq(0, 200, 50))  
  ggsave(here("output", paste0("e.figure_4_length_season_", s, ".png")), 
         g, width = 12, height = 8)
}

# Figure 5: Total seasonal rainfall

combined_rain_summaries_2 <- combined_rain_summaries %>% 
  left_join((mozambique_s_daily %>%
               left_join(combined_rain_summaries, by = c("station", "s_year"), suffix = c("", "")) %>% 
               group_by(station) %>%
               filter(s_doy >= start_rain_dry & s_doy <= end_season, .preserve = TRUE) %>%
               group_by(s_year, .add = TRUE, .drop = FALSE) %>%
               summarise(total_rain = sum(Prec, na.rm = FALSE))), by = c("station", "s_year"), suffix = c("", ""))

total_rainfall_with_mean_y <- combined_rain_summaries_2 %>% 
  group_by(station) %>% 
  dplyr::mutate(.mean_y = mean(total_rain, na.rm = TRUE))

for (s in unique(total_rainfall_with_mean_y$station)) {
  g <- total_rainfall_with_mean_y %>% filter(station == s) %>%
    ggplot(mapping = aes(x = s_year, y = as.numeric(total_rain))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y, label = paste("Mean:", round(.mean_y))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "Seasonal total rainfall (mm)") + 
    labs(caption = "Seasonal rainfall: Total rainfall between the start of rains and the end of the season.") +
    scale_x_continuous(breaks = seq(1900, 2022, 4)) + 
    scale_y_continuous(breaks = seq(0, 2500, 100))
  ggsave(here("output", paste0("f.figure_5_total_seasonal_rain_", s, ".png")),
         g, width = 12, height = 8)
}

# Figure 6: Largest 5-day rainfall per year 1 Jan - 31 March

max_5sum_rain <- combined_rain_summaries %>% 
  left_join((mozambique_s_daily %>%
               filter(s_doy >= 154 & s_doy <= 244) %>% 
               group_by(station) %>%
               mutate(sum_5day = RcppRoll::roll_sumr(x = Prec, n = 5, fill = NA, na.rm = FALSE)) %>%
               group_by(s_year, year, .add = TRUE) %>% 
               summarise(max_sum_5day = max(sum_5day, na.rm = FALSE))), 
            by = c("station", "s_year"), suffix = c("", ""))

max_5d_total_rainfall_with_mean_y <- max_5sum_rain %>% 
  group_by(station) %>% dplyr::mutate(.mean_y = mean(x = max_sum_5day, na.rm = TRUE))

for (s in unique(max_5d_total_rainfall_with_mean_y$station)) {
  g <- max_5d_total_rainfall_with_mean_y %>% filter(station == s) %>%
    ggplot(mapping = aes(x = year, y = as.numeric(max_sum_5day))) + 
    geom_line(colour = "blue", linewidth = 0.8) + 
    geom_point(size = 3, colour = "red") + 
    geom_hline(mapping = aes(yintercept = .mean_y), linewidth = 1.5) + 
    geom_label(mapping = aes(x = -Inf, y = .mean_y, label = paste("Mean:", round(.mean_y))), hjust = 0, vjust = -0.3) + 
    theme_annual + 
    xlab(label = "") + 
    ylab(label = "Maximum 5-day total") + 
    labs(caption = "Largest 5-day rainfall total between 1 January and 31 March.") +
    scale_x_continuous(breaks = seq(1900, 2022, 4)) + 
    scale_y_continuous(breaks = seq(0, 600, 50))
  ggsave(here("output", paste0("g.figure_6_max_5d_total_rain_", s, ".png")), 
         g, width = 12, height = 8)
}

# Figure 7: Chances of receiving 450mm and 600mm of rainfall within 100 days for different planting dates

prec_100_day <- mozambique_s_daily %>%
  group_by(station) %>%
  mutate(sum_100day = RcppRoll::roll_suml(x = Prec, n = 100, fill = NA, na.rm = FALSE)) %>%
  group_by(station, s_doy) %>%
  summarise(`450mm` = sum(sum_100day >= 450, na.rm = TRUE)/n(),
            `600mm` = sum(sum_100day >= 600, na.rm = TRUE)/n())

prec_100_day_stack <- prec_100_day %>%
  pivot_longer(cols = c(`450mm`, `600mm`), names_to = "variable", values_to = "values", 
               names_ptypes = factor(levels = c("450mm", "600mm")))

for (s in unique(prec_100_day_stack$station)) {
  g <- prec_100_day_stack %>% filter(station == s & s_doy >= 32 & s_doy <= 275) %>%
    ggplot(mapping = aes(x = as.Date(s_doy, origin = "2015-07-31"), y = values, colour = variable)) + 
    geom_smooth(span = 0.3, se = FALSE, method = "loess") + 
    xlab(label = "") + 
    ylab(label = "") + 
    scale_x_date(date_labels = "%d %b", date_breaks = "1 months") + 
    scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
    labs(colour = "Total rainfall in 100 days")
  ggsave(here("output", paste0("h.figure_7_100d_total_rain_", s, ".png")), 
         g, width = 12, height = 8)
}

# Table 4: Maximum temperatures exceeding 40C, 42C, 44C (at any point) in November to January

max_temp_probs <- mozambique_s_daily %>%
  filter(s_doy >= 93 & s_doy <= 184) %>% 
  group_by(station, s_year) %>%
  summarise(max_tmax = max(Tmax, na.rm = TRUE)) %>% 
  mutate(max_temp_40 = max_tmax > 40,
         max_temp_42 = max_tmax > 42,
         max_temp_44 = max_tmax > 44) %>% 
  group_by(station) %>%
  summarise(prob_max_temp_40 = sum(max_temp_40, na.rm = TRUE)/n(),
            prob_max_temp_42 = sum(max_temp_42, na.rm = TRUE)/n(),
            prob_max_temp_44 = sum(max_temp_44, na.rm = TRUE)/n())

# Table 4: Season start date probabilities and seasonal rainfall

summary_probs <- combined_rain_summaries_2 %>%
  mutate(start_dec_1 = start_rain_dry >= 123,
         start_dec_15 = start_rain_dry >= 137,
         total_rain_700 = total_rain > 700) %>% 
  group_by(station) %>%
  summarise(prob_start_dec_1 = sum(start_dec_1, na.rm = TRUE)/na_non_na(start_dec_1),
            prob_start_dec_15 = sum(start_dec_15, na.rm = TRUE)/na_non_na(start_dec_15),
            prob_total_rain_700 = sum(total_rain_700, na.rm = TRUE)/na_non_na(total_rain_700))

# Table 4: Daily rainfall exceeding 150mm

max_prec_probs <- mozambique_s_daily %>%
  group_by(station, s_year) %>%
  summarise(max_rain = max(Prec, na.rm = TRUE)) %>% 
  mutate(max_rain_150 = max_rain > 150) %>% 
  group_by(station) %>%
  summarise(prob_max_rain_150 = sum(max_rain_150, na.rm = TRUE)/na_non_na(max_rain_150))

table_4 <- max_temp_probs %>% 
  full_join(summary_probs, by = "station") %>%
  full_join(max_prec_probs, by = "station")

write.csv(table_4, here("output", "i.table_4.csv"), 
          row.names = FALSE)
