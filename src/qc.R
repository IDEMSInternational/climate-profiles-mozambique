library(dplyr)
library(tidyr)
library(here)
library(ggplot2)

source(here("src", "helper_funs.R"))

MZ3 <- readRDS(here("data/MOZ3.RDS"))

mozambique_s_daily <- MZ3$get_data_frame(data_name = "moz3")

mozambique_s_daily$month_abbr <- factor(mozambique_s_daily$month_abbr, 
                                        levels = c(month.abb[8:12], month.abb[1:7]))

# Rainfall ----------------------------------------------------------------

neg_rain <- mozambique_s_daily %>% filter(Prec < 0)
if(nrow(neg_rain) > 0) View(neg_rain)
# Replace negative with NA
if(nrow(neg_rain) > 0) {
  mozambique_s_daily <- mozambique_s_daily %>% 
    mutate(Prec = ifelse(Prec < 0, NA, rain))
}

by_month <- mozambique_s_daily %>%
  group_by(station, s_year, month_abbr) %>%
  summarise(n_rain = sum(Prec > 0.85),
            t_rain = sum(Prec))

# Rain amounts visual check
ggplot(mozambique_s_daily %>% filter(Prec > 0), aes(x = month_abbr, y = Prec)) +
  geom_boxplot(varwidth = TRUE) +
  facet_wrap(~station)

# Number rain days visual check
ggplot(by_month, aes(x = month_abbr, y = n_rain)) +
  geom_boxplot() +
  facet_wrap(~station)

# Inventory plot
ggplot(mozambique_s_daily, aes(x = date, y = station, fill = !is.na(Prec))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(mozambique_s_daily$station)) + 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

# Not needed as done in R-Instat
# # Fill Date gaps
# dates_list <- list()
# for(s in unique(mozambique_s_daily$station)) {
#   
#   dates <- seq(min((mozambique_s_daily %>% filter(station == s))$date), 
#                max((mozambique_s_daily %>% filter(station == s))$date),
#                by = 1)
#   dd <- data.frame(station = s, date = dates)
#   dates_list[[length(dates_list) + 1]] <- dd
# }
# date_df <- bind_rows(dates_list)
# 
# nr <- nrow(date_df)
# if(nrow(mozambique_s_daily) < nr) {
#   mozambique_s_daily <- full_join(date_df, mozambique_s_daily, by = c("station", "date"))
#   print(paste("Filled", nrow(mozambique_s_daily) - nr, "rows"))
#   mozambique_s_daily <- mozambique_s_daily %>%
#     mutate(year = year(date), month = factor(month(date)))
#   # Inventory plot again
#   ggplot(mozambique_s_daily, aes(x = date, y = station, fill = !is.na(rain))) +
#     geom_tile() +
#     geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(mozambique_s_daily$station)) + 1))
# }

# Large or negative values check
large_check <- mozambique_s_daily %>% 
  filter(Prec > 250)
if(nrow(large_check) > 0) View(large_check)
# Some large values but no rainfall over 300mm.
# Do not remove any values.

# Consecutive non-zero values check
consec_check <- mozambique_s_daily %>% 
  group_by(station) %>%
  mutate(same = rep(rle(as.numeric(Prec))$lengths, rle(as.numeric(Prec))$lengths)) %>%
  filter(Prec > 1.5 & same >= 2)
if(nrow(consec_check) > 0) View(consec_check)
# Some repeated values but no more than 2 consecutive and generally small values
# Do not remove any values.

# Consecutive rain days check
raindays_check <- mozambique_s_daily %>%
  group_by(station) %>%
  mutate(raindays = cumsum(Prec > 0) - cummax(((Prec > 0) == 0) * cumsum(Prec > 0))) %>%
  filter(raindays > 10)
if(nrow(raindays_check) > 0) View(raindays_check)
# Only oddity is August 1978 - Beira - 11 consecutive wet days but all small values < 1mm
# Do not remove any values.

# Dry months check - strict
drymonths_check <- mozambique_s_daily %>%
  filter(month_abbr %in% c("Nov", "Dec", "Jan", "Feb", "Mar")) %>%
  group_by(station, s_year, month_abbr) %>%
  summarise(t_rain = sum(Prec)) %>%
  filter(t_rain == 0)
if(nrow(drymonths_check) > 0) View(drymonths_check)

# Tete October & December 2010 all 0 with missing in surrounding months
# and no temperature data
# Make these months missing
mozambique_s_daily$Prec[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$year == "2010" & 
                          mozambique_s_daily$month_abbr %in% c("Oct", "Dec")] <- NA

# Other periods of dry November months do not seem impossible. Do not remove values.


# Temperature -------------------------------------------------------------

# Inventory plot
mozambique_stack <- pivot_longer(mozambique_s_daily, cols = c("Tmin", "Tmax"), names_to = "element", values_to = "value")
ggplot(mozambique_stack, aes(x = date, y = element, fill = !is.na(value))) +
  geom_tile() +
  geom_hline(yintercept = seq(0.5, by = 1, length.out = length(unique(mozambique_stack$element)) + 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  facet_grid(vars(station)) +
  theme(strip.text = element_text(size = 10))

by_month_temp <- mozambique_s_daily %>%
  group_by(station, s_year, month_abbr) %>%
  summarise(mean_tmax = mean(Tmax, na.rm = TRUE),
            mean_tmin = mean(Tmin, na.rm = TRUE))

# Temp amounts visual check
ggplot(mozambique_s_daily, aes(x = month_abbr, y = Tmax)) +
  geom_boxplot(aes(colour = "tmax"), alpha = 0.5) +
  geom_boxplot(aes(y = Tmin, colour = "tmin"), alpha = 0.5) +
  facet_wrap(vars(station))

# Very high values
vhigh <- mozambique_s_daily %>% filter(Tmax > 50 | Tmin > 50)
if(nrow(vhigh) > 0) View(vhigh)

# Very low tmax values
vlow <- mozambique_s_daily %>% filter(Tmax < 10)
if(nrow(vlow) > 0) View(vlow)
# Chomoio 2016-05-09, tmax value too low. 
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Chimoio" & 
                          mozambique_s_daily$date == as.Date("2016/05/09")] <- NA

# Very low tmin values
vlow <- mozambique_s_daily %>% filter(Tmin < 10)
if(nrow(vlow) > 0) View(vlow)
# Occasional values of 8-9 in Jun-Aug. Looks reasonable.
# Values 5-9 are common in Chimoio.
vlow <- mozambique_s_daily %>% filter(Tmin < 5)
if(nrow(vlow) > 0) View(vlow)
# Some low value in Chimoio but look possible and surrounded by similar values.

# Tmin > Tmax values
maxmin <- mozambique_s_daily %>% filter(Tmax - Tmin <= 0)
if(nrow(maxmin) > 0) View(maxmin)

# Beira 2019-11-14, unclear which value is incorrect, make both missing
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Beira" & 
                          mozambique_s_daily$date == as.Date("2019/11/14")] <- NA
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Beira" & 
                          mozambique_s_daily$date == as.Date("2019/11/14")] <- NA

# Beira 2021-06-21, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Beira" & 
                          mozambique_s_daily$date == as.Date("2021/06/21")] <- NA

# Beira 2021-09-12, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Beira" & 
                          mozambique_s_daily$date == as.Date("2021/09/12")] <- NA

# Tete 2018-07-07, Tmin and Tmax look switched. Correct values: 18.8 & 24.7
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2018/07/07")] <- 18.8
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2018/07/07")] <- 24.7

# Tete 2019-12-22, unclear which value is incorrect, make both missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2019/12/22")] <- NA
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2019/12/22")] <- NA

# Tete 2020-01-25, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2020/01/25")] <- NA

# Tete 2021-01-30, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2021/01/30")] <- NA

# Tete 2021-02-07, Tmax looks too low, make missing
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2021/02/07")] <- NA

# Tete 2021-02-16, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2021/02/16")] <- NA

# Tete 2021-10-17, Tmax looks too low, make missing
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2021/10/17")] <- NA

# Tete 2021-11-14, Tmax looks too low, make missing
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Tet" & 
                          mozambique_s_daily$date == as.Date("2021/11/14")] <- NA

# Chimoio 2020-01-25, Tmax looks too low, make missing
mozambique_s_daily$Tmax[mozambique_s_daily$station == "Chimoio" & 
                          mozambique_s_daily$date == as.Date("2020/01/25")] <- NA

# Chimoio 2021-09-05, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Chimoio" & 
                          mozambique_s_daily$date == as.Date("2021/09/05")] <- NA

# Chimoio 2022-04-22, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Chimoio" & 
                          mozambique_s_daily$date == as.Date("2022/04/22")] <- NA

# Recheck Tmin > Tmax values after corrections
maxmin <- mozambique_s_daily %>% filter(Tmax - Tmin <= 0)
if(nrow(maxmin) > 0) View(maxmin)

# Tmin close to Tmax values
maxmin <- mozambique_s_daily %>% filter(Tmax - Tmin <= 2)
if(nrow(maxmin) > 0) View(maxmin)
# Appears to happen occasionally. Cannot be sure of incorrect values. Do not remove values.

mozambique_daily_lags <- mozambique_s_daily %>%
  mutate(tmax_diff = Tmax - dplyr::lag(Tmax),
         tmax_diff2 = dplyr::lead(Tmax) - Tmax,
         tmin_diff = Tmin - dplyr::lag(Tmin),
         tmin_diff2 = dplyr::lead(Tmin) - Tmin)

# Large difference over 2 days
bigdiff <- mozambique_daily_lags %>% 
  filter(abs(tmin_diff) > 10 | abs(tmin_diff2) > 10) %>%
  dplyr::select(station, date, Tmax, tmax_diff, Tmin, tmin_diff)
if(nrow(bigdiff) > 0) View(bigdiff)
# Generally no large differences over 11.5. Do not remove values.

# Chimoio 2018-01-02, Tmin looks too high, make missing
mozambique_s_daily$Tmin[mozambique_s_daily$station == "Chimoio" & 
                          mozambique_s_daily$date == as.Date("2018/01/02")] <- NA

# Consecutive values check
consec_check <- mozambique_s_daily %>% 
  group_by(station) %>%
  mutate(same_tmax = rep(rle(as.numeric(Tmax))$lengths, rle(as.numeric(Tmax))$lengths),
         same_tmin = rep(rle(as.numeric(Tmin))$lengths, rle(as.numeric(Tmin))$lengths)) %>%
  filter(same_tmin >= 3) %>%
  dplyr::select(station, date, Tmin, same_tmin, Tmax, same_tmax)
if(nrow(consec_check) > 0) View(consec_check)
# No more than 2 consecutive tmax values
# Some 3 consecutive tmin values. Two 4 consec and one 5 consec but these are whole numbers so likely rounding problem.
# Do not correct values.

# homogeneity and trend checks
ggplot(mozambique_s_daily, aes(x = factor(year), y = Tmin)) +
  geom_boxplot(varwidth = TRUE) + 
  facet_grid(vars(station))

ggplot(mozambique_s_daily, aes(x = factor(year), y = Tmax)) +
  geom_boxplot(varwidth = TRUE) + 
  facet_grid(vars(station))
# No unnatural changes observed apart from Chimoio Tmin and Tmax sudden drop from 1978 to 1979.
# Filter data to start in 1979.
mozambique_s_daily_filter <- mozambique_s_daily %>% 
  filter(!(station == "Chimoio" & year < 1979))

# recheck homogeneity and trend checks
ggplot(mozambique_s_daily_filter, aes(x = factor(year), y = Tmin)) +
  geom_boxplot(varwidth = TRUE) + 
  facet_grid(vars(station))

ggplot(mozambique_s_daily_filter, aes(x = factor(year), y = Tmax)) +
  geom_boxplot(varwidth = TRUE) + 
  facet_grid(vars(station))

mozambique_s_daily_filter$station <- forcats::fct_recode(mozambique_s_daily_filter$station, Tete = "Tet")

saveRDS(mozambique_s_daily_filter, here("data", "moz3_qc.RDS"))
