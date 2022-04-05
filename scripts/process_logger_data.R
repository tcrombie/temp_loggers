library(tidyverse)

# Set working directory
setwd(glue::glue("{dirname(rstudioapi::getActiveDocumentContext()$path)}/.."))

# read in button data for each logger
b1 <- data.table::fread("~/repos/temp_loggers/data/20220404_hawaii_temp_button1.csv") %>%
  tidyr::separate(`Date/Time`, into = c("date", "time", "unit"), sep = " ") %>%
  dplyr::mutate(time2 = lubridate::hms(time),
                time3 = case_when(unit == "PM" ~ time2+lubridate::hours(12),
                                  unit != "PM" ~ time2),
                date2 = lubridate::mdy(date),
                datetime = lubridate::ymd_hms(date2+time3),
                timezone = "CST",
                logger = "Oahu",
                high_alarm = ifelse(Value >= 27, T, F)) %>%
  dplyr::filter(datetime > lubridate::ymd_hms("2022-4-1 21:00:00" & datetime < lubridate::ymd_hms("2022-4-4 12:00:00"))) %>%
  dplyr::select(datetime, timezone, temp_c = Value, logger, high_alarm) %>%
  dplyr::arrange(datetime)

b2 <- data.table::fread("~/repos/temp_loggers/data/20220404_hawaii_temp_button2.csv") %>%
  tidyr::separate(`Date/Time`, into = c("date", "time", "unit"), sep = " ") %>%
  dplyr::mutate(time2 = lubridate::hms(time),
                time3 = case_when(unit == "PM" ~ time2+lubridate::hours(12),
                                  unit != "PM" ~ time2),
                date2 = lubridate::mdy(date),
                datetime = lubridate::ymd_hms(date2+time3),
                timezone = "CST",
                logger = "Big Island",
                high_alarm = ifelse(Value >= 27, T, F)) %>%
  dplyr::filter(datetime > lubridate::ymd_hms("2022-3-31 17:00:00" & datetime < lubridate::ymd_hms("2022-4-4 12:00:00"))) %>%
  dplyr::select(datetime, timezone, temp_c = Value, logger, high_alarm) %>%
  dplyr::arrange(datetime)

b3 <- data.table::fread("~/repos/temp_loggers/data/20220404_hawaii_temp_button3.csv") %>%
  tidyr::separate(`Date/Time`, into = c("date", "time", "unit"), sep = " ") %>%
  dplyr::mutate(time2 = lubridate::hms(time),
                time3 = case_when(unit == "PM" ~ time2+lubridate::hours(12),
                                  unit != "PM" ~ time2),
                date2 = lubridate::mdy(date),
                datetime = lubridate::ymd_hms(date2+time3),
                timezone = "CST",
                logger = "Kauai",
                high_alarm = ifelse(Value >= 27, T, F)) %>%
  dplyr::filter(datetime > lubridate::ymd_hms("2022-3-28 09:00:00") & datetime < lubridate::ymd_hms("2022-4-4 12:00:00")) %>%
  dplyr::select(datetime, timezone, temp_c = Value, logger, high_alarm) %>%
  dplyr::arrange(datetime)

# find high temp alarm run duration in minutes
alarm_runs1 <- tibble::tibble(run_length = rle(b1$high_alarm)[[1]], value = rle(b1$high_alarm)[[2]], duration = lubridate::minutes((run_length*2))) # yikes!
alarm_runs2 <- tibble::tibble(run_length = rle(b2$high_alarm)[[1]], value = rle(b2$high_alarm)[[2]], duration = lubridate::minutes((run_length*2)))
alarm_runs3 <- tibble::tibble(run_length = rle(b3$high_alarm)[[1]], value = rle(b3$high_alarm)[[2]], duration = lubridate::minutes((run_length*2)))

# bind the data for buttons
bound_df <- dplyr::bind_rows(b1, b2, b3)

# plot temps with 27C indicator
p <- ggplot2::ggplot(data = bound_df) +
  ggplot2::geom_line(aes(x = datetime, y = temp_c)) +
  ggplot2::theme_bw() +
  ggplot2::ylim(0,max(bound_df$temp_c + 5)) +
  ggplot2::labs(x = "Time", y = "temperature °C") +
  ggplot2::facet_grid(~logger) +
  ggplot2::geom_hline(yintercept = 27, linetype = 2, color = "red")
p

cowplot::ggsave2(p, filename = "plots/shipment_temps.png", width = 7.5, height = 5)

