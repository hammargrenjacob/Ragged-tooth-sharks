rm(list = ls()) #clear environment

##-------Packages--------
#Check if the following packages are installed, and install them if they aren't
list.of.packages <- c("tidyr", "ggplot2", "dplyr","kableExtra", "lubridate", "tibble", "sf", "readxl", "scales", "rmarkdown", "knitr", "bookdown", "lme4", "lmerTest", "sjPlot", "broom", "fishualize", "viridis", "cowplot", "plotrix", "ggh4x")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

library(tidyr)
library(ggplot2)
library(dplyr)
library(kableExtra)
library(lubridate)
library(tibble)
library(sf)
library(readxl)
library(scales)
library(rmarkdown)
library(knitr)
library(bookdown) #rmarkdown for books
library(lme4) #linear models
library(lmerTest) #testing of linear models
library(sjPlot) # tab_model creates a nice html table summary of model
library(broom)
library(fishualize)
library(viridis)
library(cowplot)
library(plotrix)
library(ggh4x)
# mgcv: used for GAM models (seems to be non-parametric linear regeression)
#ggpubr: publication ready plot designs
#bookdown: use with r markdown
#todor: manage comments (things to include later) in r markdown
#if (!require("remotes")) 
#  install.packages("remotes", repos = "https://cran.rstudio.org")
#remotes::install_github("rstudio/bookdown")
#remotes::install_github("ismayc/thesisdown")


##---------Data Read and Filtered---------
raggie_data<- read.csv("raggie_data.csv", header = TRUE)
raggie_data_2020_2021<- read.csv("raggie_data_2020-2021.csv", header = TRUE) 
raggie_data_2022_2023<- read.csv("raggie_data_2022-2023.csv", header = TRUE) 
season_uvc_2020_2021<- read.csv("season_uvc_2020_2021.csv", header = TRUE)
season_uvc_2022_2023<- read.csv("season_uvc_2022_2023.csv", header = TRUE)
season_maxn_2020_2021<- read.csv("season_maxn_2020_2021.csv", header = TRUE)
season_maxn_2022_2023<- read.csv("season_maxn_2022_2023.csv", header = TRUE)
raggie_interactions <-read.csv("raggie_interactions.csv", header = TRUE)

model_comparison_cleaned <-read.csv("model_comparison_cleaned.csv", header = TRUE)


# creating date and days_since_start:
raggie_data$date <- as.Date(paste(raggie_data$year, 
                                  raggie_data$month, 
                                  raggie_data$day, 
                                  sep = "-"))
raggie_data$days_since_start <- time_length(interval(ymd("2020-12-01"),
                                                     raggie_data$date),
                                            "day")
days_since_start <- numeric(nrow(raggie_data))
for (i in 1:nrow(raggie_data)) {
  if (raggie_data$date[i] < ymd("2022-12-01")) {
    days_since_start[i] <- time_length(interval(ymd("2020-12-01"), raggie_data$date[i]), "day")
  } else {
    days_since_start[i] <- time_length(interval(ymd("2022-12-01"), raggie_data$date[i]), "day")
  }
}
raggie_data$days_since_start <- days_since_start


raggie_data_2020_2021$date <- as.Date(paste(raggie_data_2020_2021$year, 
                                            raggie_data_2020_2021$month, 
                                            raggie_data_2020_2021$day, 
                                            sep = "-"))
raggie_data_2020_2021$days_since_start <- time_length(interval(ymd("2020-12-01"),raggie_data_2020_2021$date),
                                                      "day")
raggie_data_2022_2023$date <- as.Date(paste(raggie_data_2022_2023$year, 
                                            raggie_data_2022_2023$month, 
                                            raggie_data_2022_2023$day, 
                                            sep = "-"))
raggie_data_2022_2023$days_since_start <- time_length(interval(ymd("2022-12-01"),
                                                               raggie_data_2022_2023$date),
                                                      "day")

season_uvc_2020_2021$date <- as.Date(paste(season_uvc_2020_2021$year, 
                                           season_uvc_2020_2021$month, 
                                           season_uvc_2020_2021$day, 
                                           sep = "-"))
season_maxn_2020_2021$date <- as.Date(paste(season_maxn_2020_2021$year, 
                                            season_maxn_2020_2021$month,
                                            season_maxn_2020_2021$day,
                                            sep = "-"))
season_uvc_2022_2023$date <- as.Date(paste(season_uvc_2022_2023$year, 
                                           season_uvc_2022_2023$month, 
                                           season_uvc_2022_2023$day, 
                                           sep = "-"))
season_maxn_2022_2023$date <- as.Date(paste(season_maxn_2022_2023$year, 
                                            season_maxn_2022_2023$month, 
                                            season_maxn_2022_2023$day, 
                                            sep = "-"))
#Filtered data: 
raggie_data_RR <- raggie_data %>% 
  filter(reef == "RR")
raggie_data_QM <- raggie_data %>% 
  filter(reef == "QM")
raggie_data_nodivers<- raggie_data %>% 
  filter(divers == "N")
raggie_data_divers<- raggie_data %>% 
  filter(divers == "Y")
raggie_data_QM_nodivers <- raggie_data_QM %>% 
  filter(divers == "N")

raggie_data_with_envvar <- raggie_data %>% 
  filter(!is.null(days_since_start) & !is.na(days_since_start)) %>%
  filter(!is.null(current_speed) & !is.na(current_speed)) %>%
  filter(!is.null(swell) & !is.na(swell)) %>%
  filter(!is.null(vis) & !is.na(vis)) %>%
  filter(!is.null(vis) & !is.na(sst))


#----------R0. Testing the random variables of a mixed model-------
# A Linear model that could be used to investigate effect of each "random" variable
lm_environ_nodivers <- lm(rest_seconds ~ 
                            reef + 
                            days_since_start + 
                            current_speed + 
                            swell + 
                            vis +
                            sst,  
                          data = raggie_data_nodivers)
summary(lm_environ_nodivers)
tab_model(lm_environ_nodivers, 
          show.est = TRUE,
          show.se = TRUE,
          show.df = TRUE,
          show.p = TRUE,
          show.ci = FALSE,
          show.stat = TRUE,
          emph.p = TRUE,
          col.order = c("est", "se", "df", "stat", "p"),
          string.pred = "Fixed effects",
          string.resp = "Response",
          string.est = "Est.",
          string.df = "df",
          string.p = "p-value",
          string.se = "SE",
          string.stat = "t-value",
          dv.labels = "Resting Time",
          p.style = "numeric"
)

#Every rand var was tested against a linear model to see if it had a signif impact on model strength, if it did it was included:
#(adjusted to only look at data that had a value for all environmental variables (n = 500)
raggie_data_with_envvar <- raggie_data %>% 
  filter(!is.null(days_since_start) & !is.na(days_since_start)) %>%
  filter(!is.null(current_speed) & !is.na(current_speed)) %>%
  filter(!is.null(swell) & !is.na(swell)) %>%
  filter(!is.null(vis) & !is.na(vis)) %>%
  filter(!is.null(vis) & !is.na(sst))


lm_covtest_base <- lm(rest_seconds ~ reef + divers, data = raggie_data_with_envvar)
summary(lm_covtest_base)


#days_since_start
lmer_w_days_since_start <- lmer(rest_seconds ~ reef + divers + 
                                  (1|days_since_start), 
                                data = raggie_data_with_envvar)
tidy_comparison_days <- tidy(anova(lmer_w_days_since_start, lm_covtest_base))


#current_speed 
lmer_w_current_speed <- lmer(rest_seconds ~ reef + divers +
                               (1|current_speed), 
                             data = raggie_data_with_envvar)
tidy_comparison_current_speed <- tidy(anova(lmer_w_current_speed, lm_covtest_base))


#swell
lmer_w_swell <- lmer(rest_seconds ~ reef + divers + 
                       (1|swell), 
                     data = raggie_data_with_envvar)
tidy_comparison_swell <- tidy(anova(lmer_w_swell, lm_covtest_base))


#vis
lmer_w_vis <- lmer(rest_seconds ~ reef + divers + 
                     (1|vis), 
                   data = raggie_data_with_envvar)
tidy_comparison_vis <- tidy(anova(lmer_w_vis, lm_covtest_base))


#sst
lmer_w_sst <- lmer(rest_seconds ~ reef + divers + 
                     (1|sst), 
                   data = raggie_data_with_envvar)
tidy_comparison_sst <-(anova(lmer_w_sst, lm_covtest_base))

model_comparison <- bind_rows(
  mutate(tidy_comparison_days, Model = "Time of Season"),
  mutate(tidy_comparison_current_speed, Model = "Current Speed"),
  mutate(tidy_comparison_swell, Model = "Swell"),
  mutate(tidy_comparison_vis, Model = "Visibility"),
  mutate(tidy_comparison_sst, Model = "Surface Temperature")
)

write.csv(model_comparison, "model_comparison.csv", row.names=TRUE)
model_comparison_cleaned <- read.csv("model_comparison_cleaned.csv")
# Combining results into pretty table
model_comparison_table <- kable(model_comparison_cleaned, "html") %>%
  kable_styling("striped", full_width = TRUE)

print(model_comparison_cleaned)

#Table cleaned in excel:
read.csv(model_comparison_cleaned)



#Mixed with all parameters is better than lm (p=2e-6)
lm_test<- lm(rest_seconds ~ divers, data = raggie_data_QM)
mixed_model_test <- lmer(rest_seconds ~ divers + 
                           (1|days_since_start) + 
                           (1|current_speed) + 
                           (1|vis) + 
                           (1|swell) + 
                           (1|sst), 
                         data = raggie_data_QM)
summary(mixed_model_test)
isSingular(mixed_model_test)

anova(mixed_model_test, lm_test)
#----------R1. Seasonal residence time at each reef-----------

#2020-2021
#plot_season_2020_2021 <- 
ggplot() +
  geom_bar(data = season_uvc_2020_2021, aes(x = date, y = sharks), color = "grey", fill = "darkgrey", show.legend = FALSE, stat = "identity", alpha = 0.5) +
  geom_bar(data = season_maxn_2020_2021, aes(x = date, y = sharks, fill = reef), show.legend = FALSE, stat = "identity", alpha = 0.8) +
  scale_y_continuous( breaks = c(0, 20, 40, 60),
                      labels = c(0, 20, 40, 60)) +
  theme(strip.background = element_rect(fill= "#5fa69d", size = 5, linetype = 1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.15, linetype = 'solid',
                                        colour = "lightgrey"),
        axis.text.x = element_text(size = 12),  # Change X-axis numbering font size
        axis.text.y = element_text(size = 12),  # Change Y-axis numbering font size
        axis.title.x = element_text(size = 14),  # Change X-axis label font size
        axis.title.y = element_text(size = 14)   # Change Y-axis label font size
  ) +
  labs(x = "Month", 
       y = "C. taurus abundance") +
  facet_grid(reef ~ .,) +
  #facet_grid2(reef ~ , strip = strip_themed(background_y = elem_list_rect(fill = c("#5fa69d", #31a8c4"))) +
  scale_fill_manual(values = c("RR" = "#5fa69d", "QM" = "#5d99b8")) 

#2022-2023
season_uvc_2022_2023_filtered<- season_uvc_2022_2023 %>% 
  filter(sharks <= 60)

#plot_season_2022_2023 <- 
ggplot() +
  geom_bar(data = season_uvc_2022_2023_filtered, aes(x = date, y = sharks), color = "grey", fill = "darkgrey", show.legend = FALSE, stat = "identity", alpha = 0.5) +
  geom_bar(data = season_maxn_2022_2023, aes(x = date, y = sharks, fill = reef), show.legend = FALSE, stat = "identity", alpha = 0.8) +
  scale_y_continuous( breaks = c(0, 20, 40, 60),
                      labels = c(0, 20, 40, 60)) +
  theme(strip.background = element_rect(fill= "#5fa69d", size = 5, linetype = 1),
        panel.background = element_rect(fill = "white",
                                        colour = "white",
                                        linewidth = 0.5, linetype = "solid"),
        panel.grid.major = element_line(linewidth = 0.25, linetype = 'solid',
                                        colour = "lightgrey"), 
        panel.grid.minor = element_line(linewidth = 0.15, linetype = 'solid',
                                        colour = "lightgrey"),
        axis.text.x = element_text(size = 12),  # Change X-axis numbering font size
        axis.text.y = element_text(size = 12),  # Change Y-axis numbering font size
        axis.title.x = element_text(size = 14),  # Change X-axis label font size
        axis.title.y = element_text(size = 14) 
  ) +
  labs(x = "Month", 
       y = "C. taurus abundance") +
  facet_grid(reef ~ .,) +
  #facet_grid2(reef ~ , strip = strip_themed(background_y = elem_list_rect(fill = c("#5fa69d", #31a8c4"))) +
  scale_fill_manual(values = c("RR" = "#5fa69d", "QM" = "#5d99b8")) 

#plot_grid(plot_season_2020_2021, plot_season_2022_2023, ncol = 2)




