#===== =============== =============== ==========
# Curves_test.r
# Used to test functions in the Curves.r file
#===== =============== =============== ==========
#library(tarr)
library(dplyr)
library(lubridate)
library(grates)
library(ggplot2)

rm(list=ls())
#paths

conds <- c("Typhus fever-fleaborne, murine","Acute Flaccid Myelitis (AFM)", "Salmonellosis",
           "Shiga toxin-producing Escherichia coli (STEC)","Shigellosis", "Campylobacteriosis")
onset_range <- seq.Date(mdy("03-10-2022") - 95, mdy("03-10-2022"), by = 1)

# create a data frame to use for testing
dis <- data.frame(Condition = sample(conds,size = 2000, replace = TRUE, prob = c(.1, .02, .3, .1, .38, .1)),
                  Onset     = sample(onset_range, size = 2000, replace = TRUE),
                  Sex       = sample(c("F", "M"), size =2000, replace = TRUE),
                  Phylum     = sample(c("Chordates", "Avian", "Arthropods", "Molluscs", "Nematodes", "Annelids" ), 
                                      size = 2000, replace = TRUE, prob = c(.1, .15, .5, .05, .1, .1))
)

# Show an outbreak 30 days ago
dis$Onset[dis$Condition == "Shigellosis"] <- 
  rnorm(n = length(dis$Onset[dis$Condition == "Shigellosis"]),
        mean = as.numeric(mdy("03-10-2022") - 35),
        sd = 12) %>% 
  as.Date(origin = "1970-01-01")

dis <- dis |> mutate(Dx = Onset + rnorm(n = 2000, mean = 4, sd = 2))

dis <- dis %>%
  filter(Condition %in% conds) %>%
  droplevels()


tmp <- incidence2::incidence(x = dis,
                             date_names_to = "Dates",
                             #date_index = c("Onset", "Dx"), 
                             date_index = c("Onset"), 
                             count_values_to = "Cases",
                             groups = c("Condition"), 
                             interval = "epiweek")

attributes(tmp)
tmp$Dates |> class()

palette.pals()
palette.colors(palette = "Accent")
palette.colors()
user_pal <- partial(palette.colors, palette = "Set 1")
user_pal(6)

#incidence2:::plot.incidence2()
#undebug(incidence2:::plot.incidence2)
tmp_plot <- plot(tmp, colour_palette = user_pal,
                 angle = 90, 
                 width = 0.95,
                 title = "Epidemic Curves", ) + 
  scale_y_continuous(expand = expansion(mult = c(0,0.1)))+
  #theme_few() +
  theme(legend.position = "none")
tmp_plot

#undebug(epi_curve.incidence2)
crv <- epi_curve(x = tmp, color_pal = user_pal,  
                 width = 0.95,
                 facet = FALSE,
                 angle = 90,
                 title = "Epidemic Curve",
                 sub_title = "Different Conditions",
                 caption = "Caption here",
                 nrow = 2
                 )

crv$layers[[1]]$computed_mapping
crv |> class()
summary(crv)

summary(tmp_plot)

fn <- crv["facet"]
tmp_plot["facet"] <- fn
tmp_plot$layers[[1]]$computed_mapping["fill"] <- rlang::new_quosure(~.data[["Condition"]])
tmp_plot

shig_incid <- incidence(x = dis %>% filter(Condition == "Shigellosis"),
                        count_values_to = "Cases",
                        date_names_to = "Dates",
                        date_index = c("Onset", "Dx"), 
                        groups = c("Sex"),
                        interval = "epiweek"
                        )

 View(incidence2::incidence)
#undebug(incidence2:::plot.incidence2)
shig_curve <- epi_curve(x = shig_incid, color_pal = user_pal, 
                         facet = FALSE, angle = 90
                         )

shig_curve
attributes(shig_incid)


test <- dis %>% 
  mutate(Week = aweek::date2week(x = Onset, week_start = "Sun", floor_day = TRUE)) %>% 
  group_by(Week) %>% 
  summarise(Cases = n()) %>% 
  mutate(Week = aweek::week2date(Week, week_start = "Sun"))

tmp <- ggplot(test, aes(x = Week, y = Cases))+
  geom_bar(stat = "identity", fill = "Navy")
tmp

interestPeriod <-   lubridate::interval(lubridate::floor_date(Sys.Date()-years(1),unit = "year"),
                                        lubridate::ceiling_date(Sys.Date()- years(1),unit = "year"))
comparePeriod <- lubridate::interval(lubridate::floor_date(Sys.Date()-years(10),unit = "year"),
                                     lubridate::ceiling_date(Sys.Date()- years(2),unit = "year")-1)

interestPeriod
comparePeriod
tmp <- Current_To_Previous_Compare(.df=dis %>% filter(Condition == "Salmonellosis"),
                                   .dateFld = Event.Date,
                                   .currPeriod = interestPeriod,
                                   .smooth = 3,
                                   .cond = "Salmonellosis")

# look at mean vs median
tmp <- purrr::map(c("mean","median"), .f = function(x)
  Current_To_Previous_Compare(.df = dis %>% filter(Condition == "Shiga toxin-producing Escherichia coli (STEC)"),
                              .currPeriod = interestPeriod,
                              .prevPeriod = comparePeriod,
                              .dateFld = Event.Date,
                              #.interval = "week",
                              .cond = "STEC",
                              .smooth = 4, .type = x) +
    scale_x_continuous(breaks = 1:52))


tmp


# pert <- dis %>%
#   filter(Condition=="Shigellosis")
# tmp <- incidence::incidence(pert$Event.Date,interval = "week") %>%
#     as.data.frame()
# outlier <- outlier_iqr_detect(.df = tmp,
#                               .dateFld = dates,
#                               .caseFld = counts,
#                               .method = date_span_classify("year"))
# head(outlier$outliers)
#
