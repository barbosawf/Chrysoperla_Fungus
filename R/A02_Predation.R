# Packages ----------------------------------------------------------------


library(tidyverse)
library(survival)
library(survminer)
library(emmeans)
library(readxl)
library(gamlss)
library(hnp)
library(DHARMa)
library(ggplotify)



# Useful Functions --------------------------------------------------------


create_dir_as_doc <-
  readRDS("A01_Useful_Functions/create_dir_as_doc.rds")


simulate_BI <-
  readRDS("A01_Useful_Functions/simulate_BI.rds")


d_fun <-
  readRDS("A01_Useful_Functions/d_fun.rds")


s_fun_BI <-
  readRDS("A01_Useful_Functions/s_fun_BI.rds")


f_fun_BI_externa <-
  readRDS("A01_Useful_Functions/f_fun_BI_externa.rds")


f_fun_BI_cubana <-
  readRDS("A01_Useful_Functions/f_fun_BI_cubana.rds")



# Creating directory as script document -----------------------------------


#create_dir_as_doc()



# Data --------------------------------------------------------------------


list.files("Data")


"Data/predacao.xlsx" |>
  excel_sheets()



"Data/predacao.xlsx" |>
  read_xlsx(sheet = "Chrysoperla_externa") |>
  mutate_at(vars(Specie), ~ str_c("*", str_replace(., pattern = "_", " "), "*")) |>
  mutate_at(vars(Specie:Treatment), as_factor) |>
  mutate(Time = Min + Sec / 60) ->
  df_c_externa


"Data/predacao.xlsx" |>
  read_xlsx(sheet = "Chrysoperla_cubana") |>
  mutate_at(vars(Specie), ~ str_c("*", str_replace(., pattern = "_", " "), "*")) |>
  mutate_at(vars(Specie:Treatment), as_factor) |>
  mutate(Time = Min + Sec / 60) ->
  df_c_cubana


df_c_externa |>
  group_by(Treatment) |>
  summarize(Count =  as.integer(sum(Predation)), N = n()) ->
  df_bin_c_externa


df_c_cubana |>
  group_by(Treatment) |>
  summarize(Count =  as.integer(sum(Predation)), N = n()) ->
  df_bin_c_cubana



# Kaplan-Meyer Fit --------------------------------------------------------


c_externa_fit <-
  survfit(Surv(Time, Predation) ~ Treatment, data = df_c_externa)


summary(c_externa_fit)



c_cubana_fit <-
  survfit(Surv(Time, Predation) ~ Treatment, data = df_c_cubana)



summary(c_cubana_fit)



# Simple Kaplan-Meyer Plot ------------------------------------------------


plot(c_externa_fit)


plot(c_cubana_fit)



# Pairwise contrasts ------------------------------------------------------


c_externa_diff <-
  survdiff(Surv(Time, Predation) ~ Treatment, data = df_c_externa)


c_externa_diff


1 - pchisq(c_externa_diff$chisq, df = 1)


c_cubana_diff <-
  survdiff(Surv(Time, Predation) ~ Treatment, data = df_c_cubana)


c_cubana_diff


1 - pchisq(c_cubana_diff$chisq, df = 1)



# Kaplan-Meyer Survival Plot ----------------------------------------------


# C. externa
ggsurvplot(
  # survfit object with calculated statistics.
  c_externa_fit,
  # data used to fit survival curves.
  data = df_c_externa,
  ylab = "Predation probability",
  # Invert the scale
  fun = "event",
  censor = F,
  size = 2,
  ylim = c(0, 1),
  legend.title = "",
  # risk.table.col = "strata",
  risk.table = F,
  # show risk table.
  cumevents = TRUE,
  linetype = "strata",
  # show confidence intervals for
  # point estimates of survival curves.
  conf.int = F,

  palette = "jco",
  xlim = c(0, 10),
  # present narrower X axis, but not affect
  # survival estimates.
  # customize X axis label.
  xlab = "Time (min)",

  break.time.by = 1,
  # break X axis in time intervals by 500.
  ggtheme = theme_bw(),
  # customize plot and risk table with a theme.
  risk.table.y.text.col = T,
  # colour risk table text annotations.
  risk.table.height = 0.3,
  # the height of the risk table
  risk.table.y.text = F,
  #  tables.theme = theme_cleantable(),  # Clean theme for tables
  tables.y.text = FALSE,
  # show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = F,
  # plot the number of censored subjects at time t
  #ncensor.plot.height = 0.25,
  conf.int.style = "ribbon",
  font.x = c("bold", 16),
  font.y = c("bold", 16),
  font.legend = 14,
  font.tickslab = c(12, 'plain', 'black'),
  #surv.median.line = "hv",  # add the median survival pointer.
  # change legend labels.
  legend.labs = c("Control", "Treated")

) -> p_c_externa


p_c_externa


p_c_externa$cumevents <- p_c_externa$cumevents +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(.15, 'cm'),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, face = 'bold'),
    panel.grid = element_blank()
  )


p_c_externa$plot <- p_c_externa$plot +
  theme(
    legend.text = ggtext::element_markdown(),
    axis.ticks.length = unit(.15, 'cm'),
    panel.grid = element_blank(),
    legend.key.width = unit(2.5, 'cm') # 2.5 for svg
  )


p_c_externa


ggpubr::ggarrange(
  p_c_externa$plot,
  p_c_externa$cumevents,
  labels = paste0(LETTERS[1:2], ')'),
  heights  = c(1, 0.4),
  nrow = 2
) -> p_c_externa



p_c_externa


# C. cubana

ggsurvplot(
  # survfit object with calculated statistics.
  c_cubana_fit,
  # data used to fit survival curves.
  data = df_c_cubana,
  ylab = "Predation probability",
  # Invert the scale
  fun = "event",
  censor = F,
  size = 2,
  ylim = c(0, 1),
  legend.title = "",
  # risk.table.col = "strata",
  risk.table = F,
  # show risk table.
  cumevents = TRUE,
  linetype = "strata",
  # show confidence intervals for
  # point estimates of survival curves.
  conf.int = F,

  palette = "jco",
  xlim = c(0, 10),
  # present narrower X axis, but not affect
  # survival estimates.
  # customize X axis label.
  xlab = "Time (min)",

  break.time.by = 1,
  # break X axis in time intervals by 500.
  ggtheme = theme_bw(),
  # customize plot and risk table with a theme.
  risk.table.y.text.col = T,
  # colour risk table text annotations.
  risk.table.height = 0.3,
  # the height of the risk table
  risk.table.y.text = F,
  #  tables.theme = theme_cleantable(),  # Clean theme for tables
  tables.y.text = FALSE,
  # show bars instead of names in text annotations
  # in legend of risk table.
  ncensor.plot = F,
  # plot the number of censored subjects at time t
  #ncensor.plot.height = 0.25,
  conf.int.style = "ribbon",
  font.x = c("bold", 16),
  font.y = c("bold", 16),
  font.legend = 14,
  font.tickslab = c(12, 'plain', 'black'),
  #surv.median.line = "hv",  # add the median survival pointer.
  # change legend labels.
  legend.labs = c("Control", "Treated")

) -> p_c_cubana


p_c_cubana


p_c_cubana$cumevents <- p_c_cubana$cumevents +
  theme(
    plot.title = element_text(
      size = 16,
      color = "black",
      face = "bold"
    ),
    axis.ticks = element_line(colour = 'black'),
    axis.ticks.length = unit(.15, 'cm'),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, face = 'bold'),
    panel.grid = element_blank()
  )


p_c_cubana$plot <- p_c_cubana$plot +
  theme(
    legend.text = ggtext::element_markdown(),
    axis.ticks.length = unit(.15, 'cm'),
    panel.grid = element_blank(),
    legend.key.width = unit(2.5, 'cm') # 2.5 for svg
  )


p_c_cubana


ggpubr::ggarrange(
  p_c_cubana$plot,
  p_c_cubana$cumevents,
  labels = paste0(LETTERS[1:2], ')'),
  heights  = c(1, 0.4),
  nrow = 2
) -> p_c_cubana



p_c_cubana



# Saving graphics ---------------------------------------------------------


A02_graphic_dir <- c(SVG = "A02_Predation/SVG", JPG = "A02_Predation/JPG", PDF = "A02_Predation/PDF")


# A02_graphic_dir |>
#   map(dir.create)


A02_graphic_dir |>
  imap(\(x, idx) {
    ggsave(
      filename = paste0("A02_Surv_C_externa.", tolower(idx)),
      plot = p_c_externa,
      units = 'mm',
      width = 210,
      height = 170,
      path = x,
      dpi = 600
    )

  })


A02_graphic_dir |>
  imap(\(x, idx) {
    ggsave(
      filename = paste0("A02_Surv_C_cubana.", tolower(idx)),
      plot = p_c_cubana,
      units = 'mm',
      width = 210,
      height = 170,
      path = x,
      dpi = 600
    )

  })



# Binomial fit ------------------------------------------------------------


# C. externa
df_bin_c_externa


glm_c_externa <-
  gamlss(Predation ~ Treatment, family = BI, data = df_c_externa)


plot(glm_c_externa)


for (i in 1:2) {
  try(hnp(
    glm_c_externa,
    newclass = TRUE,
    diagfun = d_fun,
    simfun = s_fun_BI,
    fitfun = f_fun_BI_externa
  ))
}


sim_glm_c_externa <-
  simulate_BI(glm_c_externa, n = 500)


DHARMa_glm_c_externa <-
  createDHARMa(
    simulatedResponse = sim_glm_c_externa,
    observedResponse = df_c_externa$Predation,
    fittedPredictedResponse = predict(glm_c_externa, type = "response"),
    integerResponse = TRUE
  )


(DHARMa_c_externa <-
    as.ggplot(function() plot(DHARMa_glm_c_externa,
     quantreg = FALSE,
     form = df_c_externa$Treatment)))


drop1(glm_c_externa)


glm_c_externa |>
  emmeans(~ Treatment, type = "response")



# C. cubana
df_bin_c_cubana


glm_c_cubana <-
  gamlss(Predation ~ Treatment, family = BI, data = df_c_cubana)


plot(glm_c_cubana)


for (i in 1:2) {
  try(hnp(
    glm_c_cubana,
    newclass = TRUE,
    diagfun = d_fun,
    simfun = s_fun_BI,
    fitfun = f_fun_BI_cubana
  ))
}


sim_glm_c_cubana <-
  simulate_BI(glm_c_cubana, n = 500)


DHARMa_glm_c_cubana <-
  createDHARMa(
    simulatedResponse = sim_glm_c_cubana,
    observedResponse = df_c_cubana$Predation,
    fittedPredictedResponse = predict(glm_c_cubana),
    integerResponse = TRUE
  )


(DHARMa_c_cubana<-
    as.ggplot(function() plot(DHARMa_glm_c_cubana,
                              quantreg = FALSE,
                              form = df_c_cubana$Treatment)))



drop1(glm_c_cubana)


glm_c_cubana |>
  emmeans(~ Treatment, type = "response")




# gamlss graphics ---------------------------------------------------------


glm_c_externa |>
  emmeans(~ Treatment, type = "response") |>
  as_tibble() |>
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", alpha = 1) +
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand =  expansion(mult = c(0, 0.05)),
    name = "Predation probability"
  )  +
  geom_segment(aes(
    x = "Control",
    y = 0.75,
    xend = "Treated",
    yend = 0.75
  )) +
  geom_text(aes(x = 1.5, y = 0.75),
            label = 'ns',
            size = 5,
            vjust = -1) +
  xlab(NULL) +
  ggsci::scale_fill_jco() +
  theme_bw() +
  theme(
    axis.ticks.length = unit(.2, 'cm'),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.text.x = element_text(
      size = 12,
      colour = "black",
      face = "bold"
    ),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "none",
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), 'lines')
  ) ->
  p_c_externa_bin



glm_c_cubana |>
  emmeans(~ Treatment, type = "response") |>
  as_tibble() |>
  ggplot(aes(x = Treatment, y = response, fill = Treatment)) +
  geom_bar(stat = "identity", alpha = 1) +
  geom_errorbar(aes(ymin = response - SE, ymax = response + SE), width = 0.2) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    expand =  expansion(mult = c(0, 0.05)),
    name = "Predation probability"
  )  +
  geom_segment(aes(
    x = "Control",
    y = 0.465,
    xend = "Treated",
    yend = 0.465
  )) +
  geom_text(
    aes(x = 1.5, y = 0.465),
    label = 'ns',
    size = 5,
    vjust = -1
  ) +
  xlab(NULL) +
  ggsci::scale_fill_jco() +
  theme_bw() +
  theme(
    axis.ticks.length = unit(.2, 'cm'),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, colour = "black"),
    axis.text.x = element_text(
      size = 12,
      colour = "black",
      face = "bold"
    ),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size = 12, colour = "black"),
    legend.position = "none",
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), 'lines')
  ) ->
  p_c_cubana_bin


ggpubr::ggarrange(
  p_c_externa_bin,
  p_c_cubana_bin,
  labels = paste0(LETTERS[1:2], ')'),
  heights  = c(1, 1),
  ncol = 2
) -> p_bin


p_bin


A02_graphic_dir |>
  imap(\(x, idx) {
    ggsave(
      filename = paste0("A02_Bin.", tolower(idx)),
      plot = p_bin,
      units = 'mm',
      width = 210,
      height = 100,
      path = x,
      dpi = 600
    )

  })



# Saving DHARMa plots -----------------------------------------------------


list(DHARMa_c_externa = DHARMa_c_externa,
     DHARMa_c_cubana = DHARMa_c_cubana) |>
  imap(\(y, idy) {

    A02_graphic_dir |>
      imap(\(x, idx) {
        ggsave(
          filename = paste0(idy, ".", idx),
          plot = y,
          path = x,
          units = "mm",
          width = 210,
          height = 150,
          dpi = 600
        )
      })

  })







