# Function for creating dir with the same name of the R document ----------


create_dir_as_doc <- function() {
  rstudioapi::documentPath() |>
    str_remove("\\.R") |>
    strsplit("/") |>
    unlist() |>
    (\(x) {
      l <- x[length(x)]
      dir.create(l)
    })()
}


create_dir_as_doc |>
  saveRDS("A01_Useful_Functions/create_dir_as_doc.rds")



# simulate_BI (DHARMa) ----------------------------------------------------


simulate_BI <- function(fittedModel, n) {
  pred = predict(fittedModel, type = "response")
  nObs = length(pred)
  sim = matrix(nrow = nObs, ncol = n)
  for (i in 1:n)
    sim[, i] = rBI(
      nObs,
      bd = fittedModel$bd,
      mu = fittedModel$mu.fv
    )
  return(sim)
}


simulate_BI |>
  saveRDS("A01_Useful_Functions/simulate_BI.rds")



# d_fun (hnp) -------------------------------------------------------------


d_fun <- function(fittedModel) {
  residuals(fittedModel)
}


d_fun |>
  saveRDS("A01_Useful_Functions/d_fun.rds")



# s_fun_BI (hnp) ----------------------------------------------------------


s_fun_BI<- function(n, fittedModel) {
  rBI(
    n,
    bd = fittedModel$bd,
    mu = fittedModel$mu.fv
  )
}


s_fun_BI |>
  saveRDS("A01_Useful_Functions/s_fun_BI.rds")



# f_fun_BI (hnp) ---------------------------------------------------------


f_fun_BI_externa <- function(y.) {
  gamlss(y. ~ Treatment, family = BI, data = df_c_externa)
}


f_fun_BI_externa |>
  saveRDS("A01_Useful_Functions/f_fun_BI_externa.rds")


f_fun_BI_cubana <- function(y.) {
  gamlss(y. ~ Treatment, family = BI, data = df_c_cubana)
}


f_fun_BI_cubana |>
  saveRDS("A01_Useful_Functions/f_fun_BI_cubana.rds")


