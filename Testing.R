coef_table <- tibble(
  group = c(
    "",
    rep("Sex", 2),
    rep("Age", 4),
    rep("Body-Mass index", 2),
    rep("Race", 3),
    rep("Baseline statin treatment", 2),
    rep("Intensity of statin treatment", 2),
    rep("Metabolic disease", 3),
    rep("Renal function", 3)
  ),
  subgroup = c(
    "All Patients",
    "Male", "Female",
    "<65 yr", ">= 65 yr", "<75 yr", ">=75 yr",
    "<=Median", ">Median",
    "White", "Black", "Other",
    "Yes", "No",
    "High", "Not high",
    "Diabetes", "Metabolic syndrome", "Neither",
    "Normal", "Mild impairment", "Moderate impairment"
  ),
  Inclisiran = c(
    781, 535,246,297,484,638,143,394,387,653,110,18,701,80,538,243,371,195,215,395,269,113
  ),
  Placebo = c(
    780,548,232,333,447,649,131,385,394,685,87,8,692,88,546,234,331,207,242,410,260,107
  ),
  coefficients = c(-60,-55,-68,-58,-55,-57,-58,-55,-48,-58,-57,-49,-44,-58,-55,-57,-54,-52,-54,-53, -54,-52)
) %>% 
  mutate(
    conf_range = runif(22, min = 5, max = 10),
    conf_lower = coefficients - conf_range,
    conf_higher = coefficients + conf_range
  ) %>%
  mutate(
    image = spec_pointrange(
      x = coefficients, 
      xmin = conf_lower, 
      xmax = conf_higher, 
      same_lim = TRUE,
      lim = c(-100, 25),
      vline = 0,
      width = 550,
      cex = .75,
      col = "black"
    )
  )

mtcars %>% 
  group_by(cyl) %>% 
  summarize(mpg_data = list(mpg), .groups = "drop") %>% 
  gt() %>% 
  # note you can leave mpg_data unquoted for the tidyeval
  # but have to quote mpg_data for the pluck
  gt_spark(mpg_data, "mpg_data")


tibble(
  var = c("mpg", "wt"),
  sparkline1 = "",
  sparkline2 = "",
  box = ""
) %>% 
  gt() %>% 
  text_transform(
    locations = cells_body(sparkline1),
    fn = function(x){
      sparkline <- map(list(mtcars$mpg, mtcars$wt), ~spk_chr(values = .x, chartRangeMin = 0))
      map(sparkline, gt::html)
    }
  )
