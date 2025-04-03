## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRwaitinglist)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)

# set a seed so that these plots are always the same
set.seed(2)

## -----------------------------------------------------------------------------
waiting_list <- wl_simulator(
  start_date = "2020-01-01",
  end_date = "2024-03-31",
  demand = 10, # simulating 10 patient arrivals per week
  capacity = 9 # simulating 9 patients being treated per week
)

head(waiting_list, 10)

## ----fig.height=3, fig.width=6------------------------------------------------
# calculate the queue size
queue_size <- wl_queue_size(waiting_list)

head(queue_size)

tail(queue_size)

# visualise the queue with a plot
ggplot(queue_size, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "A growing waiting list"
  )

## -----------------------------------------------------------------------------
referral_stats <- wl_referral_stats(waiting_list)

head(referral_stats)

## -----------------------------------------------------------------------------
removal_stats <- wl_removal_stats(waiting_list)

head(removal_stats)

## -----------------------------------------------------------------------------
overall_stats <- wl_stats(
  waiting_list = waiting_list,
  target_wait = 18 # standard NHS 18 weeks target
)

head(overall_stats)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(
    mean_demand,
    mean_capacity,
    load,
    load_too_big
  ),
  align = "c"
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(
    queue_size,
    target_queue_size,
    queue_too_big,
    mean_wait
  ),
  align = "c"
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(mean_wait),
  align = "c"
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(
    cv_arrival,
    cv_removal
  ),
  align = "c"
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(target_capacity, relief_capacity),
  align = "c"
)

## ----echo=FALSE---------------------------------------------------------------
knitr::kable(
  overall_stats |> dplyr::select(pressure),
  align = "c"
)

## -----------------------------------------------------------------------------
waiting_list <- wl_simulator(
  start_date = "2020-01-01",
  end_date = "2024-03-31",
  demand = 10, # simulating 10 patient arrivals per week
  capacity = 10.2 # simulating 10.2 patients being treated per week
)

referral_stats <- wl_referral_stats(waiting_list)
head(referral_stats)

removal_stats <- wl_removal_stats(waiting_list)
head(removal_stats)

# calculate the queue size
queue_size <- wl_queue_size(waiting_list)

## ----fig.height=3, fig.width=6------------------------------------------------
# visualise the queue with a plot
ggplot(queue_size, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "A finely-balanced waiting list"
  )

## -----------------------------------------------------------------------------
overall_stats <- wl_stats(
  waiting_list = waiting_list,
  target_wait = 18 # standard NHS 18wk target
)

head(overall_stats)

## -----------------------------------------------------------------------------
waiting_list <- wl_simulator(
  start_date = "2020-01-01",
  end_date = "2024-03-31",
  demand = 10, # simulating 10 patient arrivals per week
  capacity = 10.3 # simulating 10.3 patients being treated per week
)

referral_stats <- wl_referral_stats(waiting_list)
head(referral_stats)

removal_stats <- wl_removal_stats(waiting_list)
head(removal_stats)

# calculate the queue size
queue_size <- wl_queue_size(waiting_list)

## ----fig.height=3, fig.width=6------------------------------------------------
# visualise the queue with a plot
ggplot(queue_size, aes(dates, queue_size)) +
  geom_line() +
  labs(
    title = "A stable waiting list"
  )

## -----------------------------------------------------------------------------
overall_stats <- wl_stats(
  waiting_list = waiting_list,
  target_wait = 18 # standard NHS 18 weeks target
)

head(overall_stats)

