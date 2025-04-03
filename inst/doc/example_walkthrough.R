## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(NHSRwaitinglist)

## -----------------------------------------------------------------------------
# Queue size (patients)
queue_size <- 1200

# Waiting time target (weeks)
waiting_time_target <- 52

# Average waiting time in the queue (weeks)
avg_waiting_time <- 63

# Proportion of waiting list who have missed the 52 week target (%)
perc_missing_target <- 0.51

# Demand (patients per week)
demand <- 30

# Capacity (procedures per week)
capacity <- 27

# Standard deviation of number of operations per week
std_dev_procedures <- 160

## -----------------------------------------------------------------------------
load <- calc_queue_load(demand, capacity)
load

## -----------------------------------------------------------------------------
target_mean_wait <- calc_target_mean_wait(waiting_time_target)
target_mean_wait

## -----------------------------------------------------------------------------
target_queue_size <- calc_target_queue_size(demand, waiting_time_target)
target_queue_size

queue_ratio <- queue_size / target_queue_size
queue_ratio

## -----------------------------------------------------------------------------
weeks_until_target_acheived <- 26

relief_capacity <- calc_relief_capacity(
  demand = demand,
  queue_size = queue_size,
  target_queue_size = target_queue_size,
  time_to_target = weeks_until_target_acheived
)
relief_capacity

## -----------------------------------------------------------------------------
# set the "F" variability parameter
f_1 <- 1

target_capacity_1 <- calc_target_capacity(
  demand = demand,
  target_wait = waiting_time_target
)
target_capacity_1

## -----------------------------------------------------------------------------
f_2 <- 6.58

target_capacity_2 <- calc_target_capacity(
  demand = demand,
  target_wait = waiting_time_target
)
target_capacity_2

## -----------------------------------------------------------------------------
waiting_list_pressure_p4 <-
  calc_waiting_list_pressure(
    avg_waiting_time,
    waiting_time_target
  )
waiting_list_pressure_p4

## -----------------------------------------------------------------------------
queue_size_p2 <- 220
avg_waiting_time_p2 <- 24
waiting_time_target_p2 <- 4

waiting_list_pressure_p2 <-
  calc_waiting_list_pressure(
    avg_waiting_time_p2,
    waiting_time_target_p2
  )
waiting_list_pressure_p2

