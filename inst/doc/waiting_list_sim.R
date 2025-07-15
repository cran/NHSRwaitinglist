## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 8
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup--------------------------------------------------------------------
library(NHSRwaitinglist)
library(ggplot2)

# set up a theme for our charts
theme_set(
  theme_minimal() +
    theme(
      plot.subtitle = element_text(face = "italic")
    )
)

# Fix the random number seed for this vignette
# Don't do this in real world application
set.seed(123)

## ----overview-----------------------------------------------------------------
# What is the quantile value for target waiting list,
# based on 92% of exponential distribution
qt92 <- qexp(0.92)

# Target mean wait for 18 weeks
target_mean_wait <- calc_target_mean_wait(18, qt92)
target_mean_wait

## ----sim_wl_setup-------------------------------------------------------------
end_date <- as.Date("2023-03-31")

# WL is 200, simulation is weekly, and capacity / demand difference is 20,
# 20 added to WL each week. Therefore 420 / 20 = 21, 21 week difference for
# sim start and end date:  7 x 21 = 147 days.
# We take 1 day away too, to discount the end_date

start_date <- end_date - (((840 / 20) * 7) - 1)

current_wl1 <- wl_simulator(start_date = start_date,
                            end_date = end_date,
                            demand = 100,
                            capacity = 80)

tail(wl_queue_size(current_wl1))

## ----dump_wl_setup------------------------------------------------------------
current_wl1 <-
  data.frame(
    Referral = rep(as.Date("2023-03-31"), 840)
    , Removal = rep(as.Date(NA), 840)
  )

# Only a single day in our waiting list so we don't need `tail()` this time
wl_queue_size(current_wl1)

## ----target1------------------------------------------------------------------
target_queue1 <-
  calc_target_queue_size(demand = 100, target_wait = 18, factor = qt92)
target_queue1

## ----sim_raise_capacity-------------------------------------------------------
raised_capacity_wl <-
  wl_simulator(start_date = as.Date("2023-04-01"),
               end_date = as.Date("2024-03-31"),
               demand = 100,
               capacity = 105,
               waiting_list = current_wl1)

raised_capacity_queue <- wl_queue_size(raised_capacity_wl)

tail(raised_capacity_queue)

## ----sim_reduce_demand--------------------------------------------------------
reduced_demand_wl <-
  wl_simulator(start_date = as.Date("2023-04-01"),
               end_date = as.Date("2024-03-31"),
               demand = 77,
               capacity = 80,
               waiting_list = current_wl1)

reduced_demand_queue <- wl_queue_size(reduced_demand_wl)

tail(reduced_demand_queue)

## ----plot_raise_capacity, message=FALSE, warning=FALSE------------------------

combine_queue <-
  rbind(
    cbind(raised_capacity_queue, type = "raised capacity"),
    cbind(reduced_demand_queue, type = "reduced demand")
  )


ggplot(combine_queue, aes(y = queue_size, x = dates)) +
  geom_line(aes(linetype = type)) +
  geom_hline(yintercept = target_queue1, colour = "#440154FF") +
  annotate("text", label = "Target queue size", x = as.Date("2024-03-31")
           , y = target_queue1, vjust = 1.2
           , hjust = 1.2, colour = "#440154FF") +
  labs(y = "Queue Size", x = "Date"
       , title = "Simulated reduction in waiting list using demand or capacity")

## ----target2------------------------------------------------------------------
target_queue2 <-
  calc_target_queue_size(demand = 50, target_wait = 18, factor = qt92)
target_queue2

## ----list_growth_2------------------------------------------------------------
current_queue <- 82
queue_growth <- target_queue2 - current_queue
weeks_to_grow <- 26 #52 weeks per year / 2 for 6 months

queue_growth / weeks_to_grow


## ----example_2----------------------------------------------------------------
current_wl2 <-
  data.frame(
    Referral = rep(as.Date("2024-03-31"), current_queue)
    , Removal = rep(as.Date(NA), current_queue)
  )

raise_wl_sim <-
  wl_simulator(start_date = as.Date("2024-04-01"),
               end_date = as.Date("2024-09-30"),
               demand = 50,
               capacity =  (50 - 10), # 10 = floor(queue_growth / weeks_to_grow)
               waiting_list = current_wl2)

raised_wl <- wl_queue_size(raise_wl_sim)

ggplot(raised_wl, aes(y = queue_size, x = dates)) +
  geom_line() +
  geom_hline(yintercept = target_queue2, colour = "#21908CFF") +
  annotate("text", label = "Target queue size", x = as.Date("2024-04-01")
           , y = target_queue2, vjust = 1.2, hjust = 0, colour = "#21908CFF") +
  labs(y = "Queue Size", x = "Date"
       , title = "Simulated waiting list after allowing waiting list to grow")

## ----target3------------------------------------------------------------------
# Target mean wait for 95% at 18 weeks
target_mean_wait3 <- calc_target_mean_wait(18, qexp(0.95))
target_mean_wait3

target_queue3 <-
  calc_target_queue_size(demand = 100, target_wait = 18, factor = qexp(0.95))
target_queue3

## ----control_table------------------------------------------------------------
ctrl_tbl <- data.frame(
  start_date = as.Date(c("2024-04-01", "2025-04-01", "2026-04-01"
                         , "2027-04-01", "2028-04-01"), format = "%Y-%m-%d"),
  end_date = as.Date(c("2025-03-31", "2026-03-31", "2027-03-31"
                       , "2028-03-31", "2029-03-31"), format = "%Y-%m-%d"),
  demand = 100,
  capacity = 100
)

# Manually setting sim properties to illustrate:
ctrl_tbl$demand[2] <- ctrl_tbl$demand[1] * 1.05
ctrl_tbl$demand[3] <- ctrl_tbl$demand[2] * 1.05 * 0.8 # 5% growth, 20% reduction
ctrl_tbl$demand[4] <- ctrl_tbl$demand[3] * 1.05
ctrl_tbl$demand[5] <- ctrl_tbl$demand[4] * 1.05

ctrl_tbl

## ----target_queue_size_dynamic------------------------------------------------
ctrl_tbl$target_queue_size <-
  calc_target_queue_size(demand = ctrl_tbl$demand
                         , target_wait = 18
                         , factor = qexp(0.95))

## ----setup_wl_3---------------------------------------------------------------
current_wl3 <-
  data.frame(Referral = rep(as.Date("2024-03-31"), 500)
             , Removal = rep(as.Date(NA), 500))

## ----sim3_1-------------------------------------------------------------------
# Initialise a list of the same length as the table
sim3 <- list(NROW(ctrl_tbl))

# Run the first line and current waiting list
sim3[[1]] <-
  wl_simulator(start_date = ctrl_tbl$start_date[1]
               , end_date = ctrl_tbl$end_date[1]
               , demand = ctrl_tbl$demand[1]
               , capacity = ctrl_tbl$capacity[1]
               , waiting_list = current_wl3)

## ----sim3_loop----------------------------------------------------------------
# Loop through and simulate each section
# Loop picks up each previous waiting list
for (i in seq(2, 5)) {
  sim3[[i]] <-
    wl_simulator(start_date = ctrl_tbl$start_date[i]
                 , end_date = ctrl_tbl$end_date[i]
                 , demand = ctrl_tbl$demand[i]
                 , capacity = ctrl_tbl$capacity[i]
                 , waiting_list = sim3[[i - 1]])
}

queue_over_time <- wl_queue_size(sim3[[5]])

## ----plot3--------------------------------------------------------------------
ggplot(queue_over_time, aes(y = queue_size, x = dates)) +
  geom_line() +
  geom_line(aes(y = target_queue_size, x = start_date)
            , colour = "#A69D75FF"
            , data =
              data.frame(
                target_queue_size =
                c(ctrl_tbl$target_queue_size, ctrl_tbl$target_queue_size[5]),
                start_date =
                c(ctrl_tbl$start_date, ctrl_tbl$end_date[5])
              )) +
  annotate("text", label = "Target queue size", x = ctrl_tbl$start_date[5]
           , y = ctrl_tbl$target_queue_size[5], vjust = 1.2, hjust = 0.2
           , colour = "#A69D75FF") +
  geom_vline(xintercept = as.Date("2026-04-01"), colour = "red") +
  annotate("text", label = "20% demand\nreduction", x = as.Date("2026-04-01")
           , y = 200, hjust = 1.1, colour = "red") +
  labs(y = "Queue Size", x = "Date"
       , title = "Simulated waiting list, 5% year on year growth."
       , subtitle = "20% demand reduction from Apr-2026")


## ----montecarlo_1-------------------------------------------------------------
sim_func <- function(run_id) {
  sim <- wl_simulator(start_date = as.Date("2023-04-01"),
                      end_date = as.Date("2024-03-31"),
                      demand = 100,
                      capacity = 105,
                      waiting_list = current_wl1)

  cbind(wl_queue_size(sim), run_id)
}

# sequence to iterate over
run_sequence <- 1:50

raised_capacity_wl_mc <- lapply(run_sequence, sim_func)

## ----montecarlo_2-------------------------------------------------------------
mc_bind <- do.call("rbind", raised_capacity_wl_mc)
mc_agg <-
  aggregate(
    queue_size ~ dates
    , data = mc_bind
    , FUN = \(x) {
      c(mean_q = mean(x),
        median_q = median(x),
        lower_95CI = mean(x) -  (1.96 * (sd(x) / sqrt(length(x)))),
        upper_95CI = mean(x) +  (1.96 * (sd(x) / sqrt(length(x)))),
        q_25 = quantile(x, .025, names = FALSE),
        q_75 = quantile(x, .975, names = FALSE))
    }
  )
mc_agg <- data.frame(dates = as.Date(mc_agg$dates), unlist(mc_agg$queue_size))

## ----montecarlo_3-------------------------------------------------------------
ggplot(mc_bind, aes(x = dates)) +
  geom_line(aes(y = queue_size, group = i), alpha = 0.5, col = "grey") +
  geom_ribbon(aes(y = mean_q, ymin = lower_95CI, ymax = upper_95CI)
              , alpha = 0.5, data = mc_agg, fill = "red") +
  geom_line(aes(y = mean_q), data = mc_agg, col = "black") +
  geom_hline(yintercept = target_queue1, colour = "#440154FF") +
  annotate("text", label = "Target queue size", x = as.Date("2024-02-01")
           , y = target_queue1, vjust = -1, colour = "#440154FF") +
  labs(y = "Queue Size", x = "Date"
       , title = "Simulated waiting list after raising capacity"
       , subtitle = "Average WL over 50 runs, with 95% confidence interval")

