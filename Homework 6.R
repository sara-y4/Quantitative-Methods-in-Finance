#####Problem 1#####
library(tidyquant)
library(tidyverse)
library(lubridate)

info <-tidyquant::tq_get("GOOG",from = lubridate::ymd("2012-01-03"),
                         to = lubridate::ymd("2021-10-26")) %>%
  dplyr::select(symbol, date, adjusted)

dates <- base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd("2012-01-03"),
                                                           to = lubridate::ymd("2021-10-26"),
                                                           by = "day")),
                          Symbol = c(base::rep("GOOG",3585)))

DATA <- dates %>%
  dplyr::left_join(info, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")


SMA <- DATA %>%
  dplyr::mutate(SMA20 = SMA(adjusted, n = 20),
                SD20 = RcppRoll::roll_sd(adjusted, n =20, align = "right", fill = NA),
                Upper = SMA20+2*SD20,
                Lower = SMA20-2*SD20,
                buy_sell = case_when(lag(adjusted)<lag(Upper) & adjusted > Upper ~ "sell",
                                     lag(adjusted)> lag(Lower) & adjusted<Lower~"buy")) %>%
  dplyr::mutate(ProfitCoeff = (adjusted/lag(adjusted)),
                ProfitCoeff = ifelse(is.na(ProfitCoeff), 1, ProfitCoeff),
                BenchmarkMoney = 100 * cumprod(ProfitCoeff),
                buy_sell_for_comparison = buy_sell)%>%
  tidyr::fill(buy_sell_for_comparison, .direction = "down")%>%
  dplyr::mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison), "buy", buy_sell_for_comparison),
                ProfitCoeff_strategy = case_when(buy_sell_for_comparison == "sell"~ 1,
                                                 buy_sell_for_comparison == "buy"~ ProfitCoeff),
                StrategyMoney = 100 * cumprod(ProfitCoeff_strategy)) 


#####Problem 2#####
RSI <- DATA %>%
  dplyr::mutate(gain_loss = (adjusted/lag(adjusted))-1,
                gain_loss = if_else(is.na(gain_loss), 0, gain_loss),
                average_gain1 = case_when(gain_loss >= 0 ~gain_loss, 
                                          gain_loss <0 ~ 0),
                average_gain = SMA(average_gain1, 14),
                average_loss1 = case_when(gain_loss <= 0 ~abs(gain_loss), 
                                          gain_loss >0 ~ 0),
                average_loss = SMA(average_loss1, 14),
                RSI1 = 100-(100/(1+(average_gain/average_loss))),
                RSI = 100-(100/(1+((13*lag(average_gain)+ average_gain1)/(13*lag(average_loss)+average_loss1)))),
                buy_sell = case_when(RSI > 65 ~ "sell",
                                     RSI < 35 ~ "buy"),
                ProfitCoeff = adjusted/lag(adjusted),
                ProfitCoeff = if_else(is.na(ProfitCoeff), 1, ProfitCoeff),
                BenchmarkMoney = 100 * cumprod(ProfitCoeff),
                buy_sell_for_comparison = buy_sell)%>%
  tidyr::fill(buy_sell_for_comparison, .direction = "down")%>%
  dplyr::mutate(buy_sell_for_comparison = if_else(is.na(buy_sell_for_comparison), "buy", buy_sell_for_comparison),
                ProfitCoeff_strategy = case_when(buy_sell_for_comparison == "sell"~ 1,
                                                 buy_sell_for_comparison == "buy"~ ProfitCoeff),
                StrategyMoney = 100 * cumprod(ProfitCoeff_strategy))