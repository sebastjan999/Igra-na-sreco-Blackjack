setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

#source("simulacije.R")

library(ggplot2)
library(dplyr)
library(tidyr)


theme_set(theme_minimal(base_size = 13))

# ================================================================
# 1) PRIMERJAVA STRATEGIJ (DEMO / BASIC / HI-LO)
# ================================================================

strategije_df <- results_all %>%
  filter(scenario %in% c("demo_random", "basic_S17", "hilo_S17")) %>%
  mutate(
    strategija = case_when(
      scenario == "demo_random"   ~ "Demo / random",
      scenario == "basic_S17"     ~ "Basic (shoe)",
      scenario == "hilo_main_S17" ~ "Hi-Lo (shoe + spread)",
      TRUE ~ scenario
    )
  )

# EV po strategijah
ggplot(strategije_df, aes(x = strategija, y = HE_per_bet, fill = strategija)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "House edge (HE) na enoto stave po strategijah",
    x = "Strategija",
    y = "HE na enoto stave"
  ) +
  theme(legend.position = "none")

# ================================================================
# 2) HOUSE EDGE: S17 VS H17 (BASIC & HI-LO)
# ================================================================

he_rules_df <- results_all %>%
  filter(scenario %in% c("basic_S17_rule", "basic_H17_rule",
                         "hilo_S17_rule",  "hilo_H17_rule")) %>%
  mutate(
    pravilo = rule,
    strategija = if_else(strategy == "basic", "Basic", "Hi-Lo")
  )

ggplot(he_rules_df, aes(x = pravilo, y = HE_per_bet, fill = strategija)) +
  geom_col(position = position_dodge(width = 0.7)) +
  labs(
    title = "House edge pri različnih pravilih delivca",
    x = "Pravilo delivca",
    y = "HE (na enoto stave)",
    fill = "Strategija"
  )

# ================================================================
# 3) HI-LO: VPLIV PENETRACIJE
#    (scenariji: npr. 'hilo_pen_0.5', 'hilo_pen_0.75', ...)
# ================================================================

hilo_pen_df <- results_all %>%
  filter(strategy == "hilo", grepl("^hilo_pen_", scenario)) %>%
  arrange(penetration)

# EV vs penetration
ggplot(hilo_pen_df, aes(x = penetration, y = EV)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Hi-Lo: EV glede na penetracijo shoe-a",
    x = "Penetracija",
    y = "EV na roko"
  )

# HE vs penetration
ggplot(hilo_pen_df, aes(x = penetration, y = HE_per_bet)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Hi-Lo: HE_per_bet glede na penetracijo shoe-a",
    x = "Penetracija",
    y = "HE (na enoto stave)"
  )

# ROI vs penetration
ggplot(hilo_pen_df, aes(x = penetration, y = ROI)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Hi-Lo: ROI glede na penetracijo shoe-a",
    x = "Penetracija",
    y = "ROI"
  )

# ================================================================
# 4) BASIC STRATEGIJA: PRAVILA IGRALECA (DOUBLE / SURRENDER / SPLIT)
# ================================================================

# a) DOUBLE ON/OFF
double_df <- results_all %>%
  filter(scenario %in% c("basic_double_ON", "basic_double_OFF", "hilo_double_ON", "hilo_double_OFF")) %>%
  mutate(label = if_else(can_double, "Double ON", "Double OFF"),
         strategija = if_else(strategy == "basic", "Basic", "Hi-Lo")
  )

ggplot(double_df, aes(x = label, y = HE_per_bet, fill = strategija)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila DOUBLE na HE_per_bet (BASIC)",
    x = "Double",
    y = "HE (na enoto stave)"
  ) 

# b) SURRENDER ON/OFF
surr_df <- results_all %>%
  filter(scenario %in% c("basic_surrender_ON", "basic_surrender_OFF", "hilo_surrender_ON", "hilo_surrender_OFF")) %>%
  mutate(label = if_else(can_surrender, "Surrender ON", "Surrender OFF"),
         strategija = if_else(strategy == "basic", "Basic", "Hi-Lo")
  )

ggplot(surr_df, aes(x = label, y = HE_per_bet, fill = strategija)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila SURRENDER na HE_per_bet (BASIC)",
    x = "Surrender",
    y = "HE (na enoto stave)"
  ) 

# c) SPLIT ON/OFF
split_df <- results_all %>%
  filter(scenario %in% c("basic_split_ON", "basic_split_OFF","hilo_split_ON", "hilo_split_OFF")) %>%
  mutate(label = if_else(can_split, "Split ON", "Split OFF"),
         strategija = if_else(strategy == "basic", "Basic", "Hi-Lo")
  )

ggplot(split_df, aes(x = label, y = HE_per_bet, fill = strategija)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila SPLIT na HE_per_bet (BASIC)",
    x = "Split",
    y = "HE (na enoto stave)"
  ) 

# ================================================================
# DoD) Best vs wors case
# ================================================================
bw_df <- results_all %>%
  filter(scenario %in% c("hilo_best", "hilo_worst")) %>%
  mutate(case = if_else(scenario == "hilo_best", "Best case", "Worst case"))

bw_df <- bw_df %>%
  select(case, HE_per_bet, ROI) %>%
  pivot_longer(cols = c(HE_per_bet, ROI),
               names_to = "metric",
               values_to = "value") %>%
  mutate(metric = recode(metric,
                         HE_per_bet = "HE (na enoto stave)",
                         ROI = "ROI (donos na vložek)"))

ggplot(bw_df, aes(x = case, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Hi–Lo: Best vs Worst case (HE in ROI)",
    x = "",
    y = ""
  )

library(patchwork)

bw_df2 <- results_all %>%
  filter(scenario %in% c("hilo_best", "hilo_worst")) %>%
  mutate(case = if_else(scenario == "hilo_best", "Best", "Worst"))

p_bet <- ggplot(bw_df2, aes(x = case, y = avg_bet_per_hand, fill = case)) +
  geom_col(width = 0.65) +
  labs(title = "Povprečna stava (avg_bet)", x = NULL, y = "avg_bet") +
  theme(legend.position = "none")

p_dd <- ggplot(bw_df2, aes(x = case, y = max_drawdown, fill = case)) +
  geom_col(width = 0.65) +
  labs(title = "Največji drawdown (MaxDD)", x = NULL, y = "max_drawdown") +
  theme(legend.position = "none")

p_bet / p_dd

# ================================================================
# 5) BASIC: IZPLAČILO ZA BLACKJACK (3:2 VS 6:5)
# ================================================================

bj_payout_df <- results_all %>%
  filter(scenario %in% c("basic_BJ_3to2", "basic_BJ_6to5", "hilo_BJ_3to2", "hilo_BJ_6to5")) %>%
  mutate(
    payout_label = case_when(
      abs(payout_bj - 1.5) < 1e-9 ~ "BJ 3:2",
      abs(payout_bj - 1.2) < 1e-9 ~ "BJ 6:5",
      TRUE ~ paste("BJ", payout_bj)
    ), strategija = if_else(strategy == "basic", "Basic", "Hi-Lo"),
    payout_label = factor(payout_label, levels = c("BJ 3:2", "BJ 6:5"))
  )

ggplot(bj_payout_df, aes(x = payout_label, y = HE_per_bet, fill = strategija)) +
  geom_col(position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv izplačila za Blackjack na HE_per_bet",
    x = "Izplačilo BJ",
    y = "HE (na enoto stave)"
  ) 

# ================================================================
# 6) HI-LO: EV GLEDE NA TRUE COUNT
# ================================================================

set.seed(123)

res_hilo_big <- simulate_with_shoe_hilo(
  N            = 1e6,
  n_decks      = 6,
  penetration  = 0.75,
  hit_soft_17  = FALSE,  # S17
  bet          = 1,
  payout_bj    = 1.5,
  can_double   = TRUE,
  can_split    = FALSE,
  can_surrender= TRUE
)

tc_round <- round(res_hilo_big$true_count)
EV_by_TC <- tapply(res_hilo_big$gains, tc_round, mean)
N_by_TC  <- table(tc_round)

ev_tc_df <- data.frame(
  TC = as.numeric(names(EV_by_TC)),
  EV = as.numeric(EV_by_TC),
  N  = as.numeric(N_by_TC[names(EV_by_TC)])
)

ggplot(ev_tc_df, aes(x = TC, y = EV)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point(aes(size = N)) +
  labs(
    title = "Hi-Lo: pričakovani dobiček glede na True Count",
    x = "True Count (TC)",
    y = "EV na roko",
    size = "Št. opazovanj"
  )

# ================================================================
# 7) HI-LO: BANKROLL KRIVULJA IN HISTOGRAM DOBITKOV
# ================================================================

# Bankroll skozi čas
bankroll_df <- data.frame(
  hand = seq_along(res_hilo_big$bankroll),
  bankroll = res_hilo_big$bankroll
)

ggplot(bankroll_df, aes(x = hand, y = bankroll)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Hi-Lo: potek bankrolla skozi igre",
    x = "Igra",
    y = "Bankroll"
  )

# Histogram dobitkov po igrah
gains_df <- data.frame(gain = res_hilo_big$gains)

ggplot(gains_df, aes(x = gain)) +
  geom_histogram(bins = 50) +
  labs(
    title = "Porazdelitev dobitkov (Hi-Lo)",
    x = "Dobiček na roko",
    y = "Frekvenca"
  )

