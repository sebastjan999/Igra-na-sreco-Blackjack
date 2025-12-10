setwd("C:/Users/sebas/OneDrive/Namizje/MZR/Igra-na-sreco-Blackjack")

source("simulacije.R")

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
ggplot(strategije_df, aes(x = strategija, y = EV, fill = strategija)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Pričakovani dobiček (EV) po strategijah",
    x = "Strategija",
    y = "EV na roko"
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

ggplot(he_rules_df, aes(x = pravilo, y = HE, fill = strategija)) +
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
ggplot(hilo_pen_df, aes(x = penetration, y = HE)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    title = "Hi-Lo: House edge glede na penetracijo shoe-a",
    x = "Penetracija",
    y = "HE na enoto stave"
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
  filter(scenario %in% c("basic_double_ON", "basic_double_OFF")) %>%
  mutate(label = if_else(can_double, "Double ON", "Double OFF"))

ggplot(double_df, aes(x = label, y = EV, fill = label)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila DOUBLE na EV (basic)",
    x = "",
    y = "EV na roko"
  ) +
  theme(legend.position = "none")

# b) SURRENDER ON/OFF
surr_df <- results_all %>%
  filter(scenario %in% c("basic_surrender_ON", "basic_surrender_OFF")) %>%
  mutate(label = if_else(can_surrender, "Surrender ON", "Surrender OFF"))

ggplot(surr_df, aes(x = label, y = EV, fill = label)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila SURRENDER na EV (basic)",
    x = "",
    y = "EV na roko"
  ) +
  theme(legend.position = "none")

# c) SPLIT ON/OFF
split_df <- results_all %>%
  filter(scenario %in% c("basic_split_ON", "basic_split_OFF")) %>%
  mutate(label = if_else(can_split, "Split ON", "Split OFF"))

ggplot(split_df, aes(x = label, y = EV, fill = label)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv pravila SPLIT na EV (basic)",
    x = "",
    y = "EV na roko"
  ) +
  theme(legend.position = "none")

# ================================================================
# 5) BASIC: IZPLAČILO ZA BLACKJACK (3:2 VS 6:5)
# ================================================================

bj_payout_df <- results_all %>%
  filter(scenario %in% c("basic_BJ_3to2", "basic_BJ_6to5")) %>%
  mutate(
    payout_label = case_when(
      abs(payout_bj - 1.5) < 1e-9 ~ "BJ 3:2",
      abs(payout_bj - 1.2) < 1e-9 ~ "BJ 6:5",
      TRUE ~ paste("BJ", payout_bj)
    )
  )

ggplot(bj_payout_df, aes(x = payout_label, y = EV, fill = payout_label)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Vpliv izplačila za Blackjack na EV (basic)",
    x = "Izplačilo BJ",
    y = "EV na roko"
  ) +
  theme(legend.position = "none")

# ================================================================
# 6) HI-LO: EV GLEDE NA TRUE COUNT
# ================================================================

set.seed(123)

res_hilo_big <- simulate_with_shoe_hilo(
  N            = 50000,
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

