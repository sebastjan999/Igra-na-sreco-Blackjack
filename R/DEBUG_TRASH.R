BS_TABLE <- read.csv("basic_strategy.csv", stringsAsFactors = FALSE)

a = basic_action_bs(c(11,7), dealer_up = 6)  # A,7 vs 6 -> pričakujemo "double"
b = basic_action_bs(c(11,6), dealer_up = 2)  # A, 6 vs 2 -> "hit"
c = basic_action_bs(c(8,8), dealer_up = 10, can_split = TRUE) # par 8 vs 10 -> "split"
d = basic_action_bs(c(10,10), dealer_up = 6, can_split = TRUE) # 10,10 vs 6 -> "stand"

nrow(subset(BS_TABLE, player_group == "soft" & player_total == 18 & dealer_up == 6))
nrow(subset(BS_TABLE, player_group == "soft" & player_total == 18 & dealer_up == 2))
