# Igra-na-sreco-Blackjack

Ta projekt implementira **Monte Carlo simulacije Blackjacka** z uporabo:

- osnovne strategije (S17/H17),
- CSV tabel strategij,
- podporo za *surrender*, *double*, *pair splitting* (po potrebi),
- realistično igro z *shoe*, penetracijo in mešanjem,
- napredno verzijo z **Hi–Lo štetjem kart** in **bet spread**.

Projekt omogoča analizo house edge, ROI, volatilnosti, bankroll poteka in učinkov uporabe strategij.

---
## Struktura projekta

├── BJ_skeleton.R       # ogrodna verzija za testiranje/demos \
├── R/  \
│   ├── 01_cards.R          # definicije kart, helper funkcije  \
│   ├── 02_shoe.R           # shoe, penetracija, reshuffling  \
│   ├── 03_strategy.R       # osnovna strategija (CSV S17/H17)  \
│   ├── 04_play_hand.R      # igralčeve poteze, dealerjeva logika  \
│   ├── 05_simulation.R     # vse Monte Carlo simulacije  \
│   ├── DEBUG_TRASH.R       # dodatni debug skript (ne vpliva na simulacije)  \
│    \
│     \
├── basic_strategy.csv      # tabela osnovne strategije za S17  \
├── basic_strategy_H17.csv  # tabela osnovne strategije za H17  \
│  \
├── main.R                  # enotna vstopna točka (source vseh skript)  \
├── porocilo.Rmd            # glavno poročilo  \
├── LICENSE  \
└── README.md  \

##  Kako pognati kodo (bom cim odpravm se eno pomankljivost v kodi XD)

1️. Kloniraj repozitorij (najlazji kr fork) \
2. Odpri main.R v RStudiu \
3. V prvi vrstici te scripte nastavi svoj working directory (na mapo, kjer je celoten fork-an repozitorij) \
4. Zaženi enega od 3 pripravljenih primerov (na vrhu main.R). \
### i) Demo verzija (hitro preverjanje)

```r
set.seed(100)
simulate_n(
  N = 100,
  n_decks = 6,
  hit_soft_17 = FALSE,
  bet = 1,
  payout_bj = 1.5
)
```

### ii) Simulacija s »shoe« in reshufflom (brez štetja)

```r
simulate_with_shoe(
  N = 1000,
  n_decks = 6,
  penetration = 0.75,
  hit_soft_17 = TRUE,
  bet = 1,
  payout_bj = 1.5,
  can_double = FALSE,
  can_split = FALSE,
  can_surrender = FALSE
)
```

### iii) Simulacija s »shoe« + Hi–Lo štetje

```r
simulate_with_shoe_hilo(
  N = 1000,
  n_decks = 6,
  penetration = 0.75,
  hit_soft_17 = TRUE,
  bet = 1,
  payout_bj = 1.5,
  can_double = FALSE,
  can_split = FALSE,
  can_surrender = FALSE
)
```

Namig: za reproducibilnost vedno nastavi set.seed(...). Za večjo natančnost zvišaj N (npr. 1e5 ali 1e6). Vse ostale parametre spreminjaj po zelji :) 
OPOZORILO: Za enkrat can_split se ni implementiran, zato neglede na to ali je TRUE ali FALSE vrača iste rezultate (bom popravilo, soon-ish)

### Parametri (povzetek)

**N**: št. iger v simulaciji (Monte Carlo ponovitve)

**n_decks**: število kompletov kart v »shoe«

**penetration**: delež premešanih kart, preden se shoe ponovno premeša (npr. 0.75)

**hit_soft_17**: delivec vleče na mehki 17 (TRUE = H17; FALSE = S17)

**bet**: osnovna stava na hand

**payout_bj**: izplačilo za naravni blackjack (običajno 1.5 = 3:2; lahko 1.2 = 6:5)

**can_double, can_split, can_surrender**: (TRUE = pravila omogočena; FALSE = pravila onemogočena)
