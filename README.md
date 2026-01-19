# Igra-na-sreco-Blackjack

Ta projekt implementira **Monte Carlo simulacije Blackjacka** z uporabo:

- osnovne strategije (S17/H17),
- CSV tabel strategij,
- podporo za *surrender*, *double*, *pair splitting* (po potrebi),
- realistično igro z *shoe*, penetracijo in mešanjem,
- napredno verzijo z **Hi–Lo štetjem kart** in **bet spread**.

Projekt uporablja Monte Carlo simulacije z realistično odvisnostjo med igrami znotraj iste seje (shoe, penetracija, card counting), pri čemer so posamezne simulirane seje med sabo neodvisne, kar zadošča za Monte Carlo ocenjevanje pričakovanih vrednosti in porazdelitev.
Projekt omogoča analizo house edge, ROI, volatilnosti, bankroll poteka in učinkov uporabe strategij.
Vsa ključna logika projekta se nahaja v mapi R/, kjer so shranjene funkcije in ostala pomembna koda. To kodo uporabljamo v naslednjih pomembnih R skriptah:

-> main.R – skripta za "testerja"; služi kot osnovna točka za zagon in preverjanje delovanja.

-> simulacije.R – skripta za izvajanje simulacij ter zbiranje metrik.

-> grafi.R – skripta za vizualizacijo zbranih podatkov.

Mapa tests/ vsebuje kodo za test-driven development, kjer so implementirani testi za preverjanje pravilnosti delovanja funkcij iz mape R.

---
## Struktura projekta

├── BJ_skeleton.R       # nepomembna skripta (vse kar je tu in še več je v mapi R) \
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
├── results_all.csv         # tabela  z vsemi rezultati izvajanih MC simulacij  \
│  \
├── main.R                  # enotna vstopna točka (source vseh skript)  \
├── simulacije.R            # izvajanje simulacij za zbiranje metrik za analizo in risanje grafov \
├── grafi-R                 # \
│    \
│    \
├── tests/                  \
│   ├── testthat.R          #   \
│   ├── testthat/           #  \
│    \
│    \
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
OPOZORILO: can_split ni polno implementiran, nedovoljujemo (RS) ReSplit in (DAS) DoubleAfterSplit.

### Parametri (povzetek)

**N**: št. iger v simulaciji (Monte Carlo ponovitve)

**n_decks**: število kompletov kart v »shoe«

**penetration**: delež premešanih kart, preden se shoe ponovno premeša (npr. 0.75)

**hit_soft_17**: delivec vleče na mehki 17 (TRUE = H17; FALSE = S17)

**bet**: osnovna stava na hand

**payout_bj**: izplačilo za naravni blackjack (običajno 1.5 = 3:2; lahko 1.2 = 6:5)

**can_double, can_split, can_surrender**: (TRUE = pravila omogočena; FALSE = pravila onemogočena)

## DODATNO

V scripti simulacije.R so vse simulacije, ki sem jih izvajal, s pomočjo katerih sem zbral metrike za primerjavo in prikaz grafov, ki jih generiram v skripti grafi.R. simulacije.R je zaenkrat (bo morda odpravljeno) zelo neurejena in nametana, ker ni namenjena za ocenjevanje, kodo lahko poganja vsak s svojimi parametri kakor je opisano zgoraj.
