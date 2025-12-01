# Igra-na-sreco-Blackjack

Ta projekt implementira **Monte Carlo simulacije Blackjacka** z uporabo:

- osnovne strategije (S17/H17),
- CSV tabel strategij,
- podporo za *surrender*, *double*, *pair splitting* (po potrebi),
- realistično igro z *shoe*, penetracijo in mešanjem,
- napredno verzijo z **Hi–Lo štetjem kart** in **bet spread**.

Projekt omogoča analizo house edge, ROI, volatilnosti, bankroll poteka in učinkov uporabe strategij.

---

## 🏁 Kako pognati kodo (bom cim odpravm se eno pomankljivost v kodi XD)

├── BJ_skeleton.R       # ogrodna verzija za testiranje/demos
├── R/
│   ├── 01_cards.R          # definicije kart, helper funkcije
│   ├── 02_shoe.R           # shoe, penetracija, reshuffling
│   ├── 03_strategy.R       # osnovna strategija (CSV S17/H17)
│   ├── 04_play_hand.R      # igralčeve poteze, dealerjeva logika
│   ├── 05_simulation.R     # vse Monte Carlo simulacije
│   ├── DEBUG_TRASH.R       # dodatni debug skript (ne vpliva na simulacije)
│   
│
├── basic_strategy.csv      # tabela osnovne strategije za S17
├── basic_strategy_H17.csv  # tabela osnovne strategije za H17
│
├── main.R                  # enotna vstopna točka (source vseh skript)
├── porocilo.Rmd            # glavno poročilo
├── LICENSE
└── README.md

