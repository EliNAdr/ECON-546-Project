## Data
Frequency: monthly
Timeframe: 1992-2019 (include Covid with control for robustness)
Sources: FRED, Statscan, World Bank, BIS

## Baseline Model
Type: VAR
Lags: 12 lags
Variables: overnight rate, CPI-median, log GDP, unemployment
Identification strategy: Impact restriction (Cholesky); log rGDP, CPI-median, unemployment, overnight rate

## Extended Model
Type: VAR-X or BVAR
Lags: optimal selection
Variables: baseline, commodity prices/oil, exchange rate, inflation expectations
Identification strategy: 

## Considerations for specifications
6 vs 12 lags
use CPI and CPI median
Include COVID and restricted samples
-inflation target period 1992-93 so on
-s&p index in the US and tsx/sp in Canada
-excess bond premium from Gilchrist and Zakrejsek (2012):
"micro-level aspect of the data to decompose our high–information content credit spread index into two components: a component that captures the systematic     movements in default risk of individual firms and a residual component—the excess bond premium" 
  - One part reflects real firm risk and the other part (EBP) reflects tightness of financial conditions

MAKE SURE VARIABLES ARE STATIONARY!! if not take first differences 
