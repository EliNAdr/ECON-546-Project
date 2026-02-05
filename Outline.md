## Data
Frequency: monthly
Timeframe: 2014-2019 (include Covid with control for robustness)
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
