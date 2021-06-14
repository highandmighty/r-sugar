# r-sugar
Personal R helper functions

### Usage
```
devtools::install_github('highandmighty/r-sugar')
library(sugarr)

> ?fetch_airtable
> ?download_feather
> ?download_parquet
```

### Functions
- `download_feather(url)` — fetches Feather file from URL into dataframe
- `download_parquet(url)` — fetches Parquet file from URL into dataframe
- `fetch_airtable(base, table, token, query)` — fetches data from Airtable API into dataframe
