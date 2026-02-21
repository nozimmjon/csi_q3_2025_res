# CSI Q3 2025 — Consumer Sentiment Index, Republic of Uzbekistan

Consumer Sentiment Index (CSI) analysis pipeline for the Q3 2025 national
survey. Processes raw phone-survey data from 14 regions, applies survey
weights, and produces publication-ready tables.

---

## Project Overview

| Item | Detail |
|------|--------|
| Survey period | Q3 2025 (July–September) |
| Method | Computer-Assisted Telephone Interviewing (CATI) |
| Coverage | All 14 regions (viloyats) + Republic of Karakalpakstan |
| Sample frame | 206 districts (tumans), stratified random sampling |
| Total raw responses | ~18,764 |
| Analytic N (after cleaning) | ~18,100 (varies by exclusion criteria) |
| Weighting | Base weights + post-stratification by district x age x gender |
| Output | Multi-sheet Excel workbook with frequency tables, CSI index, etc. |

---

## Directory Structure

```
csi_q3_2025_res/
|
|-- data/                          # Raw survey data (14 regional .xlsx files)
|   |-- andijon.xlsx
|   |-- buxoro.xlsx
|   |-- fergana.xlsx
|   |-- jizzax.xlsx
|   |-- karakalpak.xlsx
|   |-- kashkadaryo.xlsx
|   |-- namangan.xlsx
|   |-- navoiy.xlsx
|   |-- samarkand.xlsx
|   |-- sirdaryo.xlsx
|   |-- surxondaryo.xlsx
|   |-- tashkent_city.xlsx
|   |-- toshkent.xlsx
|   |-- xorazm.xlsx
|
|-- script/                        # Analysis pipeline
|   |-- 00_ingest.R                # Step 0: Read, validate, merge raw data
|   |-- 01_clean.R                 # Step 1: Rename, validate, deduplicate
|   |-- 02_weights.R               # Step 2: Survey weights + design object
|   |-- 03_tables.R                # Step 3: Generate all output tables
|   |-- 04_q10.R                   # Step 4: Open-ended text categorization
|   |-- 99_run_all.R               # Master runner (sources steps in order)
|   |
|   |-- 00_ingest_merge.R          # (legacy) Original ingest script
|   |-- 01_standardize_reshape.R   # (legacy) Original cleaning script
|   |-- 03_weights_design.R        # (legacy) Original weighting script
|   |-- 04_tables_simple.R         # (legacy) Original table generation
|   |-- 04_q10_regex_tables.R      # (legacy) Original Q10 regex script
|   |-- 000_reading_data.R         # Standalone diagnostic tool (not in pipeline)
|
|-- checkpoint/                    # Intermediate .rds files (auto-created)
|   |-- 00_raw.rds
|   |-- 01_clean.rds
|   |-- 02_design.rds
|
|-- tables/                        # Output (auto-created)
|   |-- all_survey_data.xlsx
|
|-- sample_size.xlsx               # District population + sample size reference
|-- population_age_group.xlsx      # Population by district x age x gender
|-- Survey_Data_Dictionary_v2.xlsx # Variable definitions, choice lists, conventions
|-- q3_2025_republic upd.Rproj    # RStudio project file
```

---

## How to Run

### Prerequisites

R >= 4.1 with the following packages:

```r
install.packages(c(
  "readxl", "dplyr", "stringr", "tidyr", "here",
  "survey", "srvyr", "writexl", "rlang", "scales"
))
```

### Full Pipeline

Open the `.Rproj` in RStudio (or set working directory to the project root),
then:

```r
source("script/99_run_all.R")
```

This runs all steps in order and writes `tables/all_survey_data.xlsx`.

### Restart from Checkpoint

Each step saves an `.rds` file in `checkpoint/`. To skip earlier steps:

```r
library(here)

# Example: restart from weights (skip ingest + clean)
d_clean     <- readRDS(here("checkpoint", "01_clean.rds"))
code_to_name <- readRDS(here("checkpoint", "01_clean.rds")) # loaded via 01_clean.R
source(here("script", "02_weights.R"))
source(here("script", "04_q10.R"))
source(here("script", "03_tables.R"))
```

---

## Pipeline Steps in Detail

### Step 0: Ingest (`00_ingest.R`)

**Input:** 14 `.xlsx` files in `data/`
**Output:** `df_raw` (character data frame), `checkpoint/00_raw.rds`

- Reads all regional Excel files with `readxl::read_excel(col_types = "text")`
- Standardizes column headers (lowercase, trim, collapse whitespace)
- Drops trailing empty/placeholder columns (named `...1`, `...2`, etc.)
- Keeps only the first 17 core columns per the data dictionary
- Adds `source_file` column for provenance tracking
- Aligns column names across files and row-binds into one data frame

### Step 1: Clean (`01_clean.R`)

**Input:** `df_raw`
**Output:** `d_clean`, `code_to_name`, `checkpoint/01_clean.rds`

1. **Rename** columns to English short names (see Variable Dictionary below)
2. **Map district codes** to text names using `sample_size.xlsx` lookup
3. **Drop** rows with unmappable/missing district
4. **Deduplicate** by `phone_number` (keeps first occurrence)
5. **Normalize** text variants (`Йуқ` -> `Йўқ`, district name harmonization)
6. **Exclude** specific districts (`Давлатобод`, `Ғозғон шаҳри`) and invalid genders
7. **Parse** numeric fields: `age`, `income`, `tuman_hokimi`, `viloyat_hokimi`
8. **Validate** age [18, 120] and hokim scores [1, 10] (out-of-range -> NA)
9. **Create** `age_group` factor matching `population_age_group.xlsx` breaks
10. **Print** full exclusion audit trail with retention rate

### Step 2: Weights (`02_weights.R`)

**Input:** `d_clean`, `code_to_name`
**Output:** `svy_design`, `sample_data_weighted`, `checkpoint/02_design.rds`

Methodology:

| Stage | Description |
|-------|-------------|
| Base weight | `w = pop_total / n_sampled` per district (inverse probability) |
| Design declaration | `svydesign(ids=~1, strata=~district, fpc=~pop_total)` |
| Post-stratification | `postStratify(~ district + age_group + gender)` using population_age_group.xlsx |
| Weight trimming | Cap at 95th percentile, re-normalize to preserve population total |
| Lonely PSU | `options(survey.lonely.psu = "adjust")` |

Diagnostics printed:
- N respondents, sum/mean/median/min/max of weights
- Coefficient of variation (CV) of weights
- Design effect due to unequal weighting: `deff = 1 + CV^2`
- Strata with fewer than 10 observations

### Step 3: Tables (`03_tables.R`)

**Input:** `svy_design`, optionally `q10_by_region_wide`
**Output:** `tables/all_survey_data.xlsx`

Generates 25+ sheets:

| Sheet group | Sheets | Description |
|------------|--------|-------------|
| Q1-Q6 frequency (regional) | `q1_regional` ... `q6_regional` | Weighted % by region, plus republic average |
| Q1-Q6 frequency (district) | `q1_district` ... `q6_district` | Weighted % by district, plus viloyat average |
| Balance scores | `bs_regional`, `bs_district`, `bs_district_scores` | CSI index: overall, current situation, expectations |
| Hokim evaluation | `eval_region`, `eval_district` | Mean score (1-10) for regional/district hokims |
| Employment | `employment_status_regional/district` | Working/not-working % (excl. retirees) |
| Formal employment | `formal_employment_regional/district` | Formal/informal % among employed |
| Q7 problems | `q7_regional`, `q7_district` | Main local problems (open-ended, categorized) |
| Income | `income_regions`, `income_tumanlar` | Income bracket distribution |
| Q10 changes | `q10_by_region_percentages` | Positive changes (regex-categorized, optional) |

### Step 4: Q10 Open-Ended (`04_q10.R`)

**Input:** `svy_design`
**Output:** `q10_by_region_wide`, `q10_overall`

Categorizes free-text Q7 responses into 22 thematic buckets (e.g., "Drinking
water improved", "Road quality improved") using regex pattern matching on
normalized Uzbek Cyrillic text. Produces weighted percentage tables both
overall and by region.

---

## Reference Data Files

### `sample_size.xlsx`

206 rows, one per district. Columns:

| Column | Type | Description |
|--------|------|-------------|
| `code` | integer | 7-digit administrative STATCODE (e.g. 1703401) |
| `district` | text | District name in Uzbek Cyrillic (e.g. Андижон ш.) |
| `sample_size` | numeric | Target sample size (fractional, calculated) |
| `base_weight` | numeric | Pre-calculated base weight |
| `population` | numeric | District population **in thousands** |

### `population_age_group.xlsx`

1,648 rows (= 206 districts x 2 genders x 4 age groups). Columns:

| Column | Type | Description |
|--------|------|-------------|
| `district_id` | integer | 7-digit STATCODE (matches `sample_size.xlsx$code`) |
| `district_name` | text | District name in Uzbek Latin script |
| `gender` | text | `Аёл` (Female) or `Эркак` (Male) — Cyrillic |
| `age_group` | text | `18-29`, `30-49`, `50-65`, `Over 65` |
| `population` | integer | Population count (actual persons) |
| `district` | text | **Broken** — contains unevaluated VLOOKUP formulas. Do not use. |

### `Survey_Data_Dictionary_v2.xlsx`

Five sheets:

| Sheet | Content |
|-------|---------|
| `variables` | 18 variable definitions: name, Uzbek/English labels, type, valid values, skip logic |
| `choices_q7` | 11 code options for Q7 (multi-select: positive changes observed) |
| `choices_q8` | 22 code options for Q8 (single-select: biggest local problem) |
| `crosswalk` | Maps English short names to raw Uzbek column headers |
| `conventions` | Special codes: -97=Don't know, -96=Refused, -98=N/A skip, -99=Missing |

---

## Variable Dictionary

The 17 core columns from the raw survey data, in positional order:

| # | Pipeline name | Raw Uzbek header | Type | Valid values |
|---|--------------|------------------|------|-------------|
| 0 | `phone_number` | Телефон рақам | string | Phone number (identifier) |
| 1 | `gender` | Жинси | text | Эркак (Male), Аёл (Female) |
| 2 | `region` | Вилоят/шаҳар | text | 14 region names |
| 3 | `district_code` | Яшаш манзилингиз (туман) | integer | 7-digit STATCODE |
| 4 | `age` | Ёшингиз | integer | 18–80 per dictionary |
| 5 | `is_working` | Ишлайсизми | text | Ҳа (Yes), Йўқ (No), Пенсиядаман (Retired) |
| 6 | `is_official` | Расмийлаштирилганми | text | Ҳа (Yes), Йўқ (No); skip if not working |
| 7 | `q1` | Q1: Иқтисодий ҳолат (3 ой) | text | Яхшиланади / Ўзгармайди / Ёмонлашади |
| 8 | `q2` | Q2: Даромад ўзгариши | text | Ошди / Ўзгармади / Пасайди |
| 9 | `q3` | Q3: Даромад кутилмаси | text | Кўпаяди / Ўзгармайди / Қисқаради |
| 10 | `income` | Бир ойлик даромад | numeric/text | UZS amount or text refusal |
| 11 | `q4` | Q4: Иш ўринлари ўзгариши | text | Кўпайди / Ўзгармади / Камайди |
| 12 | `q5` | Q5: Иш ўринлари кутилмаси | text | Кўпаяди / Ўзгармайди / Камаяди |
| 13 | `q6` | Q6: Товар сотиб олиш қулайми | text | Ҳа / Йўқ / Билмайман |
| 14 | `q7` | Q7: Ҳудуддаги муаммо/ўзгариш | text | Open-ended text |
| 15 | `tuman_hokimi` | Q9: Туман ҳокими баҳоси | integer | 1–10 |
| 16 | `viloyat_hokimi` | Q10: Вилоят ҳокими баҳоси | integer | 1–10 |

**Derived columns** (created during processing):

| Name | Created in | Description |
|------|-----------|-------------|
| `district` | 01_clean.R | Text district name (mapped from `district_code`) |
| `age_group` | 01_clean.R | Factor: 18-29, 30-49, 50-65, Over 65 |
| `source_file` | 00_ingest.R | Original filename for provenance |
| `base_weight` | 02_weights.R | Inverse probability weight |
| `final_weight` | 02_weights.R | Post-stratified, trimmed weight |
| `income_group` | 03_tables.R | Income bracket for tabulation |

---

## CSI Balance Score Methodology

The Consumer Sentiment Index uses a **diffusion index** (balance score):

```
BS = (share_positive - share_negative) x 100 + 100
```

| Value | Interpretation |
|-------|---------------|
| 200 | All respondents gave the positive answer |
| 100 | Equal share of positive and negative (neutral) |
| 0 | All respondents gave the negative answer |

Three composite indices are calculated:

| Index | Components | Interpretation |
|-------|-----------|----------------|
| **Жорий ҳолат** (Current situation) | avg(Q2_bs, Q4_bs, Q6_bs) | How things are now |
| **Кутилмалар** (Expectations) | avg(Q1_bs, Q3_bs, Q5_bs) | How things will be |
| **Умумий индекс** (Overall CSI) | avg(Current, Expectations) | Composite index |

Question mapping for balance scores:

| Question | Positive | Negative |
|----------|----------|----------|
| Q1 (economy outlook) | Яхшиланади (Will improve) | Ёмонлашади (Will worsen) |
| Q2 (income change) | Ошди (Increased) | Пасайди (Decreased) |
| Q3 (income expect) | Кўпаяди (Will increase) | Қисқаради (Will decrease) |
| Q4 (jobs change) | Кўпайди (Increased) | Камайди (Decreased) |
| Q5 (jobs expect) | Кўпаяди (Will increase) | Камаяди (Will decrease) |
| Q6 (durable goods) | Ҳа (Yes, good time) | Йўқ (No) |

---

## Weighting Methodology

### Sampling Design

- **Type:** Stratified random sample
- **Strata:** 206 districts (tumans)
- **Sampling unit:** Individual (phone interview)
- **Frame:** Phone number lists per district
- **No clustering:** Each respondent is independently selected

### Weight Construction

1. **Base weight** = `population_district / n_interviewed_district`
   - Compensates for unequal selection probabilities across districts
   - `population` from `sample_size.xlsx` (in thousands, multiplied by 1000)

2. **Post-stratification** adjusts the base-weighted design to match known
   population margins by **district x age_group x gender** using
   `population_age_group.xlsx`. This corrects for differential non-response
   (e.g., if young males are underrepresented in a district).

3. **Weight trimming** caps extreme weights at the 95th percentile and
   re-normalizes remaining weights so the weighted total is preserved.
   This limits the influence of outlier observations.

4. **Finite population correction (FPC)** is applied when the sampling
   fraction within a district is large, improving standard error estimates.

### Key Diagnostics

- **CV of weights** — measures how unequal the weights are. CV > 1 indicates
  substantial variation.
- **Design effect** = `1 + CV^2` — the factor by which the effective sample
  size is reduced due to unequal weighting. A deff of 1.5 means the weighted
  sample is equivalent to an unweighted sample of `N / 1.5`.

---

## Income Brackets

| Bracket (Uzbek) | Range (UZS) |
|-----------------|-------------|
| Даромади мавжуд эмас | 0 (no income) |
| 1 млн сўмгача | 1 – 1,000,000 |
| 1-3 млн | 1,000,001 – 3,000,000 |
| 3 млн сўмдан баланд | > 3,000,000 |

---

## Known Data Issues

1. **`population_age_group.xlsx` has a broken `district` column** that contains
   unevaluated Excel VLOOKUP formulas. The pipeline uses `district_id`
   (numeric STATCODE) instead and maps it to text names via `sample_size.xlsx`.

2. **Raw district values are numeric STATCODEs**, not text names. The pipeline
   converts them using the `code_to_name` lookup in `01_clean.R`.

3. **Some files have extra trailing columns** (up to 24 total). These are empty
   placeholder columns from Excel formatting and are dropped during ingest.

4. **Text encoding variants** exist for Йўқ/Йуқ across regional files.
   Normalized in `01_clean.R`.

5. **District name changes:** Several districts were renamed between the
   survey design and data collection. Harmonized via `case_match()` in
   `01_clean.R` (e.g., Бўз -> Бўстон, Хаваст -> Ховос).

6. **Two districts are excluded** from analysis: Давлатобод and Ғозғон ш.

---

## R Package Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| readxl | >= 1.4 | Read .xlsx files |
| dplyr | >= 1.1 | Data manipulation |
| tidyr | >= 1.3 | Pivot/reshape |
| stringr | >= 1.5 | String operations |
| here | >= 1.0 | Project-relative paths |
| survey | >= 4.2 | Survey design, postStratify, svydesign |
| srvyr | >= 1.2 | Tidyverse-friendly survey wrappers |
| writexl | >= 1.4 | Write .xlsx output |
| rlang | >= 1.1 | Tidy evaluation helpers |
| scales | >= 1.3 | Percentage formatting (04_q10.R) |
