# MLB All-Stars and Attendance Analysis

This is a reproducible sports analytics project evaluating whether superstar players (WAR ≥ 5) increase MLB game-level attendance beyond what can be explained by winning percentage, payroll, and market size.

The analysis uses 2023–2025 MLB data across all 30 teams (90 team-season observations) and applies multiple statistical and machine learning models to assess whether star concentration creates measurable attendance ROI.

---

## Project Objectives

Determine whether investing in one or more superstar players meaningfully increases team attendance after controlling for:
* Winning percentage (`w_l`)
* Payroll
* Market size (metro population)

Key business question:
Should teams invest heavily in one all-star, or distribute payroll across a more balanced roster to maximize attendance?

---

## Data Sources

1. Baseball Reference — Game-level attendance data
2. FanGraphs and ESPN — Player WAR and team payroll
3. U.S. Census–based metro population estimates

Each team-season observation includes:
* `attend_game` (average game attendance)
* `w_l` (winning percentage)
* `stars_per_team` (WAR ≥ 5 players)
* `payroll`
* `population`

---

## Methodologies

The following models were implemented in R:
1. Linear Regression
2. LASSO Regression
3. Decision Tree
4. Hierarchical Clustering
5. XGBoost with SHAP Values

---

## Key Findings

### 1. Payroll Is the Strongest Attendance Driver
* $10M increase in payroll → ~1,680 additional fans per game
* Statistically significant at the 95% confidence level
* Most influential variable in decision tree and XGBoost with SHAP value models

---

### 2. Winning Percentage Matters
* 5% increase in win percentage → ~1,214 more fans per game
* Significant across regression and ML models
* Drives attendance more than star concentration

---

### 3. Star Players Do Not Independently Increase Attendance
* `stars_per_team` not statistically significant
* Minimal marginal impact in SHAP value analysis
* Cluster models group teams by payroll and win percentage, not star count
* Star players create value through improved team performance — not direct attendance lift.

---

## Strategic Implications

### For Small-Market Teams
Two potential strategies emerge:

1. Competitive Window Strategy
* Rebuild and accumulate payroll flexibility
* Concentrate spending in pennant-contending seasons

2. Stability Strategy
* Maintain consistent win-loss performance
* Avoid extreme payroll spikes
* Sustain steady attendance levels

Signing a single marquee star without improving overall competitiveness is unlikely to greatly boost attendance.

---

### For Large-Market Teams
Attendance advantage appears driven by:
* Payroll capacity
* Sustained competitiveness

---

## Repository Contents
* `02.26_mlb_superstars.Rmd` — Full modeling workflow
* `02.26_base_data.xlsx` — Compiled dataset
* Final report (.docx and .pptx)
* Visualizations and model outputs

---

## Final Insights
Teams with multiple all-stars often have high attendance, but once payroll and winning percentage are controlled, star count no longer predicts fan turnout.
Championship teams have stars, and attendance-maximizing teams have balanced, competitive rosters.

---
