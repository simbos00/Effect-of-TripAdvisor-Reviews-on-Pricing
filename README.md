# Effect of TripAdvisor Reviews on the Pricing of Hotels in Rome
 

# Overview
This project investigates the economic impact of online reviews in the hospitality sector. It's natural to think that high ratings equal high prices, but establishing a causal link is challenging.
 
This analysis aims to answer the question: How much does crossing the 4.0 to 4.5 "bubble" rating threshold on TripAdvisor causally impact the price of a hotel in Rome?

The findings are relevant for hotel managers in making pricing and reputation management decisions, and for consumers seeking to understand price drivers.

# Methods
Regression Discontinuity Design.

## Pros

Leverages a "natural experiment" created by TripAdvisor's rounding mechanism (e.g., a score of 4.24 rounds to 4.0 bubbles, while 4.25 rounds to 4.5).
By comparing hotels just below and just above the 4.25 score cutoff, we can isolate the effect of the rating change, as these hotels are likely very similar in all other aspects.

Strong internal validity: the assumptions of the RD design are tested and verified.

## Cons

The effect is only estimated locally around the cutoff and may not apply to other rating thresholds (e.g., 3.5 vs 4.0).

Weak external validit: the city of Rome may not generalize to other markets and the results on the TripAdvisor platform may differ than other platoforms' results.


# Dataset
The dataset was created by scraping hotel data from TripAdvisor for Rome in December 2019. It was then cleaned to prepare it for the RDD analysis.
Source: https://data.mendeley.com/datasets/6t6nv9z9mm/1


# Requirements
- R
- R libraries: tidyverse, readr, rdd, rddtools, rddensity, magrittr, rdrobust

# How to Run
1. Clone this repository.
2. Download the dataset (https://data.mendeley.com/datasets/6t6nv9z9mm/1)
3. Open the 'script.R' file in RStudio or any R environment.
4. Install the required packages.
5. At line 2 insert the directory of the dataset
6. Run the script.

# Results
Conclusions: The analysis finds no statistically significant causal effect on hotel prices from crossing the 4.0 to 4.5 bubble rating threshold. The estimates are not significant, even after testing multiple model specifications, polynomial orders, and bandwidths.

This suggests that this specific 0.5-star difference may not be perceived as a strong enough quality signal for immediate price adjustments by consumers or businesses. More important factors (e.g., location, brand) might be stronger drivers of pricing at this margin. We could also consider management choices: pricing adjustments related to ratings might be less gradual or based on broader metrics.