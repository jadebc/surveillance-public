#!/bin/bash

# Remove previous results
cd "/Users/jadederong/Documents/CRG/flu/surveillance/results/results-data/"
rm -f \
"flu_surv_differences.RDS" \
"flu_surv_differences_race.RDS" \
"flu_surv_ratios.RDS" \
"los_differences_outlier.RDS"

# Run folder scripts and produce output
cd "/Users/jadederong/Documents/CRG/flu/surveillance/2-analyze/"

R CMD BATCH 1-est-DID.R
R CMD BATCH 2-est-DID-race.R
R CMD BATCH 3-est-ratios.R
R CMD BATCH 4-est-diff-length.R
