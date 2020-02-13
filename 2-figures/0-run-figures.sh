#!/bin/bash

# Remove previous results
cd "/Users/jadederong/Documents/CRG/flu/surveillance/results/figures/"
rm -f \
"flu_hosprate_week_allpre.pdf" "flu_hosprate_week.pdf" \
"fluhosp_did_race.pdf" \
"fluhosp_did_seas.pdf" \
"fluhosp_did.pdf" \
"fluhosp_irr.pdf" \
"flu_mort_to201718.pdf"  "flu_icu_to201718.pdf"\
"flu_hosp_did_sens.pdf" "flu_hosp_diff_sens.pdf" "flu_hosp_irr_sens.pdf" "flu_hosp_ir_sens.pdf" \
"fluhosp_los_did.pdf" "fluhosp_los_outlier_did.pdf" \
"fig-cdc-ve.pdf" \

# Run folder scripts and produce output
cd "/Users/jadederong/Documents/CRG/flu/surveillance/3-figures/"

R CMD BATCH eip-hosp-time.R
R CMD BATCH eip-ie-fig-did-race.R
R CMD BATCH eip-ie-fig-did-seas.R
R CMD BATCH eip-ie-fig-did.R
R CMD BATCH eip-ie-fig-irr.R
R CMD BATCH eip-ie-fig-rates-mort-icu.R
R CMD BATCH eip-ie-fig-sens.R
R CMD BATCH eip-los-fig-did.R
R CMD BATCH fig-cdc-ve.R