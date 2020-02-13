#!/bin/bash

# Remove previous results
cd "/Users/jadederong/Documents/CRG/flu/surveillance/results/tables/"
rm -f "fluhosp_table_did.csv" "fluhosp_table_rdid.csv" "fluhosp_table_rir.csv"

# Run folder scripts and produce output
cd "/Users/jadederong/Documents/CRG/flu/surveillance/4-tables/"

R CMD BATCH eip-ie-tab-diff.R
R CMD BATCH eip-ie-tab-reldiff.R
R CMD BATCH eip-ie-tab-rrr.R
R CMD BATCH eip-tab-compare-acs-chars.R

