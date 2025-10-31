#!/bin/bash
# run_pipeline.sh

module load StdEnv/2023 gcc/12.3 r/4.5.0 geos/3.12.0 gdal/3.9.1 udunits/2.2.28 gsl/2.7 jags/4.3.2
# Step 1: run R on login node to get string
REPO=$(Rscript scripts/sdm_repo.R)
echo "Got parameter: $REPO"

# Step 2: submit job with env var
sbatch --export=ALL,REPO="$REPO" job.sh