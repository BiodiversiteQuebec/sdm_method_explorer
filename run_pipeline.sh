#!/bin/bash
# run_pipeline.sh

module load StdEnv/2023 gcc/12.3 r/4.5.0 geos/3.12.0 gdal/3.9.1 udunits/2.2.28 gsl/2.7 jags/4.3.2
# Step 1: run R on login node to get string

REPO=$(Rscript scripts/sdm_repo.R)
echo "REPO: $REPO"

LABEL="This is an example label"
echo "LABEL: $LABEL"

# Step 2: submit main job, capture its ID
JOBID=$(sbatch --parsable --export=ALL,REPO="$REPO",LABEL="$LABEL" job.sh)
echo "Submitted job.sh as $JOBID"

# Step 3: submit final job inline, runs after job.sh terminates no matter what
sbatch --dependency=afterany:$JOBID <<'EOF'
#!/bin/bash

#SBATCH --account=rpp-gonzalez
#SBATCH --time=00:20:00
#SBATCH --mem=8G
#SBATCH --cpus-per-task=1
#SBATCH --job-name=sdm_metadata_json

module load StdEnv/2023 gcc/12.3 r/4.5.0 geos/3.12.0 gdal/3.9.1 udunits/2.2.28 gsl/2.7 jags/4.3.2
Rscript scripts/sdm_json.R
EOF