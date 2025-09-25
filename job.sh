#!/bin/bash
#SBATCH --array=1-860%100
#SBATCH --account=rpp-gonzalez
#SBATCH --time=02:30:00
#SBATCH --cpus-per-task=2
#SBATCH --mem=200G
#SBATCH --job-name=ebv_pffq_plants_test
##SBATCH --mail-user=francois.rousseu@usherbrooke.ca
##SBATCH --mail-type=ALL

echo $SLURM_ARRAY_TASK_ID
module load StdEnv/2023 gcc/12.3 r/4.5.0 geos/3.12.0 gdal/3.9.1 udunits/2.2.28 gsl/2.7 jags/4.3.2
export JAVA_TOOL_OPTIONS="-Xmx200g" # for Maxent
#export NODE_OPTIONS="--max-old-space-size=102400" # for concaveman V8 engine
Rscript scripts/sdm_runs.R ebv_pffq_plants.R

# ls results/graphics | grep -E "Pseudacris_triseriata|Hemidactylium_scutatum|Gyrinophilus_porphyriticus|Desmognathus_ochrophaeus|Emydoidea_blandingii|Glyptemys_insculpta|Nerodia_sipedon|Lampropeltis_triangulum|Aquila_chrysaetos|Catharus_bicknelli|Setophaga_cerulea|Coturnicops_noveboracensis|Ixobrychus_exilis|Glaucomys_volans"

# ssh frousseu@rorqual.alliancecan.ca 'ls /home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics | grep -E "Pseudacris_triseriata|Hemidactylium_scutatum|Gyrinophilus_porphyriticus|Desmognathus_ochrophaeus|Emydoidea_blandingii|Glyptemys_insculpta|Nerodia_sipedon|Lampropeltis_triangulum|Aquila_chrysaetos|Catharus_bicknelli|Setophaga_cerulea|Coturnicops_noveboracensis|Ixobrychus_exilis|Glaucomys_volans"' | xargs -I{} scp frousseu@rorqual.alliancecan.ca:{} ./destination/

#find "$PWD/results/graphics" | grep -E "Aquila_ch|Catharus_bi"

#find /home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics

# ssh frousseu@rorqual.alliancecan.ca 'find /home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics | grep -E "Pseudacris_triseriata|Hemidactylium_scutatum|Gyrinophilus_porphyriticus|Desmognathus_ochrophaeus|Emydoidea_blandingii|Glyptemys_insculpta|Nerodia_sipedon|Lampropeltis_triangulum|Aquila_chrysaetos|Catharus_bicknelli|Setophaga_cerulea|Coturnicops_noveboracensis|Ixobrychus_exilis|Glaucomys_volans"' | xargs -I{} scp frousseu@rorqual.alliancecan.ca:{} ./destination/

# ssh frousseu@rorqual.alliancecan.ca 'find /home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics | grep -E "Pseudacris_triseriata"' | xargs -I{} scp frousseu@rorqual.alliancecan.ca:{} /home/frousseu/Downloads/test

# ssh frousseu@rorqual.alliancecan.ca 'find /home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics -type f | grep -E "Pseudacris_triseriata"' | rsync -av --files-from=- frousseu@rorqual.alliancecan.ca:/ /home/frousseu/Downloads/test/

# ./s5cmd --dry-run --numworkers 8 cp -acl public-read --sp '/home/frousseu/links/projects/rpp-gonzalez/frousseu/niches_climatiques/results/graphics/niches/*.png' s3://bq-io/niches_climatiques/figures/