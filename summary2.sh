#!/usr/bin/env bash
# Usage: seff_summary.sh JOBID [JOBID2 ...]
# Summarizes CPU, memory, and state for all array tasks using seff output

if [ "$#" -lt 1 ]; then
  echo "Usage: $0 JOBID [JOBID2 ...]"
  exit 1
fi

# Initialize totals
jobs=0
memtot=0
cputot=0
declare -A states

for main_job in "$@"; do
  # Get all array tasks (if any)
  job_list=$(sacct -j "$main_job" --format=JobIDRaw --noheader | grep '_[0-9]\+' | sort -u)

  # If no array tasks, just use the main job
  [ -z "$job_list" ] && job_list="$main_job"

  for job in $job_list; do
    # Extract info from seff
    info=$(seff "$job" 2>/dev/null)
    if [ -z "$info" ]; then
      echo "$job: seff failed or job not found"
      continue
    fi

    # Parse fields correctly
    state=$(echo "$info" | awk -F: '/State:/ {gsub(/^[ \t]+/,"",$2); print $2}')
    elapsed=$(echo "$info" | awk -F: '/ElapsedTime:/ {gsub(/^[ \t]+/,"",$2); print $2}')
    cpu=$(echo "$info" | awk -F: '/CPU Utilized:/ {gsub(/[^0-9.]/,"",$2); print $2}')
    mem=$(echo "$info" | awk -F: '/Memory Utilized:/ {gsub(/[^0-9.]/,"",$2); print $2}')

    # Print per-job line
    printf "%s\tElapsed=%s\tMem=%s%%\tCPU=%s%%\tState=%s\n" "$job" "$elapsed" "$mem" "$cpu" "$state"

    # Accumulate totals
    jobs=$((jobs+1))
    memtot=$(awk -v a="$memtot" -v b="$mem" 'BEGIN{printf "%.4f", a+b}')
    cputot=$(awk -v a="$cputot" -v b="$cpu" 'BEGIN{printf "%.4f", a+b}')
    states["$state"]=$((states["$state"]+1))
  done
done

# Print summary
if [ "$jobs" -gt 0 ]; then
  avgmem=$(awk -v a="$memtot" -v n="$jobs" 'BEGIN{printf "%.1f", a/n}')
  avgcpu=$(awk -v a="$cputot" -v n="$jobs" 'BEGIN{printf "%.1f", a/n}')
  summary="SUMMARY\tJobs=$jobs\tAvgMem=${avgmem}%\tAvgCPU=${avgcpu}%"
  for s in "${!states[@]}"; do
    summary="${summary}\t${s}=${states[$s]}"
  done
  echo -e "$summary"
fi
