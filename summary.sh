#!/usr/bin/env bash
# Usage: sacct_summary.sh JOBID
# Mimics seff: reports memory %, CPU utilization, elapsed, and job state per job step

if [ -z "$1" ]; then
  echo "Usage: $0 JOBID"
  exit 1
fi

JOBID=$1

sacct -j "$JOBID" \
  --format=JobID,JobName,State,Elapsed,MaxRSS,ReqMem,AllocCPUs,CPUTimeRAW \
  --parsable2 | \
awk -F'|' '
  # Save parent/erosion line (requested memory, elapsed, state)
  $2=="erosion" {
    jobid=$1; sub(/\..*/,"",jobid)
    parent_elapsed[jobid]=$4
    parent_reqmem[jobid]=$6
    parent_state[jobid]=$3
  }

  # Batch line -> actual usage
  $2=="batch" {
    jobid=$1; sub(/\..*/,"",jobid)

    # MaxRSS in GB
    maxrss=$5; sub(/K$/,"",maxrss)
    maxrss_gb = maxrss / 1024 / 1024

    # Requested memory from parent
    reqmem=parent_reqmem[jobid]; sub(/G$/,"",reqmem)
    reqmem_gb=reqmem

    # % memory usage
    mempct = (reqmem_gb>0) ? (maxrss_gb/reqmem_gb*100) : 0

    # Elapsed time in seconds from parent
    elapsed_str=parent_elapsed[jobid]
    n=split(elapsed_str,t,":")
    if (n==3)      elapsed_sec = t[1]*3600 + t[2]*60 + t[3]
    else if (n==2) elapsed_sec = t[1]*60 + t[2]
    else           elapsed_sec = t[1]

    # CPU utilization (%)
    cpus=$7; cputime=$8
    cpu_util = (elapsed_sec*cpus>0) ? (cputime/(elapsed_sec*cpus)*100) : 0

    # State from parent
    state = parent_state[jobid]

    # Accumulate for summary
    jobs++; memtot+=mempct; cputot+=cpu_util; states[state]++

    printf "%s\tElapsed=%s\tMem=%.1f%% of %sG\tCPU=%.1f%%\tState=%s\n", \
           jobid, elapsed_str, mempct, reqmem_gb, cpu_util, state
  }

  END {
    if (jobs>0) {
      printf "SUMMARY\tJobs=%d\tAvgMem=%.1f%%\tAvgCPU=%.1f%%", jobs, memtot/jobs, cputot/jobs
      for (s in states) printf "\t%s=%d", s, states[s]
      printf "\n"
    }
  }
'
