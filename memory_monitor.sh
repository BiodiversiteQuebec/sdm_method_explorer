#!/bin/bash

# Define the log file with a timestamp in the filename to avoid overwriting
LOG_FILE="memory_usage_$(TZ='America/New_York' date '+%Y-%m-%d_%H-%M-%S').log"

# Write a header to the log file
echo "Timestamp | Memory Usage (%)" > "$LOG_FILE"

# Start monitoring memory usage in a loop
while true; do
    # Get the current timestamp
    TIMESTAMP=$(TZ='America/New_York' date '+%Y-%m-%d %H:%M:%S %Z')  # Includes timezone

    # Capture the memory usage percentage
    MEM_USAGE=$(free -m | awk '/Mem/{printf "%.2f", $3/$2}')

    # Append the data to the log file
    echo "$TIMESTAMP | $MEM_USAGE" >> "$LOG_FILE"

    # Wait for the specified interval
    sleep 10  # log every 5 seconds
done
