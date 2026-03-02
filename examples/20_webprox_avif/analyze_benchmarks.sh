#!/bin/bash

# Analyze benchmark logs and extract performance metrics
echo "=== BENCHMARK ANALYSIS ==="
echo "=========================="

# Function to extract metrics from log file
analyze_log() {
    local log_file="$1"
    local config_name="$2"
    
    if [[ ! -f "$log_file" ]]; then
        echo "ERROR: Log file $log_file not found"
        return
    fi
    
    echo ""
    echo "=== $config_name ==="
    
    # Extract encoder configuration
    echo "Encoder Configuration:"
    grep "Encoder config:" "$log_file" | head -1
    
    # Extract encoding latencies
    echo ""
    echo "Encoding Latency Analysis:"
    grep "encoding latency:" "$log_file" | grep -o "encoding latency: [0-9.]*ms" | sed 's/encoding latency: //' | sort -n > /tmp/latencies_$$_
    if [[ -s /tmp/latencies_$$_ ]]; then
        echo "  Frames analyzed: $(wc -l < /tmp/latencies_$$_)"
        echo "  Min latency: $(awk 'BEGIN{min=9999} {if($1<min) min=$1} END {print min "ms"}' /tmp/latencies_$$_)"
        echo "  Max latency: $(awk 'BEGIN{max=0} {if($1>max) max=$1} END {print max "ms"}' /tmp/latencies_$$_)"
        echo "  Avg latency: $(awk '{sum+=$1; count++} END {printf "%.1fms", sum/count}' /tmp/latencies_$$_)"
        echo "  Median latency: $(awk '{a[NR]=$1} END {asort(a); print a[int(NR/2)] "ms"}' /tmp/latencies_$$_)"
    else
        echo "  No encoding latency data found"
    fi
    
    # Extract compression metrics
    echo ""
    echo "Compression Analysis:"
    grep "compression:" "$log_file" | grep -o "compression: [0-9.]*x" | sed 's/compression: //' | sort -n > /tmp/compression_$$_
    grep "size reduction:" "$log_file" | grep -o "reduction: [0-9.]*%" | sed 's/reduction: //' | sort -n > /tmp/reduction_$$_
    
    if [[ -s /tmp/compression_$$_ ]]; then
        echo "  Avg compression: $(awk '{sum+=$1; count++} END {printf "%.1fx", sum/count}' /tmp/compression_$$_)"
        echo "  Max compression: $(awk 'BEGIN{max=0} {if($1>max) max=$1} END {print max "x"}' /tmp/compression_$$_)"
        echo "  Min compression: $(awk 'BEGIN{min=9999} {if($1<min) min=$1} END {print min "x"}' /tmp/compression_$$_)"
    fi
    
    if [[ -s /tmp/reduction_$$_ ]]; then
        echo "  Avg size reduction: $(awk '{sum+=$1; count++} END {printf "%.1f%%", sum/count}' /tmp/reduction_$$_)"
    fi
    
    # Extract frame sizes
    echo ""
    echo "Frame Size Analysis:"
    grep "bytes.*encoding latency:" "$log_file" | grep -o "with [0-9]* bytes" | sed 's/with //' | sed 's/ bytes//' | sort -n > /tmp/sizes_$$_
    if [[ -s /tmp/sizes_$$_ ]]; then
        echo "  Frames analyzed: $(wc -l < /tmp/sizes_$$_)"
        echo "  Min size: $(awk 'BEGIN{min=999999} {if($1<min) min=$1} END {print min " bytes"}' /tmp/sizes_$$_)"
        echo "  Max size: $(awk 'BEGIN{max=0} {if($1>max) max=$1} END {print max " bytes"}' /tmp/sizes_$$_)"
        echo "  Avg size: $(awk '{sum+=$1; count++} END {printf "%.0f bytes", sum/count}' /tmp/sizes_$$_)"
    fi
    
    # Clean up temp files
    rm -f /tmp/latencies_$$_ /tmp/compression_$$_ /tmp/reduction_$$_ /tmp/sizes_$$_
}

# Analyze all benchmark logs
echo "Analyzing benchmark results..."
echo ""

# Check which logs exist
logs_found=false
for log in benchmark_*.log; do
    if [[ -f "$log" ]]; then
        logs_found=true
        config_name=$(echo "$log" | sed 's/benchmark_//' | sed 's/.log//' | tr '[:lower:]' '[:upper:]')
        analyze_log "$log" "$config_name"
    fi
done

if [[ "$logs_found" == false ]]; then
    echo "No benchmark logs found. Please run benchmark scripts first."
    echo ""
    echo "Available benchmark scripts:"
    echo "  ./benchmark_fast.sh"
    echo "  ./benchmark_balanced.sh" 
    echo "  ./benchmark_quality.sh"
    echo "  ./benchmark_ultra.sh"
fi

echo ""
echo "Analysis completed!"
