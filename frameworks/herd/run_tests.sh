#!/bin/bash

INPUT_FILE="litmus_final_allow.txt"
OUTPUT_DIR="output_final_allow"

mkdir -p "$OUTPUT_DIR"

while IFS=',' read -r path status; do
    # 提取路径文件名部分用于命名日志文件
    log_file="$OUTPUT_DIR/$(basename "$path").log"
    
    echo "Running test on $path ..."
    
    racket test_rvwmo.rkt "$path" > "$log_file" 2>&1
done < "$INPUT_FILE"
