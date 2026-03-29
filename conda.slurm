#!/bin/bash
#SBATCH -p INTEL_8576
#SBATCH -N 1          # 节点数
#SBATCH -n 32         # 核心数
#SBATCH -o job_%j_out.log   # 标准输出
#SBATCH -e job_%j_err.log   # 标准错误

source ~/.bashrc
conda activate $USER
Rscript elder_happy_poststratification_test.R >> out.log