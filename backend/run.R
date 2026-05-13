#!/usr/bin/env Rscript
# run.R - 启动 RNAseqTool 后端 API 服务器

# 设置工作目录为 backend/
setwd("/home/chao/projects/RNAseqTool/backend")
cat("Working directory:", getwd(), "\n")

# 预加载所有源文件
cat("Loading source files...\n")
source("R/fct_data.R")
source("R/fct_pca.R")
source("R/fct_deseq2.R")
source("R/fct_volcano.R")
source("R/fct_enrich.R")
source("R/fct_msigdbr.R")
source("R/fct_gsea.R")
source("R/fct_genetrend.R")
source("R/fct_wgcna.R")
source("R/fct_workspace.R")
source("R/fct_plot_save.R")

# 加载 plumber
library(plumber)

# 创建并启动 API 服务器
cat("Starting API server on port 8000...\n")
pr <- plumber::plumb("api/plumber.R")
pr$run(port = 8000, host = "0.0.0.0")
