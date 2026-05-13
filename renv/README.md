# R Environment Packaging Guide

This document describes how to manage R dependencies using `renv` and how
to bundle a portable R runtime with RNAseqTool.

## 1. Initialize renv (one-time)

```bash
cd backend
Rscript -e "renv::init()"
```

This creates `renv/`, `renv.lock`, and `.Rprofile` in the backend directory.

## 2. Install all project dependencies

```bash
Rscript -e "renv::install()"
```

Or install specific packages:

```bash
Rscript -e "renv::install(c('plumber', 'DESeq2', 'clusterProfiler', 'WGCNA'))"
```

## 3. Generate lockfile

After installing/updating packages, snapshot the current state:

```bash
Rscript -e "renv::snapshot()"
```

This updates `renv.lock` with exact versions of all loaded packages.

## 4. Restore from lockfile (on a new machine)

```bash
Rscript -e "renv::restore()"
```

## 5. Portable R Runtime (Optional)

For end users who do not have R installed, you can bundle a portable R.

### Download Links

| Platform | Download |
|----------|----------|
| Windows  | https://cran.r-project.org/bin/windows/base/ |
| macOS    | https://cran.r-project.org/bin/macosx/ |
| Linux    | Use system package manager or https://cran.r-project.org/bin/linux/ |

### Rtools (Windows only)

Users compiling from source on Windows need Rtools:
https://cran.r-project.org/bin/windows/Rtools/

### Integration with Launcher

The Go launcher (`rnaseqtool`) already includes R detection logic:

1. Checks `R_HOME` environment variable
2. Checks common install paths
3. Falls back to `R` on PATH

To bundle portable R, place it at:

```
RNAseqTool/
  R/                    # Portable R installation
    bin/
      R                 # R executable
      Rscript
    library/            # Base R packages
  backend/
  frontend/
  rnaseqtool
```

Then set `R_HOME` before launching, or update the launcher to detect
the bundled R directory.

## 6. Key R Packages for RNAseqTool

These are the core dependencies (add to `renv.lock`):

```r
# API framework
renv::install("plumber")

# RNA-seq analysis
renv::install(c(
  "DESeq2",
  "clusterProfiler",
  "enrichplot",
  "DOSE",
  "WGCNA",
  "msigdbr",
  "org.Hs.eg.db"
))

# Visualization
renv::install(c(
  "ggplot2",
  "pheatmap",
  "EnhancedVolcano"
))

# Utilities
renv::install(c(
  "jsonlite",
  "DT",
  "shiny"
))
```

## 7. Troubleshooting

### renv::restore() fails

Try:
```r
renv::install()  # Manual install instead of restore
renv::snapshot() # Then snapshot
```

### Package compilation errors on Windows

Install Rtools and ensure it's on PATH:
```bash
# Verify Rtools
Rscript -e "Sys.which('make')"
```

### Missing system libraries (Linux)

On Ubuntu/Debian:
```bash
sudo apt-get install libcurl4-openssl-dev libxml2-dev libssl-dev
sudo apt-get install libfontconfig1-dev libharfbuzz-dev libfribidi-dev
sudo apt-get install libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev
```

On CentOS/RHEL:
```bash
sudo yum install libcurl-devel libxml2-devel openssl-devel
```
