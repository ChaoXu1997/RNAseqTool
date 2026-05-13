# RNAseqTool - Installation Guide

## System Requirements

- **R** >= 4.1.0 (must be installed and on PATH)
  - Download: https://cran.r-project.org/
- **Operating System**: Windows 10+, macOS 10.13+, or Linux (x86_64)
- **Disk Space**: ~500 MB (including R dependencies)
- **RAM**: 4 GB minimum, 8 GB+ recommended for large datasets

---

## Windows

1. Download `RNAseqTool-windows.zip`
2. Extract to any folder (e.g., `C:\Program Files\RNAseqTool`)
3. Double-click `rnaseqtool.exe`
4. The application opens in your default web browser

### First Launch (Windows)

On first launch, RNAseqTool will:
- Detect your R installation (or prompt you to install R)
- Install required R packages (this may take 5-10 minutes)
- Open the application in your browser

### Windows Firewall

If Windows Firewall prompts you, click "Allow access" to let RNAseqTool
serve its web interface on localhost.

---

## macOS

1. Download `RNAseqTool-macos.tar.gz`
2. Extract the archive
3. Drag `RNAseqTool.app` to your `Applications` folder
4. Double-click `RNAseqTool.app` to launch

### First Launch (macOS)

On first launch, macOS may show a security warning because the app is
not signed by an identified developer. To allow it:

1. Right-click (or Control-click) `RNAseqTool.app`
2. Select "Open" from the context menu
3. Click "Open" in the dialog

Or via Terminal:
```bash
xattr -cr /Applications/RNAseqTool.app
```

### macOS Permissions

If R package installation fails, you may need to grant Terminal or
RNAseqTool full disk access in System Preferences > Security & Privacy.

---

## Linux

1. Download `RNAseqTool-linux.tar.gz`
2. Extract:
   ```bash
   tar xzf RNAseqTool-linux.tar.gz
   ```
3. Run:
   ```bash
   cd RNAseqTool
   ./rnaseqtool
   ```

### Linux Dependencies

Install system dependencies if R package compilation fails:

**Ubuntu/Debian:**
```bash
sudo apt-get update
sudo apt-get install r-base r-base-dev
sudo apt-get install libcurl4-openssl-dev libxml2-dev libssl-dev
sudo apt-get install libfontconfig1-dev libharfbuzz-dev libfribidi-dev
```

**CentOS/RHEL:**
```bash
sudo yum install R R-devel
sudo yum install libcurl-devel libxml2-devel openssl-devel
```

---

## Using RNAseqTool

1. After launch, the application opens in your browser at
   `http://localhost:8080` (or the port shown in the terminal)
2. Upload your RNA-seq count matrix and sample information
3. Run DESeq2 analysis, PCA, volcano plots, GSEA, and more

---

## Troubleshooting

### "R not found"

RNAseqTool requires R to be installed and accessible on your system PATH.

**Fix:**
- Install R from https://cran.r-project.org/
- Ensure `R` and `Rscript` are on your PATH
- On Windows, restart your terminal after installing R

### Port already in use

If port 8080 is occupied, RNAseqTool will automatically try the next
available port. Check the terminal output for the actual URL.

### R packages fail to install

1. Ensure you have a working internet connection
2. On Linux, install system development libraries (see above)
3. On Windows, install Rtools: https://cran.r-project.org/bin/windows/Rtools/
4. Try running R manually and installing packages:
   ```r
   install.packages("plumber")
   ```

### Browser does not open

Manually open the URL shown in the terminal output, typically:
`http://localhost:8080`

---

## Building from Source

If you prefer to build from source:

```bash
git clone <repository-url>
cd RNAseqTool
chmod +x build.sh
./build.sh
```

See `renv/README.md` for R dependency management details.

---

## Support

For issues and questions, please visit the project repository.
