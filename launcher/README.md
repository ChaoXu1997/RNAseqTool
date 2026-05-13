# RNAseqTool Launcher

A cross-platform desktop launcher for RNAseqTool that manages the R backend and opens the web interface.

## Features

- **R Environment Detection**: Automatically finds R installation in system PATH or portable R
- **Version Validation**: Ensures R version >= 4.x
- **Dependency Management**: Checks and restores R packages using renv
- **Automatic Port Assignment**: Finds available ports to avoid conflicts
- **Health Monitoring**: Waits for backend to be ready before opening browser
- **Graceful Shutdown**: Handles SIGINT/SIGTERM signals for clean exit
- **Cross-Platform**: Works on Linux, macOS, and Windows

## Usage

### Basic Usage

```bash
./rnaseq-launcher
```

### Command Line Options

```bash
./rnaseq-launcher [options]

Options:
  -port int
    	Port to run the server on (0 for auto-assign)
  -log string
    	Log file path (empty for stdout only)
  -verbose
    	Enable verbose logging
```

### Examples

```bash
# Run with auto-assigned port
./rnaseq-launcher

# Run on specific port
./rnaseq-launcher -port 8080

# Run with logging to file
./rnaseq-launcher -log launcher.log

# Run with verbose output
./rnaseq-launcher -verbose
```

## R Installation

The launcher searches for R in the following order:

1. System PATH (`R` command)
2. Common installation paths:
   - Linux: `/usr/bin/R`, `/usr/local/bin/R`, `/opt/R/bin/R`
   - macOS: `/usr/local/bin/R`, `/opt/homebrew/bin/R`, `/Library/Frameworks/R.framework/Resources/R`
   - Windows: `C:\Program Files\R\R-*\bin\R.exe`
3. Portable R installation in `./r-portable/bin/R`

### Installing R

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install r-base
```

**macOS (Homebrew):**
```bash
brew install r
```

**Windows:**
Download from https://cran.r-project.org/bin/windows/base/

### Portable R

You can also use a portable R installation by placing it in the `r-portable` directory:

```
RNAseqTool/
├── r-portable/
│   └── bin/
│       └── R          # or R.exe on Windows
├── launcher/
│   └── rnaseq-launcher
└── ...
```

## Development

### Building from Source

```bash
cd launcher
go build -o rnaseq-launcher .
```

### Running Tests

```bash
cd launcher
go test -v ./...
```

### Test Coverage

```bash
cd launcher
go test -cover ./...
```

## Architecture

The launcher consists of four main components:

### 1. R Environment Detection (`rcheck.go`)
- `FindR()`: Searches for R in system PATH and common locations
- `FindRPortable()`: Looks for portable R installation
- `CheckCorePackages()`: Verifies required R packages are installed
- `RestoreRenv()`: Restores packages using renv

### 2. Server Management (`server.go`)
- `ServerManager`: Manages the R backend process
- `GetRandomPort()`: Finds an available port
- `CheckHealth()`: Polls the health endpoint until ready
- `BuildRCommand()`: Constructs the R command to start plumber

### 3. Browser Opening (`browser.go`)
- `OpenBrowser()`: Opens URL in default browser
- Cross-platform support for Linux, macOS, and Windows

### 4. Main Entry Point (`main.go`)
- Command line argument parsing
- Signal handling for graceful shutdown
- Orchestrates the startup sequence

## API Endpoints

The launcher starts the R backend which provides the following endpoints:

- `GET /api/health` - Health check endpoint
- Other endpoints defined in `backend/api/plumber.R`

## Troubleshooting

### R Not Found

If you see "R not found in system PATH or common locations":

1. Install R 4.x or later
2. Ensure R is in your system PATH
3. Or place a portable R installation in `./r-portable/`

### Port Already in Use

If the default port is in use:

```bash
./rnaseq-launcher -port 8080
```

### Package Dependencies

If packages are missing, the launcher will attempt to restore them using renv. You can also run manually:

```bash
Rscript -e "renv::restore()"
```

## License

See the main RNAseqTool project license.
