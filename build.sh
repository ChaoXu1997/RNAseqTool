#!/usr/bin/env bash
set -euo pipefail

# ============================================================================
# RNAseqTool Build Script
# Builds distributable packages for Linux, macOS, and Windows
# ============================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$SCRIPT_DIR"
DIST_DIR="$PROJECT_ROOT/dist"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info()  { echo -e "${GREEN}[INFO]${NC} $*"; }
log_warn()  { echo -e "${YELLOW}[WARN]${NC} $*"; }
log_error() { echo -e "${RED}[ERROR]${NC} $*"; }

# ============================================================================
# Step 1: Build Frontend
# ============================================================================
build_frontend() {
    log_info "Building frontend..."
    cd "$PROJECT_ROOT/frontend"

    if [ ! -d "node_modules" ]; then
        log_info "Installing frontend dependencies..."
        npm install
    fi

    npm run build

    if [ ! -d "dist" ]; then
        log_error "Frontend build failed: dist/ not found"
        exit 1
    fi

    log_info "Frontend build complete."
}

# ============================================================================
# Step 2: Copy frontend assets to backend
# ============================================================================
copy_frontend_to_backend() {
    log_info "Copying frontend assets to backend/inst/www/..."
    mkdir -p "$PROJECT_ROOT/backend/inst/www"
    cp -r "$PROJECT_ROOT/frontend/dist/"* "$PROJECT_ROOT/backend/inst/www/"
    log_info "Frontend assets copied."
}

# ============================================================================
# Step 3: Compile Go launcher (cross-platform)
# ============================================================================
build_launcher() {
    log_info "Building Go launcher for all platforms..."
    cd "$PROJECT_ROOT/launcher"

    mkdir -p dist

    # Linux amd64
    log_info "  Compiling for Linux (amd64)..."
    GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o dist/rnaseqtool-linux .

    # macOS amd64
    log_info "  Compiling for macOS (amd64)..."
    GOOS=darwin GOARCH=amd64 CGO_ENABLED=0 go build -o dist/rnaseqtool-macos .

    # macOS arm64 (Apple Silicon)
    log_info "  Compiling for macOS (arm64)..."
    GOOS=darwin GOARCH=arm64 CGO_ENABLED=0 go build -o dist/rnaseqtool-macos-arm64 .

    # Windows amd64
    log_info "  Compiling for Windows (amd64)..."
    GOOS=windows GOARCH=amd64 CGO_ENABLED=0 go build -o dist/rnaseqtool.exe .

    log_info "Launcher builds complete."
}

# ============================================================================
# Step 4: Assemble distribution packages
# ============================================================================
assemble_packages() {
    log_info "Assembling distribution packages..."
    cd "$PROJECT_ROOT"

    rm -rf "$DIST_DIR"
    mkdir -p "$DIST_DIR"

    # --- Common backend and frontend source ---
    local BACKEND_SRC="$PROJECT_ROOT/backend"
    local FRONTEND_SRC="$PROJECT_ROOT/frontend/dist"

    # =========================================================================
    # Linux Package
    # =========================================================================
    log_info "  Assembling Linux package..."
    local LINUX_DIR="$DIST_DIR/linux/RNAseqTool"
    mkdir -p "$LINUX_DIR/backend/R"
    mkdir -p "$LINUX_DIR/backend/api"
    mkdir -p "$LINUX_DIR/backend/data"
    mkdir -p "$LINUX_DIR/frontend"

    cp "$PROJECT_ROOT/launcher/dist/rnaseqtool-linux" "$LINUX_DIR/rnaseqtool"
    chmod +x "$LINUX_DIR/rnaseqtool"

    cp -r "$BACKEND_SRC/R/"*       "$LINUX_DIR/backend/R/"
    cp -r "$BACKEND_SRC/api/"*     "$LINUX_DIR/backend/api/"
    cp -r "$BACKEND_SRC/data/"*    "$LINUX_DIR/backend/data/"
    cp -r "$FRONTEND_SRC/"*        "$LINUX_DIR/frontend/"

    # Copy README and install guide
    cp "$PROJECT_ROOT/INSTALL.md" "$LINUX_DIR/README.md" 2>/dev/null || true

    # Create tar.gz
    cd "$DIST_DIR/linux"
    tar czf "$DIST_DIR/RNAseqTool-linux.tar.gz" RNAseqTool/
    cd "$PROJECT_ROOT"
    log_info "  Linux: $DIST_DIR/RNAseqTool-linux.tar.gz"

    # =========================================================================
    # macOS Package (.app bundle)
    # =========================================================================
    log_info "  Assembling macOS package..."
    local MACOS_APP="$DIST_DIR/macos/RNAseqTool.app"
    local MACOS_CONTENTS="$MACOS_APP/Contents"
    local MACOS_BIN="$MACOS_CONTENTS/MacOS"
    local MACOS_RES="$MACOS_CONTENTS/Resources"

    mkdir -p "$MACOS_BIN"
    mkdir -p "$MACOS_RES/backend/R"
    mkdir -p "$MACOS_RES/backend/api"
    mkdir -p "$MACOS_RES/backend/data"
    mkdir -p "$MACOS_RES/frontend"

    # Copy launcher (use amd64 as default, arm64 also available)
    cp "$PROJECT_ROOT/launcher/dist/rnaseqtool-macos" "$MACOS_BIN/rnaseqtool"
    chmod +x "$MACOS_BIN/rnaseqtool"

    # Copy backend and frontend
    cp -r "$BACKEND_SRC/R/"*       "$MACOS_RES/backend/R/"
    cp -r "$BACKEND_SRC/api/"*     "$MACOS_RES/backend/api/"
    cp -r "$BACKEND_SRC/data/"*    "$MACOS_RES/backend/data/"
    cp -r "$FRONTEND_SRC/"*        "$MACOS_RES/frontend/"

    # Create Info.plist
    cat > "$MACOS_CONTENTS/Info.plist" << 'PLIST'
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>RNAseqTool</string>
    <key>CFBundleDisplayName</key>
    <string>RNAseqTool</string>
    <key>CFBundleIdentifier</key>
    <string>com.rnaseqtool.app</string>
    <key>CFBundleVersion</key>
    <string>1.0.0</string>
    <key>CFBundleShortVersionString</key>
    <string>1.0.0</string>
    <key>CFBundleExecutable</key>
    <string>rnaseqtool</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>NSHighResolutionCapable</key>
    <true/>
    <key>LSMinimumSystemVersion</key>
    <string>10.13</string>
</dict>
</plist>
PLIST

    # Also copy arm64 version for reference
    cp "$PROJECT_ROOT/launcher/dist/rnaseqtool-macos-arm64" "$DIST_DIR/macos/rnaseqtool-arm64"
    chmod +x "$DIST_DIR/macos/rnaseqtool-arm64"

    # Create tar.gz for macOS
    cd "$DIST_DIR/macos"
    tar czf "$DIST_DIR/RNAseqTool-macos.tar.gz" .
    cd "$PROJECT_ROOT"
    log_info "  macOS: $DIST_DIR/RNAseqTool-macos.tar.gz"

    # =========================================================================
    # Windows Package
    # =========================================================================
    log_info "  Assembling Windows package..."
    local WIN_DIR="$DIST_DIR/windows"
    mkdir -p "$WIN_DIR/backend/R"
    mkdir -p "$WIN_DIR/backend/api"
    mkdir -p "$WIN_DIR/backend/data"
    mkdir -p "$WIN_DIR/frontend"

    cp "$PROJECT_ROOT/launcher/dist/rnaseqtool.exe" "$WIN_DIR/rnaseqtool.exe"

    cp -r "$BACKEND_SRC/R/"*       "$WIN_DIR/backend/R/"
    cp -r "$BACKEND_SRC/api/"*     "$WIN_DIR/backend/api/"
    cp -r "$BACKEND_SRC/data/"*    "$WIN_DIR/backend/data/"
    cp -r "$FRONTEND_SRC/"*        "$WIN_DIR/frontend/"

    # Create zip for Windows
    cd "$DIST_DIR"
    zip -r "$DIST_DIR/RNAseqTool-windows.zip" windows/
    cd "$PROJECT_ROOT"
    log_info "  Windows: $DIST_DIR/RNAseqTool-windows.zip"

    log_info "All packages assembled."
}

# ============================================================================
# Step 5: Generate checksums
# ============================================================================
generate_checksums() {
    log_info "Generating checksums..."
    cd "$DIST_DIR"
    sha256sum RNAseqTool-linux.tar.gz RNAseqqTool-macos.tar.gz RNAseqTool-windows.zip > checksums.sha256 2>/dev/null || \
    sha256sum RNAseqTool-*.tar.gz RNAseqTool-*.zip > checksums.sha256 2>/dev/null || true
    log_info "Checksums written to $DIST_DIR/checksums.sha256"
}

# ============================================================================
# Main
# ============================================================================
main() {
    log_info "=========================================="
    log_info "RNAseqTool Build Script"
    log_info "=========================================="
    log_info "Project root: $PROJECT_ROOT"

    build_frontend
    copy_frontend_to_backend
    build_launcher
    assemble_packages
    generate_checksums

    log_info "=========================================="
    log_info "Build complete! Output in: $DIST_DIR"
    log_info "=========================================="
    ls -lh "$DIST_DIR"/*.tar.gz "$DIST_DIR"/*.zip 2>/dev/null || true
}

# Run main
main "$@"
