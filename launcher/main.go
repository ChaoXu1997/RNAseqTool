package main

import (
	"context"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"syscall"
	"time"
)

func main() {
	// Parse command line flags
	port := flag.Int("port", 0, "Port to run the server on (0 for auto-assign)")
	logFile := flag.String("log", "", "Log file path (empty for stdout only)")
	verbose := flag.Bool("verbose", false, "Enable verbose logging")
	flag.Parse()

	// Set up logging
	if *logFile != "" {
		f, err := os.OpenFile(*logFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to open log file: %v\n", err)
		} else {
			defer f.Close()
		}
	}

	fmt.Println("RNAseqTool Launcher")
	fmt.Println("===================")

	// Set up signal handling
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)

	go func() {
		sig := <-sigChan
		fmt.Printf("\nReceived signal %v, shutting down...\n", sig)
		cancel()
	}()

	// Check R installation
	fmt.Println("Checking R installation...")
	rPath, err := FindR()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		fmt.Fprintln(os.Stderr, "Please install R 4.x or place a portable R in ./r-portable/")
		os.Exit(1)
	}
	fmt.Printf("Found R at: %s\n", rPath)

	// Check R version
	fmt.Println("Validating R version...")
	cmd := exec.Command(rPath, "--version")
	output, err := cmd.Output()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to get R version: %v\n", err)
		os.Exit(1)
	}
	fmt.Printf("R version: %s\n", string(output))

	// Check core packages
	fmt.Println("Checking core R packages...")
	installed, missing := CheckCorePackages(rPath)
	if *verbose {
		fmt.Printf("Installed packages: %v\n", installed)
	}
	if len(missing) > 0 {
		fmt.Printf("Missing packages: %v\n", missing)
		fmt.Println("Attempting to restore packages with renv...")
		if err := RestoreRenv(rPath); err != nil {
			fmt.Fprintf(os.Stderr, "Failed to restore packages: %v\n", err)
			fmt.Fprintln(os.Stderr, "Please run 'Rscript -e \"renv::restore()\"' manually")
		}
	}

	// Get API path
	apiPath := GetAPIPath()
	fmt.Printf("API path: %s\n", apiPath)

	// Get project root directory
	execDir, err := GetExecutableDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Failed to get executable directory: %v\n", err)
		os.Exit(1)
	}
	projectRoot := filepath.Dir(execDir)
	if *verbose {
		fmt.Printf("Project root: %s\n", projectRoot)
	}

	// Create and start server manager
	fmt.Println("Starting R backend server...")
	sm := NewServerManager()
	sm.SetRPath(rPath)
	sm.SetAPIPath(apiPath)
	if *port > 0 {
		sm.SetPort(*port)
	}

	if err := sm.Start(ctx); err != nil {
		fmt.Fprintf(os.Stderr, "Failed to start server: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Server started on port %d\n", sm.Port)
	fmt.Printf("API endpoint: http://localhost:%d/api\n", sm.Port)
	fmt.Printf("Health check: http://localhost:%d/api/health\n", sm.Port)

	// Open browser
	browserURL := GetBrowserURL(sm.Port)
	fmt.Printf("Opening browser at: %s\n", browserURL)
	OpenBrowser(browserURL)

	fmt.Println("\nPress Ctrl+C to stop the server")
	fmt.Println("================================")

	// Wait for context cancellation (signal)
	<-ctx.Done()

	// Graceful shutdown
	fmt.Println("\nShutting down server...")
	shutdownCtx, shutdownCancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer shutdownCancel()

	if err := sm.Stop(); err != nil {
		fmt.Fprintf(os.Stderr, "Error during shutdown: %v\n", err)
	}

	// Wait for shutdown or timeout
	select {
	case <-shutdownCtx.Done():
		fmt.Println("Shutdown timed out")
	case <-time.After(1 * time.Second):
		fmt.Println("Server stopped successfully")
	}
}
