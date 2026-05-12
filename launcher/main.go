package main

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

func main() {
	fmt.Println("RNAseqTool Launcher")
	fmt.Printf("OS: %s\n", runtime.GOOS)

	// Check for R
	rPath, err := findR()
	if err != nil {
		fmt.Println("R not found. Using portable R...")
		// TODO: use portable R
		os.Exit(1)
	}
	fmt.Printf("Found R at: %s\n", rPath)

	// Start plumber
	fmt.Println("Starting R backend...")
	cmd := exec.Command(rPath, "-e", "plumber::plumb('api/plumber.R')$run(port=8000)")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Start(); err != nil {
		fmt.Printf("Failed to start R backend: %v\n", err)
		os.Exit(1)
	}

	// Open browser
	fmt.Println("Opening browser...")
	openBrowser("http://localhost:8000")

	// Wait for exit
	fmt.Println("Press Ctrl+C to stop")
	cmd.Wait()
}

func findR() (string, error) {
	paths := []string{"R", "/usr/bin/R", "/usr/local/bin/R"}
	for _, p := range paths {
		if _, err := exec.LookPath(p); err == nil {
			return p, nil
		}
	}
	return "", fmt.Errorf("R not found")
}

func openBrowser(url string) {
	var cmd *exec.Cmd
	switch runtime.GOOS {
	case "darwin":
		cmd = exec.Command("open", url)
	case "windows":
		cmd = exec.Command("cmd", "/c", "start", url)
	default:
		cmd = exec.Command("xdg-open", url)
	}
	cmd.Start()
}
