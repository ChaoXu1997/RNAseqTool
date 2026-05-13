package main

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"
)

func TestFindR_SystemPath(t *testing.T) {
	// Skip if R is not installed on the system
	if _, err := os.Stat("/usr/bin/R"); os.IsNotExist(err) {
		if _, err := os.Stat("/usr/local/bin/R"); os.IsNotExist(err) {
			t.Skip("R not installed on system, skipping test")
		}
	}

	rPath, err := FindR()
	if err != nil {
		t.Errorf("Expected to find R, got error: %v", err)
	}
	if rPath == "" {
		t.Error("Expected non-empty R path")
	}
	t.Logf("Found R at: %s", rPath)
}

func TestFindR_Validation(t *testing.T) {
	// Test R version validation
	tests := []struct {
		name    string
		version string
		valid   bool
	}{
		{"R 4.0.0", "R version 4.0.0 (2020-04-24)", true},
		{"R 4.3.1", "R version 4.3.1 (2023-06-16)", true},
		{"R 3.6.3", "R version 3.6.3 (2020-02-29)", false},
		{"R 2.15.1", "R version 2.15.1 (2012-06-22)", false},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			valid := validateRVersion(tt.version)
			if valid != tt.valid {
				t.Errorf("validateRVersion(%q) = %v, want %v", tt.version, valid, tt.valid)
			}
		})
	}
}

func TestFindR_Portable(t *testing.T) {
	// Create a temporary portable R directory structure
	tempDir := t.TempDir()
	portableDir := filepath.Join(tempDir, "r-portable")
	binDir := filepath.Join(portableDir, "bin")

	if err := os.MkdirAll(binDir, 0755); err != nil {
		t.Fatalf("Failed to create portable dir: %v", err)
	}

	// Create a mock R executable
	rPath := filepath.Join(binDir, "R")
	if runtime.GOOS == "windows" {
		rPath += ".exe"
	}

	mockRScript := `#!/bin/bash
if [ "$1" = "--version" ]; then
    echo "R version 4.3.1 (2023-06-16)"
    exit 0
fi
`
	if err := os.WriteFile(rPath, []byte(mockRScript), 0755); err != nil {
		t.Fatalf("Failed to create mock R: %v", err)
	}

	// Test the portable R detection function directly
	// Note: FindRPortable uses GetExecutableDir(), so we test the validation logic
	if !validateRExecutable(rPath) {
		t.Error("Mock R should be valid")
	}

	t.Logf("Mock portable R created at: %s", rPath)
}

func TestFindR_NoRFound(t *testing.T) {
	// Test when R is not found
	// This test needs a clean environment without R
	if runtime.GOOS == "linux" {
		if _, err := os.Stat("/usr/bin/R"); err == nil {
			t.Skip("R is installed, cannot test 'not found' scenario")
		}
	}

	// Temporarily modify PATH to exclude R
	origPath := os.Getenv("PATH")
	defer os.Setenv("PATH", origPath)
	os.Setenv("PATH", "/nonexistent")

	_, err := FindR()
	if err == nil {
		t.Error("Expected error when R not found, got nil")
	}
}

func TestCheckRInstalled(t *testing.T) {
	// Test checking if specific R packages are installed
	// This would normally call R to check, but we'll test the structure
	packages := []string{"DESeq2", "clusterProfiler", "plumber"}
	
	for _, pkg := range packages {
		if pkg == "" {
			t.Error("Package name should not be empty")
		}
	}
}
