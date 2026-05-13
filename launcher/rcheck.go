package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"
)

// FindR searches for R in the system PATH and common locations
func FindR() (string, error) {
	// Check common paths based on OS
	var searchPaths []string
	
	switch runtime.GOOS {
	case "darwin":
		searchPaths = []string{
			"/usr/local/bin/R",
			"/opt/homebrew/bin/R",
			"/Library/Frameworks/R.framework/Resources/R",
		}
	case "linux":
		searchPaths = []string{
			"/usr/bin/R",
			"/usr/local/bin/R",
			"/opt/R/bin/R",
		}
	case "windows":
		searchPaths = []string{
			"C:\\Program Files\\R\\R-4.3.1\\bin\\R.exe",
			"C:\\Program Files\\R\\R-4.2.3\\bin\\R.exe",
			"C:\\Program Files\\R\\R-4.1.3\\bin\\R.exe",
		}
	}

	// First check PATH
	if rPath, err := exec.LookPath("R"); err == nil {
		if validateRExecutable(rPath) {
			return rPath, nil
		}
	}

	// Check common installation paths
	for _, p := range searchPaths {
		if _, err := os.Stat(p); err == nil {
			if validateRExecutable(p) {
				return p, nil
			}
		}
	}

	// Try portable R
	portablePath, err := FindRPortable()
	if err == nil {
		return portablePath, nil
	}

	return "", fmt.Errorf("R not found in system PATH or common locations")
}

// FindRPortable searches for a portable R installation
func FindRPortable() (string, error) {
	execDir, err := GetExecutableDir()
	if err != nil {
		return "", err
	}

	var portablePath string
	if runtime.GOOS == "windows" {
		portablePath = filepath.Join(execDir, "r-portable", "bin", "R.exe")
	} else {
		portablePath = filepath.Join(execDir, "r-portable", "bin", "R")
	}

	if _, err := os.Stat(portablePath); err != nil {
		return "", fmt.Errorf("portable R not found at %s", portablePath)
	}

	if !validateRExecutable(portablePath) {
		return "", fmt.Errorf("portable R at %s is not valid", portablePath)
	}

	return portablePath, nil
}

// validateRExecutable checks if the R executable is valid and has version >= 4.x
func validateRExecutable(rPath string) bool {
	cmd := exec.Command(rPath, "--version")
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	return validateRVersion(string(output))
}

// validateRVersion checks if R version string indicates version >= 4.0
func validateRVersion(versionOutput string) bool {
	// Parse version from output like "R version 4.3.1 (2023-06-16)"
	re := regexp.MustCompile(`R version (\d+)\.`)
	matches := re.FindStringSubmatch(versionOutput)
	if len(matches) < 2 {
		return false
	}

	majorVersion := matches[1]
	return majorVersion >= "4"
}

// GetExecutableDir returns the directory of the current executable
func GetExecutableDir() (string, error) {
	execPath, err := os.Executable()
	if err != nil {
		return "", err
	}
	return filepath.Dir(execPath), nil
}

// CheckRPackageInstalled checks if a specific R package is installed
func CheckRPackageInstalled(rPath, packageName string) bool {
	cmd := exec.Command(rPath, "-e", fmt.Sprintf("requireNamespace('%s', quietly = TRUE)", packageName))
	output, err := cmd.Output()
	if err != nil {
		return false
	}

	return strings.Contains(string(output), "TRUE")
}

// CheckCorePackages checks if core R packages are installed
func CheckCorePackages(rPath string) ([]string, []string) {
	corePackages := []string{"DESeq2", "clusterProfiler", "plumber", "renv"}
	installed := []string{}
	missing := []string{}

	for _, pkg := range corePackages {
		if CheckRPackageInstalled(rPath, pkg) {
			installed = append(installed, pkg)
		} else {
			missing = append(missing, pkg)
		}
	}

	return installed, missing
}

// RestoreRenv restores R packages using renv
func RestoreRenv(rPath string) error {
	cmd := exec.Command(rPath, "-e", "renv::restore()")
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}
