package main

import (
	"fmt"
	"os/exec"
	"runtime"
)

// OpenBrowser opens the specified URL in the default browser
func OpenBrowser(url string) {
	cmd := buildOpenCommand(runtime.GOOS, url)
	if cmd != nil {
		cmd.Start()
	}
}

// buildOpenCommand builds the command to open a URL based on the OS
func buildOpenCommand(goos, url string) *exec.Cmd {
	var cmd *exec.Cmd
	switch goos {
	case "darwin":
		cmd = exec.Command("open", url)
	case "windows":
		cmd = exec.Command("cmd", "/c", "start", url)
	default: // linux, freebsd, etc.
		cmd = exec.Command("xdg-open", url)
	}
	return cmd
}

// GetBrowserURL returns the full URL for the browser
func GetBrowserURL(port int) string {
	return fmt.Sprintf("http://localhost:%d", port)
}
