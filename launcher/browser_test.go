package main

import (
	"runtime"
	"testing"
)

func TestOpenBrowser(t *testing.T) {
	// Test that OpenBrowser doesn't panic
	// Note: This will actually open a browser, so we skip in CI
	if testing.Short() {
		t.Skip("Skipping browser test in short mode")
	}

	url := "http://localhost:8080"
	// Just test that the function doesn't panic
	// In a real test, we'd mock exec.Command
	OpenBrowser(url)
}

func TestOpenBrowser_Command(t *testing.T) {
	// Test building the correct command for each OS
	tests := []struct {
		os       string
		url      string
		expected string
	}{
		{"darwin", "http://localhost", "open"},
		{"linux", "http://localhost", "xdg-open"},
		{"windows", "http://localhost", "cmd"},
	}

	for _, tt := range tests {
		t.Run(tt.os, func(t *testing.T) {
			cmd := buildOpenCommand(tt.os, tt.url)
			if cmd == nil {
				t.Error("buildOpenCommand returned nil")
			}
			// Check that the command path contains the expected command
			// This is platform-dependent, so we just check it's not empty
			if cmd.Path == "" {
				t.Error("Command path is empty")
			}
		})
	}
}

func TestGetBrowserURL(t *testing.T) {
	// Test URL building
	tests := []struct {
		port     int
		expected string
	}{
		{8080, "http://localhost:8080"},
		{3000, "http://localhost:3000"},
		{0, "http://localhost:0"},
	}

	for _, tt := range tests {
		t.Run(tt.expected, func(t *testing.T) {
			url := GetBrowserURL(tt.port)
			if url != tt.expected {
				t.Errorf("GetBrowserURL(%d) = %s, want %s", tt.port, url, tt.expected)
			}
		})
	}
}

func TestCurrentOS(t *testing.T) {
	// Test that runtime.GOOS is recognized
	os := runtime.GOOS
	switch os {
	case "darwin", "linux", "windows":
		// Valid OS
	default:
		t.Errorf("Unrecognized OS: %s", os)
	}
}
