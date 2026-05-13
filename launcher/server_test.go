package main

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"
)

func TestGetRandomPort(t *testing.T) {
	// Test getting a random available port
	port, err := GetRandomPort()
	if err != nil {
		t.Errorf("Failed to get random port: %v", err)
	}
	if port < 1024 || port > 65535 {
		t.Errorf("Port %d is not in valid range (1024-65535)", port)
	}
	t.Logf("Got random port: %d", port)
}

func TestGetRandomPort_Multiple(t *testing.T) {
	// Test getting multiple unique ports
	ports := make(map[int]bool)
	for i := 0; i < 10; i++ {
		port, err := GetRandomPort()
		if err != nil {
			t.Errorf("Failed to get random port: %v", err)
		}
		if ports[port] {
			t.Errorf("Got duplicate port: %d", port)
		}
		ports[port] = true
	}
}

func TestIsPortAvailable(t *testing.T) {
	// Test checking if a port is available
	port, err := GetRandomPort()
	if err != nil {
		t.Fatalf("Failed to get random port: %v", err)
	}

	if !IsPortAvailable(port) {
		t.Errorf("Port %d should be available", port)
	}

	// Start a listener on the port
	listener, err := net.Listen("tcp", fmt.Sprintf(":%d", port))
	if err != nil {
		t.Fatalf("Failed to listen on port %d: %v", port, err)
	}
	defer listener.Close()

	if IsPortAvailable(port) {
		t.Errorf("Port %d should not be available after listener started", port)
	}
}

func TestServerManager_New(t *testing.T) {
	// Test creating a new server manager
	sm := NewServerManager()
	if sm == nil {
		t.Error("NewServerManager returned nil")
	}
	if sm.Port != 0 {
		t.Errorf("Expected port 0, got %d", sm.Port)
	}
	if sm.RPath != "" {
		t.Errorf("Expected empty RPath, got %s", sm.RPath)
	}
}

func TestServerManager_SetPort(t *testing.T) {
	// Test setting port on server manager
	sm := NewServerManager()
	port := 8080
	sm.SetPort(port)
	if sm.Port != port {
		t.Errorf("Expected port %d, got %d", port, sm.Port)
	}
}

func TestServerManager_SetRPath(t *testing.T) {
	// Test setting R path on server manager
	sm := NewServerManager()
	rPath := "/usr/bin/R"
	sm.SetRPath(rPath)
	if sm.RPath != rPath {
		t.Errorf("Expected RPath %s, got %s", rPath, sm.RPath)
	}
}

func TestHealthCheck(t *testing.T) {
	// Create a mock health endpoint
	mux := http.NewServeMux()
	mux.HandleFunc("/api/health", func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
		w.Write([]byte(`{"status":"ok"}`))
	})

	server := &http.Server{
		Addr:    ":0",
		Handler: mux,
	}

	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		t.Fatalf("Failed to create listener: %v", err)
	}

	port := listener.Addr().(*net.TCPAddr).Port
	server.Addr = fmt.Sprintf(":%d", port)

	go server.Serve(listener)
	defer server.Shutdown(context.Background())

	// Wait for server to start
	time.Sleep(100 * time.Millisecond)

	// Test health check
	err = CheckHealth(port, 5, 100*time.Millisecond)
	if err != nil {
		t.Errorf("Health check failed: %v", err)
	}
}

func TestHealthCheck_Timeout(t *testing.T) {
	// Test health check timeout
	port := 59999 // Unlikely to be in use
	err := CheckHealth(port, 2, 100*time.Millisecond)
	if err == nil {
		t.Error("Expected health check to timeout, but it succeeded")
	}
}

func TestServerManager_StartStop(t *testing.T) {
	// Skip if R is not installed
	if _, err := exec.LookPath("R"); err != nil {
		t.Skip("R not installed, skipping server start test")
	}

	// Create a temporary test directory
	tempDir := t.TempDir()
	plumberFile := filepath.Join(tempDir, "plumber.R")
	plumberContent := `#* @apiTitle Test API
#* @get /health
function() {
  list(status = "ok")
}
`
	if err := os.WriteFile(plumberFile, []byte(plumberContent), 0644); err != nil {
		t.Fatalf("Failed to create test plumber file: %v", err)
	}

	sm := NewServerManager()
	sm.SetRPath("R")
	sm.SetPort(0) // Auto-assign port
	sm.SetAPIPath(plumberFile)

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	err := sm.Start(ctx)
	if err != nil {
		t.Errorf("Failed to start server: %v", err)
	}

	if sm.Port == 0 {
		t.Error("Expected port to be assigned")
	}

	// Wait for server to be ready
	err = CheckHealth(sm.Port, 10, 500*time.Millisecond)
	if err != nil {
		t.Errorf("Server not ready: %v", err)
	}

	// Stop server
	err = sm.Stop()
	if err != nil {
		t.Errorf("Failed to stop server: %v", err)
	}
}

func TestGetAPIPath(t *testing.T) {
	// Test getting the API path relative to executable
	apiPath := GetAPIPath()
	if apiPath == "" {
		t.Error("API path should not be empty")
	}
	t.Logf("API path: %s", apiPath)
}

func TestGetAPIPath_Custom(t *testing.T) {
	// Test with custom API path
	customPath := "/custom/path/to/api.R"
	path := GetAPIPathWithBase(customPath)
	if path != customPath {
		t.Errorf("Expected %s, got %s", customPath, path)
	}
}

func TestBuildRCommand(t *testing.T) {
	// Test building R command
	rPath := "/usr/bin/R"
	apiPath := "api/plumber.R"
	port := 8080

	cmd := BuildRCommand(rPath, apiPath, port)
	if cmd == nil {
		t.Error("BuildRCommand returned nil")
	}
	if cmd.Path != rPath {
		t.Errorf("Expected command path %s, got %s", rPath, cmd.Path)
	}
}
