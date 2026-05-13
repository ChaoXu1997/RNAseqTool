package main

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// ServerManager manages the R backend server process
type ServerManager struct {
	Port    int
	RPath   string
	APIPath string
	cmd     *exec.Cmd
	ctx     context.Context
	cancel  context.CancelFunc
}

// NewServerManager creates a new server manager instance
func NewServerManager() *ServerManager {
	return &ServerManager{
		Port: 0,
	}
}

// SetPort sets the port for the server
func (sm *ServerManager) SetPort(port int) {
	sm.Port = port
}

// SetRPath sets the R executable path
func (sm *ServerManager) SetRPath(rPath string) {
	sm.RPath = rPath
}

// SetAPIPath sets the path to the plumber API file
func (sm *ServerManager) SetAPIPath(apiPath string) {
	sm.APIPath = apiPath
}

// Start starts the R backend server
func (sm *ServerManager) Start(ctx context.Context) error {
	sm.ctx, sm.cancel = context.WithCancel(ctx)

	// Auto-assign port if not set
	if sm.Port == 0 {
		port, err := GetRandomPort()
		if err != nil {
			return fmt.Errorf("failed to get random port: %w", err)
		}
		sm.Port = port
	}

	// Get API path if not set
	if sm.APIPath == "" {
		sm.APIPath = GetAPIPath()
	}

	// Build R command
	sm.cmd = BuildRCommand(sm.RPath, sm.APIPath, sm.Port)

	// Set up command output
	sm.cmd.Stdout = os.Stdout
	sm.cmd.Stderr = os.Stderr

	// Start the command
	if err := sm.cmd.Start(); err != nil {
		return fmt.Errorf("failed to start R backend: %w", err)
	}

	// Wait for server to be ready
	if err := CheckHealth(sm.Port, 30, 500*time.Millisecond); err != nil {
		sm.Stop()
		return fmt.Errorf("server failed to start: %w", err)
	}

	return nil
}

// Stop stops the R backend server
func (sm *ServerManager) Stop() error {
	if sm.cancel != nil {
		sm.cancel()
	}

	if sm.cmd != nil && sm.cmd.Process != nil {
		if err := sm.cmd.Process.Kill(); err != nil {
			return fmt.Errorf("failed to kill R process: %w", err)
		}
		// Wait for process to exit
		sm.cmd.Wait()
	}

	return nil
}

// GetRandomPort returns a random available port
func GetRandomPort() (int, error) {
	listener, err := net.Listen("tcp", ":0")
	if err != nil {
		return 0, err
	}
	defer listener.Close()

	port := listener.Addr().(*net.TCPAddr).Port
	return port, nil
}

// IsPortAvailable checks if a port is available
func IsPortAvailable(port int) bool {
	addr := fmt.Sprintf(":%d", port)
	listener, err := net.Listen("tcp", addr)
	if err != nil {
		return false
	}
	listener.Close()
	return true
}

// CheckHealth polls the health endpoint until it returns 200 or timeout
func CheckHealth(port int, maxRetries int, retryInterval time.Duration) error {
	url := fmt.Sprintf("http://localhost:%d/api/health", port)
	client := &http.Client{
		Timeout: 5 * time.Second,
	}

	for i := 0; i < maxRetries; i++ {
		resp, err := client.Get(url)
		if err == nil {
			resp.Body.Close()
			if resp.StatusCode == http.StatusOK {
				return nil
			}
		}
		time.Sleep(retryInterval)
	}

	return fmt.Errorf("health check failed after %d retries", maxRetries)
}

// GetAPIPath returns the path to the plumber API file relative to the executable
func GetAPIPath() string {
	execDir, err := GetExecutableDir()
	if err != nil {
		// Fallback to current directory
		return "backend/api/plumber.R"
	}

	// Check if we're in the launcher directory
	parentDir := filepath.Dir(execDir)
	apiPath := filepath.Join(parentDir, "backend", "api", "plumber.R")

	if _, err := os.Stat(apiPath); err == nil {
		return apiPath
	}

	// Check relative to executable
	apiPath = filepath.Join(execDir, "backend", "api", "plumber.R")
	if _, err := os.Stat(apiPath); err == nil {
		return apiPath
	}

	// Default fallback
	return "backend/api/plumber.R"
}

// GetAPIPathWithBase returns the API path with a custom base directory
func GetAPIPathWithBase(basePath string) string {
	return basePath
}

// BuildRCommand builds the R command to start the plumber server
func BuildRCommand(rPath, apiPath string, port int) *exec.Cmd {
	rScript := fmt.Sprintf("plumber::plumb('%s')$run(port=%d)", apiPath, port)
	cmd := exec.Command(rPath, "-e", rScript)
	return cmd
}
