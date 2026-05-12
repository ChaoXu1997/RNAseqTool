import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, fireEvent, waitFor } from '@testing-library/react'
import WorkspaceManager from './WorkspaceManager'
import * as apiClient from '../api/client'
import { useWorkspaceStore } from '../stores/workspace'

// Mock the API client
vi.mock('../api/client', () => ({
  saveWorkspace: vi.fn(),
  loadWorkspace: vi.fn(),
  getWorkspaceStatus: vi.fn(),
}))

// Mock URL.createObjectURL and revokeObjectURL
const mockCreateObjectURL = vi.fn(() => 'blob:mock-url')
const mockRevokeObjectURL = vi.fn()
vi.stubGlobal('URL', {
  ...URL,
  createObjectURL: mockCreateObjectURL,
  revokeObjectURL: mockRevokeObjectURL,
})

describe('WorkspaceManager', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    // Reset store
    useWorkspaceStore.getState().reset()
  })

  it('renders save and load buttons', () => {
    render(<WorkspaceManager />)
    
    expect(screen.getByText('保存 Workspace')).toBeInTheDocument()
    expect(screen.getByText('加载 Workspace')).toBeInTheDocument()
  })

  it('displays "无" when no steps completed', () => {
    render(<WorkspaceManager />)
    
    expect(screen.getByText('无')).toBeInTheDocument()
  })

  it('displays completed steps from store', () => {
    // Set completed steps in store
    useWorkspaceStore.getState().setCompletedSteps(['DESeq2', 'PCA'])
    
    render(<WorkspaceManager />)
    
    expect(screen.getByText('DESeq2')).toBeInTheDocument()
    expect(screen.getByText('PCA')).toBeInTheDocument()
  })

  it('calls saveWorkspace when save button is clicked', async () => {
    const mockBlob = new Blob(['test'], { type: 'application/octet-stream' })
    vi.mocked(apiClient.saveWorkspace).mockResolvedValue(mockBlob)
    
    render(<WorkspaceManager />)
    
    fireEvent.click(screen.getByText('保存 Workspace'))
    
    await waitFor(() => {
      expect(apiClient.saveWorkspace).toHaveBeenCalled()
    })
  })

  it('shows file input when load button is clicked', () => {
    render(<WorkspaceManager />)
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement
    expect(fileInput).toBeInTheDocument()
    expect(fileInput.accept).toContain('.rds')
  })

  it('calls loadWorkspace when file is selected', async () => {
    vi.mocked(apiClient.loadWorkspace).mockResolvedValue({
      status: 'ok',
      message: 'Loaded',
      steps: ['DESeq2', 'GSEA'],
    })
    vi.mocked(apiClient.getWorkspaceStatus).mockResolvedValue({
      status: 'ok',
      steps: ['DESeq2', 'GSEA'],
      version: '0.1.0',
    })
    
    render(<WorkspaceManager />)
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement
    const file = new File(['rds content'], 'workspace.rds', { type: 'application/octet-stream' })
    
    fireEvent.change(fileInput, { target: { files: [file] } })
    
    await waitFor(() => {
      expect(apiClient.loadWorkspace).toHaveBeenCalledWith(file)
    })
  })

  it('updates completed steps after successful load', async () => {
    vi.mocked(apiClient.loadWorkspace).mockResolvedValue({
      status: 'ok',
      message: 'Loaded',
      steps: ['DESeq2', 'Enrich'],
    })
    vi.mocked(apiClient.getWorkspaceStatus).mockResolvedValue({
      status: 'ok',
      steps: ['DESeq2', 'Enrich'],
      version: '0.1.0',
    })
    
    render(<WorkspaceManager />)
    
    const fileInput = document.querySelector('input[type="file"]') as HTMLInputElement
    const file = new File(['rds content'], 'workspace.rds', { type: 'application/octet-stream' })
    
    fireEvent.change(fileInput, { target: { files: [file] } })
    
    await waitFor(() => {
      const state = useWorkspaceStore.getState()
      expect(state.completedSteps).toEqual(['DESeq2', 'Enrich'])
      expect(state.workspaceVersion).toBe('0.1.0')
    })
  })

  it('refreshes status when refresh button is clicked', async () => {
    vi.mocked(apiClient.getWorkspaceStatus).mockResolvedValue({
      status: 'ok',
      steps: ['PCA', 'WGCNA'],
      version: '0.1.0',
    })
    
    render(<WorkspaceManager />)
    
    fireEvent.click(screen.getByText('刷新'))
    
    await waitFor(() => {
      expect(apiClient.getWorkspaceStatus).toHaveBeenCalled()
      const state = useWorkspaceStore.getState()
      expect(state.completedSteps).toEqual(['PCA', 'WGCNA'])
    })
  })

  it('displays version when available', () => {
    useWorkspaceStore.getState().setWorkspaceVersion('0.1.0')
    
    render(<WorkspaceManager />)
    
    expect(screen.getByText('版本: 0.1.0')).toBeInTheDocument()
  })
})
