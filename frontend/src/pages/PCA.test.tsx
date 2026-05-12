import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  startAnalysis: vi.fn(),
  getTaskStatus: vi.fn(),
  getPlot: vi.fn(),
  exportPlot: vi.fn(),
}))

// Mock workspace store
const mockSetExprNorm = vi.fn()
const mockSetSampleInfo = vi.fn()
const mockSetAnalysisResult = vi.fn()
const mockSetPlotParams = vi.fn()

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    const state: Record<string, unknown> = {
      exprNorm: null,
      sampleInfo: null,
      pcaResult: null,
      plotParams: {},
      setExprNorm: mockSetExprNorm,
      setSampleInfo: mockSetSampleInfo,
      setAnalysisResult: mockSetAnalysisResult,
      setPlotParams: mockSetPlotParams,
    }
    return selector(state)
  },
}))

import PCA from './PCA'
import * as api from '../api/client'

describe('PCA Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('renders PCA page title', () => {
    render(<PCA />)
    expect(screen.getByText('PCA 分析')).toBeInTheDocument()
  })

  it('renders DataUpload components for norm and sampleInfo', () => {
    render(<PCA />)
    expect(screen.getByTestId('pca-data-section')).toBeInTheDocument()
    expect(screen.getByText('表达矩阵（Norm）')).toBeInTheDocument()
    expect(screen.getByText('样本信息')).toBeInTheDocument()
  })

  it('renders load demo data button', () => {
    render(<PCA />)
    expect(screen.getByTestId('load-demo-data')).toBeInTheDocument()
  })

  it('renders parameter panel', () => {
    render(<PCA />)
    expect(screen.getByTestId('pca-params')).toBeInTheDocument()
    expect(screen.getByTestId('param-title')).toBeInTheDocument()
    expect(screen.getByTestId('param-ellipse')).toBeInTheDocument()
    expect(screen.getByTestId('param-labels')).toBeInTheDocument()
    expect(screen.getByTestId('param-dot-size')).toBeInTheDocument()
  })

  it('loads demo data when button is clicked', async () => {
    vi.mocked(api.loadDemoData).mockImplementation(async (type: string) => ({
      data: {
        type,
        preview: [['Gene', 'S1'], ['A', '10']],
        rows: 100,
        cols: 10,
      },
      status: 'ok',
    }))

    render(<PCA />)
    const demoBtn = screen.getByTestId('load-demo-data')
    await userEvent.click(demoBtn)

    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('expr_norm')
    })
    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('sampleInfo')
    })
  })

  it('renders run analysis button', () => {
    render(<PCA />)
    expect(screen.getByTestId('run-pca')).toBeInTheDocument()
  })

  it('disables run button when no data is loaded', () => {
    render(<PCA />)
    const runBtn = screen.getByTestId('run-pca')
    expect(runBtn).toBeDisabled()
  })

  it('renders plot display area', () => {
    render(<PCA />)
    expect(screen.getByTestId('pca-plot-area')).toBeInTheDocument()
  })

  it('renders export buttons', () => {
    render(<PCA />)
    expect(screen.getByTestId('export-svg')).toBeInTheDocument()
    expect(screen.getByTestId('export-png')).toBeInTheDocument()
  })

  it('renders xlsx download button', () => {
    render(<PCA />)
    expect(screen.getByTestId('download-xlsx')).toBeInTheDocument()
  })

  it('toggles ellipse switch', async () => {
    render(<PCA />)
    const ellipseSwitch = screen.getByTestId('param-ellipse')
    expect(ellipseSwitch).not.toBeChecked()
    await userEvent.click(ellipseSwitch)
    expect(ellipseSwitch).toBeChecked()
  })

  it('toggles labels switch', async () => {
    render(<PCA />)
    const labelsSwitch = screen.getByTestId('param-labels')
    expect(labelsSwitch).not.toBeChecked()
    await userEvent.click(labelsSwitch)
    expect(labelsSwitch).toBeChecked()
  })

  it('changes title input', async () => {
    render(<PCA />)
    const titleInput = screen.getByTestId('param-title') as HTMLInputElement
    await userEvent.clear(titleInput)
    await userEvent.type(titleInput, 'My PCA Plot')
    expect(titleInput.value).toBe('My PCA Plot')
  })
})
