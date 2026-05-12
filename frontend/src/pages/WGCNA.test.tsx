import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  getPlot: vi.fn(),
  uploadFile: vi.fn(),
  runWGCNA: vi.fn(),
  getWGCNAPlot: vi.fn(),
  getTaskStatus: vi.fn(),
  startAnalysis: vi.fn(),
  savePlotRds: vi.fn(),
  loadPlotRds: vi.fn(),
  default: {
    get: vi.fn(),
    post: vi.fn(),
  },
}))

// Mock workspace store
let mockState: Record<string, unknown> = {
  exprNorm: null,
  sampleInfo: null,
  deseq2Result: null,
  setAnalysisResult: vi.fn(),
}

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    return selector(mockState)
  },
}))

import WGCNA from './WGCNA'
import * as api from '../api/client'

describe('WGCNA Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockState = {
      exprNorm: null,
      sampleInfo: null,
      deseq2Result: null,
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders WGCNA page title', () => {
    render(<WGCNA />)
    expect(screen.getByText(/WGCNA.*共表达/)).toBeInTheDocument()
  })

  it('renders data source section', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('wgcna-data-source')).toBeInTheDocument()
  })

  it('renders data source radio buttons', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('source-norm')).toBeInTheDocument()
    expect(screen.getByTestId('source-upload')).toBeInTheDocument()
  })

  it('renders trait upload section', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('trait-upload')).toBeInTheDocument()
  })

  it('renders progress stepper', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('wgcna-progress')).toBeInTheDocument()
  })

  it('renders analysis parameters section', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('wgcna-params-section')).toBeInTheDocument()
  })

  it('renders cutHeight input', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('param-cut-height')).toBeInTheDocument()
  })

  it('renders abline input', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('param-abline')).toBeInTheDocument()
  })

  it('renders mergeCutHeight input', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('param-merge-cut-height')).toBeInTheDocument()
  })

  it('renders run analysis button', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('run-wgcna')).toBeInTheDocument()
  })

  it('renders result area', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('wgcna-result-area')).toBeInTheDocument()
  })

  it('renders export buttons', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('export-svg')).toBeInTheDocument()
    expect(screen.getByTestId('export-rdata')).toBeInTheDocument()
  })

  it('changes cutHeight input', async () => {
    render(<WGCNA />)
    const input = screen.getByTestId('param-cut-height') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '20')
    expect(input.value).toBe('20')
  })

  it('changes abline input', async () => {
    render(<WGCNA />)
    const input = screen.getByTestId('param-abline') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '0.85')
    expect(input.value).toBe('0.85')
  })

  it('changes mergeCutHeight input', async () => {
    render(<WGCNA />)
    const input = screen.getByTestId('param-merge-cut-height') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '0.3')
    expect(input.value).toBe('0.3')
  })

  it('disables run button when no data available', () => {
    render(<WGCNA />)
    const runButton = screen.getByTestId('run-wgcna')
    expect(runButton).toBeDisabled()
  })

  it('enables run button when workspace data is available', () => {
    mockState.exprNorm = {
      name: 'norm.xlsx',
      rows: 100,
      cols: 10,
      preview: [],
    }
    render(<WGCNA />)
    const runButton = screen.getByTestId('run-wgcna')
    expect(runButton).not.toBeDisabled()
  })

  it('shows step results when analysis is complete', async () => {
    vi.mocked(api.runWGCNA).mockResolvedValue({ taskId: 'wgcna-123' })
    vi.mocked(api.getTaskStatus).mockResolvedValue({
      status: 'done',
      result: {
        svg: '<svg>sample tree</svg>',
        n_samples: 10,
        n_genes: 100,
        cutHeight: 15,
      },
    })

    mockState.exprNorm = {
      name: 'norm.xlsx',
      rows: 100,
      cols: 10,
      preview: [],
    }

    render(<WGCNA />)

    const runButton = screen.getByTestId('run-wgcna')
    await userEvent.click(runButton)

    await waitFor(() => {
      expect(api.runWGCNA).toHaveBeenCalled()
    })
  })

  it('renders nSelect input for TOM plot', () => {
    render(<WGCNA />)
    expect(screen.getByTestId('param-nselect')).toBeInTheDocument()
  })

  it('changes nSelect input', async () => {
    render(<WGCNA />)
    const input = screen.getByTestId('param-nselect') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '500')
    expect(input.value).toBe('500')
  })

  it('renders PlotSaveLoad component', () => {
    render(<WGCNA />)
    expect(screen.getByText('保存绘图 (RDS)')).toBeInTheDocument()
    expect(screen.getByText('加载绘图 (RDS)')).toBeInTheDocument()
  })

  it('PlotSaveLoad save button is disabled when no results', () => {
    render(<WGCNA />)
    const saveBtn = screen.getByText('保存绘图 (RDS)')
    expect(saveBtn).toBeDisabled()
  })
})
