import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  getPlot: vi.fn(),
  uploadFile: vi.fn(),
  exportPlot: vi.fn(),
}))

// Mock workspace store - create a mutable state object
let mockState: Record<string, unknown> = {
  deseq2Result: null,
  setAnalysisResult: vi.fn(),
}

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    return selector(mockState)
  },
}))

import Volcano from './Volcano'
import * as api from '../api/client'

describe('Volcano Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    // Reset mock state
    mockState = {
      deseq2Result: null,
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders Volcano page title', () => {
    render(<Volcano />)
    expect(screen.getByText('Volcano 火山图')).toBeInTheDocument()
  })

  it('renders data source section', () => {
    render(<Volcano />)
    expect(screen.getByTestId('volcano-data-section')).toBeInTheDocument()
    expect(screen.getByText('DESeq2 结果数据')).toBeInTheDocument()
  })

  it('renders file upload for DESeq2 results', () => {
    render(<Volcano />)
    expect(screen.getByTestId('deseq2-file-input')).toBeInTheDocument()
  })

  it('renders load demo data button', () => {
    render(<Volcano />)
    expect(screen.getByTestId('load-demo-data')).toBeInTheDocument()
  })

  it('renders parameter panel with color inputs', () => {
    render(<Volcano />)
    expect(screen.getByTestId('volcano-params')).toBeInTheDocument()
    expect(screen.getByTestId('param-title')).toBeInTheDocument()
    expect(screen.getByTestId('param-colr-up')).toBeInTheDocument()
    expect(screen.getByTestId('param-colr-down')).toBeInTheDocument()
    expect(screen.getByTestId('param-colr-not')).toBeInTheDocument()
  })

  it('renders xlim and xbr inputs', () => {
    render(<Volcano />)
    expect(screen.getByTestId('param-xlim-min')).toBeInTheDocument()
    expect(screen.getByTestId('param-xlim-max')).toBeInTheDocument()
    expect(screen.getByTestId('param-xbr')).toBeInTheDocument()
  })

  it('renders plot display area', () => {
    render(<Volcano />)
    expect(screen.getByTestId('volcano-plot-area')).toBeInTheDocument()
  })

  it('renders export buttons', () => {
    render(<Volcano />)
    expect(screen.getByTestId('export-svg')).toBeInTheDocument()
    expect(screen.getByTestId('export-png')).toBeInTheDocument()
    expect(screen.getByTestId('export-pdf')).toBeInTheDocument()
    expect(screen.getByTestId('export-tif')).toBeInTheDocument()
  })

  it('renders save RData button', () => {
    render(<Volcano />)
    expect(screen.getByTestId('save-rdata')).toBeInTheDocument()
  })

  it('changes title input', async () => {
    render(<Volcano />)
    const titleInput = screen.getByTestId('param-title') as HTMLInputElement
    await userEvent.clear(titleInput)
    await userEvent.type(titleInput, 'My Volcano Plot')
    expect(titleInput.value).toBe('My Volcano Plot')
  })

  it('changes colr_up color input', async () => {
    render(<Volcano />)
    const colrUpInput = screen.getByTestId('param-colr-up') as HTMLInputElement
    await userEvent.clear(colrUpInput)
    await userEvent.type(colrUpInput, '#ff0000')
    expect(colrUpInput.value).toBe('#ff0000')
  })

  it('changes xlim min input', async () => {
    render(<Volcano />)
    const xlimMinInput = screen.getByTestId('param-xlim-min') as HTMLInputElement
    await userEvent.clear(xlimMinInput)
    // Use a positive value since userEvent.type has issues with negative numbers in number inputs
    await userEvent.type(xlimMinInput, '8')
    expect(xlimMinInput.value).toBe('8')
  })

  it('changes xbr input', async () => {
    render(<Volcano />)
    const xbrInput = screen.getByTestId('param-xbr') as HTMLInputElement
    await userEvent.clear(xbrInput)
    await userEvent.type(xbrInput, '2')
    expect(xbrInput.value).toBe('2')
  })

  it('loads demo data when button is clicked', async () => {
    vi.mocked(api.loadDemoData).mockResolvedValue({
      data: {
        type: 'deseq2_result',
        preview: [
          ['Gene', 'log2FoldChange', 'padj', 'change'],
          ['G1', '3.5', '0.001', 'Up'],
          ['G2', '-2.1', '0.01', 'Down'],
        ],
        rows: 100,
        cols: 8,
      },
      status: 'ok',
    })

    render(<Volcano />)
    const demoBtn = screen.getByTestId('load-demo-data')
    await userEvent.click(demoBtn)

    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('deseq2_result')
    })
  })

  it('renders contrast selector when DESeq2 result is available', () => {
    mockState.deseq2Result = {
      taskId: 'test-123',
      status: 'done',
      data: {
        contrasts: {
          Control_vs_Treatment: [
            { Gene: 'Gene1', log2FoldChange: 2.5, padj: 0.001, change: 'Up' },
          ],
        },
        summary: [{ contrast: 'Control_vs_Treatment', up_genes: 100, down_genes: 50, total_genes: 1000 }],
        parameters: { fc: 2, fdr: 0.05 },
      },
    }

    render(<Volcano />)
    expect(screen.getByTestId('volcano-contrast-selector')).toBeInTheDocument()
  })
})
