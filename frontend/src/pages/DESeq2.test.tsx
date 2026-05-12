import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  startAnalysis: vi.fn(),
  getTaskStatus: vi.fn(),
  getContrastPairs: vi.fn(),
  uploadFile: vi.fn(),
}))

// Mock workspace store - create a mutable state object
let mockState: Record<string, unknown> = {
  exprRaw: null,
  sampleInfo: null,
  deseq2Result: null,
  setExprRaw: vi.fn(),
  setSampleInfo: vi.fn(),
  setAnalysisResult: vi.fn(),
}

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    return selector(mockState)
  },
}))

import DESeq2 from './DESeq2'
import * as api from '../api/client'

describe('DESeq2 Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    // Reset mock state
    mockState = {
      exprRaw: null,
      sampleInfo: null,
      deseq2Result: null,
      setExprRaw: vi.fn(),
      setSampleInfo: vi.fn(),
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders DESeq2 page title', () => {
    render(<DESeq2 />)
    expect(screen.getByText('DESeq2 差异分析')).toBeInTheDocument()
  })

  it('renders data upload sections for raw counts and sampleInfo', () => {
    render(<DESeq2 />)
    expect(screen.getByTestId('deseq2-data-section')).toBeInTheDocument()
    expect(screen.getByText('原始 Counts 矩阵')).toBeInTheDocument()
    expect(screen.getByText('样本信息')).toBeInTheDocument()
  })

  it('renders load demo data button', () => {
    render(<DESeq2 />)
    expect(screen.getByTestId('load-demo-data')).toBeInTheDocument()
  })

  it('renders contrast selector section', () => {
    render(<DESeq2 />)
    expect(screen.getByTestId('contrast-selector')).toBeInTheDocument()
    expect(screen.getByText('请先加载样本信息')).toBeInTheDocument()
  })

  it('renders parameter panel with FC and FDR inputs', () => {
    render(<DESeq2 />)
    expect(screen.getByTestId('deseq2-params')).toBeInTheDocument()
    expect(screen.getByTestId('param-fc')).toBeInTheDocument()
    expect(screen.getByTestId('param-fdr')).toBeInTheDocument()
  })

  it('renders run analysis button', () => {
    render(<DESeq2 />)
    expect(screen.getByTestId('run-deseq2')).toBeInTheDocument()
  })

  it('disables run button when no data is loaded', () => {
    render(<DESeq2 />)
    const runBtn = screen.getByTestId('run-deseq2')
    expect(runBtn).toBeDisabled()
  })

  it('loads demo data when button is clicked', async () => {
    vi.mocked(api.loadDemoData).mockImplementation(async (type: string) => ({
      data: {
        type,
        preview: [['Gene', 'S1', 'S2'], ['A', '10', '20']],
        rows: 100,
        cols: 3,
      },
      status: 'ok',
    }))

    render(<DESeq2 />)
    const demoBtn = screen.getByTestId('load-demo-data')
    await userEvent.click(demoBtn)

    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('expr_raw')
    })
    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('sampleInfo')
    })
  })

  it('changes FC threshold input', async () => {
    render(<DESeq2 />)
    const fcInput = screen.getByTestId('param-fc') as HTMLInputElement
    await userEvent.clear(fcInput)
    await userEvent.type(fcInput, '3')
    expect(fcInput.value).toBe('3')
  })

  it('changes FDR threshold input', async () => {
    render(<DESeq2 />)
    const fdrInput = screen.getByTestId('param-fdr') as HTMLInputElement
    await userEvent.clear(fdrInput)
    await userEvent.type(fdrInput, '0.01')
    expect(fdrInput.value).toBe('0.01')
  })

  it('fetches contrast pairs when sampleInfo is loaded', async () => {
    vi.mocked(api.getContrastPairs).mockResolvedValue({
      status: 'ok',
      pairs: [
        { group1: 'Control', group2: 'Treatment1' },
        { group1: 'Control', group2: 'Treatment2' },
      ],
      groups: ['Control', 'Treatment1', 'Treatment2'],
    })

    // Set sampleInfo in mock state
    mockState.sampleInfo = {
      name: 'test',
      rows: 10,
      cols: 3,
      preview: [['Sample', 'Group'], ['S1', 'Control'], ['S2', 'Treatment1']],
    }

    render(<DESeq2 />)

    // Wait for contrast pairs to be fetched
    await waitFor(() => {
      expect(api.getContrastPairs).toHaveBeenCalled()
    })
  })

  it('renders downstream module links when analysis is complete', () => {
    // Set completed analysis result in mock state
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

    render(<DESeq2 />)

    // Check downstream links are rendered
    expect(screen.getByText('火山图')).toBeInTheDocument()
    expect(screen.getByText('富集分析')).toBeInTheDocument()
    expect(screen.getByText('GSEA')).toBeInTheDocument()
    expect(screen.getByText('基因趋势')).toBeInTheDocument()
    expect(screen.getByText('WGCNA')).toBeInTheDocument()
  })

  it('renders result table when results are available', () => {
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

    render(<DESeq2 />)
    expect(screen.getByTestId('result-table')).toBeInTheDocument()
  })

  it('renders filter input when results are available', () => {
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

    render(<DESeq2 />)
    expect(screen.getByTestId('filter-input')).toBeInTheDocument()
  })
})
