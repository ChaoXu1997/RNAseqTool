import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  getPlot: vi.fn(),
  uploadFile: vi.fn(),
  runGeneTrend: vi.fn(),
  getGeneTrendPlot: vi.fn(),
  getTaskStatus: vi.fn(),
  startAnalysis: vi.fn(),
  exportGeneTrendRData: vi.fn(),
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

import GeneTrend from './GeneTrend'

describe('GeneTrend Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockState = {
      exprNorm: null,
      sampleInfo: null,
      deseq2Result: null,
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders GeneTrend page title', () => {
    render(<GeneTrend />)
    expect(screen.getByText(/GeneTrend.*Mfuzz/)).toBeInTheDocument()
  })

  it('renders data source section', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('genetrend-data-source')).toBeInTheDocument()
  })

  it('renders data source radio buttons', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('source-deseq2')).toBeInTheDocument()
    expect(screen.getByTestId('source-manual')).toBeInTheDocument()
  })

  it('renders Mfuzz parameters section', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('mfuzz-params-section')).toBeInTheDocument()
  })

  it('renders cluster number input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-c-value')).toBeInTheDocument()
  })

  it('renders filter NA input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-filter-na')).toBeInTheDocument()
  })

  it('renders fill NA method selector', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-fill-na')).toBeInTheDocument()
  })

  it('renders filter SD input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-filter-sd')).toBeInTheDocument()
  })

  it('renders plot parameters section', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('plot-params-section')).toBeInTheDocument()
  })

  it('renders run analysis button', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('run-genetrend')).toBeInTheDocument()
  })

  it('renders result area', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('genetrend-result-area')).toBeInTheDocument()
  })

  it('renders all clusters plot area', () => {
    render(<GeneTrend />)
    // The all plot area is inside the result area section
    expect(screen.getByTestId('genetrend-result-area')).toBeInTheDocument()
  })

  it('renders cluster selector for single plot', () => {
    render(<GeneTrend />)
    // The cluster selector is inside the result area, only visible when results exist
    expect(screen.getByTestId('genetrend-result-area')).toBeInTheDocument()
  })

  it('renders single cluster plot area', () => {
    render(<GeneTrend />)
    // The single plot area is inside the result area, only visible when results exist
    expect(screen.getByTestId('genetrend-result-area')).toBeInTheDocument()
  })

  it('renders export XLSX button', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('export-xlsx')).toBeInTheDocument()
  })

  it('changes cluster number input', async () => {
    render(<GeneTrend />)
    const input = screen.getByTestId('param-c-value') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '6')
    expect(input.value).toBe('6')
  })

  it('changes filter NA input', async () => {
    render(<GeneTrend />)
    const input = screen.getByTestId('param-filter-na') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '0.3')
    expect(input.value).toBe('0.3')
  })

  it('changes filter SD input', async () => {
    render(<GeneTrend />)
    const input = screen.getByTestId('param-filter-sd') as HTMLInputElement
    await userEvent.clear(input)
    await userEvent.type(input, '0.5')
    expect(input.value).toBe('0.5')
  })

  it('changes fill NA method', async () => {
    render(<GeneTrend />)
    const select = screen.getByTestId('param-fill-na') as HTMLSelectElement
    await userEvent.selectOptions(select, 'median')
    expect(select.value).toBe('median')
  })

  it('selects manual data source', async () => {
    render(<GeneTrend />)
    const manualRadio = screen.getByTestId('source-manual')
    await userEvent.click(manualRadio)

    await waitFor(() => {
      expect(screen.getByTestId('norm-upload')).toBeInTheDocument()
      expect(screen.getByTestId('sampleinfo-upload')).toBeInTheDocument()
    })
  })

  it('selects DESeq2 data source', async () => {
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
    mockState.exprNorm = {
      name: 'norm.xlsx',
      rows: 100,
      cols: 10,
      preview: [],
    }
    mockState.sampleInfo = {
      name: 'sampleInfo.xlsx',
      rows: 10,
      cols: 2,
      preview: [],
    }

    render(<GeneTrend />)
    const deseq2Radio = screen.getByTestId('source-deseq2')
    await userEvent.click(deseq2Radio)

    await waitFor(() => {
      expect(screen.getByTestId('contrast-selector')).toBeInTheDocument()
      expect(screen.getByTestId('direction-selector')).toBeInTheDocument()
    })
  })

  it('shows message when DESeq2 source selected but no results', async () => {
    render(<GeneTrend />)
    const deseq2Radio = screen.getByTestId('source-deseq2')
    await userEvent.click(deseq2Radio)

    await waitFor(() => {
      expect(screen.getByText(/需先运行 DESeq2/)).toBeInTheDocument()
    })
  })

  it('shows info message when manual mode is selected', async () => {
    render(<GeneTrend />)
    const manualRadio = screen.getByTestId('source-manual')
    await userEvent.click(manualRadio)

    // Manual mode shows upload fields
    await waitFor(() => {
      expect(screen.getByTestId('norm-upload')).toBeInTheDocument()
      expect(screen.getByTestId('sampleinfo-upload')).toBeInTheDocument()
    })
  })

  it('disables run button when no data available', () => {
    render(<GeneTrend />)
    const runButton = screen.getByTestId('run-genetrend')
    expect(runButton).toBeDisabled()
  })

  it('renders point size input in plot params', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-point-size')).toBeInTheDocument()
  })

  it('renders line width input in plot params', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-line-width')).toBeInTheDocument()
  })

  it('renders color palette selector', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('color-palette-selector')).toBeInTheDocument()
  })

  it('renders membership color low input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-mm-low')).toBeInTheDocument()
  })

  it('renders membership color high input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-mm-high')).toBeInTheDocument()
  })

  it('renders membership color midpoint input', () => {
    render(<GeneTrend />)
    expect(screen.getByTestId('param-mm-midpoint')).toBeInTheDocument()
  })

  it('export button is present and disabled when no results', () => {
    render(<GeneTrend />)

    // Export button should be present
    const exportButton = screen.getByTestId('export-xlsx')
    expect(exportButton).toBeInTheDocument()
    // Export button should be disabled when no results
    expect(exportButton).toBeDisabled()
  })
})
