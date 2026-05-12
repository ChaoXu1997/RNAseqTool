import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  getPlot: vi.fn(),
  uploadFile: vi.fn(),
  runEnrich: vi.fn(),
  getDatabases: vi.fn(),
  getSpecies: vi.fn(),
  savePlotRds: vi.fn(),
  loadPlotRds: vi.fn(),
  default: {
    get: vi.fn(),
    post: vi.fn(),
  },
}))

// Mock workspace store
let mockState: Record<string, unknown> = {
  deseq2Result: null,
  setAnalysisResult: vi.fn(),
}

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    return selector(mockState)
  },
}))

import Enrich from './Enrich'
import * as api from '../api/client'

describe('Enrich Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockState = {
      deseq2Result: null,
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders Enrich page title', () => {
    render(<Enrich />)
    expect(screen.getByText('富集分析')).toBeInTheDocument()
  })

  it('renders gene input section', () => {
    render(<Enrich />)
    expect(screen.getByTestId('enrich-gene-input-section')).toBeInTheDocument()
  })

  it('renders gene source radio buttons', () => {
    render(<Enrich />)
    expect(screen.getByTestId('gene-source-deseq2')).toBeInTheDocument()
    expect(screen.getByTestId('gene-source-manual')).toBeInTheDocument()
  })

  it('renders manual gene input area', () => {
    render(<Enrich />)
    const manualRadio = screen.getByTestId('gene-source-manual')
    expect(manualRadio).toBeInTheDocument()
  })

  it('renders database selector', () => {
    render(<Enrich />)
    expect(screen.getByTestId('database-selector')).toBeInTheDocument()
  })

  it('renders species selector', () => {
    render(<Enrich />)
    expect(screen.getByTestId('species-selector')).toBeInTheDocument()
  })

  it('renders gene type selector', () => {
    render(<Enrich />)
    expect(screen.getByTestId('gene-type-selector')).toBeInTheDocument()
  })

  it('renders run enrich button', () => {
    render(<Enrich />)
    expect(screen.getByTestId('run-enrich')).toBeInTheDocument()
  })

  it('renders result table area', () => {
    render(<Enrich />)
    expect(screen.getByTestId('enrich-result-area')).toBeInTheDocument()
  })

  it('renders plot type selector', () => {
    render(<Enrich />)
    expect(screen.getByTestId('plot-type-selector')).toBeInTheDocument()
  })

  it('renders plot display area', () => {
    render(<Enrich />)
    expect(screen.getByTestId('enrich-plot-area')).toBeInTheDocument()
  })

  it('renders p-value cutoff input', () => {
    render(<Enrich />)
    expect(screen.getByTestId('param-p-cutoff')).toBeInTheDocument()
  })

  it('renders show category input', () => {
    render(<Enrich />)
    expect(screen.getByTestId('param-show-category')).toBeInTheDocument()
  })

  it('changes p-value cutoff', async () => {
    render(<Enrich />)
    const pInput = screen.getByTestId('param-p-cutoff') as HTMLInputElement
    await userEvent.clear(pInput)
    await userEvent.type(pInput, '0.01')
    expect(pInput.value).toBe('0.01')
  })

  it('changes show category input', async () => {
    render(<Enrich />)
    const catInput = screen.getByTestId('param-show-category') as HTMLInputElement
    await userEvent.clear(catInput)
    await userEvent.type(catInput, '15')
    expect(catInput.value).toBe('15')
  })

  it('selects manual gene input mode', async () => {
    render(<Enrich />)
    const manualRadio = screen.getByTestId('gene-source-manual')
    await userEvent.click(manualRadio)
    // Should show text area for manual input
    await waitFor(() => {
      expect(screen.getByTestId('gene-textarea')).toBeInTheDocument()
    })
  })

  it('selects DESeq2 source mode', async () => {
    // Set up mock DESeq2 result
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

    render(<Enrich />)
    const deseq2Radio = screen.getByTestId('gene-source-deseq2')
    await userEvent.click(deseq2Radio)

    // Should show direction selector
    await waitFor(() => {
      expect(screen.getByTestId('direction-selector')).toBeInTheDocument()
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

    render(<Enrich />)
    expect(screen.getByTestId('enrich-contrast-selector')).toBeInTheDocument()
  })

  it('shows disabled message when DESeq2 source selected but no results', async () => {
    render(<Enrich />)
    const deseq2Radio = screen.getByTestId('gene-source-deseq2')
    await userEvent.click(deseq2Radio)

    await waitFor(() => {
      expect(screen.getByText(/需先运行 DESeq2/)).toBeInTheDocument()
    })
  })

  it('renders export buttons', () => {
    render(<Enrich />)
    expect(screen.getByTestId('export-xlsx')).toBeInTheDocument()
  })

  it('calls API when genes are manually entered and run is clicked', async () => {
    vi.mocked(api.runEnrich).mockResolvedValue({
      status: 'ok',
      results: {
        Description: ['pathway1'],
        ID: ['ID1'],
        pvalue: [0.001],
        'p.adjust': [0.005],
        Count: [5],
        GeneRatio: ['5/100'],
        BgRatio: ['10/500'],
        geneID: ['GENE1/GENE2'],
        Database: ['GO_BP'],
        EnrichFactor: [0.2],
      },
    })

    render(<Enrich />)

    // Select manual input
    const manualRadio = screen.getByTestId('gene-source-manual')
    await userEvent.click(manualRadio)

    // Enter genes
    const textarea = await screen.findByTestId('gene-textarea')
    await userEvent.type(textarea, 'TP53{enter}BRCA1{enter}MYC')

    // Click run
    const runBtn = screen.getByTestId('run-enrich')
    await userEvent.click(runBtn)

    await waitFor(() => {
      expect(api.runEnrich).toHaveBeenCalledWith(expect.objectContaining({
        genes: expect.any(Array),
        database: expect.any(String),
      }))
    })
  })

  it('generates plot when plot button is clicked', async () => {
    vi.mocked(api.runEnrich).mockResolvedValue({
      status: 'ok',
      results: {
        Description: ['pathway1'],
        ID: ['ID1'],
        pvalue: [0.001],
        'p.adjust': [0.005],
        Count: [5],
        GeneRatio: ['5/100'],
        BgRatio: ['10/500'],
        geneID: ['GENE1/GENE2'],
        Database: ['GO_BP'],
        EnrichFactor: [0.2],
      },
    })

    vi.mocked(api.getPlot).mockResolvedValue('<svg>test plot</svg>')

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

    render(<Enrich />)

    const runBtn = screen.getByTestId('run-enrich')
    await userEvent.click(runBtn)

    await waitFor(() => {
      expect(api.runEnrich).toHaveBeenCalled()
    })
  })
})
