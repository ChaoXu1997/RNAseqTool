import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  loadDemoData: vi.fn(),
  getPlot: vi.fn(),
  uploadFile: vi.fn(),
  runGSEA: vi.fn(),
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

import GSEA from './GSEA'
import * as api from '../api/client'

describe('GSEA Page', () => {
  beforeEach(() => {
    vi.clearAllMocks()
    mockState = {
      deseq2Result: null,
      setAnalysisResult: vi.fn(),
    }
  })

  it('renders GSEA page title', () => {
    render(<GSEA />)
    expect(screen.getByText('GSEA 基因集富集分析')).toBeInTheDocument()
  })

  it('renders gene input section', () => {
    render(<GSEA />)
    expect(screen.getByTestId('gsea-gene-input-section')).toBeInTheDocument()
  })

  it('renders gene source radio buttons', () => {
    render(<GSEA />)
    expect(screen.getByTestId('gene-source-deseq2')).toBeInTheDocument()
    expect(screen.getByTestId('gene-source-upload')).toBeInTheDocument()
  })

  it('renders database selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('database-selector')).toBeInTheDocument()
  })

  it('renders species selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('species-selector')).toBeInTheDocument()
  })

  it('renders gene type selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('gene-type-selector')).toBeInTheDocument()
  })

  it('renders run GSEA button', () => {
    render(<GSEA />)
    expect(screen.getByTestId('run-gsea')).toBeInTheDocument()
  })

  it('renders result area', () => {
    render(<GSEA />)
    expect(screen.getByTestId('gsea-result-area')).toBeInTheDocument()
  })

  it('renders plot display area', () => {
    render(<GSEA />)
    expect(screen.getByTestId('gsea-plot-area')).toBeInTheDocument()
  })

  it('renders p-value cutoff input', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-p-cutoff')).toBeInTheDocument()
  })

  it('renders pAdjustMethod selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('padj-method-selector')).toBeInTheDocument()
  })

  it('changes p-value cutoff', async () => {
    render(<GSEA />)
    const pInput = screen.getByTestId('param-p-cutoff') as HTMLInputElement
    await userEvent.clear(pInput)
    await userEvent.type(pInput, '0.01')
    expect(pInput.value).toBe('0.01')
  })

  it('renders plot parameters panel', () => {
    render(<GSEA />)
    expect(screen.getByTestId('plot-params-panel')).toBeInTheDocument()
  })

  it('renders geneSetID input', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-genesetid')).toBeInTheDocument()
  })

  it('renders addGene checkbox', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-addgene')).toBeInTheDocument()
  })

  it('renders addPval checkbox', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-addpval')).toBeInTheDocument()
  })

  it('renders subPlot selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-subplot')).toBeInTheDocument()
  })

  it('renders base_size input', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-basesize')).toBeInTheDocument()
  })

  it('renders export buttons', () => {
    render(<GSEA />)
    expect(screen.getByTestId('export-svg')).toBeInTheDocument()
    expect(screen.getByTestId('export-rdata')).toBeInTheDocument()
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

    render(<GSEA />)
    expect(screen.getByTestId('gsea-contrast-selector')).toBeInTheDocument()
  })

  it('shows disabled message when DESeq2 source selected but no results', async () => {
    render(<GSEA />)
    const deseq2Radio = screen.getByTestId('gene-source-deseq2')
    await userEvent.click(deseq2Radio)

    await waitFor(() => {
      expect(screen.getByText(/需先运行 DESeq2/)).toBeInTheDocument()
    })
  })

  it('calls API when run GSEA is clicked with DESeq2 source', async () => {
    vi.mocked(api.runGSEA).mockResolvedValue({
      status: 'ok',
      result: {
        ID: ['pathway1'],
        Description: ['Pathway 1'],
        setSize: [100],
        enrichmentScore: [0.5],
        NES: [1.5],
        pvalue: [0.001],
        p_adjust: [0.005],
        qvalues: [0.003],
        rank: [50],
        leading_edge: ['tags=30%, list=10%, signal=25%'],
        core_enrichment: ['GENE1/GENE2/GENE3'],
      },
    })

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

    render(<GSEA />)

    const runBtn = screen.getByTestId('run-gsea')
    await userEvent.click(runBtn)

    await waitFor(() => {
      expect(api.runGSEA).toHaveBeenCalledWith(expect.objectContaining({
        source: 'deseq2',
        database: expect.any(String),
      }))
    })
  })

  it('generates plot when results are available', async () => {
    vi.mocked(api.runGSEA).mockResolvedValue({
      status: 'ok',
      result: {
        ID: ['pathway1'],
        Description: ['Pathway 1'],
        setSize: [100],
        enrichmentScore: [0.5],
        NES: [1.5],
        pvalue: [0.001],
        p_adjust: [0.005],
        qvalues: [0.003],
        rank: [50],
        leading_edge: ['tags=30%, list=10%, signal=25%'],
        core_enrichment: ['GENE1/GENE2/GENE3'],
      },
    })

    vi.mocked(api.getPlot).mockResolvedValue('<svg>test gsea plot</svg>')

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

    render(<GSEA />)

    const runBtn = screen.getByTestId('run-gsea')
    await userEvent.click(runBtn)

    await waitFor(() => {
      expect(api.runGSEA).toHaveBeenCalled()
    })
  })

  it('renders newGsea checkbox', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-newgsea')).toBeInTheDocument()
  })

  it('renders termWidth input', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-termwidth')).toBeInTheDocument()
  })

  it('renders arrowType selector', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-arrowtype')).toBeInTheDocument()
  })

  it('renders geneCol color input', () => {
    render(<GSEA />)
    expect(screen.getByTestId('param-genecol')).toBeInTheDocument()
  })

  it('renders PlotSaveLoad component', () => {
    render(<GSEA />)
    expect(screen.getByText('保存绘图 (RDS)')).toBeInTheDocument()
    expect(screen.getByText('加载绘图 (RDS)')).toBeInTheDocument()
  })

  it('PlotSaveLoad save button is disabled when no results', () => {
    render(<GSEA />)
    const saveBtn = screen.getByText('保存绘图 (RDS)')
    expect(saveBtn).toBeDisabled()
  })

  it('PlotSaveLoad save button is enabled when results exist', async () => {
    vi.mocked(api.runGSEA).mockResolvedValue({
      status: 'ok',
      result: {
        ID: ['pathway1'],
        Description: ['Pathway 1'],
        setSize: [100],
        enrichmentScore: [0.5],
        NES: [1.5],
        pvalue: [0.001],
        p_adjust: [0.005],
        qvalues: [0.003],
        rank: [50],
        leading_edge: ['tags=30%, list=10%, signal=25%'],
        core_enrichment: ['GENE1/GENE2/GENE3'],
      },
    })

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

    render(<GSEA />)

    const runBtn = screen.getByTestId('run-gsea')
    await userEvent.click(runBtn)

    await waitFor(() => {
      const saveBtn = screen.getByText('保存绘图 (RDS)')
      expect(saveBtn).not.toBeDisabled()
    })
  })

  it('calls savePlotRds when save button is clicked', async () => {
    vi.mocked(api.runGSEA).mockResolvedValue({
      status: 'ok',
      result: {
        ID: ['pathway1'],
        Description: ['Pathway 1'],
        setSize: [100],
        enrichmentScore: [0.5],
        NES: [1.5],
        pvalue: [0.001],
        p_adjust: [0.005],
        qvalues: [0.003],
        rank: [50],
        leading_edge: ['tags=30%, list=10%, signal=25%'],
        core_enrichment: ['GENE1/GENE2/GENE3'],
      },
    })

    vi.mocked(api.savePlotRds).mockResolvedValue({
      status: 'ok',
      data: 'base64data',
      filename: 'gsea_plot.rds',
    })

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

    render(<GSEA />)

    const runBtn = screen.getByTestId('run-gsea')
    await userEvent.click(runBtn)

    await waitFor(() => {
      expect(api.runGSEA).toHaveBeenCalled()
    })

    const saveBtn = screen.getByText('保存绘图 (RDS)')
    await userEvent.click(saveBtn)

    await waitFor(() => {
      expect(api.savePlotRds).toHaveBeenCalledWith(
        'gsea',
        expect.objectContaining({
          geneSetID: expect.any(String),
        }),
        expect.any(Object)
      )
    })
  })

  it('calls loadPlotRds when file is uploaded', async () => {
    vi.mocked(api.loadPlotRds).mockResolvedValue({
      status: 'ok',
      module: 'gsea',
      params: {
        geneSetID: 'pathway1',
        addGene: true,
        addPval: true,
      },
      svg: '<svg>test</svg>',
    })

    render(<GSEA />)

    const fileInput = screen.getByTestId('load-plot-file-input') as HTMLInputElement

    const file = new File(['test'], 'test.rds', { type: 'application/octet-stream' })
    await userEvent.upload(fileInput, file)

    await waitFor(() => {
      expect(api.loadPlotRds).toHaveBeenCalled()
    })
  })
})
