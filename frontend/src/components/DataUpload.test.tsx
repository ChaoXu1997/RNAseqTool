import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock the API client
vi.mock('../api/client', () => ({
  uploadFile: vi.fn(),
  loadDemoData: vi.fn(),
}))

// Mock the workspace store - return proper selector-based mock
const mockSetExprRaw = vi.fn()
const mockSetExprNorm = vi.fn()
const mockSetSampleInfo = vi.fn()

vi.mock('../stores/workspace', () => ({
  useWorkspaceStore: (selector: (state: Record<string, unknown>) => unknown) => {
    const state: Record<string, unknown> = {
      setExprRaw: mockSetExprRaw,
      setExprNorm: mockSetExprNorm,
      setSampleInfo: mockSetSampleInfo,
    }
    return selector(state)
  },
}))

import DataUpload from './DataUpload'
import * as api from '../api/client'

describe('DataUpload', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('renders file upload input', () => {
    render(<DataUpload />)
    expect(screen.getByTestId('file-input')).toBeInTheDocument()
  })

  it('renders data type selector with all types', () => {
    render(<DataUpload />)
    expect(screen.getByTestId('type-expr_raw')).toBeInTheDocument()
    expect(screen.getByTestId('type-expr_norm')).toBeInTheDocument()
    expect(screen.getByTestId('type-sampleInfo')).toBeInTheDocument()
    expect(screen.getByTestId('type-deseq2_result')).toBeInTheDocument()
  })

  it('renders demo data buttons', () => {
    render(<DataUpload />)
    expect(screen.getByTestId('demo-expr_raw')).toBeInTheDocument()
    expect(screen.getByTestId('demo-sampleInfo')).toBeInTheDocument()
  })

  it('loads demo data when demo button is clicked', async () => {
    const mockData = {
      status: 'ok',
      data: {
        type: 'expr_raw',
        preview: [['Gene', 'S1', 'S2'], ['A', '10', '20']],
        rows: 2,
        cols: 3,
      },
    }
    vi.mocked(api.loadDemoData).mockResolvedValue(mockData)

    render(<DataUpload />)
    const demoBtn = screen.getByTestId('demo-expr_raw')
    await userEvent.click(demoBtn)

    await waitFor(() => {
      expect(api.loadDemoData).toHaveBeenCalledWith('expr_raw')
    })

    await waitFor(() => {
      expect(screen.getByTestId('data-preview')).toBeInTheDocument()
    })
  })

  it('displays error message when demo data load fails', async () => {
    vi.mocked(api.loadDemoData).mockRejectedValue(new Error('Failed to load'))

    render(<DataUpload />)
    const demoBtn = screen.getByTestId('demo-expr_raw')
    await userEvent.click(demoBtn)

    await waitFor(() => {
      expect(screen.getByTestId('error-message')).toBeInTheDocument()
    })
  })

  it('uploads a file when file is selected', async () => {
    const mockResponse = {
      status: 'ok',
      data: {
        preview: [['Gene', 'S1'], ['A', '10']],
        rows: 1,
        cols: 2,
      },
    }
    vi.mocked(api.uploadFile).mockResolvedValue(mockResponse)

    render(<DataUpload />)
    const fileInput = screen.getByTestId('file-input') as HTMLInputElement
    const file = new File(['Gene,S1\nA,10'], 'test.csv', { type: 'text/csv' })

    await userEvent.upload(fileInput, file)

    await waitFor(() => {
      expect(api.uploadFile).toHaveBeenCalledWith(file, 'expr_raw')
    })
  })

  it('displays error when upload fails', async () => {
    vi.mocked(api.uploadFile).mockRejectedValue(new Error('Server validation failed'))

    render(<DataUpload />)
    const fileInput = screen.getByTestId('file-input') as HTMLInputElement
    // Use .csv extension so jsdom triggers onChange
    const file = new File(['Gene,S1\nA,10'], 'bad.csv', { type: 'text/csv' })

    await userEvent.upload(fileInput, file)

    await waitFor(() => {
      expect(screen.getByTestId('error-message')).toHaveTextContent('Server validation failed')
    })
  })

  it('shows preview table with correct dimensions', async () => {
    const mockData = {
      status: 'ok',
      data: {
        type: 'sampleInfo',
        preview: [['Sample', 'Group'], ['S1', 'A'], ['S2', 'B']],
        rows: 2,
        cols: 2,
      },
    }
    vi.mocked(api.loadDemoData).mockResolvedValue(mockData)

    render(<DataUpload />)
    await userEvent.click(screen.getByTestId('demo-sampleInfo'))

    await waitFor(() => {
      expect(screen.getByText('2 rows x 2 cols')).toBeInTheDocument()
    })
  })

  it('changes selected data type', async () => {
    render(<DataUpload />)

    const sampleInfoBtn = screen.getByTestId('type-sampleInfo')
    await userEvent.click(sampleInfoBtn)

    expect(sampleInfoBtn).toHaveClass('bg-blue-100')
  })
})
