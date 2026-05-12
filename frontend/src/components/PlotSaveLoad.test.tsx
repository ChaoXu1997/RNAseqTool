import { describe, it, expect, vi, beforeEach } from 'vitest'
import { render, screen, waitFor } from '@testing-library/react'
import userEvent from '@testing-library/user-event'

// Mock API client
vi.mock('../api/client', () => ({
  savePlotRds: vi.fn(),
  loadPlotRds: vi.fn(),
}))

import { PlotSaveLoad } from '../components/PlotSaveLoad'
import * as api from '../api/client'

describe('PlotSaveLoad Component', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('renders save and load buttons', () => {
    render(
      <PlotSaveLoad
        module="pca"
        params={{ title: 'Test' }}
        plotData={{ coordinates: [] }}
        disabled={false}
      />
    )
    expect(screen.getByTestId('save-plot-rds')).toBeInTheDocument()
    expect(screen.getByTestId('load-plot-rds')).toBeInTheDocument()
  })

  it('calls savePlotRds when save button is clicked', async () => {
    vi.mocked(api.savePlotRds).mockResolvedValue({
      status: 'ok',
      data: 'base64data',
      filename: 'pca_plot.rds',
    })

    render(
      <PlotSaveLoad
        module="pca"
        params={{ title: 'My PCA', dotSize: 3 }}
        plotData={{ coordinates: [{ PC1: 1, PC2: 2 }] }}
        disabled={false}
        onParamsLoaded={vi.fn()}
      />
    )

    const saveBtn = screen.getByTestId('save-plot-rds')
    await userEvent.click(saveBtn)

    await waitFor(() => {
      expect(api.savePlotRds).toHaveBeenCalledWith(
        'pca',
        { title: 'My PCA', dotSize: 3 },
        { coordinates: [{ PC1: 1, PC2: 2 }] }
      )
    })
  })

  it('renders load file input when load button is clicked', async () => {
    render(
      <PlotSaveLoad
        module="pca"
        params={{}}
        plotData={{}}
        disabled={false}
      />
    )

    const loadBtn = screen.getByTestId('load-plot-rds')
    expect(loadBtn).toBeInTheDocument()
  })

  it('calls loadPlotRds and onParamsLoaded when file is uploaded', async () => {
    const onParamsLoaded = vi.fn()

    vi.mocked(api.loadPlotRds).mockResolvedValue({
      status: 'ok',
      module: 'pca',
      params: { title: 'Loaded PCA', dotSize: 5, colors: ['red', 'blue'] },
      svg: '<svg>test</svg>',
    })

    render(
      <PlotSaveLoad
        module="pca"
        params={{}}
        plotData={{}}
        disabled={false}
        onParamsLoaded={onParamsLoaded}
      />
    )

    // Find the hidden file input
    const fileInput = screen.getByTestId('load-plot-file-input')
    const file = new File(['fake content'], 'test.rds', { type: 'application/octet-stream' })

    // Simulate file upload
    await userEvent.upload(fileInput, file)

    await waitFor(() => {
      expect(api.loadPlotRds).toHaveBeenCalled()
    })

    await waitFor(() => {
      expect(onParamsLoaded).toHaveBeenCalledWith({
        title: 'Loaded PCA',
        dotSize: 5,
        colors: ['red', 'blue'],
      })
    })
  })

  it('calls onSvgLoaded when SVG is returned from load', async () => {
    const onSvgLoaded = vi.fn()

    vi.mocked(api.loadPlotRds).mockResolvedValue({
      status: 'ok',
      module: 'pca',
      params: { title: 'Test' },
      svg: '<svg>loaded svg content</svg>',
    })

    render(
      <PlotSaveLoad
        module="pca"
        params={{}}
        plotData={{}}
        disabled={false}
        onSvgLoaded={onSvgLoaded}
      />
    )

    const fileInput = screen.getByTestId('load-plot-file-input')
    const file = new File(['fake'], 'test.rds', { type: 'application/octet-stream' })

    await userEvent.upload(fileInput, file)

    await waitFor(() => {
      expect(onSvgLoaded).toHaveBeenCalledWith('<svg>loaded svg content</svg>')
    })
  })

  it('disables buttons when disabled prop is true', () => {
    render(
      <PlotSaveLoad
        module="pca"
        params={{}}
        plotData={{}}
        disabled={true}
      />
    )

    expect(screen.getByTestId('save-plot-rds')).toBeDisabled()
  })

  it('disables save when plotData is empty', () => {
    render(
      <PlotSaveLoad
        module="pca"
        params={{}}
        plotData={null}
        disabled={false}
      />
    )

    expect(screen.getByTestId('save-plot-rds')).toBeDisabled()
  })

  it('triggers download on successful save', async () => {
    vi.mocked(api.savePlotRds).mockResolvedValue({
      status: 'ok',
      data: 'SGVsbG8gV29ybGQ=',
      filename: 'pca_plot.rds',
    })

    // Mock URL.createObjectURL and revokeObjectURL
    const mockUrl = 'blob:http://localhost/fake'
    globalThis.URL.createObjectURL = vi.fn(() => mockUrl)
    globalThis.URL.revokeObjectURL = vi.fn()

    render(
      <PlotSaveLoad
        module="pca"
        params={{ title: 'Test' }}
        plotData={{ coordinates: [] }}
        disabled={false}
      />
    )

    const saveBtn = screen.getByTestId('save-plot-rds')
    await userEvent.click(saveBtn)

    await waitFor(() => {
      expect(globalThis.URL.createObjectURL).toHaveBeenCalled()
    })
  })
})
