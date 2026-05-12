import { useState, useCallback, useEffect } from 'react'
import { loadDemoData, getPlot, uploadFile } from '../api/client'
import { useWorkspaceStore } from '../stores/workspace'
import { PlotSaveLoad } from '../components/PlotSaveLoad'

const DEFAULT_PARAMS = {
  title: 'Volcano Plot',
  colr_up: '#FC4E2A',
  colr_down: '#4393C3',
  colr_not: '#00000033',
  xlim_min: -10,
  xlim_max: 10,
  xbr: 5,
}

type VolcanoParams = typeof DEFAULT_PARAMS

interface DESeq2ContrastResult {
  contrasts: Record<string, Record<string, unknown>[]>
  summary: Array<{ contrast: string; up_genes: number; down_genes: number; total_genes: number }>
  parameters: { fc: number; fdr: number }
}

export default function Volcano() {
  // Store state
  const deseq2Result = useWorkspaceStore((s) => s.deseq2Result)

  // Local state
  const [params, setParams] = useState<VolcanoParams>(DEFAULT_PARAMS)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [svg, setSvg] = useState<string | null>(null)
  const [plotLoading, setPlotLoading] = useState(false)
  const [degsData, setDegsData] = useState<Record<string, unknown>[] | null>(null)
  const [selectedContrast, setSelectedContrast] = useState<string | null>(null)
  const [dataSource, setDataSource] = useState<'workspace' | 'upload'>('workspace')

  // Check if DESeq2 results are available from workspace
  const hasWorkspaceResults = deseq2Result?.status === 'done' && deseq2Result.data !== null

  // Default to 'upload' when no workspace results
  useEffect(() => {
    if (!hasWorkspaceResults && dataSource === 'workspace') {
      setDataSource('upload')
    }
  }, [hasWorkspaceResults, dataSource])

  // Load DESeq2 results from workspace when available
  useEffect(() => {
    if (hasWorkspaceResults && dataSource === 'workspace') {
      const data = deseq2Result!.data as DESeq2ContrastResult
      const firstContrast = Object.keys(data.contrasts)[0]
      if (firstContrast) {
        setSelectedContrast(firstContrast)
        setDegsData(data.contrasts[firstContrast])
      }
    }
  }, [hasWorkspaceResults, dataSource, deseq2Result])

  // Handle contrast selection change
  const handleContrastChange = useCallback(
    (contrastName: string) => {
      setSelectedContrast(contrastName)
      if (deseq2Result?.data) {
        const data = deseq2Result.data as DESeq2ContrastResult
        setDegsData(data.contrasts[contrastName] || null)
        setSvg(null)
      }
    },
    [deseq2Result]
  )

  // Handle DESeq2 result file upload
  const handleDeseq2Upload = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return
      setError(null)
      setLoading(true)
      setDataSource('upload')
      try {
        const res = await uploadFile(file, 'deseq2_result')
        // The uploaded data should be in preview format
        const preview = res.data.preview as string[][]
        if (preview && preview.length > 1) {
          const headers = preview[0]
          const rows = preview.slice(1).map((row: string[]) => {
            const obj: Record<string, unknown> = {}
            headers.forEach((h: string, i: number) => {
              obj[h] = row[i]
            })
            return obj
          })
          setDegsData(rows)
        }
      } catch (err: unknown) {
        const msg =
          (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
          (err as Error)?.message ?? 'Upload failed'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    []
  )

  // Load demo data
  const handleLoadDemo = useCallback(async () => {
    setError(null)
    setLoading(true)
    setDataSource('upload')
    try {
      const res = await loadDemoData('deseq2_result')
      const preview = res.data.preview as string[][]
      if (preview && preview.length > 1) {
        const headers = preview[0]
        const rows = preview.slice(1).map((row: string[]) => {
          const obj: Record<string, unknown> = {}
          headers.forEach((h: string, i: number) => {
            obj[h] = row[i]
          })
          return obj
        })
        setDegsData(rows)
      }
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Failed to load demo data'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [])

  // Generate volcano plot
  const handlePlot = useCallback(async () => {
    if (!degsData) return
    setError(null)
    setSvg(null)
    setPlotLoading(true)

    try {
      const svgContent = await getPlot('volcano', {
        degs: degsData,
        params: {
          title: params.title,
          colr_up: params.colr_up,
          colr_down: params.colr_down,
          colr_not: params.colr_not,
          xlim: [params.xlim_min, params.xlim_max],
          xbr: params.xbr,
        },
      })
      setSvg(typeof svgContent === 'string' ? svgContent : (svgContent as { svg: string }).svg)
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Failed to generate volcano plot'
      setError(String(msg))
    } finally {
      setPlotLoading(false)
    }
  }, [degsData, params])

  // Auto-plot when data is loaded
  useEffect(() => {
    if (degsData) {
      handlePlot()
    }
  }, [degsData])

  // Export handlers
  const handleExport = useCallback(
    async (format: 'svg' | 'png' | 'pdf' | 'tif') => {
      if (!svg) return
      try {
        if (format === 'svg') {
          const blob = new Blob([svg], { type: 'image/svg+xml' })
          const url = URL.createObjectURL(blob)
          const a = document.createElement('a')
          a.href = url
          a.download = `volcano_plot.svg`
          a.click()
          URL.revokeObjectURL(url)
        } else if (format === 'png') {
          // Render SVG to canvas then export as PNG
          const canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (!ctx) return

          const img = new Image()
          const svgBlob = new Blob([svg], { type: 'image/svg+xml;charset=utf-8' })
          const url = URL.createObjectURL(svgBlob)

          img.onload = () => {
            canvas.width = img.width * 2
            canvas.height = img.height * 2
            ctx.scale(2, 2)
            ctx.drawImage(img, 0, 0)
            canvas.toBlob((blob) => {
              if (blob) {
                const pngUrl = URL.createObjectURL(blob)
                const a = document.createElement('a')
                a.href = pngUrl
                a.download = 'volcano_plot.png'
                a.click()
                URL.revokeObjectURL(pngUrl)
              }
            }, 'image/png')
            URL.revokeObjectURL(url)
          }
          img.src = url
        } else {
          // PDF/TIF - client-side approximation using canvas
          const canvas = document.createElement('canvas')
          const ctx = canvas.getContext('2d')
          if (!ctx) return

          const img = new Image()
          const svgBlob = new Blob([svg], { type: 'image/svg+xml;charset=utf-8' })
          const url = URL.createObjectURL(svgBlob)

          img.onload = () => {
            canvas.width = img.width * 2
            canvas.height = img.height * 2
            ctx.scale(2, 2)
            ctx.drawImage(img, 0, 0)
            const mimeType = format === 'pdf' ? 'application/pdf' : 'image/tiff'
            canvas.toBlob((blob) => {
              if (blob) {
                const exportUrl = URL.createObjectURL(blob)
                const a = document.createElement('a')
                a.href = exportUrl
                a.download = `volcano_plot.${format}`
                a.click()
                URL.revokeObjectURL(exportUrl)
              }
            }, mimeType)
            URL.revokeObjectURL(url)
          }
          img.src = url
        }
      } catch {
        setError('Export failed')
      }
    },
    [svg]
  )

  // Save plot object as RData
  const handleSaveRData = useCallback(async () => {
    if (!degsData) return
    try {
      const api = (await import('../api/client')).default
      const res = await api.post(
        '/export/volcano/rdata',
        {
          degs: degsData,
          params: {
            title: params.title,
            colr_up: params.colr_up,
            colr_down: params.colr_down,
            colr_not: params.colr_not,
            xlim: [params.xlim_min, params.xlim_max],
            xbr: params.xbr,
          },
        },
        { responseType: 'json' }
      )
      const data = res.data as { status: string; data: string; filename: string }
      if (data.status === 'ok') {
        // Decode base64 and download
        const byteChars = atob(data.data)
        const byteNumbers = new Array(byteChars.length)
        for (let i = 0; i < byteChars.length; i++) {
          byteNumbers[i] = byteChars.charCodeAt(i)
        }
        const byteArray = new Uint8Array(byteNumbers)
        const blob = new Blob([byteArray], { type: 'application/octet-stream' })
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = data.filename
        a.click()
        URL.revokeObjectURL(url)
      }
    } catch {
      setError('Failed to save RData')
    }
  }, [degsData, params])

  // Handle params loaded from RDS
  const handleParamsLoaded = useCallback(
    (loadedParams: Record<string, unknown>) => {
      setParams((prev) => ({
        ...prev,
        title: (loadedParams.title as string) ?? prev.title,
        colr_up: (loadedParams.colr_up as string) ?? prev.colr_up,
        colr_down: (loadedParams.colr_down as string) ?? prev.colr_down,
        colr_not: (loadedParams.colr_not as string) ?? prev.colr_not,
        xlim_min: (loadedParams.xlim as number[] | undefined)?.[0] ?? prev.xlim_min,
        xlim_max: (loadedParams.xlim as number[] | undefined)?.[1] ?? prev.xlim_max,
        xbr: (loadedParams.xbr as number) ?? prev.xbr,
      }))
    },
    []
  )

  // Handle SVG loaded from RDS
  const handleSvgLoaded = useCallback((svgContent: string) => {
    setSvg(svgContent)
  }, [])

  // Get available contrasts
  const getContrasts = (): string[] => {
    if (!hasWorkspaceResults) return []
    const data = deseq2Result!.data as DESeq2ContrastResult
    return Object.keys(data.contrasts)
  }

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">Volcano 火山图</h2>

      {/* Data Source Section */}
      <div data-testid="volcano-data-section" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">DESeq2 结果数据</h3>

        {/* Data source toggle */}
        <div className="flex gap-4 mb-4">
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              checked={dataSource === 'workspace'}
              onChange={() => setDataSource('workspace')}
              disabled={!hasWorkspaceResults}
              className="w-4 h-4"
            />
            <span className="text-sm">从 Workspace 加载</span>
            {!hasWorkspaceResults && (
              <span className="text-xs text-gray-400">(需先运行 DESeq2)</span>
            )}
          </label>
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              checked={dataSource === 'upload'}
              onChange={() => setDataSource('upload')}
              className="w-4 h-4"
            />
            <span className="text-sm">上传 DESeq2 结果文件</span>
          </label>
        </div>

        {/* Workspace contrast selector */}
        {dataSource === 'workspace' && hasWorkspaceResults && (
          <div data-testid="volcano-contrast-selector" className="mb-4">
            <label className="block text-sm text-gray-600 mb-1">选择 Contrast</label>
            <select
              value={selectedContrast ?? ''}
              onChange={(e) => handleContrastChange(e.target.value)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              {getContrasts().map((name) => (
                <option key={name} value={name}>
                  {name}
                </option>
              ))}
            </select>
          </div>
        )}

        {/* Upload section */}
        {dataSource === 'upload' && (
          <div className="space-y-3">
            <div>
              <label className="block text-sm text-gray-600 mb-1">上传 DESeq2 结果 (.xlsx/.csv)</label>
              <input
                type="file"
                accept=".csv,.xlsx"
                onChange={handleDeseq2Upload}
                disabled={loading}
                data-testid="deseq2-file-input"
                className="block w-full text-sm text-gray-500 file:mr-4 file:py-1.5 file:px-3 file:rounded file:border-0 file:text-sm file:bg-blue-50 file:text-blue-700"
              />
            </div>
            <div>
              <button
                data-testid="load-demo-data"
                onClick={handleLoadDemo}
                disabled={loading}
                className="px-4 py-2 rounded-md text-sm bg-green-50 text-green-700 hover:bg-green-100 disabled:opacity-50"
              >
                {loading ? '加载中...' : '加载示例数据'}
              </button>
            </div>
          </div>
        )}

        {/* Data loaded indicator */}
        {degsData && (
          <div className="text-sm text-green-700 bg-green-50 p-2 rounded mt-2">
            ✓ 已加载 {degsData.length} 个基因的 DESeq2 结果
          </div>
        )}
      </div>

      {/* Parameter Panel */}
      <div data-testid="volcano-params" className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">绘图参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {/* Title */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">标题</label>
            <input
              data-testid="param-title"
              type="text"
              value={params.title}
              onChange={(e) => setParams((p) => ({ ...p, title: e.target.value }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Color Up */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">上调颜色</label>
            <input
              data-testid="param-colr-up"
              type="text"
              value={params.colr_up}
              onChange={(e) => setParams((p) => ({ ...p, colr_up: e.target.value }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Color Down */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">下调颜色</label>
            <input
              data-testid="param-colr-down"
              type="text"
              value={params.colr_down}
              onChange={(e) => setParams((p) => ({ ...p, colr_down: e.target.value }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Color Not */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">非显著颜色</label>
            <input
              data-testid="param-colr-not"
              type="text"
              value={params.colr_not}
              onChange={(e) => setParams((p) => ({ ...p, colr_not: e.target.value }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Xlim Min */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">X 轴最小值</label>
            <input
              data-testid="param-xlim-min"
              type="number"
              value={params.xlim_min}
              onChange={(e) => setParams((p) => ({ ...p, xlim_min: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Xlim Max */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">X 轴最大值</label>
            <input
              data-testid="param-xlim-max"
              type="number"
              value={params.xlim_max}
              onChange={(e) => setParams((p) => ({ ...p, xlim_max: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* X Break */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">X 轴刻度间隔</label>
            <input
              data-testid="param-xbr"
              type="number"
              min={1}
              value={params.xbr}
              onChange={(e) => setParams((p) => ({ ...p, xbr: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>

        {/* Re-plot button */}
        <div>
          <button
            data-testid="run-volcano"
            onClick={handlePlot}
            disabled={!degsData || plotLoading}
            className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {plotLoading ? '生成中...' : '生成火山图'}
          </button>
        </div>
      </div>

      {/* Error */}
      {error && (
        <div data-testid="error-message" className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Plot Area */}
      <div data-testid="volcano-plot-area" className="border border-gray-200 rounded-lg p-4 min-h-[400px]">
        {plotLoading && <div className="text-center text-gray-500 py-20">生成图表中...</div>}
        {svg && !plotLoading && (
          <div dangerouslySetInnerHTML={{ __html: svg }} className="flex justify-center" />
        )}
        {!svg && !plotLoading && (
          <div className="text-center text-gray-400 py-20">加载数据后将在此显示火山图</div>
        )}
      </div>

      {/* Export & Save Buttons */}
      <div className="flex gap-3 flex-wrap">
        <button
          data-testid="export-svg"
          onClick={() => handleExport('svg')}
          disabled={!svg}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          导出 SVG
        </button>
        <button
          data-testid="export-png"
          onClick={() => handleExport('png')}
          disabled={!svg}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          导出 PNG
        </button>
        <button
          data-testid="export-pdf"
          onClick={() => handleExport('pdf')}
          disabled={!svg}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          导出 PDF
        </button>
        <button
          data-testid="export-tif"
          onClick={() => handleExport('tif')}
          disabled={!svg}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          导出 TIF
        </button>
        <button
          data-testid="save-rdata"
          onClick={handleSaveRData}
          disabled={!degsData}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          保存为 RData
        </button>
        <PlotSaveLoad
          module="volcano"
          params={{
            title: params.title,
            colr_up: params.colr_up,
            colr_down: params.colr_down,
            colr_not: params.colr_not,
            xlim: [params.xlim_min, params.xlim_max],
            xbr: params.xbr,
          }}
          plotData={degsData ? { degs: degsData } : null}
          disabled={!degsData}
          onParamsLoaded={handleParamsLoaded}
          onSvgLoaded={handleSvgLoaded}
        />
      </div>
    </div>
  )
}
