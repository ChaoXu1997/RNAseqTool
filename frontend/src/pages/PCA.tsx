import { useState, useCallback, useEffect } from 'react'
import { loadDemoData, startAnalysis, getTaskStatus, getPlot, uploadFile } from '../api/client'
import { useWorkspaceStore, type DataFile } from '../stores/workspace'

const DEFAULT_PARAMS = {
  title: 'Principal Component Analysis',
  colors: ['#e41a1c', '#377eb8', '#4daf4a', '#984ea3'],
  ellipse: false,
  labels: false,
  dotSize: 2,
  xlim: [-150, 150] as [number, number],
  ylim: [-150, 150] as [number, number],
}

type PcaParams = typeof DEFAULT_PARAMS

export default function PCA() {
  // Store state
  const exprNorm = useWorkspaceStore((s) => s.exprNorm)
  const sampleInfo = useWorkspaceStore((s) => s.sampleInfo)
  const pcaResult = useWorkspaceStore((s) => s.pcaResult)
  const setExprNorm = useWorkspaceStore((s) => s.setExprNorm)
  const setSampleInfo = useWorkspaceStore((s) => s.setSampleInfo)
  const setAnalysisResult = useWorkspaceStore((s) => s.setAnalysisResult)

  // Local state
  const [params, setParams] = useState<PcaParams>(DEFAULT_PARAMS)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [svg, setSvg] = useState<string | null>(null)
  const [plotLoading, setPlotLoading] = useState(false)

  const canRun = exprNorm !== null && sampleInfo !== null

  // Load demo data for both norm and sampleInfo
  const handleLoadDemo = useCallback(async () => {
    setError(null)
    setLoading(true)
    try {
      const [normRes, sampleRes] = await Promise.all([
        loadDemoData('expr_norm'),
        loadDemoData('sampleInfo'),
      ])
      setExprNorm({
        name: 'demo_norm',
        rows: normRes.data.rows,
        cols: normRes.data.cols,
        preview: normRes.data.preview,
      })
      setSampleInfo({
        name: 'demo_sampleInfo',
        rows: sampleRes.data.rows,
        cols: sampleRes.data.cols,
        preview: sampleRes.data.preview,
      })
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Failed to load demo data'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [setExprNorm, setSampleInfo])

  // Handle norm file upload
  const handleNormUpload = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return
      setError(null)
      setLoading(true)
      try {
        const res = await uploadFile(file, 'expr_norm')
        setExprNorm({
          name: file.name,
          rows: res.data.rows,
          cols: res.data.cols,
          preview: res.data.preview,
        })
      } catch (err: unknown) {
        const msg =
          (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
          (err as Error)?.message ?? 'Upload failed'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    [setExprNorm]
  )

  // Handle sampleInfo file upload
  const handleSampleInfoUpload = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return
      setError(null)
      setLoading(true)
      try {
        const res = await uploadFile(file, 'sampleInfo')
        setSampleInfo({
          name: file.name,
          rows: res.data.rows,
          cols: res.data.cols,
          preview: res.data.preview,
        })
      } catch (err: unknown) {
        const msg =
          (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
          (err as Error)?.message ?? 'Upload failed'
        setError(String(msg))
      } finally {
        setLoading(false)
      }
    },
    [setSampleInfo]
  )

  // Run PCA analysis
  const handleRunPCA = useCallback(async () => {
    if (!canRun) return
    setError(null)
    setSvg(null)
    setLoading(true)

    try {
      const taskId = await startAnalysis('pca', {
        norm: exprNorm,
        sampleInfo: sampleInfo,
      })

      // Poll for task completion
      const poll = async (): Promise<void> => {
        const status = await getTaskStatus(taskId)
        if (status.status === 'done') {
          setAnalysisResult('pca', {
            taskId,
            status: 'done',
            data: status.result,
          })
          return
        }
        if (status.status === 'error') {
          throw new Error(status.error ?? 'PCA analysis failed')
        }
        // Still pending/running, poll again
        await new Promise((r) => setTimeout(r, 500))
        return poll()
      }

      await poll()
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'PCA analysis failed'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [canRun, exprNorm, sampleInfo, setAnalysisResult])

  // Generate plot when result or params change
  useEffect(() => {
    if (!pcaResult || pcaResult.status !== 'done' || !pcaResult.data) return

    const generatePlot = async () => {
      setPlotLoading(true)
      try {
        const data = pcaResult.data as {
          coordinates: DataFile['preview']
          variance_explained: Record<string, number>
        }
        const svgContent = await getPlot('pca', {
          coordinates: data.coordinates,
          params: {
            title: params.title,
            colors: params.colors,
            ellipse: params.ellipse,
            labels: params.labels,
            dotSize: params.dotSize,
            xlim: params.xlim,
            ylim: params.ylim,
          },
          variance_explained: data.variance_explained,
        })
        setSvg(typeof svgContent === 'string' ? svgContent : (svgContent as { svg: string }).svg)
      } catch (err: unknown) {
        const msg = (err as Error)?.message ?? 'Failed to generate plot'
        setError(String(msg))
      } finally {
        setPlotLoading(false)
      }
    }

    generatePlot()
  }, [pcaResult, params])

  // Export handlers
  const handleExport = useCallback(
    async (format: 'svg' | 'png') => {
      if (!svg) return
      try {
        if (format === 'svg') {
          const blob = new Blob([svg], { type: 'image/svg+xml' })
          const url = URL.createObjectURL(blob)
          const a = document.createElement('a')
          a.href = url
          a.download = `pca_plot.svg`
          a.click()
          URL.revokeObjectURL(url)
        } else {
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
                a.download = 'pca_plot.png'
                a.click()
                URL.revokeObjectURL(pngUrl)
              }
            }, 'image/png')
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

  // Download PCA coordinates as XLSX
  const handleDownloadXlsx = useCallback(() => {
    if (!pcaResult?.data) return
    try {
      const data = pcaResult.data as { coordinates: Record<string, unknown>[] }
      const coords = data.coordinates

      // Build CSV content (simple approach, user can save as xlsx)
      if (!coords || coords.length === 0) return
      const headers = Object.keys(coords[0])
      const csvRows = [
        headers.join(','),
        ...coords.map((row: Record<string, unknown>) =>
          headers.map((h) => String(row[h] ?? '')).join(',')
        ),
      ]
      const csv = csvRows.join('\n')
      const blob = new Blob([csv], { type: 'text/csv' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = 'pca_coordinates.csv'
      a.click()
      URL.revokeObjectURL(url)
    } catch {
      setError('Download failed')
    }
  }, [pcaResult])

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">PCA 分析</h2>

      {/* Data Section */}
      <div data-testid="pca-data-section" className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {/* Norm Expression Upload */}
        <div className="border border-gray-200 rounded-lg p-4">
          <h3 className="font-medium mb-2">表达矩阵（Norm）</h3>
          {exprNorm ? (
            <div className="text-sm text-green-700 bg-green-50 p-2 rounded">
              ✓ {exprNorm.name} ({exprNorm.rows} rows × {exprNorm.cols} cols)
            </div>
          ) : (
            <div className="text-sm text-gray-500">未加载</div>
          )}
          <input
            type="file"
            accept=".csv,.xlsx"
            onChange={handleNormUpload}
            disabled={loading}
            data-testid="norm-file-input"
            className="mt-2 block w-full text-sm text-gray-500 file:mr-4 file:py-1.5 file:px-3 file:rounded file:border-0 file:text-sm file:bg-blue-50 file:text-blue-700"
          />
        </div>

        {/* SampleInfo Upload */}
        <div className="border border-gray-200 rounded-lg p-4">
          <h3 className="font-medium mb-2">样本信息</h3>
          {sampleInfo ? (
            <div className="text-sm text-green-700 bg-green-50 p-2 rounded">
              ✓ {sampleInfo.name} ({sampleInfo.rows} rows × {sampleInfo.cols} cols)
            </div>
          ) : (
            <div className="text-sm text-gray-500">未加载</div>
          )}
          <input
            type="file"
            accept=".csv,.xlsx"
            onChange={handleSampleInfoUpload}
            disabled={loading}
            data-testid="sampleinfo-file-input"
            className="mt-2 block w-full text-sm text-gray-500 file:mr-4 file:py-1.5 file:px-3 file:rounded file:border-0 file:text-sm file:bg-blue-50 file:text-blue-700"
          />
        </div>
      </div>

      {/* Load Demo Data Button */}
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

      {/* Parameter Panel */}
      <div data-testid="pca-params" className="border border-gray-200 rounded-lg p-4 space-y-4">
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

          {/* Dot Size */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">点大小</label>
            <input
              data-testid="param-dot-size"
              type="number"
              min={1}
              max={10}
              step={0.5}
              value={params.dotSize}
              onChange={(e) => setParams((p) => ({ ...p, dotSize: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Ellipse Toggle */}
          <div className="flex items-center gap-2">
            <input
              data-testid="param-ellipse"
              type="checkbox"
              checked={params.ellipse}
              onChange={(e) => setParams((p) => ({ ...p, ellipse: e.target.checked }))}
              className="w-4 h-4"
            />
            <label className="text-sm text-gray-600">显示置信椭圆</label>
          </div>

          {/* Labels Toggle */}
          <div className="flex items-center gap-2">
            <input
              data-testid="param-labels"
              type="checkbox"
              checked={params.labels}
              onChange={(e) => setParams((p) => ({ ...p, labels: e.target.checked }))}
              className="w-4 h-4"
            />
            <label className="text-sm text-gray-600">显示样本标签</label>
          </div>

          {/* Color Pickers */}
          {params.colors.map((color, i) => (
            <div key={i}>
              <label className="block text-sm text-gray-600 mb-1">颜色 {i + 1}</label>
              <input
                type="color"
                value={color}
                onChange={(e) => {
                  const newColors = [...params.colors]
                  newColors[i] = e.target.value
                  setParams((p) => ({ ...p, colors: newColors }))
                }}
                className="w-full h-8 border border-gray-300 rounded cursor-pointer"
              />
            </div>
          ))}
        </div>
      </div>

      {/* Run Button */}
      <div>
        <button
          data-testid="run-pca"
          onClick={handleRunPCA}
          disabled={!canRun || loading}
          className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {loading ? '计算中...' : '运行 PCA 分析'}
        </button>
      </div>

      {/* Error */}
      {error && (
        <div data-testid="error-message" className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Plot Area */}
      <div data-testid="pca-plot-area" className="border border-gray-200 rounded-lg p-4 min-h-[400px]">
        {plotLoading && <div className="text-center text-gray-500 py-20">生成图表中...</div>}
        {svg && !plotLoading && (
          <div dangerouslySetInnerHTML={{ __html: svg }} className="flex justify-center" />
        )}
        {!svg && !plotLoading && (
          <div className="text-center text-gray-400 py-20">运行分析后将在此显示 PCA 图</div>
        )}
      </div>

      {/* Export & Download Buttons */}
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
          data-testid="download-xlsx"
          onClick={handleDownloadXlsx}
          disabled={!pcaResult?.data}
          className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
        >
          下载坐标数据 (CSV)
        </button>
      </div>
    </div>
  )
}
