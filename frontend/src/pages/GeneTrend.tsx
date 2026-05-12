import { useState, useCallback, useEffect } from 'react'
import { useWorkspaceStore } from '../stores/workspace'
import { runGeneTrend, getGeneTrendPlot, getTaskStatus } from '../api/client'

interface DESeq2ContrastResult {
  contrasts: Record<string, Record<string, unknown>[]>
  summary: Array<{ contrast: string; up_genes: number; down_genes: number; total_genes: number }>
  parameters: { fc: number; fdr: number }
}

interface ClusterResult {
  clusters: Record<string, unknown[]>
  n_clusters: number
  n_genes: number
  membership?: Record<string, Record<string, number>>
}

const DEFAULT_COLORS = [
  '#76BA99', '#EB4747', '#996699', '#f1c40f', '#ea8685',
  '#775039', '#b83570', '#e4b8d5', '#9ebc19'
]

const COLOR_PALETTES = {
  default: DEFAULT_COLORS,
  warm: ['#e74c3c', '#e67e22', '#f39c12', '#d35400', '#c0392b', '#a93226', '#b03a2e', '#784212', '#6e2c00'],
  cool: ['#3498db', '#2980b9', '#1abc9c', '#16a085', '#27ae60', '#2ecc71', '#3498db', '#2c3e50', '#1a5276'],
  pastel: ['#a8d5e5', '#f4a4a4', '#b5c7a3', '#f4d06f', '#c9a7c7', '#a7c7c7', '#e5b3a8', '#b3c7e5', '#c7e5b3'],
}

export default function GeneTrend() {
  // Store state
  const exprNorm = useWorkspaceStore((s) => s.exprNorm)
  const sampleInfo = useWorkspaceStore((s) => s.sampleInfo)
  const deseq2Result = useWorkspaceStore((s) => s.deseq2Result)

  // Check data availability
  const hasWorkspaceResults = deseq2Result?.status === 'done' && deseq2Result.data !== null
  const hasNormData = exprNorm !== null
  const hasSampleInfo = sampleInfo !== null

  // Data source state
  const [dataSource, setDataSource] = useState<'deseq2' | 'manual'>(
    hasWorkspaceResults ? 'deseq2' : 'manual'
  )
  const [selectedContrast, setSelectedContrast] = useState<string | null>(null)
  const [direction, setDirection] = useState<'Up' | 'Down' | 'All'>('Up')

  // Manual upload state
  const [normFile, setNormFile] = useState<File | null>(null)
  const [sampleInfoFile, setSampleInfoFile] = useState<File | null>(null)

  // Mfuzz parameters
  const [cValue, setCValue] = useState(4)
  const [filterNA, setFilterNA] = useState(0.25)
  const [fillNA, setFillNA] = useState<'mean' | 'median' | 'knn'>('mean')
  const [filterSD, setFilterSD] = useState(0.3)

  // Plot parameters
  const [pointSize, setPointSize] = useState(3)
  const [lineWidth, setLineWidth] = useState(0.8)
  const [colorPalette, setColorPalette] = useState<keyof typeof COLOR_PALETTES>('default')
  const [mmLow, setMmLow] = useState('#e6ee9c')
  const [mmHigh, setMmHigh] = useState('#dd2c00')
  const [mmMidpoint, setMmMidpoint] = useState(0.5)

  // Results state
  const [clusterResult, setClusterResult] = useState<ClusterResult | null>(null)
  const [selectedCluster, setSelectedCluster] = useState(1)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)

  // Plot state
  const [allPlotSvg, setAllPlotSvg] = useState<string | null>(null)
  const [singlePlotSvg, setSinglePlotSvg] = useState<string | null>(null)
  const [plotLoading, setPlotLoading] = useState(false)

  // Set initial contrast when DESeq2 results are available
  useEffect(() => {
    if (hasWorkspaceResults && !selectedContrast) {
      const data = deseq2Result!.data as DESeq2ContrastResult
      const firstContrast = Object.keys(data.contrasts)[0]
      if (firstContrast) {
        setSelectedContrast(firstContrast)
      }
    }
  }, [hasWorkspaceResults, deseq2Result, selectedContrast])

  // Auto-switch to manual when no workspace results
  useEffect(() => {
    if (!hasWorkspaceResults && dataSource === 'deseq2') {
      setDataSource('manual')
    }
  }, [hasWorkspaceResults, dataSource])

  // Get available contrasts
  const getContrasts = (): string[] => {
    if (!hasWorkspaceResults) return []
    const data = deseq2Result!.data as DESeq2ContrastResult
    return Object.keys(data.contrasts)
  }

  // Check if run is enabled
  const canRun = useCallback(() => {
    if (dataSource === 'deseq2') {
      return hasWorkspaceResults && hasNormData && hasSampleInfo
    }
    return normFile !== null && sampleInfoFile !== null
  }, [dataSource, hasWorkspaceResults, hasNormData, hasSampleInfo, normFile, sampleInfoFile])

  // Run Mfuzz analysis
  const handleRunAnalysis = useCallback(async () => {
    setError(null)
    setClusterResult(null)
    setAllPlotSvg(null)
    setSinglePlotSvg(null)
    setLoading(true)

    try {
      let normData: Record<string, unknown>
      let sampleData: Record<string, unknown>

      if (dataSource === 'manual') {
        // For manual mode, we'd need to upload files first
        // This is a simplified version - in production, you'd handle file upload
        setError('手动上传模式暂未实现，请使用 DESeq2 数据源')
        setLoading(false)
        return
      } else {
        // Use workspace data
        // In a real implementation, you'd fetch the actual data from workspace
        // For now, we'll send placeholder data
        normData = { placeholder: true }
        sampleData = { placeholder: true }
      }

      const res = await runGeneTrend({
        norm: normData,
        sampleInfo: sampleData,
        c_value: cValue,
        filterNA,
        fillNA,
        filterSD,
      })

      // Poll for task completion
      const taskId = res.taskId
      let taskDone = false
      let attempts = 0
      const maxAttempts = 60 // 5 minutes max

      while (!taskDone && attempts < maxAttempts) {
        await new Promise(resolve => setTimeout(resolve, 5000))
        const status = await getTaskStatus(taskId)

        if (status.status === 'done') {
          taskDone = true
          setClusterResult(status.result as ClusterResult)

          // Auto-generate all clusters plot
          await handleGenerateAllPlot()
        } else if (status.status === 'error') {
          throw new Error(status.error ?? 'Analysis failed')
        }
        attempts++
      }

      if (!taskDone) {
        throw new Error('Analysis timed out')
      }
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Mfuzz 分析失败'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [dataSource, cValue, filterNA, fillNA, filterSD])

  // Generate all clusters plot
  const handleGenerateAllPlot = useCallback(async () => {
    setPlotLoading(true)
    setError(null)

    try {
      const res = await getGeneTrendPlot({
        plotType: 'all',
        params: {
          colors: COLOR_PALETTES[colorPalette],
          pointSize,
          lineWidth,
          baseSize: 14,
        },
      })
      setAllPlotSvg(res.svg)
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? '生成趋势图失败'
      setError(String(msg))
    } finally {
      setPlotLoading(false)
    }
  }, [colorPalette, pointSize, lineWidth])

  // Generate single cluster plot
  const handleGenerateSinglePlot = useCallback(async (clusterNum: number) => {
    setPlotLoading(true)
    setError(null)

    try {
      const res = await getGeneTrendPlot({
        plotType: 'single',
        clusterNum,
        params: {
          mm_colr_low: mmLow,
          mm_colr_high: mmHigh,
          mm_midpoint: mmMidpoint,
          baseSize: 14,
        },
      })
      setSinglePlotSvg(res.svg)
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? '生成单簇图失败'
      setError(String(msg))
    } finally {
      setPlotLoading(false)
    }
  }, [mmLow, mmHigh, mmMidpoint])

  // Handle cluster selection
  const handleClusterSelect = useCallback((clusterNum: number) => {
    setSelectedCluster(clusterNum)
    handleGenerateSinglePlot(clusterNum)
  }, [handleGenerateSinglePlot])

  // Export XLSX
  const handleExportXLSX = useCallback(() => {
    if (!clusterResult?.clusters) return

    try {
      const clusters = clusterResult.clusters
      const rows: string[][] = [['Gene', 'Cluster', 'Membership']]

      const genes = clusters.Gene as string[]
      const clusterNames = clusters.Cluster as string[]
      const memberships = clusters.Membership as number[]

      for (let i = 0; i < genes.length; i++) {
        rows.push([
          genes[i],
          clusterNames[i],
          String(memberships[i]),
        ])
      }

      const csvContent = rows.map((r) => r.join('\t')).join('\n')
      const blob = new Blob([csvContent], { type: 'text/tab-separated-values' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = 'genetrend_clusters.tsv'
      a.click()
      URL.revokeObjectURL(url)
    } catch {
      setError('导出失败')
    }
  }, [clusterResult])

  // Export SVG
  const handleExportSVG = useCallback((svg: string | null, filename: string) => {
    if (!svg) return
    const blob = new Blob([svg], { type: 'image/svg+xml' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = filename
    a.click()
    URL.revokeObjectURL(url)
  }, [])

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">GeneTrend Mfuzz 聚类分析</h2>

      {/* Data Source Section */}
      <div data-testid="genetrend-data-source" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">数据来源</h3>

        {/* Data source toggle */}
        <div className="flex gap-4 mb-4">
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              data-testid="source-deseq2"
              checked={dataSource === 'deseq2'}
              onChange={() => setDataSource('deseq2')}
              disabled={!hasWorkspaceResults}
              className="w-4 h-4"
            />
            <span className="text-sm">从 DESeq2 筛选 DEGs</span>
            {!hasWorkspaceResults && (
              <span className="text-xs text-gray-400">(需先运行 DESeq2)</span>
            )}
          </label>
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="dataSource"
              data-testid="source-manual"
              checked={dataSource === 'manual'}
              onChange={() => setDataSource('manual')}
              className="w-4 h-4"
            />
            <span className="text-sm">手动上传数据</span>
          </label>
        </div>

        {/* DESeq2 source options */}
        {dataSource === 'deseq2' && hasWorkspaceResults && (
          <div className="space-y-3">
            <div data-testid="contrast-selector">
              <label className="block text-sm text-gray-600 mb-1">选择 Contrast</label>
              <select
                value={selectedContrast ?? ''}
                onChange={(e) => setSelectedContrast(e.target.value)}
                className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
              >
                {getContrasts().map((name) => (
                  <option key={name} value={name}>
                    {name}
                  </option>
                ))}
              </select>
            </div>
            <div data-testid="direction-selector">
              <label className="block text-sm text-gray-600 mb-1">基因方向</label>
              <select
                value={direction}
                onChange={(e) => setDirection(e.target.value as 'Up' | 'Down' | 'All')}
                className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
              >
                <option value="Up">上调 (Up)</option>
                <option value="Down">下调 (Down)</option>
                <option value="All">全部 DEGs</option>
              </select>
            </div>
          </div>
        )}

        {/* Manual upload */}
        {dataSource === 'manual' && (
          <div className="space-y-3">
            <div data-testid="norm-upload">
              <label className="block text-sm text-gray-600 mb-1">
                标准化表达矩阵 (norm)
              </label>
              <input
                type="file"
                accept=".xlsx,.csv,.tsv"
                onChange={(e) => setNormFile(e.target.files?.[0] ?? null)}
                className="w-full border border-gray-300 rounded px-3 py-2 text-sm"
              />
              {normFile && (
                <p className="text-xs text-gray-500 mt-1">
                  已选择: {normFile.name}
                </p>
              )}
            </div>
            <div data-testid="sampleinfo-upload">
              <label className="block text-sm text-gray-600 mb-1">
                样本信息 (sampleInfo)
              </label>
              <input
                type="file"
                accept=".xlsx,.csv,.tsv"
                onChange={(e) => setSampleInfoFile(e.target.files?.[0] ?? null)}
                className="w-full border border-gray-300 rounded px-3 py-2 text-sm"
              />
              {sampleInfoFile && (
                <p className="text-xs text-gray-500 mt-1">
                  已选择: {sampleInfoFile.name}
                </p>
              )}
            </div>
          </div>
        )}

        {dataSource === 'deseq2' && !hasWorkspaceResults && (
          <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded">
            需先运行 DESeq2 分析，请切换到手动上传模式或先运行 DESeq2。
          </div>
        )}
      </div>

      {/* Mfuzz Parameters Section */}
      <div data-testid="mfuzz-params-section" className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">Mfuzz 参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {/* Cluster number */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">聚类数 (c)</label>
            <input
              data-testid="param-c-value"
              type="number"
              min="2"
              max="20"
              value={cValue}
              onChange={(e) => setCValue(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Filter NA */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">NA 过滤阈值</label>
            <input
              data-testid="param-filter-na"
              type="number"
              step="0.05"
              min="0"
              max="1"
              value={filterNA}
              onChange={(e) => setFilterNA(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Fill NA method */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">NA 填充方法</label>
            <select
              data-testid="param-fill-na"
              value={fillNA}
              onChange={(e) => setFillNA(e.target.value as 'mean' | 'median' | 'knn')}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            >
              <option value="mean">均值 (mean)</option>
              <option value="median">中位数 (median)</option>
              <option value="knn">KNN</option>
            </select>
          </div>

          {/* Filter SD */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">SD 过滤阈值</label>
            <input
              data-testid="param-filter-sd"
              type="number"
              step="0.1"
              min="0"
              value={filterSD}
              onChange={(e) => setFilterSD(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>
      </div>

      {/* Plot Parameters Section */}
      <div data-testid="plot-params-section" className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">绘图参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {/* Point size */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">点大小</label>
            <input
              data-testid="param-point-size"
              type="number"
              min="1"
              max="10"
              value={pointSize}
              onChange={(e) => setPointSize(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Line width */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">线宽</label>
            <input
              data-testid="param-line-width"
              type="number"
              step="0.1"
              min="0.1"
              max="3"
              value={lineWidth}
              onChange={(e) => setLineWidth(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Color palette */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">颜色方案</label>
            <select
              data-testid="color-palette-selector"
              value={colorPalette}
              onChange={(e) => setColorPalette(e.target.value as keyof typeof COLOR_PALETTES)}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            >
              <option value="default">默认</option>
              <option value="warm">暖色</option>
              <option value="cool">冷色</option>
              <option value="pastel">柔和</option>
            </select>
          </div>
        </div>

        <div className="border-t pt-4">
          <h4 className="text-sm font-medium mb-3">单簇图参数</h4>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* MM low color */}
            <div>
              <label className="block text-sm text-gray-600 mb-1">成员度低色</label>
              <input
                data-testid="param-mm-low"
                type="color"
                value={mmLow}
                onChange={(e) => setMmLow(e.target.value)}
                className="w-full h-8 border border-gray-300 rounded"
              />
            </div>

            {/* MM high color */}
            <div>
              <label className="block text-sm text-gray-600 mb-1">成员度高色</label>
              <input
                data-testid="param-mm-high"
                type="color"
                value={mmHigh}
                onChange={(e) => setMmHigh(e.target.value)}
                className="w-full h-8 border border-gray-300 rounded"
              />
            </div>

            {/* MM midpoint */}
            <div>
              <label className="block text-sm text-gray-600 mb-1">成员度中点</label>
              <input
                data-testid="param-mm-midpoint"
                type="number"
                step="0.1"
                min="0"
                max="1"
                value={mmMidpoint}
                onChange={(e) => setMmMidpoint(Number(e.target.value))}
                className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
              />
            </div>
          </div>
        </div>
      </div>

      {/* Run Button */}
      <div>
        <button
          data-testid="run-genetrend"
          onClick={handleRunAnalysis}
          disabled={loading || !canRun()}
          className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {loading ? '分析中...' : '运行 Mfuzz 聚类'}
        </button>
      </div>

      {/* Error display */}
      {error && (
        <div data-testid="error-message" className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Results Section */}
      <div data-testid="genetrend-result-area" className="border border-gray-200 rounded-lg p-4">
        <div className="flex justify-between items-center mb-3">
          <h3 className="font-medium">聚类结果</h3>
          <div className="flex gap-2">
            <button
              data-testid="export-xlsx"
              onClick={handleExportXLSX}
              disabled={!clusterResult}
              className="px-3 py-1.5 rounded text-sm bg-green-50 text-green-700 hover:bg-green-100 disabled:opacity-50"
            >
              导出 TSV
            </button>
          </div>
        </div>

        {clusterResult ? (
          <div className="space-y-4">
            {/* Summary */}
            <div className="grid grid-cols-3 gap-4 text-center">
              <div className="bg-gray-50 p-3 rounded">
                <p className="text-2xl font-bold">{clusterResult.n_clusters}</p>
                <p className="text-sm text-gray-600">聚类数</p>
              </div>
              <div className="bg-gray-50 p-3 rounded">
                <p className="text-2xl font-bold">{clusterResult.n_genes}</p>
                <p className="text-sm text-gray-600">基因数</p>
              </div>
              <div className="bg-gray-50 p-3 rounded">
                <p className="text-2xl font-bold">
                  {Math.round(clusterResult.n_genes / clusterResult.n_clusters)}
                </p>
                <p className="text-sm text-gray-600">平均基因/簇</p>
              </div>
            </div>

            {/* All clusters plot */}
            <div data-testid="genetrend-all-plot">
              <div className="flex justify-between items-center mb-2">
                <h4 className="text-sm font-medium">所有聚类趋势图</h4>
                <button
                  onClick={() => handleExportSVG(allPlotSvg, 'genetrend_all.svg')}
                  disabled={!allPlotSvg}
                  className="px-2 py-1 text-xs bg-gray-100 hover:bg-gray-200 rounded disabled:opacity-50"
                >
                  导出 SVG
                </button>
              </div>
              {plotLoading && !allPlotSvg ? (
                <div className="flex items-center justify-center h-48 bg-gray-50 rounded">
                  <p className="text-gray-500">生成图表中...</p>
                </div>
              ) : allPlotSvg ? (
                <div
                  className="border border-gray-100 rounded bg-white p-2"
                  dangerouslySetInnerHTML={{ __html: allPlotSvg }}
                />
              ) : (
                <div className="flex items-center justify-center h-48 bg-gray-50 rounded">
                  <p className="text-gray-500">运行分析后显示趋势图</p>
                </div>
              )}
            </div>

            {/* Single cluster plot */}
            <div data-testid="genetrend-single-plot">
              <div className="flex justify-between items-center mb-2">
                <h4 className="text-sm font-medium">单簇详细图</h4>
                <div className="flex gap-2 items-center">
                  <div data-testid="cluster-selector">
                    <select
                      value={selectedCluster}
                      onChange={(e) => handleClusterSelect(Number(e.target.value))}
                      className="border border-gray-300 rounded px-2 py-1 text-sm"
                    >
                      {Array.from({ length: clusterResult.n_clusters }, (_, i) => i + 1).map((num) => (
                        <option key={num} value={num}>
                          Cluster {num}
                        </option>
                      ))}
                    </select>
                  </div>
                  <button
                    onClick={() => handleExportSVG(singlePlotSvg, `genetrend_cluster${selectedCluster}.svg`)}
                    disabled={!singlePlotSvg}
                    className="px-2 py-1 text-xs bg-gray-100 hover:bg-gray-200 rounded disabled:opacity-50"
                  >
                    导出 SVG
                  </button>
                </div>
              </div>
              {plotLoading && !singlePlotSvg ? (
                <div className="flex items-center justify-center h-48 bg-gray-50 rounded">
                  <p className="text-gray-500">生成图表中...</p>
                </div>
              ) : singlePlotSvg ? (
                <div
                  className="border border-gray-100 rounded bg-white p-2"
                  dangerouslySetInnerHTML={{ __html: singlePlotSvg }}
                />
              ) : (
                <div className="flex items-center justify-center h-48 bg-gray-50 rounded">
                  <p className="text-gray-500">选择聚类后显示详细图</p>
                </div>
              )}
            </div>
          </div>
        ) : (
          <div className="text-center text-gray-500 py-8">
            运行 Mfuzz 聚类分析后显示结果
          </div>
        )}
      </div>
    </div>
  )
}
