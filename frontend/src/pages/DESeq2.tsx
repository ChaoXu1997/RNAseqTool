import { useState, useCallback, useEffect } from 'react'
import { loadDemoData, startAnalysis, getTaskStatus, getContrastPairs, uploadFile } from '../api/client'
import { useWorkspaceStore } from '../stores/workspace'

interface ContrastPair {
  group1: string
  group2: string
  selected: boolean
}

interface DESeq2Result {
  contrasts: Record<string, Record<string, unknown>[]>
  summary: Array<{ contrast: string; up_genes: number; down_genes: number; total_genes: number }>
  parameters: { fc: number; fdr: number }
}

const DEFAULT_PARAMS = {
  fc: 2,
  fdr: 0.05,
}

export default function DESeq2() {
  // Store state
  const exprRaw = useWorkspaceStore((s) => s.exprRaw)
  const sampleInfo = useWorkspaceStore((s) => s.sampleInfo)
  const deseq2Result = useWorkspaceStore((s) => s.deseq2Result)
  const setExprRaw = useWorkspaceStore((s) => s.setExprRaw)
  const setSampleInfo = useWorkspaceStore((s) => s.setSampleInfo)
  const setAnalysisResult = useWorkspaceStore((s) => s.setAnalysisResult)

  // Local state
  const [params, setParams] = useState(DEFAULT_PARAMS)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [contrastPairs, setContrastPairs] = useState<ContrastPair[]>([])
  const [selectedContrast, setSelectedContrast] = useState<string | null>(null)
  const [progress, setProgress] = useState(0)
  const [resultData, setResultData] = useState<Record<string, unknown>[]>([])
  const [sortColumn, setSortColumn] = useState<string>('padj')
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('asc')
  const [filterText, setFilterText] = useState('')

  const canRun = exprRaw !== null && sampleInfo !== null && contrastPairs.some((p) => p.selected)

  // Load demo data for both raw counts and sampleInfo
  const handleLoadDemo = useCallback(async () => {
    setError(null)
    setLoading(true)
    try {
      const [rawRes, sampleRes] = await Promise.all([
        loadDemoData('expr_raw'),
        loadDemoData('sampleInfo'),
      ])
      setExprRaw({
        name: 'demo_raw',
        rows: rawRes.data.rows,
        cols: rawRes.data.cols,
        preview: rawRes.data.preview,
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
  }, [setExprRaw, setSampleInfo])

  // Handle raw counts file upload
  const handleRawUpload = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const file = e.target.files?.[0]
      if (!file) return
      setError(null)
      setLoading(true)
      try {
        const res = await uploadFile(file, 'expr_raw')
        setExprRaw({
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
    [setExprRaw]
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

  // Fetch contrast pairs when sampleInfo changes
  useEffect(() => {
    if (!sampleInfo) {
      setContrastPairs([])
      return
    }

    const fetchContrasts = async () => {
      try {
        const res = await getContrastPairs({ preview: sampleInfo.preview })
        if (res.status === 'ok') {
          const pairs = res.pairs.map((p) => ({
            ...p,
            selected: true, // Select all by default
          }))
          setContrastPairs(pairs)
        }
      } catch {
        // Silently fail, user can manually configure
      }
    }

    fetchContrasts()
  }, [sampleInfo])

  // Toggle contrast pair selection
  const toggleContrastPair = useCallback((index: number) => {
    setContrastPairs((prev) =>
      prev.map((p, i) => (i === index ? { ...p, selected: !p.selected } : p))
    )
  }, [])

  // Run DESeq2 analysis
  const handleRunDESeq2 = useCallback(async () => {
    if (!canRun) return
    setError(null)
    setProgress(0)
    setLoading(true)

    try {
      // Get selected contrast pairs
      const selectedPairs = contrastPairs
        .filter((p) => p.selected)
        .map((p) => [p.group1, p.group2])

      const taskId = await startAnalysis('deseq2', {
        raw: exprRaw,
        sampleInfo: sampleInfo,
        contrast_pairs: selectedPairs,
        fc: params.fc,
        fdr: params.fdr,
      })

      // Poll for task completion
      const poll = async (): Promise<void> => {
        setProgress((p) => Math.min(p + 10, 90))
        const status = await getTaskStatus(taskId)
        if (status.status === 'done') {
          setProgress(100)
          const resultData = status.result as DESeq2Result
          setAnalysisResult('deseq2', {
            taskId,
            status: 'done',
            data: resultData,
          })

          // Set first contrast result as default view
          const firstContrast = Object.keys(resultData.contrasts)[0]
          if (firstContrast) {
            setSelectedContrast(firstContrast)
            setResultData(resultData.contrasts[firstContrast])
          }
          return
        }
        if (status.status === 'error') {
          throw new Error(status.error ?? 'DESeq2 analysis failed')
        }
        // Still pending/running, poll again
        await new Promise((r) => setTimeout(r, 500))
        return poll()
      }

      await poll()
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'DESeq2 analysis failed'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [canRun, exprRaw, sampleInfo, contrastPairs, params, setAnalysisResult])

  // Handle contrast selection change
  const handleContrastChange = useCallback(
    (contrastName: string) => {
      setSelectedContrast(contrastName)
      if (deseq2Result?.data) {
        const data = deseq2Result.data as DESeq2Result
        setResultData(data.contrasts[contrastName] || [])
      }
    },
    [deseq2Result]
  )

  // Sort and filter result data
  const processedData = useCallback(() => {
    let data = [...resultData]

    // Apply filter
    if (filterText) {
      data = data.filter((row) =>
        String(row.Gene ?? '').toLowerCase().includes(filterText.toLowerCase())
      )
    }

    // Apply sort
    data.sort((a, b) => {
      const aVal = a[sortColumn]
      const bVal = b[sortColumn]
      if (aVal === null || aVal === undefined) return 1
      if (bVal === null || bVal === undefined) return -1

      const comparison = String(aVal).localeCompare(String(bVal), undefined, { numeric: true })
      return sortDirection === 'asc' ? comparison : -comparison
    })

    return data
  }, [resultData, filterText, sortColumn, sortDirection])

  // Handle column sort
  const handleSort = useCallback((column: string) => {
    setSortColumn((prev) => {
      if (prev === column) {
        setSortDirection((d) => (d === 'asc' ? 'desc' : 'asc'))
        return column
      }
      setSortDirection('asc')
      return column
    })
  }, [])

  // Download results as XLSX (CSV format)
  const handleDownloadXlsx = useCallback(() => {
    if (!resultData || resultData.length === 0) return
    try {
      const headers = Object.keys(resultData[0])
      const csvRows = [
        headers.join(','),
        ...resultData.map((row: Record<string, unknown>) =>
          headers.map((h) => {
            const val = row[h]
            // Escape CSV values
            const strVal = String(val ?? '')
            return strVal.includes(',') ? `"${strVal}"` : strVal
          }).join(',')
        ),
      ]
      const csv = csvRows.join('\n')
      const blob = new Blob([csv], { type: 'text/csv' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `deseq2_${selectedContrast ?? 'result'}.csv`
      a.click()
      URL.revokeObjectURL(url)
    } catch {
      setError('Download failed')
    }
  }, [resultData, selectedContrast])

  // Navigation links to downstream modules
  const downstreamModules = [
    { name: '火山图', path: '/volcano', icon: '🌋' },
    { name: '富集分析', path: '/enrich', icon: '📊' },
    { name: 'GSEA', path: '/gsea', icon: '📈' },
    { name: '基因趋势', path: '/genetrend', icon: '📉' },
    { name: 'WGCNA', path: '/wgcna', icon: '🧬' },
  ]

  // Check if results are available
  const hasResults = deseq2Result?.status === 'done' && deseq2Result.data !== null && deseq2Result.data !== undefined

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">DESeq2 差异分析</h2>

      {/* Data Section */}
      <div data-testid="deseq2-data-section" className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {/* Raw Counts Upload */}
        <div className="border border-gray-200 rounded-lg p-4">
          <h3 className="font-medium mb-2">原始 Counts 矩阵</h3>
          {exprRaw ? (
            <div className="text-sm text-green-700 bg-green-50 p-2 rounded">
              ✓ {exprRaw.name} ({exprRaw.rows} rows × {exprRaw.cols} cols)
            </div>
          ) : (
            <div className="text-sm text-gray-500">未加载</div>
          )}
          <input
            type="file"
            accept=".csv,.xlsx"
            onChange={handleRawUpload}
            disabled={loading}
            data-testid="raw-file-input"
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

      {/* Contrast Pair Selector */}
      <div data-testid="contrast-selector" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">Contrast 选择</h3>
        {contrastPairs.length === 0 ? (
          <div className="text-sm text-gray-500">请先加载样本信息</div>
        ) : (
          <div className="space-y-2">
            {contrastPairs.map((pair, index) => (
              <label
                key={index}
                className="flex items-center gap-2 cursor-pointer hover:bg-gray-50 p-2 rounded"
              >
                <input
                  type="checkbox"
                  checked={pair.selected}
                  onChange={() => toggleContrastPair(index)}
                  data-testid={`contrast-pair-${index}`}
                  className="w-4 h-4"
                />
                <span className="text-sm">
                  {pair.group1} vs {pair.group2}
                </span>
              </label>
            ))}
          </div>
        )}
      </div>

      {/* Parameter Panel */}
      <div data-testid="deseq2-params" className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">分析参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* FC Threshold */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">Fold Change 阈值</label>
            <input
              data-testid="param-fc"
              type="number"
              min={1}
              step={0.5}
              value={params.fc}
              onChange={(e) => setParams((p) => ({ ...p, fc: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* FDR Threshold */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">FDR 阈值</label>
            <input
              data-testid="param-fdr"
              type="number"
              min={0}
              max={1}
              step={0.01}
              value={params.fdr}
              onChange={(e) => setParams((p) => ({ ...p, fdr: Number(e.target.value) }))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>
      </div>

      {/* Run Button */}
      <div>
        <button
          data-testid="run-deseq2"
          onClick={handleRunDESeq2}
          disabled={!canRun || loading}
          className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
        >
          {loading ? '计算中...' : '运行 DESeq2 分析'}
        </button>
      </div>

      {/* Progress Bar */}
      {loading && (
        <div data-testid="progress-bar" className="w-full bg-gray-200 rounded-full h-2.5">
          <div
            className="bg-blue-600 h-2.5 rounded-full transition-all duration-300"
            style={{ width: `${progress}%` }}
          />
        </div>
      )}

      {/* Error */}
      {error && (
        <div data-testid="error-message" className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Results Section */}
      {hasResults ? (
        <div data-testid="deseq2-results" className="space-y-4">
          {/* Contrast Selector for Results */}
          <div className="flex items-center gap-4">
            <label className="text-sm font-medium">查看结果:</label>
            <select
              data-testid="result-contrast-selector"
              value={selectedContrast ?? ''}
              onChange={(e) => handleContrastChange(e.target.value)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm"
            >
              {Object.keys((deseq2Result!.data as DESeq2Result).contrasts).map((name) => (
                <option key={name} value={name}>
                  {name}
                </option>
              ))}
            </select>
          </div>

          {/* Summary Stats */}
          <div data-testid="summary-stats" className="grid grid-cols-3 gap-4">
            {(deseq2Result!.data as DESeq2Result).summary.map((s) => (
              <div key={s.contrast} className="bg-gray-50 p-3 rounded-lg text-center">
                <div className="text-sm text-gray-600">{s.contrast}</div>
                <div className="text-lg font-bold text-red-600">{s.up_genes} ↑</div>
                <div className="text-lg font-bold text-blue-600">{s.down_genes} ↓</div>
              </div>
            ))}
          </div>

          {/* Filter */}
          <div className="flex items-center gap-4">
            <input
              data-testid="filter-input"
              type="text"
              placeholder="搜索基因名..."
              value={filterText}
              onChange={(e) => setFilterText(e.target.value)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm flex-1"
            />
          </div>

          {/* Results Table */}
          <div data-testid="result-table" className="border border-gray-200 rounded-lg overflow-hidden">
            <div className="max-h-[500px] overflow-auto">
              <table className="w-full text-sm">
                <thead className="bg-gray-50 sticky top-0">
                  <tr>
                    {['Gene', 'baseMean', 'log2FoldChange', 'lfcSE', 'stat', 'pvalue', 'padj', 'change'].map(
                      (col) => (
                        <th
                          key={col}
                          onClick={() => handleSort(col)}
                          className="px-4 py-2 text-left cursor-pointer hover:bg-gray-100"
                        >
                          {col}
                          {sortColumn === col && (sortDirection === 'asc' ? ' ↑' : ' ↓')}
                        </th>
                      )
                    )}
                  </tr>
                </thead>
                <tbody>
                  {processedData().map((row, idx) => (
                    <tr key={idx} className="border-t border-gray-100 hover:bg-gray-50">
                      <td className="px-4 py-2 font-medium">{String(row.Gene ?? '')}</td>
                      <td className="px-4 py-2">{Number(row.baseMean ?? 0).toFixed(2)}</td>
                      <td className="px-4 py-2">{Number(row.log2FoldChange ?? 0).toFixed(4)}</td>
                      <td className="px-4 py-2">{Number(row.lfcSE ?? 0).toFixed(4)}</td>
                      <td className="px-4 py-2">{Number(row.stat ?? 0).toFixed(4)}</td>
                      <td className="px-4 py-2">{Number(row.pvalue ?? 0).toExponential(3)}</td>
                      <td className="px-4 py-2">{Number(row.padj ?? 0).toExponential(3)}</td>
                      <td
                        className={`px-4 py-2 font-medium ${
                          row.change === 'Up'
                            ? 'text-red-600'
                            : row.change === 'Down'
                              ? 'text-blue-600'
                              : 'text-gray-400'
                        }`}
                      >
                        {String(row.change ?? '')}
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
          </div>

          {/* Download Button */}
          <div>
            <button
              data-testid="download-xlsx"
              onClick={handleDownloadXlsx}
              disabled={resultData.length === 0}
              className="px-4 py-1.5 rounded text-sm bg-gray-100 text-gray-700 hover:bg-gray-200 disabled:opacity-50"
            >
              下载结果 (CSV)
            </button>
          </div>

          {/* Downstream Module Links */}
          <div data-testid="downstream-links" className="border border-gray-200 rounded-lg p-4">
            <h3 className="font-medium mb-3">下游分析模块</h3>
            <div className="flex flex-wrap gap-3">
              {downstreamModules.map((mod) => (
                <a
                  key={mod.path}
                  href={mod.path}
                  className="px-4 py-2 rounded-md text-sm bg-purple-50 text-purple-700 hover:bg-purple-100 flex items-center gap-2"
                >
                  <span>{mod.icon}</span>
                  <span>{mod.name}</span>
                </a>
              ))}
            </div>
          </div>
        </div>
      ) : null}
    </div>
  )
}
