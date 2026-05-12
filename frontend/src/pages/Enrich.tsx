import { useState, useCallback, useEffect } from 'react'
import { useWorkspaceStore } from '../stores/workspace'
import { runEnrich, getPlot, getDatabases, getSpecies } from '../api/client'

const DEFAULT_DATABASES = [
  'C5:GO:BP',
  'C5:GO:CC',
  'C5:GO:MF',
  'C2:CP:KEGG',
  'C2:CP:REACTOME',
  'C2:CP:WIKIPATHWAYS',
  'C2:CP:BIOCARTA',
  'C8:CELL_TYPE_SIGNATURES',
  'H:HALLMARK',
]

const DEFAULT_SPECIES = [
  'Homo sapiens',
  'Mus musculus',
  'Rattus norvegicus',
  'Danio rerio',
  'Drosophila melanogaster',
  'Caenorhabditis elegans',
]

const GENE_TYPES = [
  { value: 'symbol', label: 'Symbol (gene name)' },
  { value: 'entrezid', label: 'Entrez ID' },
  { value: 'ensembl', label: 'Ensembl ID' },
]

interface DESeq2ContrastResult {
  contrasts: Record<string, Record<string, unknown>[]>
  summary: Array<{ contrast: string; up_genes: number; down_genes: number; total_genes: number }>
  parameters: { fc: number; fdr: number }
}

interface EnrichResult {
  Description: string[]
  ID: string[]
  GeneRatio: string[]
  BgRatio: string[]
  pvalue: number[]
  p_adjust: number[]
  qvalue: number[]
  geneID: string[]
  Count: number[]
  Database: string[]
  EnrichFactor: number[]
}

export default function Enrich() {
  // Store state
  const deseq2Result = useWorkspaceStore((s) => s.deseq2Result)

  // Check if DESeq2 results are available
  const hasWorkspaceResults = deseq2Result?.status === 'done' && deseq2Result.data !== null

  // Gene input state
  const [geneSource, setGeneSource] = useState<'deseq2' | 'manual'>(
    hasWorkspaceResults ? 'deseq2' : 'manual'
  )
  const [manualGenes, setManualGenes] = useState('')
  const [selectedContrast, setSelectedContrast] = useState<string | null>(null)
  const [direction, setDirection] = useState<'Up' | 'Down' | 'All'>('Up')

  // Database and parameters
  const [database, setDatabase] = useState('C5:GO:BP')
  const [species, setSpecies] = useState('Homo sapiens')
  const [geneType, setGeneType] = useState<'symbol' | 'entrezid' | 'ensembl'>('symbol')
  const [pCutoff, setPCutoff] = useState(0.05)
  const [availableDbs, setAvailableDbs] = useState<string[]>(DEFAULT_DATABASES)
  const [availableSpecies, setAvailableSpecies] = useState<string[]>(DEFAULT_SPECIES)

  // Results and plotting
  const [enrichResult, setEnrichResult] = useState<EnrichResult | null>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [svg, setSvg] = useState<string | null>(null)
  const [plotLoading, setPlotLoading] = useState(false)

  // Plot parameters
  const [plotType, setPlotType] = useState<'bar' | 'dot' | 'lollipop'>('bar')
  const [showCategory, setShowCategory] = useState(10)
  const [plotTitle, setPlotTitle] = useState('Enrichment Analysis')

  // Load available databases and species on mount
  useEffect(() => {
    const loadOptions = async () => {
      try {
        const [dbRes, speciesRes] = await Promise.all([
          getDatabases().catch(() => null),
          getSpecies().catch(() => null),
        ])
        if (dbRes?.databases) {
          setAvailableDbs(dbRes.databases)
        }
        if (speciesRes?.species) {
          setAvailableSpecies(speciesRes.species)
        }
      } catch {
        // Use defaults
      }
    }
    loadOptions()
  }, [])

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
    if (!hasWorkspaceResults && geneSource === 'deseq2') {
      setGeneSource('manual')
    }
  }, [hasWorkspaceResults, geneSource])

  // Get available contrasts
  const getContrasts = (): string[] => {
    if (!hasWorkspaceResults) return []
    const data = deseq2Result!.data as DESeq2ContrastResult
    return Object.keys(data.contrasts)
  }

  // Run enrichment analysis
  const handleRunEnrich = useCallback(async () => {
    setError(null)
    setEnrichResult(null)
    setSvg(null)
    setLoading(true)

    try {
      let params: {
        genes?: string[]
        source?: 'deseq2'
        contrast?: string
        direction?: 'Up' | 'Down' | 'All'
        database: string
        species: string
        gene_type: 'symbol' | 'entrezid' | 'ensembl'
        p_cutoff: number
      }

      if (geneSource === 'manual') {
        // Parse manual gene input
        const genes = manualGenes
          .split(/[\n,;\t\s]+/)
          .map((g) => g.trim())
          .filter((g) => g.length > 0)

        if (genes.length === 0) {
          setError('请输入至少一个基因')
          setLoading(false)
          return
        }

        params = {
          genes,
          database,
          species,
          gene_type: geneType,
          p_cutoff: pCutoff,
        }
      } else {
        // From DESeq2 results
        params = {
          source: 'deseq2',
          contrast: selectedContrast ?? undefined,
          direction,
          database,
          species,
          gene_type: geneType,
          p_cutoff: pCutoff,
        }
      }

      const res = await runEnrich(params)

      if (res.status === 'ok' && res.results) {
        setEnrichResult(res.results as unknown as EnrichResult)

        // Auto-generate plot
        if (res.results && (res.results.Description as string[])?.length > 0) {
          await handleGeneratePlot(res.results as unknown as EnrichResult)
        }
      } else {
        setError(res.message ?? '富集分析未返回结果')
      }
    } catch (err: unknown) {
      const msg =
        (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
        (err as Error)?.message ?? '富集分析失败'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [geneSource, manualGenes, database, species, geneType, pCutoff, selectedContrast, direction])

  // Generate plot
  const handleGeneratePlot = useCallback(async (results?: EnrichResult) => {
    const data = results ?? enrichResult
    if (!data) return

    setError(null)
    setSvg(null)
    setPlotLoading(true)

    try {
      const svgContent = await getPlot('enrich', {
        results: data,
        params: {
          plot_type: plotType,
          showCategory,
          title: plotTitle,
        },
      })
      setSvg(typeof svgContent === 'string' ? svgContent : (svgContent as { svg: string }).svg)
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Failed to generate plot'
      setError(String(msg))
    } finally {
      setPlotLoading(false)
    }
  }, [enrichResult, plotType, showCategory, plotTitle])

  // Export XLSX
  const handleExportXLSX = useCallback(async () => {
    if (!enrichResult) return

    try {
      // Build CSV-like data from enrich result
      const rows: string[][] = [
        ['Pathway', 'P_value', 'Adjusted_P', 'Gene_Count', 'Gene_Ratio', 'Background_Ratio', 'Genes', 'Database', 'Enrich_Factor'],
      ]

      const descArr = enrichResult.Description ?? []
      const pvalArr = enrichResult.pvalue ?? []
      const padjArr = enrichResult.p_adjust ?? []
      const countArr = enrichResult.Count ?? []
      const geneRatioArr = enrichResult.GeneRatio ?? []
      const bgRatioArr = enrichResult.BgRatio ?? []
      const geneIdArr = enrichResult.geneID ?? []
      const dbArr = enrichResult.Database ?? []
      const efArr = enrichResult.EnrichFactor ?? []

      for (let i = 0; i < descArr.length; i++) {
        rows.push([
          String(descArr[i] ?? ''),
          String(pvalArr[i] ?? ''),
          String(padjArr[i] ?? ''),
          String(countArr[i] ?? ''),
          String(geneRatioArr[i] ?? ''),
          String(bgRatioArr[i] ?? ''),
          String(geneIdArr[i] ?? ''),
          String(dbArr[i] ?? ''),
          String(efArr[i] ?? ''),
        ])
      }

      const csvContent = rows.map((r) => r.join('\t')).join('\n')
      const blob = new Blob([csvContent], { type: 'text/tab-separated-values' })
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `enrichment_results_${database.replace(/[:/]/g, '_')}.tsv`
      a.click()
      URL.revokeObjectURL(url)
    } catch {
      setError('导出失败')
    }
  }, [enrichResult, database])

  // Export SVG
  const handleExportSVG = useCallback(() => {
    if (!svg) return
    const blob = new Blob([svg], { type: 'image/svg+xml' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `enrichment_plot_${plotType}.svg`
    a.click()
    URL.revokeObjectURL(url)
  }, [svg, plotType])

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">富集分析</h2>

      {/* Gene Input Section */}
      <div data-testid="enrich-gene-input-section" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">基因输入</h3>

        {/* Gene source toggle */}
        <div className="flex gap-4 mb-4">
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="geneSource"
              data-testid="gene-source-deseq2"
              checked={geneSource === 'deseq2'}
              onChange={() => setGeneSource('deseq2')}
              disabled={!hasWorkspaceResults}
              className="w-4 h-4"
            />
            <span className="text-sm">从 DESeq2 结果提取</span>
            {!hasWorkspaceResults && (
              <span className="text-xs text-gray-400">(需先运行 DESeq2)</span>
            )}
          </label>
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="geneSource"
              data-testid="gene-source-manual"
              checked={geneSource === 'manual'}
              onChange={() => setGeneSource('manual')}
              className="w-4 h-4"
            />
            <span className="text-sm">手动粘贴基因列表</span>
          </label>
        </div>

        {/* DESeq2 source options */}
        {geneSource === 'deseq2' && hasWorkspaceResults && (
          <div className="space-y-3">
            <div data-testid="enrich-contrast-selector">
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

        {/* Manual gene input */}
        {geneSource === 'manual' && (
          <div>
            <label className="block text-sm text-gray-600 mb-1">
              输入基因列表 (每行一个，或用逗号/空格/分号分隔)
            </label>
            <textarea
              data-testid="gene-textarea"
              value={manualGenes}
              onChange={(e) => setManualGenes(e.target.value)}
              rows={6}
              placeholder="TP53&#10;BRCA1&#10;MYC&#10;EGFR&#10;KRAS"
              className="w-full border border-gray-300 rounded px-3 py-2 text-sm font-mono"
            />
          </div>
        )}

        {geneSource === 'deseq2' && !hasWorkspaceResults && (
          <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded">
            需先运行 DESeq2 分析，请切换到手动输入模式或先运行 DESeq2。
          </div>
        )}
      </div>

      {/* Database and Parameters Section */}
      <div className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">分析参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {/* Database selector */}
          <div data-testid="database-selector">
            <label className="block text-sm text-gray-600 mb-1">数据库</label>
            <select
              value={database}
              onChange={(e) => setDatabase(e.target.value)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              {availableDbs.map((db) => (
                <option key={db} value={db}>
                  {db}
                </option>
              ))}
            </select>
          </div>

          {/* Species selector */}
          <div data-testid="species-selector">
            <label className="block text-sm text-gray-600 mb-1">物种</label>
            <select
              value={species}
              onChange={(e) => setSpecies(e.target.value)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              {availableSpecies.map((sp) => (
                <option key={sp} value={sp}>
                  {sp}
                </option>
              ))}
            </select>
          </div>

          {/* Gene type selector */}
          <div data-testid="gene-type-selector">
            <label className="block text-sm text-gray-600 mb-1">基因 ID 类型</label>
            <select
              value={geneType}
              onChange={(e) => setGeneType(e.target.value as 'symbol' | 'entrezid' | 'ensembl')}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              {GENE_TYPES.map((gt) => (
                <option key={gt.value} value={gt.value}>
                  {gt.label}
                </option>
              ))}
            </select>
          </div>

          {/* P-value cutoff */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">P 值阈值</label>
            <input
              data-testid="param-p-cutoff"
              type="number"
              step="0.01"
              min="0"
              max="1"
              value={pCutoff}
              onChange={(e) => setPCutoff(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>

        {/* Run button */}
        <div>
          <button
            data-testid="run-enrich"
            onClick={handleRunEnrich}
            disabled={loading || (geneSource === 'deseq2' && !hasWorkspaceResults)}
            className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {loading ? '分析中...' : '运行富集分析'}
          </button>
        </div>
      </div>

      {/* Error display */}
      {error && (
        <div data-testid="error-message" className="p-3 bg-red-50 border border-red-200 rounded text-sm text-red-700">
          {error}
        </div>
      )}

      {/* Result Table */}
      <div data-testid="enrich-result-area" className="border border-gray-200 rounded-lg p-4">
        <div className="flex justify-between items-center mb-3">
          <h3 className="font-medium">富集结果</h3>
          <div className="flex gap-2">
            <button
              data-testid="export-xlsx"
              onClick={handleExportXLSX}
              disabled={!enrichResult}
              className="px-3 py-1.5 rounded text-sm bg-green-50 text-green-700 hover:bg-green-100 disabled:opacity-50"
            >
              导出 TSV
            </button>
          </div>
        </div>

        {enrichResult ? (
          <div className="overflow-x-auto">
            <table className="min-w-full text-sm">
              <thead>
                <tr className="bg-gray-50">
                  <th className="px-3 py-2 text-left font-medium text-gray-600">Pathway</th>
                  <th className="px-3 py-2 text-left font-medium text-gray-600">Database</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Count</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Gene Ratio</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">P-value</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Adj. P</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Enrich Factor</th>
                </tr>
              </thead>
              <tbody>
                {(enrichResult.Description ?? []).map((desc, i) => (
                  <tr key={i} className="border-t border-gray-100 hover:bg-gray-50">
                    <td className="px-3 py-2 text-gray-800">{desc}</td>
                    <td className="px-3 py-2 text-gray-600">{enrichResult.Database?.[i]}</td>
                    <td className="px-3 py-2 text-right">{enrichResult.Count?.[i]}</td>
                    <td className="px-3 py-2 text-right font-mono text-xs">{enrichResult.GeneRatio?.[i]}</td>
                    <td className="px-3 py-2 text-right font-mono">{enrichResult.pvalue?.[i]?.toExponential(2)}</td>
                    <td className="px-3 py-2 text-right font-mono">{enrichResult.p_adjust?.[i]?.toExponential(2)}</td>
                    <td className="px-3 py-2 text-right font-mono">{enrichResult.EnrichFactor?.[i]?.toFixed(3)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <div className="text-gray-400 text-sm py-8 text-center">
            运行分析后结果将在此显示
          </div>
        )}
      </div>

      {/* Plot Section */}
      <div className="border border-gray-200 rounded-lg p-4 space-y-4">
        <h3 className="font-medium">可视化</h3>

        {/* Plot parameters */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          {/* Plot type selector */}
          <div data-testid="plot-type-selector">
            <label className="block text-sm text-gray-600 mb-1">图表类型</label>
            <select
              value={plotType}
              onChange={(e) => setPlotType(e.target.value as 'bar' | 'dot' | 'lollipop')}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              <option value="bar">柱状图 (Bar)</option>
              <option value="dot">气泡图 (Dot)</option>
              <option value="lollipop">棒棒糖图 (Lollipop)</option>
            </select>
          </div>

          {/* Show category */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">显示条目数</label>
            <input
              data-testid="param-show-category"
              type="number"
              min={1}
              max={50}
              value={showCategory}
              onChange={(e) => setShowCategory(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* Plot title */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">图表标题</label>
            <input
              data-testid="param-plot-title"
              type="text"
              value={plotTitle}
              onChange={(e) => setPlotTitle(e.target.value)}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>

        {/* Generate plot button */}
        <div className="flex gap-2">
          <button
            onClick={() => handleGeneratePlot()}
            disabled={!enrichResult || plotLoading}
            className="px-4 py-2 rounded-md text-sm font-medium bg-indigo-600 text-white hover:bg-indigo-700 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {plotLoading ? '生成中...' : '生成图表'}
          </button>
          <button
            onClick={handleExportSVG}
            disabled={!svg}
            className="px-4 py-2 rounded-md text-sm bg-gray-50 text-gray-700 hover:bg-gray-100 disabled:opacity-50"
          >
            导出 SVG
          </button>
        </div>

        {/* Plot display */}
        <div data-testid="enrich-plot-area" className="border border-gray-100 rounded p-4 min-h-[300px]">
          {plotLoading && <div className="text-center text-gray-500 py-20">生成图表中...</div>}
          {svg && !plotLoading && (
            <div dangerouslySetInnerHTML={{ __html: svg }} className="flex justify-center" />
          )}
          {!svg && !plotLoading && (
            <div className="text-gray-400 text-sm text-center py-20">
              运行分析后图表将在此显示
            </div>
          )}
        </div>
      </div>
    </div>
  )
}
