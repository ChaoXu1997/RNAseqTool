import { useState, useCallback, useEffect } from 'react'
import { useWorkspaceStore } from '../stores/workspace'
import { runGSEA, getPlot, getDatabases, getSpecies } from '../api/client'
import { PlotSaveLoad } from '../components/PlotSaveLoad'

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

const PADJ_METHODS = [
  { value: 'BH', label: 'Benjamini-Hochberg (FDR)' },
  { value: 'bonferroni', label: 'Bonferroni' },
  { value: 'holm', label: 'Holm' },
  { value: 'hochberg', label: 'Hochberg' },
  { value: 'hommel', label: 'Hommel' },
  { value: 'BY', label: 'Benjamini-Yekutieli' },
  { value: 'none', label: 'No adjustment' },
]

interface DESeq2ContrastResult {
  contrasts: Record<string, Record<string, unknown>[]>
  summary: Array<{ contrast: string; up_genes: number; down_genes: number; total_genes: number }>
  parameters: { fc: number; fdr: number }
}

interface GSEAResult {
  ID: string[]
  Description: string[]
  setSize: number[]
  enrichmentScore: number[]
  NES: number[]
  pvalue: number[]
  p_adjust: number[]
  qvalues: number[]
  rank: number[]
  leading_edge: string[]
  core_enrichment: string[]
}

export default function GSEA() {
  // Store state
  const deseq2Result = useWorkspaceStore((s) => s.deseq2Result)

  // Check if DESeq2 results are available
  const hasWorkspaceResults = deseq2Result?.status === 'done' && deseq2Result.data !== null

  // Gene input state
  const [geneSource, setGeneSource] = useState<'deseq2' | 'upload'>(
    hasWorkspaceResults ? 'deseq2' : 'upload'
  )
  const [selectedContrast, setSelectedContrast] = useState<string | null>(null)

  // Database and parameters
  const [database, setDatabase] = useState('C5:GO:BP')
  const [species, setSpecies] = useState('Homo sapiens')
  const [geneType, setGeneType] = useState<'symbol' | 'entrezid' | 'ensembl'>('symbol')
  const [pCutoff, setPCutoff] = useState(0.05)
  const [padjMethod, setPadjMethod] = useState<'BH' | 'bonferroni' | 'holm' | 'hochberg' | 'hommel' | 'BY' | 'fdr' | 'none'>('BH')
  const [availableDbs, setAvailableDbs] = useState<string[]>(DEFAULT_DATABASES)
  const [availableSpecies, setAvailableSpecies] = useState<string[]>(DEFAULT_SPECIES)

  // Results and plotting
  const [gseaResult, setGseaResult] = useState<GSEAResult | null>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [svg, setSvg] = useState<string | null>(null)
  const [plotLoading, setPlotLoading] = useState(false)

  // Plot parameters
  const [geneSetID, setGeneSetID] = useState('')
  const [addGene, setAddGene] = useState(true)
  const [addPval, setAddPval] = useState(true)
  const [pvalSize, setPvalSize] = useState(4)
  const [subPlot, setSubPlot] = useState(3)
  const [termWidth, setTermWidth] = useState(40)
  const [baseSize, setBaseSize] = useState(12)
  const [arrowType, setArrowType] = useState<'open' | 'closed'>('open')
  const [geneCol, setGeneCol] = useState('red')
  const [newGsea, setNewGsea] = useState(false)

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

  // Auto-switch to upload when no workspace results
  useEffect(() => {
    if (!hasWorkspaceResults && geneSource === 'deseq2') {
      setGeneSource('upload')
    }
  }, [hasWorkspaceResults, geneSource])

  // Get available contrasts
  const getContrasts = (): string[] => {
    if (!hasWorkspaceResults) return []
    const data = deseq2Result!.data as DESeq2ContrastResult
    return Object.keys(data.contrasts)
  }

  // Run GSEA analysis
  const handleRunGSEA = useCallback(async () => {
    setError(null)
    setGseaResult(null)
    setSvg(null)
    setLoading(true)

    try {
      let params: {
        geneList?: Record<string, number>
        source?: 'deseq2'
        contrast?: string
        database: string
        species: string
        gene_type: 'symbol' | 'entrezid' | 'ensembl'
        pvalue: number
        pAdjustMethod: 'BH' | 'bonferroni' | 'holm' | 'hochberg' | 'hommel' | 'BY' | 'fdr' | 'none'
      }

      if (geneSource === 'upload') {
        // Upload mode - would need file handling
        setError('文件上传功能待实现。请使用 DESeq2 结果模式。')
        setLoading(false)
        return
      } else {
        // From DESeq2 results
        params = {
          source: 'deseq2',
          contrast: selectedContrast ?? undefined,
          database,
          species,
          gene_type: geneType,
          pvalue: pCutoff,
          pAdjustMethod: padjMethod,
        }
      }

      const res = await runGSEA(params)

      if (res.status === 'ok' && res.result) {
        setGseaResult(res.result as unknown as GSEAResult)
      } else {
        setError(res.message ?? 'GSEA 分析未返回结果')
      }
    } catch (err: unknown) {
      const msg =
        (err as { response?: { data?: { error?: string } } })?.response?.data?.error ??
        (err as Error)?.message ?? 'GSEA 分析失败'
      setError(String(msg))
    } finally {
      setLoading(false)
    }
  }, [geneSource, database, species, geneType, pCutoff, padjMethod, selectedContrast])

  // Generate plot
  const handleGeneratePlot = useCallback(async () => {
    if (!gseaResult || !geneSetID) {
      setError('请先选择一个基因集 ID')
      return
    }

    setError(null)
    setSvg(null)
    setPlotLoading(true)

    try {
      const svgContent = await getPlot('gsea', {
        result: gseaResult,
        params: {
          geneSetID,
          addGene,
          addPval,
          pvalSize,
          subPlot,
          termWidth,
          base_size: baseSize,
          arrowType,
          geneCol,
          newGsea,
        },
      })
      setSvg(typeof svgContent === 'string' ? svgContent : (svgContent as { svg: string }).svg)
    } catch (err: unknown) {
      const msg = (err as Error)?.message ?? 'Failed to generate plot'
      setError(String(msg))
    } finally {
      setPlotLoading(false)
    }
  }, [gseaResult, geneSetID, addGene, addPval, pvalSize, subPlot, termWidth, baseSize, arrowType, geneCol, newGsea])

  // Export SVG
  const handleExportSVG = useCallback(() => {
    if (!svg) return
    const blob = new Blob([svg], { type: 'image/svg+xml' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `gsea_plot_${geneSetID}.svg`
    a.click()
    URL.revokeObjectURL(url)
  }, [svg, geneSetID])

  // Handle params loaded from RDS
  const handleParamsLoaded = useCallback(
    (loadedParams: Record<string, unknown>) => {
      if (loadedParams.geneSetID) setGeneSetID(loadedParams.geneSetID as string)
      if (loadedParams.addGene !== undefined) setAddGene(loadedParams.addGene as boolean)
      if (loadedParams.addPval !== undefined) setAddPval(loadedParams.addPval as boolean)
      if (loadedParams.pvalSize) setPvalSize(loadedParams.pvalSize as number)
      if (loadedParams.subPlot) setSubPlot(loadedParams.subPlot as number)
      if (loadedParams.termWidth) setTermWidth(loadedParams.termWidth as number)
      if (loadedParams.base_size) setBaseSize(loadedParams.base_size as number)
      if (loadedParams.arrowType) setArrowType(loadedParams.arrowType as 'open' | 'closed')
      if (loadedParams.geneCol) setGeneCol(loadedParams.geneCol as string)
      if (loadedParams.newGsea !== undefined) setNewGsea(loadedParams.newGsea as boolean)
    },
    []
  )

  // Handle SVG loaded from RDS
  const handleSvgLoaded = useCallback((svgContent: string) => {
    setSvg(svgContent)
  }, [])

  // Export RData
  const handleExportRData = useCallback(async () => {
    if (!gseaResult || !geneSetID) {
      setError('请先运行 GSEA 分析并选择基因集')
      return
    }

    try {
      const response = await fetch('/api/export/gsea/rdata', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          result: gseaResult,
          params: {
            geneSetID,
            addGene,
            addPval,
            base_size: baseSize,
            termWidth,
            subPlot,
            newGsea,
          },
        }),
      })

      const data = await response.json()

      if (data.status === 'ok' && data.data) {
        // Decode base64 and download
        const byteCharacters = atob(data.data)
        const byteNumbers = new Array(byteCharacters.length)
        for (let i = 0; i < byteCharacters.length; i++) {
          byteNumbers[i] = byteCharacters.charCodeAt(i)
        }
        const byteArray = new Uint8Array(byteNumbers)
        const blob = new Blob([byteArray], { type: 'application/octet-stream' })
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = data.filename || 'gsea_plot.rda'
        a.click()
        URL.revokeObjectURL(url)
      } else {
        setError('RData 导出失败')
      }
    } catch {
      setError('RData 导出失败')
    }
  }, [gseaResult, geneSetID, addGene, addPval, baseSize, termWidth, subPlot, newGsea])

  // Export TSV
  const handleExportTSV = useCallback(() => {
    if (!gseaResult) return

    const rows: string[][] = [
      ['Pathway', 'P_value', 'Adjusted_P', 'NES', 'Enrichment_Score', 'Set_Size', 'Rank', 'Leading_Edge', 'Core_Enrichment'],
    ]

    const idArr = gseaResult.ID ?? []
    const descArr = gseaResult.Description ?? []
    const pvalArr = gseaResult.pvalue ?? []
    const padjArr = gseaResult.p_adjust ?? []
    const nesArr = gseaResult.NES ?? []
    const esArr = gseaResult.enrichmentScore ?? []
    const sizeArr = gseaResult.setSize ?? []
    const rankArr = gseaResult.rank ?? []
    const leArr = gseaResult.leading_edge ?? []
    const ceArr = gseaResult.core_enrichment ?? []

    for (let i = 0; i < idArr.length; i++) {
      rows.push([
        String(descArr[i] ?? idArr[i] ?? ''),
        String(pvalArr[i] ?? ''),
        String(padjArr[i] ?? ''),
        String(nesArr[i] ?? ''),
        String(esArr[i] ?? ''),
        String(sizeArr[i] ?? ''),
        String(rankArr[i] ?? ''),
        String(leArr[i] ?? ''),
        String(ceArr[i] ?? ''),
      ])
    }

    const csvContent = rows.map((r) => r.join('\t')).join('\n')
    const blob = new Blob([csvContent], { type: 'text/tab-separated-values' })
    const url = URL.createObjectURL(blob)
    const a = document.createElement('a')
    a.href = url
    a.download = `gsea_results_${database.replace(/[:/]/g, '_')}.tsv`
    a.click()
    URL.revokeObjectURL(url)
  }, [gseaResult, database])

  return (
    <div className="space-y-6">
      <h2 className="text-2xl font-bold">GSEA 基因集富集分析</h2>

      {/* Gene Input Section */}
      <div data-testid="gsea-gene-input-section" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">数据来源</h3>

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
            <span className="text-sm">从 DESeq2 结果构建 geneList</span>
            {!hasWorkspaceResults && (
              <span className="text-xs text-gray-400">(需先运行 DESeq2)</span>
            )}
          </label>
          <label className="flex items-center gap-2 cursor-pointer">
            <input
              type="radio"
              name="geneSource"
              data-testid="gene-source-upload"
              checked={geneSource === 'upload'}
              onChange={() => setGeneSource('upload')}
              className="w-4 h-4"
            />
            <span className="text-sm">上传 geneList xlsx</span>
          </label>
        </div>

        {/* DESeq2 source options */}
        {geneSource === 'deseq2' && hasWorkspaceResults && (
          <div data-testid="gsea-contrast-selector">
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
        )}

        {geneSource === 'upload' && (
          <div className="text-sm text-gray-500 bg-gray-50 p-3 rounded">
            文件上传功能待实现。请使用 DESeq2 结果模式，或确保 geneList 格式为：Gene、log2FoldChange 两列。
          </div>
        )}

        {geneSource === 'deseq2' && !hasWorkspaceResults && (
          <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded">
            需先运行 DESeq2 分析，请切换到上传模式或先运行 DESeq2。
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

          {/* P-adjust method selector */}
          <div data-testid="padj-method-selector">
            <label className="block text-sm text-gray-600 mb-1">P 值校正方法</label>
            <select
              value={padjMethod}
              onChange={(e) => setPadjMethod(e.target.value as typeof padjMethod)}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              {PADJ_METHODS.map((pm) => (
                <option key={pm.value} value={pm.value}>
                  {pm.label}
                </option>
              ))}
            </select>
          </div>
        </div>

        {/* Run button */}
        <div>
          <button
            data-testid="run-gsea"
            onClick={handleRunGSEA}
            disabled={loading || (geneSource === 'deseq2' && !hasWorkspaceResults)}
            className="px-6 py-2 rounded-md text-sm font-medium bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {loading ? '分析中...' : '运行 GSEA 分析'}
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
      <div data-testid="gsea-result-area" className="border border-gray-200 rounded-lg p-4">
        <div className="flex justify-between items-center mb-3">
          <h3 className="font-medium">GSEA 结果</h3>
          <div className="flex gap-2">
            <button
              data-testid="export-svg"
              onClick={handleExportSVG}
              disabled={!svg}
              className="px-3 py-1.5 rounded text-sm bg-blue-50 text-blue-700 hover:bg-blue-100 disabled:opacity-50"
            >
              导出 SVG
            </button>
            <button
              data-testid="export-rdata"
              onClick={handleExportRData}
              disabled={!gseaResult || !geneSetID}
              className="px-3 py-1.5 rounded text-sm bg-purple-50 text-purple-700 hover:bg-purple-100 disabled:opacity-50"
            >
              导出 RData
            </button>
            <button
              onClick={handleExportTSV}
              disabled={!gseaResult}
              className="px-3 py-1.5 rounded text-sm bg-green-50 text-green-700 hover:bg-green-100 disabled:opacity-50"
            >
              导出 TSV
            </button>
          </div>
        </div>

        {gseaResult ? (
          <div className="overflow-x-auto">
            <table className="min-w-full text-sm">
              <thead>
                <tr className="bg-gray-50">
                  <th className="px-3 py-2 text-left font-medium text-gray-600">Pathway</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">NES</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">P-value</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Adjusted P</th>
                  <th className="px-3 py-2 text-right font-medium text-gray-600">Set Size</th>
                  <th className="px-3 py-2 text-center font-medium text-gray-600">操作</th>
                </tr>
              </thead>
              <tbody>
                {(gseaResult.ID ?? []).map((id, i) => (
                  <tr key={id} className="border-t border-gray-100 hover:bg-gray-50">
                    <td className="px-3 py-2">{gseaResult.Description?.[i] ?? id}</td>
                    <td className="px-3 py-2 text-right font-mono">
                      {gseaResult.NES?.[i]?.toFixed(3) ?? '-'}
                    </td>
                    <td className="px-3 py-2 text-right font-mono">
                      {gseaResult.pvalue?.[i]?.toExponential(2) ?? '-'}
                    </td>
                    <td className="px-3 py-2 text-right font-mono">
                      {gseaResult.p_adjust?.[i]?.toExponential(2) ?? '-'}
                    </td>
                    <td className="px-3 py-2 text-right">
                      {gseaResult.setSize?.[i] ?? '-'}
                    </td>
                    <td className="px-3 py-2 text-center">
                      <button
                        onClick={() => setGeneSetID(id)}
                        className="text-blue-600 hover:text-blue-800 text-xs"
                      >
                        绘图
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <p className="text-gray-500 text-sm">运行分析后结果将显示在此处</p>
        )}
      </div>

      {/* Plot Parameters Panel */}
      <div data-testid="plot-params-panel" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">绘图参数</h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {/* GeneSetID input */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">基因集 ID</label>
            <input
              data-testid="param-genesetid"
              type="text"
              value={geneSetID}
              onChange={(e) => setGeneSetID(e.target.value)}
              placeholder="选择上方表格中的基因集"
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* addGene checkbox */}
          <div className="flex items-center gap-2">
            <input
              type="checkbox"
              id="addGene"
              data-testid="param-addgene"
              checked={addGene}
              onChange={(e) => setAddGene(e.target.checked)}
              className="w-4 h-4"
            />
            <label htmlFor="addGene" className="text-sm text-gray-600">显示基因标注</label>
          </div>

          {/* addPval checkbox */}
          <div className="flex items-center gap-2">
            <input
              type="checkbox"
              id="addPval"
              data-testid="param-addpval"
              checked={addPval}
              onChange={(e) => setAddPval(e.target.checked)}
              className="w-4 h-4"
            />
            <label htmlFor="addPval" className="text-sm text-gray-600">显示 P 值</label>
          </div>

          {/* subPlot selector */}
          <div data-testid="param-subplot">
            <label className="block text-sm text-gray-600 mb-1">子图类型</label>
            <select
              value={subPlot}
              onChange={(e) => setSubPlot(Number(e.target.value))}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              <option value={1}>仅富集分数</option>
              <option value={2}>富集分数 + 排序</option>
              <option value={3}>经典三线图</option>
            </select>
          </div>

          {/* base_size input */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">基础字号</label>
            <input
              data-testid="param-basesize"
              type="number"
              min="8"
              max="24"
              value={baseSize}
              onChange={(e) => setBaseSize(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* termWidth input */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">名称宽度</label>
            <input
              data-testid="param-termwidth"
              type="number"
              min="10"
              max="80"
              value={termWidth}
              onChange={(e) => setTermWidth(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>

          {/* arrowType selector */}
          <div data-testid="param-arrowtype">
            <label className="block text-sm text-gray-600 mb-1">箭头类型</label>
            <select
              value={arrowType}
              onChange={(e) => setArrowType(e.target.value as 'open' | 'closed')}
              className="border border-gray-300 rounded px-3 py-1.5 text-sm w-full"
            >
              <option value="open">开放</option>
              <option value="closed">闭合</option>
            </select>
          </div>

          {/* geneCol color input */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">基因标注颜色</label>
            <input
              data-testid="param-genecol"
              type="color"
              value={geneCol}
              onChange={(e) => setGeneCol(e.target.value)}
              className="w-full h-8 border border-gray-300 rounded"
            />
          </div>

          {/* newGsea checkbox */}
          <div className="flex items-center gap-2">
            <input
              type="checkbox"
              id="newGsea"
              data-testid="param-newgsea"
              checked={newGsea}
              onChange={(e) => setNewGsea(e.target.checked)}
              className="w-4 h-4"
            />
            <label htmlFor="newGsea" className="text-sm text-gray-600">新版 GSEA 样式</label>
          </div>

          {/* pvalSize input */}
          <div>
            <label className="block text-sm text-gray-600 mb-1">P 值字号</label>
            <input
              type="number"
              min="2"
              max="12"
              value={pvalSize}
              onChange={(e) => setPvalSize(Number(e.target.value))}
              className="w-full border border-gray-300 rounded px-3 py-1.5 text-sm"
            />
          </div>
        </div>

        {/* Generate plot button */}
        <div className="mt-4">
          <button
            onClick={handleGeneratePlot}
            disabled={plotLoading || !gseaResult || !geneSetID}
            className="px-6 py-2 rounded-md text-sm font-medium bg-green-600 text-white hover:bg-green-700 disabled:opacity-50 disabled:cursor-not-allowed"
          >
            {plotLoading ? '生成中...' : '生成 GSEA 图'}
          </button>
        </div>
      </div>

      {/* Plot Save/Load */}
      <PlotSaveLoad
        module="gsea"
        params={{
          geneSetID,
          addGene,
          addPval,
          pvalSize,
          subPlot,
          termWidth,
          base_size: baseSize,
          arrowType,
          geneCol,
          newGsea,
        }}
        plotData={gseaResult ? { result: gseaResult } : null}
        disabled={!gseaResult}
        onParamsLoaded={handleParamsLoaded}
        onSvgLoaded={handleSvgLoaded}
      />

      {/* Plot Display Area */}
      <div data-testid="gsea-plot-area" className="border border-gray-200 rounded-lg p-4">
        <h3 className="font-medium mb-3">GSEA 富集图</h3>

        {svg ? (
          <div
            className="flex justify-center"
            dangerouslySetInnerHTML={{ __html: svg }}
          />
        ) : (
          <p className="text-gray-500 text-sm">
            {gseaResult
              ? '请在上方选择基因集 ID 并点击"生成 GSEA 图"'
              : '运行分析后可在此处生成 GSEA 图'}
          </p>
        )}
      </div>
    </div>
  )
}
