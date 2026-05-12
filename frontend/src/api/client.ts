import axios from 'axios'

const api = axios.create({
  baseURL: '/api',
  timeout: 300000, // 5 min for long analyses
})

// Upload data file
export async function uploadFile(file: File, type: string) {
  const formData = new FormData()
  formData.append('file', file)
  formData.append('type', type)
  const { data } = await api.post('/data/upload', formData)
  return data
}

// Load demo data
export async function loadDemoData(type: string) {
  const { data } = await api.post('/data/demo', { type })
  return data
}

// Analysis endpoints (async polling)
export async function startAnalysis(module: string, params: Record<string, unknown>) {
  const { data } = await api.post(`/analyze/${module}`, params)
  return data.taskId as string
}

// Get available contrast pairs from sample info
export async function getContrastPairs(sampleInfo: Record<string, unknown>) {
  const { data } = await api.post('/deseq2/contrasts', { sampleInfo })
  return data as { status: string; pairs: Array<{ group1: string; group2: string }>; groups: string[] }
}

export async function getTaskStatus(taskId: string) {
  const { data } = await api.get(`/task/${taskId}`)
  return data as { status: 'pending' | 'running' | 'done' | 'error'; progress?: number; result?: unknown; error?: string }
}

// Plot endpoints
export async function getPlot(module: string, params: Record<string, unknown>) {
  const { data } = await api.post(`/plot/${module}`, params, { responseType: 'text' })
  return data as string // SVG content
}

// Workspace
export async function saveWorkspace() {
  const { data } = await api.post('/workspace/save', null, { responseType: 'blob' })
  return data as Blob
}

export async function loadWorkspace(file: File) {
  const formData = new FormData()
  formData.append('file', file)
  const { data } = await api.post('/workspace/load', formData)
  return data as { status: string; message?: string; steps: string[] }
}

export async function getWorkspaceStatus() {
  const { data } = await api.get('/workspace/status')
  return data as { status: string; steps: string[]; version: string }
}

// Export
export async function exportPlot(module: string, format: 'svg' | 'pdf' | 'tif' | 'png', params: Record<string, unknown>) {
  const { data } = await api.post(`/export/${format}`, { module, ...params }, { responseType: 'blob' })
  return data as Blob
}

// Enrichment analysis
export async function runEnrich(params: {
  genes?: string[]
  source?: 'deseq2'
  contrast?: string
  direction?: 'Up' | 'Down' | 'All'
  database: string
  species?: string
  gene_type?: 'symbol' | 'entrezid' | 'ensembl'
  p_cutoff?: number
}) {
  const { data } = await api.post('/analyze/enrich', params)
  return data as {
    status: string
    message?: string
    results?: Record<string, unknown[]>
    gene_count?: number
    database?: string
    species?: string
  }
}

// GSEA analysis
export async function runGSEA(params: {
  geneList?: Record<string, number>
  source?: 'deseq2'
  contrast?: string
  database: string
  species?: string
  gene_type?: 'symbol' | 'entrezid' | 'ensembl'
  pvalue?: number
  pAdjustMethod?: 'BH' | 'bonferroni' | 'holm' | 'hochberg' | 'hommel' | 'BY' | 'fdr' | 'none'
}) {
  const { data } = await api.post('/analyze/gsea', params)
  return data as {
    status: string
    message?: string
    result?: Record<string, unknown[]>
    gene_count?: number
    database?: string
    species?: string
    parameters?: {
      pvalue: number
      pAdjustMethod: string
    }
  }
}

// Get available databases
export async function getDatabases() {
  const { data } = await api.get('/databases')
  return data as { status: string; databases: string[] }
}

// Get available species
export async function getSpecies() {
  const { data } = await api.get('/species')
  return data as { status: string; species: string[] }
}

// GeneTrend Mfuzz analysis
export async function runGeneTrend(params: {
  norm: Record<string, unknown>
  sampleInfo: Record<string, unknown>
  c_value?: number
  filterNA?: number
  fillNA?: 'mean' | 'median' | 'knn'
  filterSD?: number
}) {
  const { data } = await api.post('/analyze/genetrend', params)
  return data as {
    taskId: string
  }
}

// GeneTrend plot
export async function getGeneTrendPlot(params: {
  plotType: 'all' | 'single'
  clusterNum?: number
  params?: Record<string, unknown>
}) {
  const { data } = await api.post('/plot/genetrend', params)
  return data as { svg: string }
}

// Export GeneTrend RData
export async function exportGeneTrendRData() {
  const { data } = await api.post('/export/genetrend/rdata', {})
  return data as { status: string; data: string; filename: string }
}

// Save plot as RDS (ggplot object + params)
export async function savePlotRds(
  module: string,
  params: Record<string, unknown>,
  plotData: Record<string, unknown>
) {
  const { data } = await api.post('/plot/save', {
    module,
    params,
    plot_data: plotData,
  })
  return data as { status: string; data: string; filename: string }
}

// Load plot from RDS file
export async function loadPlotRds(file: File) {
  const formData = new FormData()
  formData.append('file', file)
  const { data } = await api.post('/plot/load', formData)
  return data as {
    status: string
    module: string
    params: Record<string, unknown>
    svg: string
  }
}

// WGCNA analysis (multi-step)
export async function runWGCNA(params: {
  step: 'sampletree' | 'power' | 'network' | 'module_trait' | 'mm_gs' | 'tom' | 'export'
  expr?: Record<string, unknown>
  trait?: Record<string, unknown>
  cutHeight?: number
  abline?: number
  power?: number
  mergeCutHeight?: number
  module?: string
  nSelect?: number
}) {
  const { data } = await api.post('/analyze/wgcna', params)
  return data as { taskId: string }
}

// WGCNA plot (re-render specific step)
export async function getWGCNAPlot(params: {
  step: 'mm_gs' | 'tom'
  module?: string
  trait?: string
  nSelect?: number
}) {
  const { data } = await api.post('/plot/wgcna', params)
  return data as { svg: string }
}

export default api
