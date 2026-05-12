import axios from 'axios'

const api = axios.create({
  baseURL: '/api',
  timeout: 300000, // 5 min for long analyses
})

// Upload data file
export async function uploadFile(file: File, type: 'expr' | 'sampleInfo' | 'deseq2' | 'genelist' | 'trait') {
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
  return data.task_id as string
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
  return data
}

// Export
export async function exportPlot(module: string, format: 'svg' | 'pdf' | 'tif' | 'png', params: Record<string, unknown>) {
  const { data } = await api.post(`/export/${format}`, { module, ...params }, { responseType: 'blob' })
  return data as Blob
}

export default api
