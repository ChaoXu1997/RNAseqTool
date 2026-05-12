import { create } from 'zustand'

export interface DataFile {
  name: string
  rows: number
  cols: number
  preview: string[][]
}

export interface AnalysisResult {
  taskId: string
  status: 'pending' | 'running' | 'done' | 'error'
  progress?: number
  data?: unknown
  error?: string
}

export interface PlotParams {
  [key: string]: unknown
}

interface WorkspaceState {
  // Uploaded data
  exprRaw: DataFile | null
  exprNorm: DataFile | null
  sampleInfo: DataFile | null

  // Analysis results
  deseq2Result: AnalysisResult | null
  pcaResult: AnalysisResult | null

  // Plot params per module
  plotParams: Record<string, PlotParams>

  // Actions
  setExprRaw: (data: DataFile | null) => void
  setExprNorm: (data: DataFile | null) => void
  setSampleInfo: (data: DataFile | null) => void
  setAnalysisResult: (module: string, result: AnalysisResult) => void
  setPlotParams: (module: string, params: PlotParams) => void
  reset: () => void
}

export const useWorkspaceStore = create<WorkspaceState>((set) => ({
  exprRaw: null,
  exprNorm: null,
  sampleInfo: null,
  deseq2Result: null,
  pcaResult: null,
  plotParams: {},

  setExprRaw: (data) => set({ exprRaw: data }),
  setExprNorm: (data) => set({ exprNorm: data }),
  setSampleInfo: (data) => set({ sampleInfo: data }),

  setAnalysisResult: (module, result) =>
    set(() => ({
      [`${module}Result`]: result,
    })),

  setPlotParams: (module, params) =>
    set((state) => ({
      plotParams: { ...state.plotParams, [module]: params },
    })),

  reset: () =>
    set({
      exprRaw: null,
      exprNorm: null,
      sampleInfo: null,
      deseq2Result: null,
      pcaResult: null,
      plotParams: {},
    }),
}))
