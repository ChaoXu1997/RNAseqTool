import { describe, it, expect, beforeEach } from 'vitest'
import { useWorkspaceStore } from './workspace'

describe('WorkspaceStore', () => {
  beforeEach(() => {
    // Reset store before each test
    useWorkspaceStore.getState().reset()
  })

  it('has null data initially', () => {
    const state = useWorkspaceStore.getState()
    expect(state.exprRaw).toBeNull()
    expect(state.exprNorm).toBeNull()
    expect(state.sampleInfo).toBeNull()
  })

  it('sets exprRaw data', () => {
    const { setExprRaw } = useWorkspaceStore.getState()
    const data = { name: 'test.csv', rows: 100, cols: 10, preview: [['a', 'b']] }
    setExprRaw(data)
    expect(useWorkspaceStore.getState().exprRaw).toEqual(data)
  })

  it('sets sampleInfo data', () => {
    const { setSampleInfo } = useWorkspaceStore.getState()
    const data = { name: 'sample.csv', rows: 6, cols: 3, preview: [['S1', 'A']] }
    setSampleInfo(data)
    expect(useWorkspaceStore.getState().sampleInfo).toEqual(data)
  })

  it('resets all data', () => {
    const { setExprRaw, setSampleInfo, reset } = useWorkspaceStore.getState()
    setExprRaw({ name: 'a', rows: 1, cols: 1, preview: [] })
    setSampleInfo({ name: 'b', rows: 1, cols: 1, preview: [] })

    reset()

    const state = useWorkspaceStore.getState()
    expect(state.exprRaw).toBeNull()
    expect(state.sampleInfo).toBeNull()
  })

  it('sets analysis result', () => {
    const { setAnalysisResult } = useWorkspaceStore.getState()
    const result = { taskId: 'task_1', status: 'done' as const, data: { x: 1 } }
    setAnalysisResult('deseq2', result)
    expect(useWorkspaceStore.getState().deseq2Result).toEqual(result)
  })

  it('sets plot params per module', () => {
    const { setPlotParams } = useWorkspaceStore.getState()
    setPlotParams('pca', { ntop: 500 })
    expect(useWorkspaceStore.getState().plotParams.pca).toEqual({ ntop: 500 })

    setPlotParams('volcano', { fc: 2 })
    expect(useWorkspaceStore.getState().plotParams.volcano).toEqual({ fc: 2 })
  })
})
