import { describe, it, expect, beforeEach } from 'vitest'
import { useWorkspaceStore } from './workspace'

describe('workspace store', () => {
  beforeEach(() => {
    useWorkspaceStore.getState().reset()
  })

  it('has initial empty state', () => {
    const state = useWorkspaceStore.getState()
    
    expect(state.exprRaw).toBeNull()
    expect(state.exprNorm).toBeNull()
    expect(state.sampleInfo).toBeNull()
    expect(state.deseq2Result).toBeNull()
    expect(state.pcaResult).toBeNull()
    expect(state.plotParams).toEqual({})
    expect(state.completedSteps).toEqual([])
    expect(state.workspaceVersion).toBe('')
  })

  it('sets completed steps', () => {
    const { setCompletedSteps } = useWorkspaceStore.getState()
    
    setCompletedSteps(['DESeq2', 'PCA'])
    
    const state = useWorkspaceStore.getState()
    expect(state.completedSteps).toEqual(['DESeq2', 'PCA'])
  })

  it('sets workspace version', () => {
    const { setWorkspaceVersion } = useWorkspaceStore.getState()
    
    setWorkspaceVersion('0.1.0')
    
    const state = useWorkspaceStore.getState()
    expect(state.workspaceVersion).toBe('0.1.0')
  })

  it('resets completed steps and version on reset', () => {
    const { setCompletedSteps, setWorkspaceVersion, reset } = useWorkspaceStore.getState()
    
    setCompletedSteps(['DESeq2'])
    setWorkspaceVersion('0.1.0')
    reset()
    
    const state = useWorkspaceStore.getState()
    expect(state.completedSteps).toEqual([])
    expect(state.workspaceVersion).toBe('')
  })

  it('sets analysis results dynamically', () => {
    const { setAnalysisResult } = useWorkspaceStore.getState()
    
    setAnalysisResult('deseq2', { taskId: '123', status: 'done' })
    
    const state = useWorkspaceStore.getState()
    expect(state.deseq2Result).toEqual({ taskId: '123', status: 'done' })
  })

  it('sets plot params per module', () => {
    const { setPlotParams } = useWorkspaceStore.getState()
    
    setPlotParams('pca', { color: 'blue', size: 10 })
    setPlotParams('volcano', { fc: 2 })
    
    const state = useWorkspaceStore.getState()
    expect(state.plotParams.pca).toEqual({ color: 'blue', size: 10 })
    expect(state.plotParams.volcano).toEqual({ fc: 2 })
  })
})
