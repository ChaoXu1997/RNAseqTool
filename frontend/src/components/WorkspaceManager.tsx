import { useCallback, useRef } from 'react'
import { useWorkspaceStore } from '../stores/workspace'
import { saveWorkspace, loadWorkspace, getWorkspaceStatus } from '../api/client'

export default function WorkspaceManager() {
  const { completedSteps, workspaceVersion, setCompletedSteps, setWorkspaceVersion } = useWorkspaceStore()
  const fileInputRef = useRef<HTMLInputElement>(null)

  const handleSave = useCallback(async () => {
    try {
      const blob = await saveWorkspace()
      // Trigger download
      const url = URL.createObjectURL(blob)
      const a = document.createElement('a')
      a.href = url
      a.download = `workspace_${new Date().toISOString().slice(0, 10)}.rds`
      document.body.appendChild(a)
      a.click()
      document.body.removeChild(a)
      URL.revokeObjectURL(url)
    } catch (error) {
      console.error('Failed to save workspace:', error)
      alert('Failed to save workspace')
    }
  }, [])

  const handleLoadClick = useCallback(() => {
    fileInputRef.current?.click()
  }, [])

  const handleFileChange = useCallback(async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0]
    if (!file) return

    try {
      const result = await loadWorkspace(file)
      if (result.status === 'ok') {
        setCompletedSteps(result.steps)
        // Refresh workspace status from backend
        const status = await getWorkspaceStatus()
        setCompletedSteps(status.steps)
        setWorkspaceVersion(status.version)
        alert(`Workspace loaded: ${result.message}`)
      } else {
        alert('Failed to load workspace')
      }
    } catch (error) {
      console.error('Failed to load workspace:', error)
      alert('Failed to load workspace: invalid file or version mismatch')
    }
    // Reset file input
    if (fileInputRef.current) {
      fileInputRef.current.value = ''
    }
  }, [setCompletedSteps, setWorkspaceVersion])

  const handleRefreshStatus = useCallback(async () => {
    try {
      const status = await getWorkspaceStatus()
      setCompletedSteps(status.steps)
      setWorkspaceVersion(status.version)
    } catch (error) {
      console.error('Failed to get workspace status:', error)
    }
  }, [setCompletedSteps, setWorkspaceVersion])

  return (
    <div className="workspace-manager p-3 bg-gray-50 rounded-lg border border-gray-200">
      <h3 className="text-sm font-semibold mb-2 text-gray-700">Workspace</h3>
      
      <div className="flex gap-2 mb-3">
        <button
          onClick={handleSave}
          className="px-3 py-1.5 text-xs bg-blue-500 text-white rounded hover:bg-blue-600 transition-colors"
        >
          保存 Workspace
        </button>
        <button
          onClick={handleLoadClick}
          className="px-3 py-1.5 text-xs bg-green-500 text-white rounded hover:bg-green-600 transition-colors"
        >
          加载 Workspace
        </button>
        <input
          ref={fileInputRef}
          type="file"
          accept=".rds"
          onChange={handleFileChange}
          className="hidden"
        />
      </div>

      <div className="workspace-status">
        <div className="flex items-center justify-between mb-1">
          <span className="text-xs text-gray-500">已完成分析:</span>
          <button
            onClick={handleRefreshStatus}
            className="text-xs text-blue-500 hover:text-blue-700"
          >
            刷新
          </button>
        </div>
        {completedSteps.length === 0 ? (
          <p className="text-xs text-gray-400 italic">无</p>
        ) : (
          <div className="flex flex-wrap gap-1">
            {completedSteps.map((step) => (
              <span
                key={step}
                className="px-2 py-0.5 text-xs bg-green-100 text-green-700 rounded"
              >
                {step}
              </span>
            ))}
          </div>
        )}
        {workspaceVersion && (
          <p className="text-xs text-gray-400 mt-1">版本: {workspaceVersion}</p>
        )}
      </div>
    </div>
  )
}
