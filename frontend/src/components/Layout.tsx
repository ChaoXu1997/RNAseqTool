import { Outlet, Link, useLocation } from 'react-router-dom'
import WorkspaceManager from './WorkspaceManager'

const navItems = [
  { path: '/', label: 'Home', icon: '🏠' },
  { path: '/pca', label: 'PCA', icon: '📊' },
  { path: '/deseq2', label: 'DESeq2', icon: '🔬' },
  { path: '/volcano', label: 'Volcano', icon: '🌋' },
  { path: '/enrich', label: 'Enrich', icon: '📈' },
  { path: '/gsea', label: 'GSEA', icon: '📉' },
  { path: '/genetrend', label: 'GeneTrend', icon: '🧬' },
  { path: '/wgcna', label: 'WGCNA', icon: '🕸️' },
]

export default function Layout() {
  const location = useLocation()

  return (
    <div className="flex h-screen">
      <aside className="w-56 bg-gray-50 border-r border-gray-200 p-4 flex flex-col">
        <h1 className="text-lg font-bold mb-6">RNAseqTool</h1>
        <nav className="space-y-1 flex-1">
          {navItems.map((item) => (
            <Link
              key={item.path}
              to={item.path}
              className={`block px-3 py-2 rounded-md text-sm ${
                location.pathname === item.path
                  ? 'bg-blue-100 text-blue-700 font-medium'
                  : 'text-gray-700 hover:bg-gray-100'
              }`}
            >
              {item.icon} {item.label}
            </Link>
          ))}
        </nav>
        <div className="mt-4">
          <WorkspaceManager />
        </div>
      </aside>
      <main className="flex-1 overflow-auto p-6">
        <Outlet />
      </main>
    </div>
  )
}
