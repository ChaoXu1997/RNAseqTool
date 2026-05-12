import { BrowserRouter, Routes, Route } from 'react-router-dom'
import { QueryClient, QueryClientProvider } from '@tanstack/react-query'
import Home from './pages/Home'
import PCA from './pages/PCA'
import DESeq2 from './pages/DESeq2'
import Volcano from './pages/Volcano'
import Enrich from './pages/Enrich'
import GSEA from './pages/GSEA'
import GeneTrend from './pages/GeneTrend'
import WGCNA from './pages/WGCNA'
import Layout from './components/Layout'

const queryClient = new QueryClient()

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <Routes>
          <Route element={<Layout />}>
            <Route path="/" element={<Home />} />
            <Route path="/pca" element={<PCA />} />
            <Route path="/deseq2" element={<DESeq2 />} />
            <Route path="/volcano" element={<Volcano />} />
            <Route path="/enrich" element={<Enrich />} />
            <Route path="/gsea" element={<GSEA />} />
            <Route path="/genetrend" element={<GeneTrend />} />
            <Route path="/wgcna" element={<WGCNA />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </QueryClientProvider>
  )
}
