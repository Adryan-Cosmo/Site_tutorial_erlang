"use client"

import { useEffect, useState } from "react"
import { useRouter } from "next/navigation"
import { Keyboard } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from "@/components/ui/dialog"

export default function KeyboardShortcuts() {
  const router = useRouter()
  const [isDialogOpen, setIsDialogOpen] = useState(false)

  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      // Ignora atalhos quando estiver digitando em inputs
      if (e.target instanceof HTMLInputElement || e.target instanceof HTMLTextAreaElement) {
        return
      }

      // Atalhos de navegação
      if (e.key === "ArrowLeft" && e.altKey) {
        // Navegar para o capítulo anterior
        const prevButton = document.querySelector('a[href*="Previous"]') as HTMLAnchorElement
        if (prevButton) prevButton.click()
      }

      if (e.key === "ArrowRight" && e.altKey) {
        // Navegar para o próximo capítulo
        const nextButton = document.querySelector('a[href*="Next"]') as HTMLAnchorElement
        if (nextButton) nextButton.click()
      }

      // Atalho para busca
      if (e.key === "/" && !e.ctrlKey && !e.metaKey) {
        e.preventDefault()
        const searchInput = document.querySelector('input[type="text"][placeholder*="Buscar"]') as HTMLInputElement
        if (searchInput) {
          searchInput.focus()
        }
      }

      // Atalho para mostrar ajuda de atalhos
      if (e.key === "?" || (e.key === "h" && e.ctrlKey)) {
        e.preventDefault()
        setIsDialogOpen(true)
      }

      // Atalho para voltar ao topo
      if (e.key === "t" && e.ctrlKey) {
        e.preventDefault()
        window.scrollTo({ top: 0, behavior: "smooth" })
      }
    }

    window.addEventListener("keydown", handleKeyDown)
    return () => window.removeEventListener("keydown", handleKeyDown)
  }, [router])

  return (
    <Dialog open={isDialogOpen} onOpenChange={setIsDialogOpen}>
      <DialogTrigger asChild>
        <Button variant="ghost" size="icon" aria-label="Atalhos de teclado">
          <Keyboard className="h-5 w-5" />
        </Button>
      </DialogTrigger>
      <DialogContent className="sm:max-w-md">
        <DialogHeader>
          <DialogTitle>Atalhos de Teclado</DialogTitle>
        </DialogHeader>
        <div className="space-y-4 py-4">
          <div className="grid grid-cols-2 gap-4">
            <div className="font-mono bg-gray-100 p-2 rounded text-center">/</div>
            <div>Buscar</div>

            <div className="font-mono bg-gray-100 p-2 rounded text-center">Alt + ←</div>
            <div>Capítulo anterior</div>

            <div className="font-mono bg-gray-100 p-2 rounded text-center">Alt + →</div>
            <div>Próximo capítulo</div>

            <div className="font-mono bg-gray-100 p-2 rounded text-center">Ctrl + T</div>
            <div>Voltar ao topo</div>

            <div className="font-mono bg-gray-100 p-2 rounded text-center">?</div>
            <div>Mostrar esta ajuda</div>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  )
}
