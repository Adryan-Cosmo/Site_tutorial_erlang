"use client"

import { useState, useEffect } from "react"
import { Bookmark, Edit, Trash } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Sheet, SheetContent, SheetHeader, SheetTitle, SheetTrigger } from "@/components/ui/sheet"
import { Textarea } from "@/components/ui/textarea"

interface BookmarkItem {
  id: string
  title: string
  url: string
  position: number
  notes: string
  createdAt: number
}

export default function Bookmarks() {
  const [bookmarks, setBookmarks] = useState<BookmarkItem[]>([])
  const [isOpen, setIsOpen] = useState(false)
  const [editingId, setEditingId] = useState<string | null>(null)
  const [noteText, setNoteText] = useState("")

  // Carregar marcadores do localStorage
  useEffect(() => {
    const savedBookmarks = localStorage.getItem("erlang_bookmarks")
    if (savedBookmarks) {
      try {
        setBookmarks(JSON.parse(savedBookmarks))
      } catch (e) {
        console.error("Erro ao carregar marcadores:", e)
      }
    }
  }, [])

  // Salvar marcadores no localStorage quando mudam
  useEffect(() => {
    localStorage.setItem("erlang_bookmarks", JSON.stringify(bookmarks))
  }, [bookmarks])

  const addBookmark = () => {
    const currentUrl = window.location.pathname
    const pageTitle = document.title.replace(" - Erlang Tutorial", "")
    const scrollPosition = window.scrollY

    // Verificar se já existe um marcador para esta URL
    const existingIndex = bookmarks.findIndex((b) => b.url === currentUrl)

    if (existingIndex >= 0) {
      // Atualizar marcador existente
      const updated = [...bookmarks]
      updated[existingIndex] = {
        ...updated[existingIndex],
        position: scrollPosition,
        updatedAt: Date.now(),
      }
      setBookmarks(updated)
    } else {
      // Adicionar novo marcador
      const newBookmark: BookmarkItem = {
        id: Date.now().toString(),
        title: pageTitle,
        url: currentUrl,
        position: scrollPosition,
        notes: "",
        createdAt: Date.now(),
      }
      setBookmarks([...bookmarks, newBookmark])
    }

    setIsOpen(true)
  }

  const removeBookmark = (id: string) => {
    setBookmarks(bookmarks.filter((b) => b.id !== id))
  }

  const saveNote = (id: string) => {
    setBookmarks(bookmarks.map((b) => (b.id === id ? { ...b, notes: noteText } : b)))
    setEditingId(null)
  }

  const navigateToBookmark = (bookmark: BookmarkItem) => {
    if (window.location.pathname === bookmark.url) {
      // Já estamos na página, apenas rolar para a posição
      window.scrollTo({ top: bookmark.position, behavior: "smooth" })
    } else {
      // Navegar para a página e armazenar a posição para rolar após o carregamento
      sessionStorage.setItem("scrollToPosition", bookmark.position.toString())
      window.location.href = bookmark.url
    }
    setIsOpen(false)
  }

  // Verificar se há uma posição para rolar após o carregamento da página
  useEffect(() => {
    const scrollToPosition = sessionStorage.getItem("scrollToPosition")
    if (scrollToPosition) {
      window.scrollTo({ top: Number.parseInt(scrollToPosition), behavior: "smooth" })
      sessionStorage.removeItem("scrollToPosition")
    }
  }, [])

  return (
    <>
      <Button variant="ghost" size="icon" onClick={addBookmark} aria-label="Adicionar marcador" className="relative">
        <Bookmark className="h-5 w-5" />
        {bookmarks.length > 0 && (
          <span className="absolute -top-1 -right-1 bg-green-600 text-white text-xs rounded-full h-4 w-4 flex items-center justify-center">
            {bookmarks.length}
          </span>
        )}
      </Button>

      <Sheet open={isOpen} onOpenChange={setIsOpen}>
        <SheetTrigger asChild>
          <span className="hidden">Abrir marcadores</span>
        </SheetTrigger>
        <SheetContent>
          <SheetHeader>
            <SheetTitle>Seus Marcadores</SheetTitle>
          </SheetHeader>

          {bookmarks.length === 0 ? (
            <div className="py-8 text-center text-gray-500">
              <Bookmark className="h-12 w-12 mx-auto mb-2 opacity-30" />
              <p>Você não tem marcadores salvos.</p>
              <p className="text-sm mt-2">Clique no ícone de marcador para salvar sua posição atual.</p>
            </div>
          ) : (
            <div className="mt-6 space-y-4">
              {bookmarks.map((bookmark) => (
                <div key={bookmark.id} className="border rounded-md p-3">
                  <div className="flex justify-between items-start mb-2">
                    <button
                      onClick={() => navigateToBookmark(bookmark)}
                      className="text-left font-medium hover:text-green-600"
                    >
                      {bookmark.title}
                    </button>
                    <Button
                      variant="ghost"
                      size="icon"
                      onClick={() => removeBookmark(bookmark.id)}
                      aria-label="Remover marcador"
                    >
                      <Trash className="h-4 w-4 text-red-500" />
                    </Button>
                  </div>

                  {editingId === bookmark.id ? (
                    <div className="mt-2">
                      <Textarea
                        value={noteText}
                        onChange={(e) => setNoteText(e.target.value)}
                        placeholder="Adicione suas notas aqui..."
                        className="min-h-[100px] mb-2"
                      />
                      <div className="flex justify-end gap-2">
                        <Button variant="outline" size="sm" onClick={() => setEditingId(null)}>
                          Cancelar
                        </Button>
                        <Button
                          size="sm"
                          onClick={() => saveNote(bookmark.id)}
                          className="bg-green-600 hover:bg-green-700 text-white"
                        >
                          Salvar
                        </Button>
                      </div>
                    </div>
                  ) : (
                    <>
                      {bookmark.notes ? (
                        <div className="mt-2 text-sm bg-gray-50 p-2 rounded">
                          <p>{bookmark.notes}</p>
                          <Button
                            variant="ghost"
                            size="sm"
                            onClick={() => {
                              setEditingId(bookmark.id)
                              setNoteText(bookmark.notes)
                            }}
                            className="mt-1"
                          >
                            <Edit className="h-3 w-3 mr-1" />
                            Editar
                          </Button>
                        </div>
                      ) : (
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => {
                            setEditingId(bookmark.id)
                            setNoteText("")
                          }}
                          className="text-xs"
                        >
                          <Edit className="h-3 w-3 mr-1" />
                          Adicionar nota
                        </Button>
                      )}
                    </>
                  )}
                </div>
              ))}
            </div>
          )}
        </SheetContent>
      </Sheet>
    </>
  )
}
