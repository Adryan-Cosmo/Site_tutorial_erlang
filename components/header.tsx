"use client"

import { useState, useEffect, useRef } from "react"
import { Search, X } from "lucide-react"
import { Input } from "@/components/ui/input"
import Link from "next/link"

// Dados para busca
const searchData = [
  { title: "Introdução à Erlang", path: "/" },
  { title: "Ambiente de Desenvolvimento", path: "/chapter2" },
  { title: "Sintaxe Básica", path: "/chapter3" },
  { title: "Programação Funcional", path: "/chapter4" },
  { title: "Concorrência e Processos", path: "/chapter5" },
  { title: "Comunicação entre Processos", path: "/chapter6" },
  { title: "Sistemas Distribuídos", path: "/chapter7" },
  { title: "Projeto Cliente/Servidor", path: "/chapter8" },
  { title: "Projeto Produtor/Consumidor", path: "/chapter9" },
  { title: "Boas Práticas e Padrões OTP", path: "/chapter10" },
]

export default function Header() {
  const [isOpen, setIsOpen] = useState(false)
  const [query, setQuery] = useState("")
  const [results, setResults] = useState<typeof searchData>([])
  const searchRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    const handleClickOutside = (event: MouseEvent) => {
      if (searchRef.current && !searchRef.current.contains(event.target as Node)) {
        setIsOpen(false)
      }
    }

    document.addEventListener("mousedown", handleClickOutside)
    return () => document.removeEventListener("mousedown", handleClickOutside)
  }, [])

  useEffect(() => {
    if (query.trim()) {
      const filtered = searchData.filter((item) => item.title.toLowerCase().includes(query.toLowerCase()))
      setResults(filtered)
    } else {
      setResults([])
    }
  }, [query])

  return (
    <header className="bg-white shadow-sm border-b">
      <div className="container mx-auto px-4 py-4 flex flex-col md:flex-row md:items-center md:justify-between">
        <h1 className="text-2xl font-bold text-gray-800 mb-4 md:mb-0">
          <Link href="/">Sistemas Distribuídos</Link>
        </h1>

        <div className="relative w-full md:w-64 lg:w-96" ref={searchRef}>
          <div className="relative">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
            <Input
              type="text"
              placeholder="Buscar capítulos..."
              className="w-full pl-10 pr-10 h-10"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
              onFocus={() => setIsOpen(true)}
            />
            {query && (
              <button
                className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-400 hover:text-gray-600"
                onClick={() => setQuery("")}
                aria-label="Limpar busca"
              >
                <X className="h-4 w-4" />
              </button>
            )}
          </div>

          {isOpen && results.length > 0 && (
            <div className="absolute top-full left-0 right-0 mt-1 bg-white border rounded-md shadow-lg z-50 max-h-80 overflow-y-auto">
              <ul>
                {results.map((result, index) => (
                  <li key={index} className="border-b last:border-0">
                    <Link
                      href={result.path}
                      className="block px-4 py-2 hover:bg-gray-100"
                      onClick={() => setIsOpen(false)}
                    >
                      {result.title}
                    </Link>
                  </li>
                ))}
              </ul>
            </div>
          )}

          {isOpen && query && results.length === 0 && (
            <div className="absolute top-full left-0 right-0 mt-1 bg-white border rounded-md shadow-lg z-50 p-4 text-center text-gray-500">
              Nenhum resultado encontrado para "{query}"
            </div>
          )}
        </div>
      </div>
    </header>
  )
}
