"use client"

import { useState, useEffect, useRef } from "react"
import { SearchIcon, X } from "lucide-react"
import { Input } from "@/components/ui/input"
import { Button } from "@/components/ui/button"
import Link from "next/link"

// Dados simulados para busca
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

export default function Search() {
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
    <div className="relative" ref={searchRef}>
      <div className="flex items-center">
        <div className="relative w-full">
          <SearchIcon className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
          <Input
            type="text"
            placeholder="Buscar..."
            className="w-full pl-10 pr-10 h-10"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            onFocus={() => setIsOpen(true)}
          />
          {query && (
            <Button
              variant="ghost"
              size="icon"
              className="absolute right-1 top-1/2 transform -translate-y-1/2 h-8 w-8"
              onClick={() => setQuery("")}
            >
              <X className="h-4 w-4" />
            </Button>
          )}
        </div>
      </div>

      {isOpen && results.length > 0 && (
        <div className="absolute top-full left-0 right-0 mt-1 bg-white border rounded-md shadow-lg z-50 max-h-80 overflow-y-auto">
          <ul>
            {results.map((result, index) => (
              <li key={index} className="border-b last:border-0">
                <Link href={result.path} className="block px-4 py-2 hover:bg-gray-100" onClick={() => setIsOpen(false)}>
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
  )
}
