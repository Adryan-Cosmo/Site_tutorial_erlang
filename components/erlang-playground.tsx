"use client"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { Play, Copy, Download } from "lucide-react"

interface ErlangPlaygroundProps {
  initialCode?: string
  readOnly?: boolean
}

export default function ErlangPlayground({ initialCode = "", readOnly = false }: ErlangPlaygroundProps) {
  const [code, setCode] = useState(initialCode)
  const [output, setOutput] = useState("")
  const [isLoading, setIsLoading] = useState(false)

  const runCode = async () => {
    setIsLoading(true)
    setOutput("Executando código...")

    try {
      // Simulação - em produção, isso chamaria uma API real
      await new Promise((resolve) => setTimeout(resolve, 1000))

      // Exemplo de saída simulada
      if (code.includes("io:format")) {
        setOutput("Olá, Mundo Erlang!\nok")
      } else if (code.includes("lists:seq")) {
        setOutput("[1,2,3,4,5]")
      } else {
        setOutput("Código executado com sucesso.\nok")
      }
    } catch (error) {
      setOutput(`Erro: ${error}`)
    } finally {
      setIsLoading(false)
    }
  }

  const copyCode = () => {
    navigator.clipboard.writeText(code)
  }

  return (
    <div className="border rounded-lg overflow-hidden bg-white shadow-md mb-6">
      <div className="bg-gray-100 px-4 py-2 border-b flex justify-between items-center">
        <h3 className="font-medium">Playground Erlang</h3>
        <div className="flex gap-2">
          <Button variant="ghost" size="sm" onClick={copyCode} aria-label="Copiar código">
            <Copy className="h-4 w-4 mr-1" />
            <span className="hidden sm:inline">Copiar</span>
          </Button>
          <Button
            variant="ghost"
            size="sm"
            onClick={() => {
              const blob = new Blob([code], { type: "text/plain" })
              const url = URL.createObjectURL(blob)
              const a = document.createElement("a")
              a.href = url
              a.download = "erlang_code.erl"
              a.click()
            }}
            aria-label="Baixar código"
          >
            <Download className="h-4 w-4 mr-1" />
            <span className="hidden sm:inline">Baixar</span>
          </Button>
        </div>
      </div>

      <div className="flex flex-col md:flex-row">
        <div className="w-full md:w-1/2 border-r">
          <textarea
            value={code}
            onChange={(e) => setCode(e.target.value)}
            className="w-full h-64 p-4 font-mono text-sm focus:outline-none resize-none"
            placeholder="Digite seu código Erlang aqui..."
            readOnly={readOnly}
          />
        </div>

        <div className="w-full md:w-1/2 bg-gray-900 text-white">
          <div className="p-4 font-mono text-sm h-64 overflow-auto whitespace-pre">
            {output || "A saída aparecerá aqui..."}
          </div>
        </div>
      </div>

      <div className="bg-gray-100 px-4 py-2 border-t">
        <Button
          onClick={runCode}
          disabled={isLoading || !code.trim()}
          className="bg-green-600 hover:bg-green-700 text-white"
        >
          <Play className="h-4 w-4 mr-2" />
          {isLoading ? "Executando..." : "Executar"}
        </Button>
      </div>
    </div>
  )
}
