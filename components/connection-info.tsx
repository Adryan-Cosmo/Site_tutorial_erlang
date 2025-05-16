"use client"

import { useState, useEffect } from "react"
import { Button } from "@/components/ui/button"
import { Copy, Server, Users, Wifi } from "lucide-react"

export default function ConnectionInfo() {
  const [localIP, setLocalIP] = useState<string | null>(null)
  const [port, setPort] = useState<string>("3000")
  const [isAdmin, setIsAdmin] = useState(false)
  const [copied, setCopied] = useState(false)

  useEffect(() => {
    // Verifica se é o administrador (quem está rodando o servidor)
    const checkIfAdmin = () => {
      const isLocalhost = window.location.hostname === "localhost" || window.location.hostname === "127.0.0.1"
      setIsAdmin(isLocalhost)
    }

    // Tenta obter o IP do servidor a partir da URL atual
    const getServerInfo = () => {
      const hostname = window.location.hostname
      const currentPort = window.location.port || "3000"

      if (hostname !== "localhost" && hostname !== "127.0.0.1") {
        setLocalIP(hostname)
      }

      setPort(currentPort)
    }

    checkIfAdmin()
    getServerInfo()

    // Se for admin, tenta obter o IP local via API
    if (isAdmin && !localIP) {
      fetch("/api/get-ip")
        .then((res) => res.json())
        .then((data) => {
          if (data.ip) {
            setLocalIP(data.ip)
          }
        })
        .catch((err) => {
          console.error("Erro ao obter IP:", err)
        })
    }
  }, [isAdmin, localIP])

  const copyToClipboard = () => {
    if (localIP) {
      const url = `http://${localIP}:${port}`
      navigator.clipboard.writeText(url)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    }
  }

  if (!isAdmin) return null

  return (
    <div className="fixed bottom-4 right-4 bg-white shadow-lg rounded-lg p-4 border z-50 max-w-md">
      <div className="flex items-center gap-2 mb-2 text-green-600">
        <Server className="h-5 w-5" />
        <h3 className="font-medium">Informações do Servidor</h3>
      </div>

      <div className="space-y-2 text-sm">
        <div className="flex items-center gap-2">
          <Wifi className="h-4 w-4 text-gray-500" />
          <span>
            Status: <span className="text-green-600 font-medium">Online</span>
          </span>
        </div>

        {localIP && (
          <div className="flex items-center gap-2">
            <Users className="h-4 w-4 text-gray-500" />
            <span>Compartilhe este link:</span>
            <code className="bg-gray-100 px-2 py-1 rounded text-xs">
              http://{localIP}:{port}
            </code>
            <Button variant="ghost" size="icon" className="h-6 w-6" onClick={copyToClipboard} title="Copiar link">
              <Copy className="h-3 w-3" />
            </Button>
            {copied && <span className="text-xs text-green-600">Copiado!</span>}
          </div>
        )}

        <p className="text-xs text-gray-500">Outras pessoas na mesma rede podem acessar o tutorial usando este link.</p>
      </div>
    </div>
  )
}
