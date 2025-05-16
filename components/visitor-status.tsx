"use client"

import { useState, useEffect } from "react"
import { Wifi, WifiOff } from "lucide-react"

export default function VisitorStatus() {
  const [isOnline, setIsOnline] = useState(true)
  const [isLocalhost, setIsLocalhost] = useState(false)

  useEffect(() => {
    // Verifica se é localhost
    const hostname = window.location.hostname
    setIsLocalhost(hostname === "localhost" || hostname === "127.0.0.1")

    // Monitora o status de conexão
    const handleOnline = () => setIsOnline(true)
    const handleOffline = () => setIsOnline(false)

    window.addEventListener("online", handleOnline)
    window.addEventListener("offline", handleOffline)

    // Verifica a conexão com o servidor a cada 30 segundos
    const interval = setInterval(() => {
      fetch("/api/ping")
        .then(() => setIsOnline(true))
        .catch(() => setIsOnline(false))
    }, 30000)

    return () => {
      window.removeEventListener("online", handleOnline)
      window.removeEventListener("offline", handleOffline)
      clearInterval(interval)
    }
  }, [])

  // Não mostra nada para o administrador (localhost)
  if (isLocalhost) return null

  return (
    <div className="fixed bottom-4 left-4 bg-white shadow-lg rounded-lg p-2 border z-50">
      <div className="flex items-center gap-2 text-sm">
        {isOnline ? (
          <>
            <Wifi className="h-4 w-4 text-green-600" />
            <span>Conectado ao servidor</span>
          </>
        ) : (
          <>
            <WifiOff className="h-4 w-4 text-red-600" />
            <span>Desconectado do servidor</span>
          </>
        )}
      </div>
    </div>
  )
}
