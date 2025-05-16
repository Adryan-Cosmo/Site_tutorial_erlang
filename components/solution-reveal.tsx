"use client"

import type React from "react"

import { useState } from "react"
import { Button } from "@/components/ui/button"
import { ChevronDown, ChevronUp } from "lucide-react"

interface SolutionRevealProps {
  children: React.ReactNode
  title?: string
}

export default function SolutionReveal({ children, title = "Exemplo de Solução do Desafio" }: SolutionRevealProps) {
  const [isVisible, setIsVisible] = useState(false)

  return (
    <div className="mt-4">
      <Button
        onClick={() => setIsVisible(!isVisible)}
        variant="outline"
        className="w-full flex justify-between items-center bg-green-50 hover:bg-green-100 border-green-200"
      >
        <span className="font-bold">{title}</span>
        {isVisible ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />}
      </Button>

      {isVisible && <div className="bg-green-100 p-4 rounded mt-2 transition-all duration-300">{children}</div>}
    </div>
  )
}
