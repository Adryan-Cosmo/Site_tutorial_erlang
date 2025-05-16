"use client"

import { useState, useEffect } from "react"
import { ChevronUp, ChevronDown } from "lucide-react"
import { Button } from "@/components/ui/button"
import Link from "next/link"

interface FloatingNavProps {
  prevHref?: string
  nextHref?: string
  currentChapter: string
}

export default function FloatingNav({ prevHref, nextHref, currentChapter }: FloatingNavProps) {
  const [visible, setVisible] = useState(false)

  useEffect(() => {
    const handleScroll = () => {
      // Mostra a navegação flutuante após rolar 300px
      setVisible(window.scrollY > 300)
    }

    window.addEventListener("scroll", handleScroll)
    return () => window.removeEventListener("scroll", handleScroll)
  }, [])

  if (!visible) return null

  return (
    <div className="fixed bottom-6 right-6 flex flex-col gap-2 z-40">
      <Button
        onClick={() => window.scrollTo({ top: 0, behavior: "smooth" })}
        size="icon"
        className="rounded-full bg-green-600 hover:bg-green-700 text-white shadow-lg"
        aria-label="Voltar ao topo"
      >
        <ChevronUp className="h-5 w-5" />
      </Button>

      <div className="flex gap-2">
        {prevHref && (
          <Link href={prevHref}>
            <Button
              variant="outline"
              size="icon"
              className="rounded-full bg-white shadow-lg"
              aria-label="Capítulo anterior"
            >
              <ChevronUp className="h-5 w-5 rotate-90" />
            </Button>
          </Link>
        )}
        {nextHref && (
          <Link href={nextHref}>
            <Button
              variant="outline"
              size="icon"
              className="rounded-full bg-white shadow-lg"
              aria-label="Próximo capítulo"
            >
              <ChevronDown className="h-5 w-5 rotate-90" />
            </Button>
          </Link>
        )}
      </div>
    </div>
  )
}
