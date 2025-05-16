import Link from "next/link"

interface SidebarProps {
  currentPage?: string
}

export default function Sidebar({ currentPage = "chapter1" }: SidebarProps) {
  const sidebarItems = [
    { id: "chapter1", title: "ERLANG HOME", href: "/" },
    { id: "chapter1", title: "Capítulo 1: Introdução", href: "/" },
    { id: "chapter2", title: "Capítulo 2: Ambiente de Desenvolvimento", href: "/chapter2" },
    { id: "chapter3", title: "Capítulo 3: Sintaxe Básica", href: "/chapter3" },
    { id: "chapter4", title: "Capítulo 4: Programação Funcional", href: "/chapter4" },
    { id: "chapter5", title: "Capítulo 5: Concorrência e Processos", href: "/chapter5" },
    { id: "chapter6", title: "Capítulo 6: Comunicação entre Processos", href: "/chapter6" },
    { id: "chapter7", title: "Capítulo 7: Sistemas Distribuídos", href: "/chapter7" },
    { id: "chapter8", title: "Capítulo 8: Projeto 1 - Sistema Cliente/Servidor", href: "/chapter8" },
    { id: "chapter9", title: "Capítulo 9: Projeto 2 - Produtor/Consumidor", href: "/chapter9" },
    { id: "chapter10", title: "Capítulo 10: Boas Práticas e Padrões OTP", href: "/chapter10" },
  ]

  return (
    <div className="w-64 bg-gray-100 min-h-screen">
      <div className="p-4 bg-gray-200">
        <h2 className="font-bold">Erlang Tutorial</h2>
      </div>
      <div className="overflow-y-auto h-full">
        {sidebarItems.map((item, index) => (
          <Link
            key={index}
            href={item.href}
            className={`block px-4 py-2 border-b text-sm ${
              item.id === currentPage ? "bg-green-600 text-white" : "hover:bg-gray-200"
            }`}
          >
            {item.title}
          </Link>
        ))}
      </div>
    </div>
  )
}
