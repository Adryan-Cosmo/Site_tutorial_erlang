import Link from "next/link"
import { ChevronRight, Home } from "lucide-react"

interface BreadcrumbsProps {
  items: {
    label: string
    href: string
  }[]
}

export default function Breadcrumbs({ items }: BreadcrumbsProps) {
  return (
    <nav aria-label="Breadcrumbs" className="flex items-center text-sm text-gray-500 mb-4">
      <Link href="/" className="flex items-center hover:text-green-600">
        <Home className="h-4 w-4 mr-1" />
        <span>Home</span>
      </Link>

      {items.map((item, index) => (
        <div key={index} className="flex items-center">
          <ChevronRight className="h-4 w-4 mx-2" />
          {index === items.length - 1 ? (
            <span className="font-medium text-gray-900">{item.label}</span>
          ) : (
            <Link href={item.href} className="hover:text-green-600">
              {item.label}
            </Link>
          )}
        </div>
      ))}
    </nav>
  )
}
