import Link from "next/link"
import { Search } from "lucide-react"
import { Button } from "@/components/ui/button"
import { Input } from "@/components/ui/input"

export default function TopMenu() {
  return (
    <div className="flex items-center justify-between p-2 bg-white border-b">
      <div className="flex items-center">
        <Link href="/" className="mr-4">
          <div className="flex items-center">
            <span className="text-green-600 font-bold text-2xl">W3</span>
            <span className="text-xs bg-green-600 text-white px-1 rounded-sm ml-1">3</span>
            <span className="text-xs ml-1">schools</span>
          </div>
        </Link>
        <div className="hidden md:flex space-x-2">
          <Link href="#" className="text-sm hover:bg-gray-100 px-2 py-1 rounded">
            Tutorials
          </Link>
          <Link href="#" className="text-sm hover:bg-gray-100 px-2 py-1 rounded">
            Exercises
          </Link>
          <Link href="#" className="text-sm hover:bg-gray-100 px-2 py-1 rounded">
            Certificates
          </Link>
          <Link href="#" className="text-sm hover:bg-gray-100 px-2 py-1 rounded">
            Services
          </Link>
        </div>
      </div>

      <div className="flex items-center space-x-2">
        <div className="relative hidden md:block">
          <Input type="text" placeholder="Search..." className="w-64 h-8 pl-2 pr-8 text-sm border rounded" />
          <Search className="absolute right-2 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
        </div>
        <Button variant="outline" size="sm" className="hidden md:block">
          <span className="text-sm">Plus</span>
        </Button>
        <Button variant="outline" size="sm" className="hidden md:block">
          <span className="text-sm">Spaces</span>
        </Button>
        <Button variant="outline" size="sm" className="hidden md:block">
          <span className="text-sm">For Teachers</span>
        </Button>
        <Button variant="outline" size="sm" className="hidden md:block">
          <span className="text-sm">Get Certified</span>
        </Button>
        <Button className="bg-green-600 hover:bg-green-700 text-white text-sm">Sign Up</Button>
        <Button variant="outline" size="sm">
          <span className="text-sm">Log in</span>
        </Button>
      </div>
    </div>
  )
}
