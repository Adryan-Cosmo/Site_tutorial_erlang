import Link from "next/link"

export default function Navbar() {
  const navItems = [
    "HTML",
    "CSS",
    "JAVASCRIPT",
    "SQL",
    "PYTHON",
    "JAVA",
    "PHP",
    "HOW TO",
    "W3.CSS",
    "C",
    "C++",
    "C#",
    "BOOTSTRAP",
    "REACT",
    "MYSQL",
    "JQUERY",
    "EXCEL",
    "XML",
    "DJANGO",
    "NUMPY",
    "PANDAS",
    "NODE.JS",
  ]

  return (
    <nav className="bg-gray-800 text-white overflow-x-auto whitespace-nowrap">
      <div className="flex">
        {navItems.map((item, index) => (
          <Link key={index} href="#" className="px-4 py-2 hover:bg-green-600 transition-colors duration-200 text-sm">
            {item}
          </Link>
        ))}
      </div>
    </nav>
  )
}
