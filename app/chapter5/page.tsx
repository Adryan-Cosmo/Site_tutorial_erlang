import Link from "next/link"
import Sidebar from "@/components/sidebar"
import { Button } from "@/components/ui/button"
import SolutionReveal from "@/components/solution-reveal"

export default function Chapter5() {
  return (
    <div className="flex">
      <Sidebar currentPage="chapter5" />
      <div className="flex-1 p-4 overflow-y-auto max-h-screen">
        <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

        <div className="flex justify-between mb-4">
          <Link href="/chapter4">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter6">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        <div className="bg-gray-100 p-6 mb-8">
          <h2 className="text-2xl font-bold mb-4">Capítulo 5: Concorrência e Processos Leves</h2>
          <p className="italic mb-4">
            Onde seu código ganha asas! Descubra como Erlang roda milhões de tarefas em paralelo sem suar a camisa.
          </p>

          {/* Conteúdo do capítulo... */}

          <div className="bg-yellow-100 p-4 mb-4 rounded">
            <p className="font-bold">Desafio do Capítulo 5:</p>
            <ul className="list-disc pl-6">
              <li>
                Crie um processo <code>contador</code> que armazene um número.
              </li>
              <li>
                Aceite mensagens <code>{"{incrementar, N}"}</code> para somar ao valor.
              </li>
              <li>
                Responda a <code>{"{get, From}"}</code> com o valor atual.
              </li>
              <li>Dica: Use recursão para manter o estado.</li>
            </ul>
          </div>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
          <ul className="list-disc pl-6 mb-4">
            <li>
              Documentação:{" "}
              <a href="https://www.erlang.org/doc/reference_manual/processes" className="text-blue-600 hover:underline">
                erlang.org/doc/reference_manual/processes
              </a>
            </li>
            <li>
              Livro: <em>"Designing for Scalability with Erlang/OTP"</em> (Francesco Cesarini e Steve Vinoski)
            </li>
          </ul>

          <hr className="my-6" />

          <div className="border border-gray-300 p-4 rounded mb-4">
            <h4 className="text-lg font-bold mb-2">Box de Destaque (Para Arquitetos de Sistemas):</h4>
            <p className="font-bold">Por que Processos Leves São Revolucionários?</p>
            <ul className="list-disc pl-6 mb-2">
              <li>
                <strong>Elasticidade:</strong> Adicione mais processos conforme a demanda cresce.
              </li>
              <li>
                <strong>Tolerância a Falhas Nativa:</strong> Um processo crashado não derruba o sistema.
              </li>
              <li>
                <strong>Simplicidade:</strong> Código concorrente sem <em>locks</em> ou semáforos complexos.
              </li>
            </ul>
            <p className="italic">[➔ Capítulo 6: Comunicação entre Processos – Domine a Arte das Mensagens!]</p>
          </div>

          <hr className="my-6" />

          <SolutionReveal>
            <pre className="bg-gray-800 text-white p-4 rounded mb-4">
              {`-module(contador).
-export([iniciar/0, loop/1]).

iniciar() ->
    spawn(?MODULE, loop, [0]).

loop(Valor) ->
    receive
        {incrementar, N, From} ->
            From ! {ok, Valor + N},
            loop(Valor + N);
        {get, From} ->
            From ! {ok, Valor},
            loop(Valor)
    end.

% Uso:
1> Contador = contador:iniciar().
<0.135.0>
2> Contador ! {incrementar, 5, self()}.
{ok, 5}
3> Contador ! {get, self()}.
{ok, 5}`}
            </pre>
          </SolutionReveal>
        </div>

        <div className="flex justify-between">
          <Link href="/chapter4">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter6">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        
      </div>
    </div>
  )
}
