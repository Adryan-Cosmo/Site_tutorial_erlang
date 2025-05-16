import Link from "next/link"
import Sidebar from "@/components/sidebar"
import { Button } from "@/components/ui/button"
import SolutionReveal from "@/components/solution-reveal"

export default function Chapter6() {
  return (
    <div className="flex">
      <Sidebar currentPage="chapter6" />
      <div className="flex-1 p-4 overflow-y-auto max-h-screen">
        <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

        <div className="flex justify-between mb-4">
          <Link href="/chapter5">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter7">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        <div className="bg-gray-100 p-6 mb-8">
          <h2 className="text-2xl font-bold mb-4">
            Capítulo 6: Comunicação entre Processos – A Orquestração de Mensagens
          </h2>
          <p className="italic mb-4">
            Domine a coreografia de processos que fazem sistemas distribuídos dançarem em harmonia!
          </p>

          {/* Conteúdo do capítulo... */}

          <div className="bg-yellow-100 p-4 mb-4 rounded">
            <p className="font-bold">Desafio do Capítulo 6:</p>
            <p className="mb-2">
              Implemente um <strong>sistema de leilão distribuído</strong> onde:
            </p>
            <ul className="list-disc pl-6 mb-4">
              <li>
                Um processo <strong>leiloeiro</strong> recebe lances.
              </li>
              <li>
                Processos <strong>participantes</strong> competem assincronamente.
              </li>
              <li>O leiloeiro anuncia o vencedor após um timeout.</li>
            </ul>

            <p className="font-bold mb-2">Dicas:</p>
            <ul className="list-disc pl-6 mb-4">
              <li>
                Use <code>erlang:start_timer/3</code> para limitar o tempo do leilão.
              </li>
              <li>Armazene o maior lance com o PID do participante.</li>
            </ul>
          </div>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
          <ul className="list-disc pl-6 mb-4">
            <li>
              <em>"Erlang and OTP in Action"</em> (Martin Logan et al.) – Capítulo 4.
            </li>
            <li>
              Documentação:{" "}
              <a href="https://www.erlang.org/doc/man/erlang.html#send-2" className="text-blue-600 hover:underline">
                erlang.org/doc/man/erlang.html#send-2
              </a>
            </li>
          </ul>

          <hr className="my-6" />

          <div className="border border-gray-300 p-4 rounded mb-4">
            <h4 className="text-lg font-bold mb-2">Box de Engenharia:</h4>
            <p className="font-bold">Por que Mensagens Assíncronas?</p>
            <ul className="list-disc pl-6 mb-2">
              <li>
                <strong>Desacoplamento:</strong> Remetentes não precisam esperar receptores.
              </li>
              <li>
                <strong>Falhas Isoladas:</strong> Se um processo morre, só suas mensagens pendentes são perdidas.
              </li>
              <li>
                <strong>Escalabilidade:</strong> Nós podem ser distribuídos em máquinas diferentes.
              </li>
            </ul>
          </div>

          <hr className="my-6" />

          <SolutionReveal>
            <h4 className="text-lg font-bold mb-2">Exemplo de Solução do Desafio (Leilão):</h4>
            <pre className="bg-gray-800 text-white p-4 rounded mb-4">
              {`-module(leilao).
-export([iniciar/1, participar/1, oferecer/2]).

iniciar(TempoMs) ->
    spawn(fun() -> leiloeiro(TempoMs, none, 0) end).

leiloeiro(TempoMs, MelhorPid, MaiorLance) ->
    Timer = erlang:start_timer(TempoMs, self(), fim),
    loop_leiloeiro(Timer, MelhorPid, MaiorLance).

loop_leiloeiro(Timer, MelhorPid, MaiorLance) ->
    receive
        {lance, Pid, Valor} when Valor > MaiorLance ->
            Pid ! {atual, Valor},
            loop_leiloeiro(Timer, Pid, Valor);
        {timeout, Timer, fim} ->
            MelhorPid ! {vencedor, MaiorLance},
            io:format("Leilão encerrado. Vencedor: ~p com ~p~n", [MelhorPid, MaiorLance])
    end.`}
            </pre>
          </SolutionReveal>
        </div>

        <div className="flex justify-between">
          <Link href="/chapter5">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter7">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        
      </div>
    </div>
  )
}
