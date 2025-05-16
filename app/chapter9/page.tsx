import Link from "next/link"
import Sidebar from "@/components/sidebar"
import { Button } from "@/components/ui/button"
import SolutionReveal from "@/components/solution-reveal"

export default function Chapter9() {
  return (
    <div className="flex">
      <Sidebar currentPage="chapter9" />
      <div className="flex-1 p-4 overflow-y-auto max-h-screen">
        <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

        <div className="flex justify-between mb-4">
          <Link href="/chapter8">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter10">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        <div className="bg-gray-100 p-6 mb-8">
          <h2 className="text-2xl font-bold mb-4">
            Capítulo 9: Projeto 2 - Arquitetura Produtor/Consumidor com Backpressure
          </h2>
          <p className="italic mb-4">
            Construa um pipeline de dados distribuído que automaticamente regula fluxo, balanceia carga e sobrevive a
            falhas catastróficas!
          </p>

          {/* Conteúdo do capítulo... */}

          <div className="bg-yellow-100 p-4 mb-4 rounded">
            <p className="font-bold">Desafio do Capítulo 9:</p>
            <p className="mb-2">
              Implemente <strong>prioridade múltipla</strong> no buffer:
            </p>
            <ul className="list-disc pl-6 mb-4">
              <li>High-priority: Logs de erro (processamento imediato)</li>
              <li>Low-priority: Logs de debug (processamento quando ocioso)</li>
            </ul>

            <p className="font-bold mb-2">Dicas:</p>
            <ol className="list-decimal pl-6 mb-4">
              <li>
                Use duas filas internas no <code>circular_buffer</code>
              </li>
              <li>
                Modifique o protocolo para incluir <code>{"{priority, high|low}"}</code>
              </li>
              <li>
                Implemente <code>pop/2</code> no GenServer para consumir preferencialmente
              </li>
            </ol>
          </div>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
          <ul className="list-disc pl-6 mb-4">
            <li>
              <em>"Designing for Scalability with Erlang/OTP"</em> - Cap. 7 (Padrões de Fluxo de Dados)
            </li>
            <li>
              <strong>Bibliotecas Recomendadas:</strong>
              <ul className="list-disc pl-6">
                <li>
                  <code>gen_batch</code>: Para processamento em lote
                </li>
                <li>
                  <code>jobs</code>: Gerenciamento de filas com prioridade
                </li>
                <li>
                  <code>recon</code>: Monitoramento em produção
                </li>
              </ul>
            </li>
          </ul>

          <hr className="my-6" />

          <div className="border border-gray-300 p-4 rounded mb-4">
            <h4 className="text-lg font-bold mb-2">Box de Arquitetura:</h4>
            <p className="font-bold">Por que este Design Escala para Níveis Web-Scale?</p>
            <ol className="list-decimal pl-6 mb-2">
              <li>
                <strong>Backpressure Nativo:</strong>
                <ul className="list-disc pl-6">
                  <li>Produtores automaticamente reduzem taxa quando buffers estão cheios</li>
                  <li>Evite o efeito "fan-in" que derruba sistemas tradicionais</li>
                </ul>
              </li>
              <li>
                <strong>Elasticidade Dinâmica:</strong>
                <ul className="list-disc pl-6">
                  <li>Pool de consumidores expande/contrai baseado em métricas em tempo real</li>
                </ul>
              </li>
              <li>
                <strong>Falhas Isoladas:</strong>
                <ul className="list-disc pl-6">
                  <li>Cada produtor/consumidor é supervisionado independentemente</li>
                  <li>Buffer atua como amortecedor durante recuperações</li>
                </ul>
              </li>
            </ol>
          </div>

          <hr className="my-6" />

          <SolutionReveal>
            <h4 className="text-lg font-bold mb-2">Exemplo de Solução do Desafio (Buffer com Prioridade):</h4>
            <pre className="bg-gray-800 text-white p-4 rounded mb-4">
              {`handle_call({push, Priority, Item}, _From, State) ->
    QKey = case Priority of
        high -> high_prio_q;
        _ -> low_prio_q
    end,
    {reply, ok, maps:update_with(QKey, fun(Q) -> queue:in(Item, Q) end, State)};

handle_call(pop, _From, State) ->
    case queue:out(State.high_prio_q) of
        {{value, Item}, NewQ} -> 
            {reply, {ok, Item}, State#{high_prio_q => NewQ}};
        {empty, _} -> 
            case queue:out(State.low_prio_q) of
                {{value, Item}, NewQ} -> 
                    {reply, {ok, Item}, State#{low_prio_q => NewQ}};
                {empty, _} -> 
                    {reply, empty, State}
            end
    end.`}
            </pre>
          </SolutionReveal>
        </div>

        <div className="flex justify-between">
          <Link href="/chapter8">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter10">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        
      </div>
    </div>
  )
}
