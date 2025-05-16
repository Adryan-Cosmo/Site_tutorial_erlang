import Link from "next/link"
import Sidebar from "@/components/sidebar"
import { Button } from "@/components/ui/button"
import SolutionReveal from "@/components/solution-reveal"

export default function Chapter7() {
  return (
    <div className="flex">
      <Sidebar currentPage="chapter7" />
      <div className="flex-1 p-4 overflow-y-auto max-h-screen">
        <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

        <div className="flex justify-between mb-4">
          <Link href="/chapter6">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter8">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        <div className="bg-gray-100 p-6 mb-8">
          <h2 className="text-2xl font-bold mb-4">Capítulo 7: Sistemas Distribuídos e Tolerância a Falhas</h2>
          <p className="italic mb-4">
            Construa sistemas que sobrevivem a falhas de rede, nós travados e até mesmo a apocalipses zumbis!
          </p>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Arquitetura de Sistemas Distribuídos em Erlang</h3>
          <p className="mb-4">
            Erlang foi projetada para operar em <strong>clusters de máquinas</strong> onde falhas são normais, não
            exceções. Seu segredo está em três princípios:
          </p>

          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>Transparência de Localização:</strong> Processos se comunicam da mesma forma, estejam no mesmo
              núcleo ou em outro continente.
            </li>
            <li>
              <strong>Falhas Parciais:</strong> Um nó pode cair sem derrubar todo o sistema.
            </li>
            <li>
              <strong>Recuperação Automática:</strong> "Quebre, conserte rápido, continue" é o lema.
            </li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Como Funciona na Prática?</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`% Conectando nós em um cluster:
1> net_kernel:start([n1@localhost, shortnames]).
2> net_adm:ping('n2@localhost').
pong  % Conexão estabelecida!`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Exemplo de Comunicação entre Nós:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`% No nó 1:
-module(servidor_global).
-export([iniciar/0]).
iniciar() ->
    register(servidor, self()),
    loop().

loop() ->
    receive
        {msg, De, Conteudo} ->
            io:format("Recebido de ~p: ~p~n", [De, Conteudo]),
            loop()
    end.

% No nó 2:
{servidor, 'n1@localhost'} ! {msg, node(), "Alô, mundo distribuído!"}.`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Hot Code Swapping: Atualizações em Tempo Real</h3>
          <p className="mb-4">
            Atualize sistemas em produção <strong>sem downtime</strong>, como trocar o motor de um avião em pleno voo!
          </p>

          <h4 className="text-lg font-bold mb-2">Mecanismo por Trás:</h4>
          <ol className="list-decimal pl-6 mb-4">
            <li>Duas versões do módulo são carregadas na VM (BEAM).</li>
            <li>Chamadas antigas continuam na versão anterior até concluírem.</li>
            <li>Novas chamadas usam a versão atualizada.</li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Exemplo Prático:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`% Versão 1.0:
-module(servico).
-export([iniciar/0, loop/0]).
iniciar() -> spawn(?MODULE, loop, []).
loop() ->
    receive
        chamada -> io:format("Versão 1.0~n"), loop()
    end.

% Versão 2.0 (atualização):
loop() ->
    receive
        chamada -> io:format("Versão 2.0 melhorada!~n"), loop()
    end.

% No shell:
1> c("servico.erl", [{outdir, "./ebin"}]).  % Compila a nova versão
2> code:load_file(servico).  % Carrega sem reiniciar`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Quando Usar:</h4>
          <ul className="list-disc pl-6 mb-4">
            <li>Correções críticas em sistemas 24/7.</li>
            <li>Adição de features em tempo real.</li>
          </ul>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Árvores de Supervisão (OTP): O Exército de Backup</h3>
          <p className="mb-4">
            A OTP fornece um modelo para <strong>gerenciamento de falhas</strong> através de hierarquias:
          </p>

          <h4 className="text-lg font-bold mb-2">Componentes-Chave:</h4>
          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>Supervisor:</strong> Monitora processos filhos e decide reiniciá-los ou não.
            </li>
            <li>
              <strong>Child Specs:</strong> Define como cada processo filho deve ser iniciado.
            </li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Exemplo de Implementação:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(meu_supervisor).
-behaviour(supervisor).  % Implementa o comportamento supervisor

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % Configuração da estratégia de supervisão
    SupFlags = #{
        strategy => one_for_one,  % Reinicia apenas o processo falho
        intensity => 3,           % Máximo de 3 reinícios em 5 segundos
        period => 5
    },

    % Especificações dos filhos
    ChildSpecs = [
        #{
            id => servico_critico,
            start => {servico_critico, start_link, []},
            restart => permanent,  % Sempre reinicia
            shutdown => 2000       % Tempo para shutdown
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Estratégias de Reinício:</h4>
          <div className="overflow-x-auto mb-4">
            <table className="min-w-full border border-gray-300">
              <thead>
                <tr className="bg-gray-200">
                  <th className="border border-gray-300 p-2">Estratégia</th>
                  <th className="border border-gray-300 p-2">Comportamento</th>
                  <th className="border border-gray-300 p-2">Caso de Uso</th>
                </tr>
              </thead>
              <tbody>
                <tr>
                  <td className="border border-gray-300 p-2">
                    <code>one_for_one</code>
                  </td>
                  <td className="border border-gray-300 p-2">Reinicia apenas o processo falho</td>
                  <td className="border border-gray-300 p-2">Serviços independentes</td>
                </tr>
                <tr>
                  <td className="border border-gray-300 p-2">
                    <code>one_for_all</code>
                  </td>
                  <td className="border border-gray-300 p-2">Reinicia todos os filhos</td>
                  <td className="border border-gray-300 p-2">Dependências fortes</td>
                </tr>
                <tr>
                  <td className="border border-gray-300 p-2">
                    <code>rest_for_one</code>
                  </td>
                  <td className="border border-gray-300 p-2">Reinicia o falho e os subsequentes</td>
                  <td className="border border-gray-300 p-2">Cadeias de processos</td>
                </tr>
              </tbody>
            </table>
          </div>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Exemplo Prático: Sistema de Monitoramento com Heartbeats</h3>
          <p className="mb-4">Vamos construir um sistema que:</p>
          <ol className="list-decimal pl-6 mb-4">
            <li>Monitora nós vizinhos via pacotes "heartbeat".</li>
            <li>Isola nós inativos automaticamente.</li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Implementação Completa:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(monitor).
-export([iniciar/1, loop/1]).

% Inicia o monitor com lista de nós
iniciar(Nos) ->
    spawn_link(fun() -> loop(Nos) end).

loop(Nos) ->
    lists:foreach(
        fun(Nó) ->
            case net_adm:ping(Nó) of
                pong -> 
                    io:format("Nó ~p ativo.~n", [Nó]);
                pang ->
                    io:format("ALERTA: Nó ~p inativo!~n", [Nó]),
                    % Ação de contingência (ex: redistribuir carga)
                    contingencia(Nó)
            end
        end,
        Nos
    ),
    timer:sleep(5000),  % Verifica a cada 5 segundos
    loop(Nos).

contingencia(Nó) ->
    % Exemplo: Redirecionar processos para outro nó
    case Nó of
        'n2@localhost' -> 'n3@localhost';
        _ -> primary_node()
    end.`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Testando a Resiliência:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`1> monitor:iniciar(['n2@localhost', 'n3@localhost']).
Nó 'n2@localhost' ativo.
Nó 'n3@localhost' ativo.
(...)
% Simule uma falha (kill no processo do nó 2)
ALERTA: Nó 'n2@localhost' inativo!`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Fault Injection: Testando a Resiliência</h3>
          <p className="mb-4">
            Erlang oferece ferramentas para <strong>simular falhas</strong> e validar recuperação:
          </p>

          <h4 className="text-lg font-bold mb-2">1. Matando Processos Aleatoriamente</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`% No shell:
1> Pid = spawn(fun() -> receive _ -> ok end end).
2> exit(Pid, kill).  % Força término imediato`}
          </pre>

          <h4 className="text-lg font-bold mb-2">2. Particionamento de Rede Simulado</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`% Em um nó:
1> erlang:set_cookie(node(), outra_chave).  % Quebra autenticação
2> net_kernel:stop().  % Desconecta do cluster`}
          </pre>

          <hr className="my-6" />

          <div className="bg-yellow-100 p-4 mb-4 rounded">
            <p className="font-bold">Desafio do Capítulo 7:</p>
            <p className="mb-2">
              Implemente um <strong>servidor de arquivos distribuído</strong> com:
            </p>
            <ul className="list-disc pl-6 mb-4">
              <li>
                <strong>Replicação:</strong> Cada arquivo é armazenado em 2 nós diferentes.
              </li>
              <li>
                <strong>Consistência:</strong> Se um nó falhar, o outro deve assumir.
              </li>
              <li>
                <strong>Interface:</strong>
                <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-1">
                  {`escrever_arquivo(Nome, Dados) -> ok | {erro, motivo}.
ler_arquivo(Nome) -> {ok, Dados} | {erro, não_encontrado}.`}
                </pre>
              </li>
            </ul>

            <p className="font-bold mb-2">Dicas:</p>
            <ul className="list-disc pl-6 mb-4">
              <li>
                Use <code>gen_server</code> (será detalhado no Capítulo 10).
              </li>
              <li>Implemente heartbeat entre nós para detectar falhas.</li>
            </ul>
          </div>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
          <ul className="list-disc pl-6 mb-4">
            <li>
              <em>"Designing for Scalability with Erlang/OTP"</em> (Francesco Cesarini) – Cap. 6
            </li>
            <li>
              Documentação:{" "}
              <a
                href="https://www.erlang.org/doc/design_principles/distributed_applications"
                className="text-blue-600 hover:underline"
              >
                erlang.org/doc/design_principles/distributed_applications
              </a>
            </li>
          </ul>

          <hr className="my-6" />

          <div className="border border-gray-300 p-4 rounded mb-4">
            <h4 className="text-lg font-bold mb-2">Box de Arquitetura:</h4>
            <p className="font-bold">Por que Erlang Vence em Sistemas Distribuídos?</p>
            <ol className="list-decimal pl-6 mb-2">
              <li>
                <strong>Abstração de Rede:</strong> Processos não sabem se estão local ou remotos.
              </li>
              <li>
                <strong>Tolerância a Partições:</strong> CAP theorem? Erlang escolhe{" "}
                <strong>Disponibilidade + Tolerância a Falhas</strong>.
              </li>
              <li>
                <strong>Protocolo de Consenso:</strong> Bibliotecas como <code>pg2</code> (Process Groups) sincronizam
                estados automaticamente.
              </li>
            </ol>
          </div>

          <hr className="my-6" />

          <SolutionReveal>
            <h4 className="text-lg font-bold mb-2">Exemplo de Solução do Desafio (Servidor de Arquivos):</h4>
            <pre className="bg-gray-800 text-white p-4 rounded mb-4">
              {`-module(dfs).
-behaviour(gen_server).
-export([escrever/2, ler/1]).
-export([init/1, handle_call/3]).

escrever(Nome, Dados) ->
    gen_server:call({global, dfs_coordinator}, {escrever, Nome, Dados}).

ler(Nome) ->
    gen_server:call({global, dfs_coordinator}, {ler, Nome}).

% Implementação do gen_server omitida por brevidade
% (Detalhada no Capítulo 10)`}
            </pre>
          </SolutionReveal>
        </div>

        <div className="flex justify-between">
          <Link href="/chapter6">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              ← Previous
            </Button>
          </Link>
          <Link href="/chapter8">
            <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
              Next →
            </Button>
          </Link>
        </div>

        
      </div>
    </div>
  )
}
