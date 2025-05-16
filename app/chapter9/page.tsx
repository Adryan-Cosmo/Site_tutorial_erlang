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

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Visão Geral do Sistema</h3>
          <p className="mb-4">
            Vamos criar um <strong>sistema de processamento de logs</strong> com:
          </p>
          <ul className="list-disc pl-6 mb-4">
            <li>
              <strong>Produtores</strong> que ingerem dados de múltiplas fontes (Arquivos, TCP, HTTP)
            </li>
            <li>
              <strong>Buffer inteligente</strong> com backpressure (usando filas OTP)
            </li>
            <li>
              <strong>Consumidores dinâmicos</strong> que escalam horizontalmente
            </li>
            <li>
              <strong>Supervisão em árvore</strong> com estratégias de recuperação específicas
            </li>
          </ul>

          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`Árvore de Componentes:
log_pipeline_sup (rest_for_one)
├── buffer_manager (gen_server)
├── producer_sup (simple_one_for_one)
│   ├── file_producer_1
│   └── tcp_producer_1
└── consumer_sup (simple_one_for_one)
    ├── consumer_1
    └── consumer_2`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Passo 1: Modelando o Buffer com Backpressure</h3>
          <h4 className="text-lg font-bold mb-2">
            Módulo <code>circular_buffer.erl</code> (Fila Circular com Semântica de Bloqueio)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(circular_buffer).
-behaviour(gen_server).
-export([push/2, pop/1, start_link/1]).

%% Interface Pública
push(BufferPid, Item) ->
    gen_server:call(BufferPid, {push, Item}, 5000). % Timeout de 5s

pop(BufferPid) ->
    gen_server:call(BufferPid, pop).

%% Implementação GenServer
init([Capacity]) ->
    {ok, #{queue => queue:new(), capacity => Capacity, demand => 0}}.

handle_call({push, Item}, _From, State = #{queue := Q, capacity := Cap}) when queue:len(Q) < Cap ->
    {reply, ok, State#{queue => queue:in(Item, Q)}};
handle_call({push, _}, _From, State) ->
    {reply, {error, buffer_full}, State}; % Backpressure explícito

handle_call(pop, From, State = #{queue := Q, demand := D}) ->
    case queue:out(Q) of
        {{value, Item}, NewQ} ->
            {reply, {ok, Item}, State#{queue => NewQ}};
        {empty, _} ->
            % Registra demanda não atendida para backpressure
            {noreply, State#{demand => D + 1}, {continue, notify_producers}}
    end.

handle_continue(notify_producers, State) ->
    % Envia sinal para produtores reduzirem a taxa (implementação no Passo 3)
    {noreply, State}.`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Passo 2: Produtores Adaptativos</h3>
          <h4 className="text-lg font-bold mb-2">
            Módulo <code>file_producer.erl</code> (Lê arquivos com backpressure)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(file_producer).
-behaviour(gen_server).
-export([start_link/1, handle_continue/2]).

start_link(FilePath) ->
    gen_server:start_link(?MODULE, [FilePath], []).

init([FilePath]) ->
    {ok, Fd} = file:open(FilePath, [read, binary]),
    {ok, #{fd => Fd, buffer_pid => whereis(buffer_manager)}, {continue, read_lines}}.

handle_continue(read_lines, State = #{fd := Fd, buffer_pid := BufferPid}) ->
    case file:read_line(Fd) of
        {ok, Line} ->
            case circular_buffer:push(BufferPid, Line) of
                ok -> 
                    {noreply, State, {continue, read_lines}};
                {error, buffer_full} ->
                    % Backpressure: espera antes de continuar
                    timer:sleep(1000),
                    {noreply, State, {continue, read_lines}}
            end;
        eof ->
            {stop, normal, State}
    end.`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Passo 3: Consumidores Dinâmicos</h3>
          <h4 className="text-lg font-bold mb-2">
            Módulo <code>log_consumer.erl</code> (Processamento paralelo com checkpoint)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(log_consumer).
-behaviour(gen_server).
-export([start_link/1, handle_call/3]).

start_link(BufferPid) ->
    gen_server:start_link(?MODULE, [BufferPid], []).

init([BufferPid]) ->
    erlang:send_after(0, self(), fetch_next),
    {ok, #{buffer => BufferPid, processed => 0}}.

handle_info(fetch_next, State = #{buffer := BufferPid, processed := Count}) ->
    case circular_buffer:pop(BufferPid) of
        {ok, LogEntry} ->
            % Simula processamento (parse, análise, etc.)
            process_log_entry(LogEntry),
            erlang:send_after(0, self(), fetch_next),
            {noreply, State#{processed => Count + 1}};
        empty ->
            % Buffer vazio - reduz velocidade
            timer:sleep(500),
            erlang:send_after(100, self(), fetch_next),
            {noreply, State}
    end.

process_log_entry(Entry) ->
    % Implementação real usaria bibliotecas como:
    % jsx para JSON, luerl para análise customizada, etc.
    io:format("Processed: ~p~n", [Entry]).`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Passo 4: Supervisor Especializado</h3>
          <h4 className="text-lg font-bold mb-2">
            Módulo <code>consumer_sup.erl</code> (Balanceamento dinâmico)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(consumer_sup).
-behaviour(supervisor).
-export([adjust_pool_size/1, init/1]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [#{
        id => log_consumer,
        start => {log_consumer, start_link, [whereis(buffer_manager)]},
        restart => transient
    }],
    {ok, {SupFlags, ChildSpecs}}.

adjust_pool_size(CurrentLoad) ->
    DesiredSize = calculate_pool_size(CurrentLoad),
    Current = supervisor:count_children(?MODULE),
    Active = proplists:get_value(active, Current),
    case DesiredSize - Active of
        Diff when Diff > 0 ->
            [supervisor:start_child(?MODULE, []) || _ <- lists:seq(1, Diff)];
        Diff when Diff < 0 ->
            [gen_server:stop(Pid) || {_, Pid, _, _} <- supervisor:which_children(?MODULE),
                                    _ <- lists:seq(1, abs(Diff))];
        _ -> ok
    end.

calculate_pool_size(Load) ->
    % Lógica de auto-scaling baseada em métricas
    max(5, erlang:round(Load * 0.75)). % Exemplo simplificado`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Passo 5: Integração com Ferramentas OTP</h3>
          <h4 className="text-lg font-bold mb-2">
            Módulo <code>log_pipeline_app.erl</code> (Aplicação completa)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(log_pipeline_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    % Inicia buffer com capacidade para 10k itens
    {ok, _} = circular_buffer:start_link(10000),
    
    % Inicia supervisores
    log_pipeline_sup:start_link().

stop(_State) ->
    ok.`}
          </pre>

          <h4 className="text-lg font-bold mb-2">
            Supervisor Principal (<code>log_pipeline_sup.erl</code>)
          </h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(log_pipeline_sup).
-behaviour(supervisor).
-export([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => rest_for_one},
    ChildSpecs = [
        #{id => buffer_manager, start => {circular_buffer, start_link, [10000]}},
        #{id => producer_sup, start => {producer_sup, start_link, []}},
        #{id => consumer_sup, start => {consumer_sup, start_link, []}}
    ],
    {ok, {SupFlags, ChildSpecs}}.`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Testando o Sistema</h3>
          <h4 className="text-lg font-bold mb-2">1. Inicie o Pipeline</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`$ rebar3 shell
1> application:start(log_pipeline).`}
          </pre>

          <h4 className="text-lg font-bold mb-2">2. Adicione Produtores Manualmente</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`2> producer_sup:start_child("/var/log/syslog"). % Produtor de arquivo
3> producer_sup:start_child({tcp, 8080}).      % Produtor TCP`}
          </pre>

          <h4 className="text-lg font-bold mb-2">3. Monitore Métricas em Tempo Real</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`4> observer:start().  % Acesse a aba "Applications"`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Extensões Avançadas</h3>
          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>Persistência com DETS:</strong>
              <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                {`% No buffer_manager:
handle_cast(persist_state, State) ->
    dets:insert(buffer_backup, {state, State}),
    {noreply, State}.`}
              </pre>
            </li>
            <li>
              <strong>Deploy em Cluster:</strong>
              <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                {`% No consumer_sup:
start_remote_consumer(Node) ->
    {ok, Pid} = supervisor:start_child({?MODULE, Node}, []),
    {ok, Pid}.`}
              </pre>
            </li>
            <li>
              <strong>Protocolo Binário Eficiente:</strong>
              <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                {`% Substitua term_to_binary por:
msgpack:pack(Data, [{spec, new}])  % 30% mais compacto`}
              </pre>
            </li>
          </ol>

          <hr className="my-6" />

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
