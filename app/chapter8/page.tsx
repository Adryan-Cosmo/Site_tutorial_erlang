import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

export default function Chapter8() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter8" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

                <div className="flex justify-between mb-4">
                    <Link href="/chapter7">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter9">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 8: Projeto 1 - Sistema Cliente/Servidor TCP Resiliente</h2>
                    <p className="italic mb-4">
                        Construa um servidor que sobrevive a falhas de rede, picos de carga e até mesmo a erros no código - tudo usando os princípios OTP!
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Visão Geral do Sistema</h3>
                    <p className="mb-4">
                        Vamos criar um <strong>servidor de eco TCP</strong> aprimorado com:
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Protocolo textual</strong> simples (comandos <code>ECHO</code>, <code>SHUTDOWN</code>)
                        </li>
                        <li>
                            <strong>Pool de workers</strong> para lidar com múltiplos clientes
                        </li>
                        <li>
                            <strong>Supervisão hierárquica</strong> para tolerância a falhas
                        </li>
                        <li>
                            <strong>Hot code swapping</strong> para atualizações sem downtime
                        </li>
                    </ul>

                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`Árvore de Supervisão do Projeto:
tcp_supervisor (one_for_one)
├── tcp_listener (gen_server)
└── worker_sup (simple_one_for_one)
    ├── worker_1 (gen_server)
    └── worker_2 (gen_server)`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Passo 1: Definindo o Protocolo</h3>
                    <h4 className="text-lg font-bold mb-2">Formato das Mensagens</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`% Comandos suportados:
{echo, "texto"}        % Servidor responde com "texto"
{shutdown, senha}      % Desliga o servidor se a senha estiver correta`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">
                        Módulo de Protocolo (<code>tcp_protocol.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(tcp_protocol).
-export([parse/1, build_response/1]).

parse(Data) ->
    case binary_to_term(Data) of
        {echo, Text} when is_binary(Text) -> {ok, {echo, Text}};
        {shutdown, "S3cr3t!"} -> {ok, shutdown};
        _ -> {error, invalid_command}
    end.

build_response({echo, Text}) -> term_to_binary({ok, Text});
build_response(invalid_command) -> term_to_binary({error, "Comando inválido"}).`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Passo 2: Implementando o Worker</h3>
                    <h4 className="text-lg font-bold mb-2">
                        Comportamento GenServer (<code>tcp_worker.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(tcp_worker).
-behaviour(gen_server).
-export([start_link/1, handle_call/3, init/1]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    {ok, #{socket => Socket}}.

handle_call({process, Data}, _From, State = #{socket := Socket}) ->
    Response = case tcp_protocol:parse(Data) of
        {ok, {echo, Text}} -> tcp_protocol:build_response({echo, Text});
        {ok, shutdown} -> gen_server:stop(self()), <<"shutting_down">>;
        {error, Reason} -> tcp_protocol:build_response(Reason)
    end,
    gen_tcp:send(Socket, Response),
    {reply, ok, State}.`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">Fluxo de Tratamento de Erros:</h4>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>
                            Se o worker crashar, o <code>simple_one_for_one</code> supervisor cria um novo
                        </li>
                        <li>
                            Conexões TCP são fechadas graciosamente via <code>gen_tcp:close/1</code> no <code>terminate/2</code>
                        </li>
                    </ol>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Passo 3: Supervisor de Workers</h3>
                    <h4 className="text-lg font-bold mb-2">
                        Implementação (<code>worker_sup.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(worker_sup).
-behaviour(supervisor).
-export([start_link/0, start_child/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpecs = [#{
        id => tcp_worker,
        start => {tcp_worker, start_link, []},
        restart => temporary  % Não reinicia se sair normalmente
    }],
    {ok, {SupFlags, ChildSpecs}}.

start_child(Socket) ->
    supervisor:start_child(?MODULE, [Socket]).`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Passo 4: Ouvinte TCP Principal</h3>
                    <h4 className="text-lg font-bold mb-2">
                        GenServer (<code>tcp_listener.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(tcp_listener).
-behaviour(gen_server).
-export([start_link/1, init/1, handle_info/2]).

start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, 4},
        {reuseaddr, true},
        {active, false}
    ]),
    self() ! accept_connections,
    {ok, #{listen_socket => ListenSocket}}.

handle_info(accept_connections, State = #{listen_socket := ListenSock}) ->
    {ok, Socket} = gen_tcp:accept(ListenSock),
    {ok, _Pid} = worker_sup:start_child(Socket),
    self() ! accept_connections,  % Continua aceitando conexões
    {noreply, State}.`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Passo 5: Árvore de Supervisão Principal</h3>
                    <h4 className="text-lg font-bold mb-2">
                        Aplicação OTP (<code>tcp_app.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(tcp_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    tcp_sup:start_link(8080).  % Porta padrão

stop(_State) -> ok.`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">
                        Supervisor Raiz (<code>tcp_sup.erl</code>)
                    </h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(tcp_sup).
-behaviour(supervisor).
-export([start_link/1]).

start_link(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
    SupFlags = #{strategy => one_for_one},
    ChildSpecs = [
        #{id => worker_sup, start => {worker_sup, start_link, []}},
        #{id => tcp_listener, start => {tcp_listener, start_link, [Port]}}
    ],
    {ok, {SupFlags, ChildSpecs}}.`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Testando o Sistema</h3>
                    <h4 className="text-lg font-bold mb-2">1. Inicie a Aplicação</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`$ rebar3 shell
1> application:start(tcp_app).`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">2. Conecte via Telnet</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">{`$ nc localhost 8080`}</pre>

                    <h4 className="text-lg font-bold mb-2">3. Envie Comandos (em formato Erlang binary)</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`% Exemplo de comunicação:
Client: term_to_binary({echo, "Hello"}).
Server: term_to_binary({ok, "Hello"})`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Extensões Avançadas</h3>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>
                            <strong>Rate Limiting:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% No worker, antes de processar:
case leaky_bucket:check(IP) of
    allow -> process_request();
    deny -> gen_tcp:send(Socket, <<"rate_limit_exceeded">>)
end.`}
                            </pre>
                        </li>
                        <li>
                            <strong>Monitoramento em Tempo Real:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Use folsom para métricas:
folsom_metrics:new_counter(requests),
folsom_metrics:notify({requests, {inc, 1}}).`}
                            </pre>
                        </li>
                        <li>
                            <strong>SSL/TLS:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Modifique o listener:
{ok, ListenSocket} = ssl:listen(Port, [
    {certfile, "cert.pem"},
    {keyfile, "key.pem"},
    {reuseaddr, true}
]).`}
                            </pre>
                        </li>
                    </ol>

                    <hr className="my-6" />

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 8:</p>
                        <p className="mb-2">
                            Implemente um <strong>comando "STATS"</strong> que retorne:
                        </p>
                        <ul className="list-disc pl-6 mb-4">
                            <li>Número de workers ativos</li>
                            <li>Total de requisições processadas</li>
                            <li>Uso médio de CPU</li>
                        </ul>

                        <p className="font-bold mb-2">Dicas:</p>
                        <ol className="list-decimal pl-6 mb-4">
                            <li>
                                Use <code>supervisor:count_children/1</code>
                            </li>
                            <li>
                                Armazene estatísticas em um <code>gen_server</code> separado
                            </li>
                            <li>
                                Formate a saída como JSON via <code>jsx:encode/1</code>
                            </li>
                        </ol>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <em>"Erlang in Anger"</em> (Fred Hébert) - Guia de sobrevivência para sistemas em produção
                        </li>
                        <li>
                            Documentação OTP:{' '}
                            <a href="https://erlang.org/doc/man/gen_tcp.html" className="text-blue-600 hover:underline">
                                gen_tcp
                            </a>
                            ,{' '}
                            <a href="https://erlang.org/doc/man/supervisor.html" className="text-blue-600 hover:underline">
                                supervisor
                            </a>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <div className="border border-gray-300 p-4 rounded mb-4">
                        <h4 className="text-lg font-bold mb-2">Box de Arquitetura:</h4>
                        <p className="font-bold">Por que este Design Escala?</p>
                        <ol className="list-decimal pl-6 mb-2">
                            <li>
                                <strong>Isolamento de Falhas:</strong>
                                <ul className="list-disc pl-6">
                                    <li>Workers crasham sem afetar o listener</li>
                                    <li>Cada conexão é independente</li>
                                </ul>
                            </li>
                            <li>
                                <strong>Controle de Recursos:</strong>
                                <ul className="list-disc pl-6">
                                    <li>
                                        <code>simple_one_for_one</code> permite crescimento dinâmico
                                    </li>
                                    <li>
                                        <code>temporary</code> workers evitam reinicializações desnecessárias
                                    </li>
                                </ul>
                            </li>
                            <li>
                                <strong>Extensibilidade:</strong>
                                <ul className="list-disc pl-6">
                                    <li>Hot code swapping permite atualizar workers sem desconectar clientes</li>
                                    <li>Protocolo pode evoluir sem mudar a arquitetura</li>
                                </ul>
                            </li>
                        </ol>
                    </div>

                    <hr className="my-6" />

                    <SolutionReveal>
                        <h4 className="text-lg font-bold mb-2">Exemplo de Solução do Desafio (STATS):</h4>
                        <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                            {`handle_call(get_stats, _From, State) ->
    {ok, Counts} = supervisor:count_children(worker_sup),
    {reply, #{
        workers => proplists:get_value(active, Counts),
        requests => folsom_metrics:get_metric_value(requests)
    }, State}.`}
                        </pre>
                    </SolutionReveal>
                </div>

                <div className="flex justify-between">
                    <Link href="/chapter7">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter9">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>
            </div>
        </div>
    );
}
