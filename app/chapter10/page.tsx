import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

export default function Chapter10() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter10" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

                <div className="flex justify-between mb-4">
                    <Link href="/chapter9">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700" disabled>
                        Next →
                    </Button>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 10: Boas Práticas e Padrões OTP</h2>
                    <p className="italic mb-4">
                        Eleve seu código de "funciona" para "produção industrial" com as convenções que alimentam sistemas como WhatsApp e RabbitMQ!
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Estrutura de Aplicações OTP: O Esqueleto Profissional</h3>
                    <p className="mb-4">Todo sistema Erlang profissional segue este template:</p>

                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`my_app/  
├── ebin/                  # Bytecode compilado  
├── include/               |  
│   └── my_app.hrl         - Headers compartilhados  
├── priv/                  # Assets (certificados, templates)  
├── src/                   |  
│   ├── my_app.app.src     |  
│   ├── my_app_sup.erl     |  
│   ├── my_app_app.erl     |  
│   └── main_module.erl    |  
└── test/                  # Testes EUnit/CommonTest`}
                    </pre>

                    <p className="font-bold mb-2">Regra de Ouro:</p>
                    <blockquote className="border-l-4 border-gray-500 pl-4 italic mb-4">
                        "Se parece com um processo genérico, implemente como um <em>behaviour</em> OTP"
                    </blockquote>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">GenServer: O Cavalo de Trabalho</h3>
                    <h4 className="text-lg font-bold mb-2">Template Padronizado com Boas Práticas</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(my_gen_server).  
-behaviour(gen_server).  

%% API  
-export([start_link/1, process_data/2]).  

%% Callbacks  
-export([init/1, handle_call/3, handle_cast/2]).  

-record(state, {  
    conn_pool :: pid(),  
    req_count = 0 :: non_neg_integer(),  
    cache :: ets:tid()  
}).  

%%% API  
start_link(Args) ->  
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).  

process_data(Pid, Data) ->  
    gen_server:call(Pid, {process, Data}, 5000). % Timeout explícito  

%%% Callbacks  
init(Args) ->  
    process_flag(trap_exit, true),  % Captura exits normais  
    {ok, Conn} = db_connect(Args),  
    {ok, #state{  
        conn_pool = Conn,  
        cache = ets:new(cache, [set, private])  
    }, {continue, warm_cache}}. % Inicialização assíncrona  

handle_continue(warm_cache, State) ->  
    % Pré-carrega dados sem bloquear init  
    {noreply, State}.  

handle_call({process, Data}, _From, State) ->  
    case do_processing(Data, State) of  
        {ok, Result} ->  
            {reply, {ok, Result}, State#state{req_count = State#state.req_count + 1}};  
        {error, Reason} ->  
            {reply, {error, Reason}, State}  
    end;  
handle_call(get_metrics, _From, State) ->  
    {reply, #{requests => State#state.req_count}, State}.  

handle_cast(_Msg, State) ->  
    % Sempre implemente mesmo se não usar  
    {noreply, State}.  

terminate(Reason, #state{conn_pool = Conn}) ->  
    db:close(Conn),  
    log_shutdown(Reason).`}
                    </pre>

                    <p className="font-bold mb-2">Checklist de Implementação:</p>
                    <ul className="list-none pl-6 mb-4">
                        <li>
                            ✅ Sempre defina tipos (<code>-record</code>, <code>-type</code>)
                        </li>
                        <li>
                            ✅ Use <code>handle_continue</code> para init não-bloqueante
                        </li>
                        <li>✅ Timeouts explícitos em chamadas públicas</li>
                        <li>
                            ✅ Limpeza de recursos no <code>terminate</code>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Supervisors: A Arte da Resiliência</h3>
                    <h4 className="text-lg font-bold mb-2">Estratégias de Reinício com Exemplos Reais</h4>

                    <div className="overflow-x-auto mb-4">
                        <table className="min-w-full border border-gray-300">
                            <thead>
                                <tr className="bg-gray-200">
                                    <th className="border border-gray-300 p-2">Estratégia</th>
                                    <th className="border border-gray-300 p-2">Uso Típico</th>
                                    <th className="border border-gray-300 p-2">Código de Exemplo</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>one_for_one</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Workers independentes</td>
                                    <td className="border border-gray-300 p-2">Pool de conexões DB</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>one_for_all</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Serviços interdependentes</td>
                                    <td className="border border-gray-300 p-2">API + Cache + Auth</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>rest_for_one</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Dependências lineares</td>
                                    <td className="border border-gray-300 p-2">TCP Listener → Handlers</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>simple_one_for_one</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Pools dinâmicos</td>
                                    <td className="border border-gray-300 p-2">Workers de processamento</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <p className="font-bold mb-2">Child Spec Moderno (OTP 21+):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`init([]) ->  
    SupFlags = #{  
        strategy => one_for_one,  
        intensity => 3,  
        period => 3600,  % 1 hora  
        auto_shutdown => never  
    },  

    ChildSpecs = [#{  
        id => cache_worker,  
        start => {cache_server, start_link, []},  
        restart => transient,  % Só reinicia se falhar anormalmente  
        shutdown => 2000,      % 2 segundos para graceful stop  
        type => worker,  
        modules => [cache_server]  
    }],  

    {ok, {SupFlags, ChildSpecs}}.`}
                    </pre>

                    <p className="font-bold mb-2">Dica Pro:</p>
                    <blockquote className="border-l-4 border-gray-500 pl-4 italic mb-4">
                        "Use <code>transient</code> para processos que devem completar seu trabalho, <code>permanent</code> para serviços críticos"
                    </blockquote>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Applications: O Contexto de Sistema</h3>
                    <h4 className="text-lg font-bold mb-2">`my_app.app.src` (Metadados Essenciais)</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`{application, my_app,  
 [  
    {description, "Sistema de processamento de pagamentos"},  
    {vsn, "1.2.3"},  
    {modules, [my_app_app, my_app_sup]},  
    {registered, [my_app_sup]},  
    {applications, [kernel, stdlib, crypto]},  
    {mod, {my_app_app, []}},  
    {env, [  
        {max_connections, 100},  
        {timeout, 5000}  
    ]},  
    {licenses, ["Apache-2.0"]},  
    {links, [{"GitHub", "https://github.com/meu/repo"}]}  
 ]}.`}
                    </pre>

                    <p className="font-bold mb-2">Regras de Ouro:</p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Sempre liste dependências exatas</li>
                        <li>
                            Documente variáveis de ambiente (<code>env</code>)
                        </li>
                        <li>Versionamento semântico obrigatório</li>
                    </ol>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Testes Automatizados como Religião</h3>
                    <h4 className="text-lg font-bold mb-2">Hierarquia de Testes Profissionais</h4>

                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`%% Teste Unitário (EUnit)  
processamento_test_() ->  
    [  
        ?_assertEqual(ok, parse("valid_input")),  
        ?_assertError(badarg, parse(123))  
    ].  

%% Teste de Integração (CommonTest)  
init_per_suite(Config) ->  
    {ok, _} = application:ensure_all_started(my_app),  
    Config.  

end_per_suite(_Config) ->  
    ok.  

api_test(Config) ->  
    {ok, Result} = my_app:process(Config),  
    ?assertMatch(#{status := success}, Result).  

%% Teste de Propriedades (PropEr)  
prop_parser_roundtrip() ->  
    ?FORALL(Data, valid_data(),  
        parse(encode(Data)) == {ok, Data}).`}
                    </pre>

                    <p className="font-bold mb-2">Cobertura Mínima Aceitável:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>80% para código novo</li>
                        <li>95% para módulos críticos (ex: transações financeiras)</li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Logging e Monitoramento em Produção</h3>
                    <h4 className="text-lg font-bold mb-2">Configuração do `kernel` para Logs</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`%% sys.config  
[  
    {kernel, [  
        {logger, [  
            {handler, default, logger_std_h, #{  
                config => #{file => "/var/log/my_app.log"},  
                level => info  
            }}  
        ]}  
    ]},  
    {sasl, [  
        {errlog_type, error}  % Logs de crash detalhados  
    ]}  
].`}
                    </pre>

                    <p className="font-bold mb-2">Métricas Essenciais:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`handle_info(report_metrics, State) ->  
    prometheus_gauge:set(  
        my_app_requests_total,  
        State#state.req_count  
    ),  
    erlang:send_after(5000, self(), report_metrics),  
    {noreply, State}.`}
                    </pre>

                    <p className="font-bold mb-2">Ferramentas Profissionais:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>
                                <code>recon</code>:
                            </strong>{' '}
                            Troubleshooting em tempo real
                        </li>
                        <li>
                            <strong>
                                <code>telemetry</code>:
                            </strong>{' '}
                            Eventos de aplicação padronizados
                        </li>
                        <li>
                            <strong>
                                <code>grafana</code> + <code>prometheus</code>:
                            </strong>{' '}
                            Dashboards de produção
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Anti-Padrões Comuns (Evite Como a Peste!)</h3>

                    <ol className="list-decimal pl-6 mb-4">
                        <li>
                            <strong>Processos Zombies:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Ruim  
spawn(fun() -> loop() end),  

% Bom  
{ok, Pid} = gen_server:start_link(...)`}
                            </pre>
                        </li>
                        <li>
                            <strong>Message Queue Overflow:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Ruim  
loop() ->  
    receive _ -> ok end,  % Sem filtro  
    loop().  

% Bom  
loop() ->  
    receive  
        {Type, Data} -> handle(Type, Data)  
    after 5000 -> timeout_actions()  
    end.`}
                            </pre>
                        </li>
                        <li>
                            <strong>Hot Code Swapping Ingênuo:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Ruim (sem considerar estado migratório)  
code:load_file(my_module).  

% Bom  
sys:suspend(my_server),  
code:load_file(my_module),  
sys:change_code(my_server, my_module, old_vsn, []),  
sys:resume(my_server).`}
                            </pre>
                        </li>
                    </ol>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Checklist de Deploy Profissional</h3>

                    <ol className="list-decimal pl-6 mb-4">
                        <li>
                            <strong>Release com Relx:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`# rebar.config  
{relx, [  
    {release, {my_app, "1.2.3"}, [my_app]},  
    {dev_mode, false},  
    {include_erts, true}  # Bundle completo  
]}.  

$ rebar3 release  
$ _build/default/rel/my_app/bin/my_app start`}
                            </pre>
                        </li>
                        <li>
                            <strong>Rolling Upgrade:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`# my_app.appup  
{"1.2.3", "1.2.4", [  
    {update, my_gen_server, advanced}  
]}.  

$ rebar3 appup generate`}
                            </pre>
                        </li>
                        <li>
                            <strong>Health Checks:</strong>
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`% Módulo de saúde  
-module(my_app_health).  
-export([check/0]).  

check() ->  
    [  
        {database, db:ping()},  
        {memory, vm_memory:stats()},  
        {queues, message_queue:lengths()}  
    ].`}
                            </pre>
                        </li>
                    </ol>

                    <hr className="my-6" />

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 10:</p>
                        <p className="mb-2">Transforme um módulo existente em um:</p>
                        <ol className="list-decimal pl-6 mb-4">
                            <li>
                                <strong>GenServer</strong> totalmente OTP-compliant
                            </li>
                            <li>
                                Com <strong>supervisor</strong> dedicado
                            </li>
                            <li>
                                <strong>Testes</strong> unitários e de propriedade
                            </li>
                            <li>
                                <strong>Instrumentação</strong> com métricas
                            </li>
                        </ol>

                        <p className="font-bold mb-2">Dica:</p>
                        <p className="mb-4">
                            Use <code>rebar3 new umbrella</code> para estrutura profissional.
                        </p>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Sagradas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <em>"Erlang/OTP in Action"</em> (Martin Logan)
                        </li>
                        <li>
                            <em>"Property-Based Testing with PropEr"</em> (Fred Hebert)
                        </li>
                        <li>
                            <strong>Documentação OTP:</strong>{' '}
                            <a href="https://erlang.org/doc/design_principles" className="text-blue-600 hover:underline">
                                erlang.org/doc/design_principles
                            </a>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <SolutionReveal>
                        <h4 className="text-lg font-bold mb-2">Exemplo de Solução do Desafio:</h4>
                        <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                            {`-module(my_gsvr).
-behaviour(gen_server).
-export([start_link/0, init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, _} = schedule_metrics(),
    {ok, #state{}}.

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State}.

schedule_metrics() ->
    erlang:send_after(5000, self(), report_metrics).`}
                        </pre>
                    </SolutionReveal>
                </div>

                <div className="flex justify-between">
                    <Link href="/chapter9">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700" disabled>
                        Next →
                    </Button>
                </div>
            </div>
        </div>
    );
}
