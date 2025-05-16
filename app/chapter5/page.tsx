import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

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
                    <p className="italic mb-4">Onde seu código ganha asas! Descubra como Erlang roda milhões de tarefas em paralelo sem suar a camisa.</p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Modelo de Concorrência em Erlang: O Segredo da Escala Infinita</h3>
                    <p className="mb-4">
                        Erlang não usa <em>threads do sistema operacional</em>. Em vez disso, cria <strong>processos leves</strong> gerenciados pela VM BEAM.
                        Pense neles como formigas trabalhando em equipe: cada uma faz sua parte, independente das outras!
                    </p>

                    <h4 className="text-lg font-bold mb-2">Por que processos leves?</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Isolamento total:</strong> Se um processo crashar, os outros continuam.
                        </li>
                        <li>
                            <strong>Custo quase zero:</strong> Crie <em>2 milhões</em> de processos em uma máquina comum.
                        </li>
                        <li>
                            <strong>Escalabilidade horizontal:</strong> Distribua processos entre núcleos ou máquinas.
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">Comparação com Threads Tradicionais:</h4>
                    <div className="overflow-x-auto mb-4">
                        <table className="min-w-full border border-gray-300">
                            <thead>
                                <tr className="bg-gray-200">
                                    <th className="border border-gray-300 p-2">Característica</th>
                                    <th className="border border-gray-300 p-2">Processos Erlang</th>
                                    <th className="border border-gray-300 p-2">Threads do SO</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Memória</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">~2 KB cada</td>
                                    <td className="border border-gray-300 p-2">~1 MB cada</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Criação</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">Microssegundos</td>
                                    <td className="border border-gray-300 p-2">Milissegundos</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Comunicação</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">Troca de mensagens</td>
                                    <td className="border border-gray-300 p-2">Memória compartilhada</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Criando Processos: A Magia do `spawn`</h3>
                    <p className="mb-4">
                        Use <code>spawn/1</code> ou <code>spawn/3</code> para dar vida a novos processos. Cada um recebe um <strong>PID</strong> único, como{' '}
                        <code>&lt;0.128.0&gt;</code>.
                    </p>

                    <h4 className="text-lg font-bold mb-2">Exemplo 1: Processo Básico</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`% Define uma função que o processo executará
saudacao() ->
  io:format("Olá do processo ~p!~n", [self()]).

% Cria o processo
1> Pid = spawn(fun saudacao/0).
Olá do processo <0.128.0>!
<0.128.0>`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">Exemplo 2: Processo com Argumentos</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(processo_demo).
-export([iniciar/0]).

iniciar() ->
  spawn(?MODULE, loop, ["Mensagem secreta"]).  % spawn(Module, Function, Args)

loop(Msg) ->
  io:format("Processo ~p diz: ~s~n", [self(), Msg]).`}
                    </pre>

                    <p className="font-bold mb-2">Saída:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> processo_demo:iniciar().
Processo <0.130.0> diz: Mensagem secreta
<0.130.0>`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Comunicação entre Processos: O Poder das Mensagens</h3>
                    <p className="mb-4">
                        Processos conversam via <strong>envio assíncrono de mensagens</strong> (!) e <strong>recebimento seletivo</strong> (<code>receive</code>
                        ).
                    </p>

                    <h4 className="text-lg font-bold mb-2">Passo a Passo:</h4>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>
                            <strong>Envie uma mensagem</strong> com <code>Pid ! Mensagem</code>.
                        </li>
                        <li>
                            <strong>Capture-a</strong> com <code>receive ... end</code>.
                        </li>
                    </ol>

                    <h4 className="text-lg font-bold mb-2">Exemplo: Ping-Pong entre Processos</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(ping_pong).
-export([iniciar/0, pong/0]).

iniciar() ->
  PongPid = spawn(?MODULE, pong, []),
  PongPid ! {ping, self()},
  receive
    pong -> io:format("Recebi pong!~n")
  end.

pong() ->
  receive
    {ping, From} ->
      io:format("Recebi ping! Enviando pong...~n"),
      From ! pong
  end.`}
                    </pre>

                    <p className="font-bold mb-2">Teste:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> ping_pong:iniciar().
Recebi ping! Enviando pong...
Recebi pong!
ok`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Processos vs. Threads do SO: A Batalha dos Paralelos</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Threads do SO:</strong>
                            <ul className="list-disc pl-6">
                                <li>Limitadas pelo número de núcleos.</li>
                                <li>
                                    Compartilham memória → Risco de <em>deadlocks</em> e <em>race conditions</em>.
                                </li>
                            </ul>
                        </li>
                        <li>
                            <strong>Processos Erlang:</strong>
                            <ul className="list-disc pl-6">
                                <li>
                                    Executados em <em>schedulers</em> da BEAM (1 por núcleo CPU).
                                </li>
                                <li>Isolados → Sem memória compartilhada.</li>
                            </ul>
                        </li>
                    </ul>

                    <div className="bg-blue-100 p-4 mb-4 rounded">
                        <p className="font-bold">Curiosidade:</p>
                        <p>
                            O WhatsApp usa processos Erlang para lidar com <strong>2 milhões de conexões por servidor</strong>!
                        </p>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Monitorando Processos: O Olho Que Tudo Vê</h3>
                    <p className="mb-4">
                        Use <code>spawn_link/1</code> para criar processos vinculados. Se um falhar, o outro é notificado:
                    </p>

                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`pai() ->
  spawn_link(fun filho/0),
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Filho ~p morreu. Razão: ~p~n", [Pid, Reason])
  end.

filho() ->
  exit(boom).  % Simula uma falha`}
                    </pre>

                    <p className="font-bold mb-2">Saída:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> pai().
Filho <0.131.0> morreu. Razão: boom
ok`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Exemplo Prático: Sistema de Notificações</h3>
                    <p className="mb-4">Crie um processo que envia notificações para múltiplos clientes:</p>

                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(notificador).
-export([iniciar/0, cliente/1]).

iniciar() ->
  spawn(fun() -> loop([]) end).

loop(Clientes) ->
  receive
    {registrar, Pid} ->
      io:format("Cliente ~p registrado.~n", [Pid]),
      loop([Pid | Clientes]);
    {notificar, Msg} ->
      lists:foreach(fun(Pid) -> Pid ! {msg, Msg} end, Clientes),
      loop(Clientes)
  end.

cliente(NotificadorPid) ->
  NotificadorPid ! {registrar, self()},
  loop_cliente().

loop_cliente() ->
  receive
    {msg, Msg} ->
      io:format("Cliente ~p recebeu: ~s~n", [self(), Msg]),
      loop_cliente()
  end.`}
                    </pre>

                    <p className="font-bold mb-2">Teste:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> Notificador = notificador:iniciar().
<0.132.0>
2> Cliente1 = notificador:cliente(Notificador).
Cliente <0.134.0> registrado.
<0.134.0>
3> Notificador ! {notificar, "Promoção de 50%!"}.
Cliente <0.134.0> recebeu: Promoção de 50%!
{notificar, "Promoção de 50%!"}`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Erros Comuns e Soluções</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Erro:</strong> <code>badarg</code> ao usar <code>spawn</code> → Verifique se a função está exportada ou se os argumentos
                            estão corretos.
                        </li>
                        <li>
                            <strong>Erro:</strong> Mensagens perdidas → Certifique-se de que o processo destino está vivo e o <code>receive</code> está correto.
                        </li>
                        <li>
                            <strong>Dica:</strong> Use <code>process_info(Pid)</code> para inspecionar o estado de um processo.
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Próximos Passos</h3>
                    <p className="mb-2">
                        No <strong>Capítulo 6</strong>, você vai:
                    </p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Aprofundar-se no envio e recebimento de mensagens.</li>
                        <li>
                            Explorar caixas postais e <em>pattern matching</em> avançado.
                        </li>
                        <li>Construir um sistema de chat distribuído.</li>
                    </ol>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 5:</p>
                        <ul className="list-disc pl-6">
                            <li>
                                Crie um processo <code>contador</code> que armazene um número.
                                <ul className="list-disc pl-6">
                                    <li>
                                        Aceite mensagens <code>{'{incrementar, N}'}</code> para somar ao valor.
                                    </li>
                                    <li>
                                        Responda a <code>{'{get, From}'}</code> com o valor atual.
                                    </li>
                                </ul>
                            </li>
                            <li>Dica: Use recursão para manter o estado.</li>
                        </ul>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Documentação:{' '}
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
    );
}
