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

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Modelo de Troca de Mensagens: O Protocolo dos Atores</h3>
          <p className="mb-4">
            Em Erlang, processos são <strong>atores isolados</strong> que se comunicam exclusivamente via{" "}
            <strong>mensagens assíncronas</strong>. Este modelo evita os pesadelos tradicionais de concorrência (
            <em>deadlocks</em>, <em>race conditions</em>).
          </p>

          <h4 className="text-lg font-bold mb-2">Como Funciona?</h4>
          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>Envio (!):</strong> Um processo A envia uma mensagem ao processo B. A mensagem é copiada para a
              caixa postal de B.
            </li>
            <li>
              <strong>Recebimento (receive):</strong> B escaneia sua caixa postal usando <em>pattern matching</em>.
            </li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Exemplo Técnico Detalhado:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(caixa_postal).
-export([iniciar/0, enviar/2]).

iniciar() ->
    spawn(fun() -> loop() end).

loop() ->
    receive  % Fica bloqueado até receber uma mensagem compatível
        {remetente, Pid, Msg} ->
            io:format("Processo ~p recebeu: ~p~n", [self(), Msg]),
            Pid ! {confirmacao, self()},
            loop();
        shutdown ->
            io:format("Processo ~p encerrando.~n", [self()])
    end.

enviar(Destino, Msg) ->
    Destino ! {remetente, self(), Msg},
    receive
        {confirmacao, Destino} -> ok
    after 5000 ->  % Timeout de 5 segundos
        timeout
    end.`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Análise do Código:</h4>
          <ul className="list-disc pl-6 mb-4">
            <li>
              <strong>Não bloqueante:</strong> O operador <code>!</code> é assíncrono. O remetente continua sua
              execução.
            </li>
            <li>
              <strong>Seletividade:</strong> <code>receive</code> usa pattern matching para filtrar mensagens
              específicas.
            </li>
            <li>
              <strong>Timeout:</strong> Cláusula <code>after</code> lida com falhas de comunicação.
            </li>
          </ul>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Caixas Postais: A Fila de Mensagens de Cada Processo</h3>
          <p className="mb-4">
            Cada processo tem uma <strong>caixa postal privada</strong> onde mensagens são armazenadas na ordem de
            chegada.
          </p>

          <h4 className="text-lg font-bold mb-2">Gerenciamento Avançado:</h4>
          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>Priorização:</strong> Use padrões específicos antes de genéricos.
            </li>
            <li>
              <strong>Limpeza:</strong> Mensagens não correspondidas ficam até serem lidas ou o processo morrer.
            </li>
          </ol>

          <h4 className="text-lg font-bold mb-2">Exemplo com Prioridade:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`receive
    {urgente, X} ->  % Checa primeiro mensagens urgentes
        handle_urgent(X);
    {normal, Y} ->
        handle_normal(Y)
end.`}
          </pre>

          <p className="mb-4">
            <strong>Eficiência:</strong> A BEAM usa algoritmos otimizados para escanear caixas postais rapidamente,
            mesmo com milhões de mensagens.
          </p>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Sincronização Assíncrona: Quando a Ordem Importa</h3>
          <p className="mb-4">Para garantir sequencialidade em operações distribuídas:</p>

          <h4 className="text-lg font-bold mb-2">Protocolo de Ack/Nack</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`servidor() ->
    receive
        {requisicao, From, Dados} ->
            % Processa os dados
            From ! {ack, self(), resultado},
            servidor();
        {cancelar, From} ->
            From ! {nack, self()}
    end.

cliente(Pid) ->
    Pid ! {requisicao, self(), dados},
    receive
        {ack, Pid, Resultado} ->
            Resultado;
        {nack, Pid} ->
            erro
    end.`}
          </pre>

          <p className="mb-4">
            <strong>Cenário de Uso:</strong> Sistemas de pagamento onde a ordem das transações é crítica.
          </p>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Exemplo Prático: Chat Distribuído com Registro de Usuários</h3>
          <p className="mb-4">Vamos construir um sistema onde:</p>
          <ul className="list-disc pl-6 mb-4">
            <li>
              Um processo <strong>registrador</strong> mantém a lista de usuários.
            </li>
            <li>
              Processos <strong>clientes</strong> enviam mensagens broadcast.
            </li>
          </ul>

          <h4 className="text-lg font-bold mb-2">Implementação Completa:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`-module(chat_distribuido).
-export([iniciar/0, registrar/1, enviar/1]).

% Servidor de Registro
iniciar() ->
    Registrador = spawn(fun() -> registrador([]) end),
    register(servidor_chat, Registrador).  % Registra o processo globalmente

registrador(Usuarios) ->
    receive
        {registrar, Pid, Nome} ->
            io:format("~s entrou no chat.~n", [Nome]),
            Pid ! {confirmacao, servidor_chat},
            registrador([{Pid, Nome} | Usuarios]);
        {enviar_todos, Remetente, Msg} ->
            lists:foreach(
                fun({Pid, _}) -> Pid ! {mensagem, Remetente, Msg} end,
                Usuarios
            ),
            registrador(Usuarios)
    end.

% Cliente
registrar(Nome) ->
    servidor_chat ! {registrar, self(), Nome},
    receive
        {confirmacao, servidor_chat} ->
            loop_cliente(Nome)
    end.

loop_cliente(Nome) ->
    receive
        {mensagem, De, Msg} ->
            io:format("~s diz: ~s~n", [De, Msg]),
            loop_cliente(Nome);
        enviar ->
            servidor_chat ! {enviar_todos, Nome, "Olá a todos!"},
            loop_cliente(Nome)
    end.

enviar(Msg) ->
    servidor_chat ! {enviar_todos, self(), Msg}.`}
          </pre>

          <h4 className="text-lg font-bold mb-2">Testando no Eshell:</h4>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`1> c(chat_distribuido).
2> chat_distribuido:iniciar().
3> Cliente1 = spawn(fun() -> chat_distribuido:registrar("Alice") end).
Alice entrou no chat.
4> Cliente2 = spawn(fun() -> chat_distribuido:registrar("Bob") end).
Bob entrou no chat.
5> Cliente1 ! enviar.
Alice diz: Olá a todos!
Bob diz: Olá a todos!`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Técnicas Avançadas de Comunicação</h3>

          <h4 className="text-lg font-bold mb-2">1. Monitoramento de Processos com `monitor`</h4>
          <p className="mb-4">
            Mais flexível que <code>link</code>, permite detectar mortes sem causar propagação de falhas:
          </p>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`monitorar() ->
    Pid = spawn(fun vitima/0),
    monitor(process, Pid),
    receive
        {'DOWN', _Ref, process, Pid, Reason} ->
            io:format("Processo ~p morreu: ~p~n", [Pid, Reason])
    end.

vitima() -> exit(boom).`}
          </pre>

          <h4 className="text-lg font-bold mb-2">2. Registro Global de Processos</h4>
          <p className="mb-4">
            Use <code>register/2</code> para acessar processos por nome em vez de PID:
          </p>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`register(servico_critico, spawn(fun() -> loop() end)).

% Em outro nó:
servico_critico ! mensagem.`}
          </pre>

          <hr className="my-6" />

          <h3 className="text-xl font-bold mb-2">Padrões de Design para Sistemas Resilientes</h3>
          <ol className="list-decimal pl-6 mb-4">
            <li>
              <strong>GenServer:</strong> Comportamento OTP para servidores genéricos (será detalhado no Capítulo 10).
            </li>
            <li>
              <strong>Circuit Breaker:</strong> Padrão para evitar cascatas de falhas:
            </li>
          </ol>
          <pre className="bg-gray-800 text-white p-4 rounded mb-4">
            {`circuit_breaker(MaxFalhas, Fun) ->
    case falhas < MaxFalhas of
        true ->
            try Fun() of
                Result -> {ok, Result}
            catch
                _:_ -> 
                    incrementar_falhas(),
                    {error, falha_temporaria}
            end;
        false ->
            {error, circuito_aberto}
    end.`}
          </pre>

          <hr className="my-6" />

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
