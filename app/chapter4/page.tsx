import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

export default function Chapter4() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter4" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

                <div className="flex justify-between mb-4">
                    <Link href="/chapter3">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter5">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 4: Programação Funcional em Erlang</h2>
                    <p className="italic mb-4">Onde funções se tornam superpoderes! Prepare-se para transformar código em poesia funcional.</p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Funções de Primeira Classe: Cidadãos de Primeira</h3>
                    <p className="mb-4">
                        Em Erlang, funções são tratadas como <strong>valores</strong>: podem ser passadas como argumentos, retornadas de outras funções e
                        armazenadas em variáveis. É como ter LEGOs que montam outros LEGOs!
                    </p>

                    <h4 className="text-lg font-bold mb-2">
                        Funções Anônimas (<code>fun</code>)
                    </h4>
                    <p className="mb-2">Crie funções "sem nome" para uso rápido:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> Dobrar = fun(X) -> X * 2 end.
#Fun<erl_eval.44.79398840>
2> Dobrar(5).
10`}
                    </pre>

                    <p className="font-bold mb-2">Exemplo Prático:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`3> Lista = [1, 2, 3].
4> lists:map(fun(X) -> X * 3 end, Lista).
[3, 6, 9] % Multiplica cada elemento por 3`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Funções de Alta Ordem: O Mestre dos Universos</h3>
                    <p className="mb-4">
                        Funções que manipulam outras funções. Os principais <em>super-heróis</em> são:
                    </p>

                    <h4 className="text-lg font-bold mb-2">
                        1. <code>map</code> (Transformação)
                    </h4>
                    <p className="mb-2">Aplica uma função a cada elemento de uma lista:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`quadrados(Lista) ->
    lists:map(fun(X) -> X * X end, Lista).

% Uso:
quadrados([1, 2, 3]). % Retorna [1, 4, 9]`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">
                        2. <code>filter</code> (Seleção)
                    </h4>
                    <p className="mb-2">Filtra elementos com base em uma condição:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`apenas_pares(Lista) ->
    lists:filter(fun(X) -> X rem 2 == 0 end, Lista).

% Uso:
apenas_pares([1, 2, 3, 4]). % Retorna [2, 4]`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">
                        3. <code>foldl</code>/<code>foldr</code> (Acumulação)
                    </h4>
                    <p className="mb-2">Reduz uma lista a um único valor:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`soma_total(Lista) ->
    lists:foldl(fun(X, Acc) -> X + Acc end, 0, Lista).

% Uso:
soma_total([10, 20, 30]). % Retorna 60`}
                    </pre>

                    <p className="font-bold mb-2">Analogia:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <code>map</code> → Uma linha de montagem que transforma peças.
                        </li>
                        <li>
                            <code>filter</code> → Um peneirador que separa grãos.
                        </li>
                        <li>
                            <code>fold</code> → Uma balança que soma pesos.
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Closures: Funções com Memória</h3>
                    <p className="mb-4">
                        Closures são funções que <strong>capturam variáveis do contexto externo</strong>. Pense nelas como "tesouros" que guardam segredos:
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`criar_contador() ->
    Contador = 0,
    fun() ->
        Contador + 1
    end.

% Uso:
1> Contador1 = criar_contador().
2> Contador1().
1
3> Contador1().
1 % Ops! A variável é imutável...`}
                    </pre>

                    <p className="font-bold mb-2">Versão com Estado Mutável (usando processos):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(contador).
-export([iniciar/0, incrementar/1]).

iniciar() ->
    spawn(fun() -> loop(0) end).

incrementar(Pid) ->
    Pid ! {self(), incrementar},
    receive
        {Pid, Valor} -> Valor
    end.

loop(N) ->
    receive
        {From, incrementar} ->
            From ! {self(), N + 1},
            loop(N + 1)
    end.`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Pattern Matching em Funções</h3>
                    <p className="mb-4">
                        Erlang permite múltiplas cláusulas de função com <em>pattern matching</em>:
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(geometria).
-export([area/1]).

% Círculo
area({circulo, Raio}) -> 3.14159 * Raio * Raio;
% Retângulo
area({retangulo, Largura, Altura}) -> Largura * Altura;
% Triângulo
area({triangulo, Base, Altura}) -> (Base * Altura) / 2.`}
                    </pre>

                    <p className="font-bold mb-2">Uso:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> geometria:area({circulo, 5}).
78.53975
2> geometria:area({retangulo, 4, 7}).
28`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Recursão vs. Iteração: A Dança dos Loops</h3>
                    <p className="mb-4">
                        Erlang não tem loops <code>for</code> ou <code>while</code>! A recursão é sua principal ferramenta:
                    </p>

                    <h4 className="text-lg font-bold mb-2">Recursão Clássica (Fibonacci):</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-1) + fib(N-2).`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">Recursão de Cauda (Otimizada):</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`fib_tail(N) -> fib_tail(N, 0, 1).

fib_tail(0, Acc, _) -> Acc;
fib_tail(N, Acc, Next) -> fib_tail(N-1, Next, Acc + Next).`}
                    </pre>

                    <p className="font-bold mb-2">Por que Recursão de Cauda?</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>Evita estouro de pilha.</li>
                        <li>Otimizada pela VM Erlang (BEAM) para usar memória constante.</li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">List Comprehensions: Mágica de Listas</h3>
                    <p className="mb-4">Sintaxe concisa para gerar listas:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`% Lista dos quadrados dos números pares entre 1 e 10:
1> [X * X || X <- lists:seq(1,10), X rem 2 == 0].
[4, 16, 36, 64, 100]

% Combinações de pares:
2> [{X, Y} || X <- [1,2], Y <- [a, b]].
[{1,a}, {1,b}, {2,a}, {2,b}]`}
                    </pre>

                    <p className="font-bold mb-2">Estrutura:</p>
                    <p className="mb-2">
                        <code>[Expressão || Gerador, Filtro, ...]</code>
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Gerador:</strong> <code>X &lt;- Lista</code>
                        </li>
                        <li>
                            <strong>Filtro:</strong> <code>X &gt; 5</code>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Erros Comuns e Dicas</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Erro:</strong> <code>badfun</code> → Você passou algo que não é uma função.
                        </li>
                        <li>
                            <strong>Erro:</strong> <code>function_clause</code> → Pattern matching falhou em uma cláusula de função.
                        </li>
                        <li>
                            <strong>Dica:</strong> Use <code>lists:zip/2</code> para combinar listas em pares.
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Próximos Passos</h3>
                    <p className="mb-2">
                        No <strong>Capítulo 5</strong>, você vai:
                    </p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Dominar a criação de processos leves.</li>
                        <li>Aprender a enviar e receber mensagens entre processos.</li>
                        <li>Construir um sistema concorrente simples.</li>
                    </ol>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 4:</p>
                        <ul className="list-disc pl-6">
                            <li>
                                Crie uma função <code>transformar_lista/2</code> que aceite uma função e uma lista, aplicando a função a cada elemento.
                            </li>
                            <li>
                                Exemplo: <code>transformar_lista(fun(X) -&gt; X + 1 end, [1,2,3])</code> → <code>[2,3,4]</code>.
                            </li>
                        </ul>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Livro: <em>"Functional Programming in Erlang"</em> (Simon Thompson)
                        </li>
                        <li>
                            Documentação:{' '}
                            <a href="https://www.erlang.org/doc/programming_examples/functions.html" className="text-blue-600 hover:underline">
                                erlang.org/doc/programming_examples/functions.html
                            </a>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <div className="border border-gray-300 p-4 rounded mb-4">
                        <h4 className="text-lg font-bold mb-2">Box de Destaque (Para Filósofos do Código):</h4>
                        <p className="font-bold">Por que Programação Funcional?</p>
                        <ul className="list-disc pl-6 mb-2">
                            <li>
                                <strong>Transparência Referencial:</strong> Funções puras sempre retornam o mesmo resultado para os mesmos inputs.
                            </li>
                            <li>
                                <strong>Paralelismo Natural:</strong> Sem estado compartilhado → Fácil distribuição entre núcleos.
                            </li>
                            <li>
                                <strong>Código como Lego:</strong> Funções são blocos reutilizáveis e combináveis.
                            </li>
                        </ul>
                        <p className="italic">[➔ Capítulo 5: Concorrência e Processos Leves – Domine a Arte da Paralelização!]</p>
                    </div>

                    <hr className="my-6" />

                    <SolutionReveal>
                        <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                            {`transformar_lista(Fun, Lista) ->
    lists:map(Fun, Lista).

% Uso:
1> MinhaFun = fun(X) -> X * 2 end.
2> transformar_lista(MinhaFun, [1, 2, 3]).
[2, 4, 6]`}
                        </pre>
                    </SolutionReveal>
                </div>

                <div className="flex justify-between">
                    <Link href="/chapter3">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter5">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>
            </div>
        </div>
    );
}
