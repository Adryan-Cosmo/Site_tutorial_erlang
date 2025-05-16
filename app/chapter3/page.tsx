import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

export default function Chapter3() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter3" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Erlang Tutorial</h1>

                <div className="flex justify-between mb-4">
                    <Link href="/chapter2">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter4">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 3: Sintaxe Básica</h2>
                    <p className="italic mb-4">
                        Domine os tijolos que constroem o universo Erlang! Aqui, você aprenderá a escrever código que dança entre dados imutáveis e recursão
                        elegante.
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Tipos de Dados Primitivos</h3>
                    <p className="mb-4">Erlang é como um alquimista: transforma dados simples em sistemas complexos. Conheça seus elementos básicos:</p>

                    <h4 className="text-lg font-bold mb-2">1. Números (Integers e Floats)</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Inteiros:</strong> Suportam tamanho arbitrário (sim, até 1000 dígitos!).
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`42 % Integer
16#FF % Hexadecimal (255 em decimal)`}
                            </pre>
                        </li>
                        <li>
                            <strong>Floats:</strong> Números decimais.
                            <pre className="bg-gray-800 text-white p-2 rounded mt-1 mb-2">
                                {`3.1415 % Float
2.998e8 % Notação científica (299.800.000)`}
                            </pre>
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">2. Átomos (Atoms)</h4>
                    <p className="mb-2">
                        Constantes literais que representam <strong>nomes</strong> ou <strong>rótulos</strong>. São como <em>hashtags</em> imutáveis:
                    </p>
                    <pre className="bg-gray-800 text-white p-2 rounded mb-4">
                        {`ok % Átomo comum
'Átomos com espaços' % Átomos entre aspas permitem espaços`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">3. Booleanos e Nulos</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <code>true</code>, <code>false</code>: Booleanos (na verdade, átomos especiais).
                        </li>
                        <li>
                            <code>undefined</code>: Representa ausência de valor (também um átomo).
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">4. Listas</h4>
                    <p className="mb-2">
                        Coleções ordenadas de elementos. <strong>Importante:</strong> São implementadas como listas encadeadas.
                    </p>
                    <pre className="bg-gray-800 text-white p-2 rounded mb-4">
                        {`[1, 2, 3] % Lista de inteiros
["Erlang", 42, {pi, 3.14}] % Lista heterogênea`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">5. Tuplas (Tuples)</h4>
                    <p className="mb-2">Estruturas fixas para agrupar dados relacionados.</p>
                    <pre className="bg-gray-800 text-white p-2 rounded mb-4">
                        {`{ok, "Mensagem"} % Tupla com status e detalhe
{erro, 404, "Not Found"}`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">6. Binários (Binaries)</h4>
                    <p className="mb-2">Sequências de bytes brutos (útil para dados de rede ou arquivos).</p>
                    <pre className="bg-gray-800 text-white p-2 rounded mb-4">{`<<72, 101, 108, 108, 111>> % Equivalente a "Hello" em ASCII`}</pre>

                    <h4 className="text-lg font-bold mb-2">7. PIDs e Ports</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>PID:</strong> Identificador único de um processo (ex: <code>&lt;0.128.0&gt;</code>).
                        </li>
                        <li>
                            <strong>Port:</strong> Representa comunicação com recursos externos (como sockets).
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Variáveis e Imutabilidade</h3>
                    <h4 className="text-lg font-bold mb-2">Regra de Ouro: Atribua Uma Vez, Use Sempre</h4>
                    <p className="mb-2">Em Erlang, variáveis são como tatuagens: depois de definidas, não mudam!</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> X = 10. % X é 10
10
2> X = 10. % Pattern matching: ok, X já é 10
10
3> X = 20. % Erro! Não pode reatribuir.
** exception error: no match of right hand side value 20`}
                    </pre>

                    <p className="font-bold mb-2">Pattern Matching Mágico:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`4> {A, B} = {ok, 42}. % Desestruturação
{ok, 42}
5> A.
ok
6> B.
42`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Estruturas de Controle</h3>

                    <h4 className="text-lg font-bold mb-2">1. `if` (Quando Precisar de Condições Simples)</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`classifica_idade(Idade) ->
    if
        Idade < 13 -> crianca;
        Idade < 18 -> adolescente;
        true -> adulto % 'true' age como 'else'
    end.`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">2. `case` (Para Pattern Matching Complexo)</h4>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`temperatura_agua(T) ->
    case T of
        T when T =< 0 -> solido;
        T when T >= 100 -> gas;
        _ -> liquido % '_' é curinga
    end.`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">3. Guardas (Guards)</h4>
                    <p className="mb-2">Cláusulas adicionais para refinar condições:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`max(A, B) when A > B -> A;
max(_, B) -> B.`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Funções e Recursão</h3>
                    <h4 className="text-lg font-bold mb-2">Funções: O Coração de Erlang</h4>
                    <p className="mb-2">Defina funções com múltiplas cláusulas e pattern matching:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(math).
-export([fatorial/1]).

% Caso base
fatorial(0) -> 1;
% Caso recursivo
fatorial(N) when N > 0 -> N * fatorial(N - 1).`}
                    </pre>

                    <p className="font-bold mb-2">Testando no Eshell:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> c("src/math.erl").
{ok, math}
2> math:fatorial(5).
120`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">Recursão de Cauda (Tail Recursion)</h4>
                    <p className="mb-2">Otimização para evitar estouro de pilha:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`fatorial_tail(N) -> fatorial_tail(N, 1).

fatorial_tail(0, Acc) -> Acc;
fatorial_tail(N, Acc) -> fatorial_tail(N - 1, Acc * N).`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Módulos e Exportação de Funções</h3>
                    <h4 className="text-lg font-bold mb-2">Módulos: Organizando o Caos</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Todo arquivo <code>.erl</code> é um módulo.
                        </li>
                        <li>O nome do módulo deve corresponder ao nome do arquivo.</li>
                    </ul>

                    <p className="font-bold mb-2">
                        Exemplo (<code>src/calculadora.erl</code>):
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(calculadora).
-export([soma/2, subtrai/2]). % Exporta funções com aridade

soma(A, B) -> A + B.
subtrai(A, B) -> A - B.`}
                    </pre>

                    <p className="font-bold mb-2">Uso:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> calculadora:soma(5, 3).
8
2> calculadora:subtrai(10, 4).
6`}
                    </pre>

                    <h4 className="text-lg font-bold mb-2">Funções Privadas</h4>
                    <p className="mb-2">
                        Use <code>-export</code> para funções públicas. Funções não exportadas são privadas:
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(segredo).
-export([chave_publica/0]).

chave_publica() -> gerar_chave(42).

% Função privada
gerar_chave(N) -> N * 2.`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Erros Comuns e Dicas</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Erro:</strong> <code>function undefined</code> → Verifique se a função está exportada no módulo.
                        </li>
                        <li>
                            <strong>Erro:</strong> <code>badmatch</code> → Pattern matching falhou (ex: tentar <code>X = 10</code> quando <code>X</code> já é
                            5).
                        </li>
                        <li>
                            <strong>Dica:</strong> Use <code>_</code> para ignorar valores em pattern matching (ex: <code>{'{ok, _} = {ok, 42}'}</code>).
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Próximos Passos</h3>
                    <p className="mb-2">
                        No <strong>Capítulo 4</strong>, você vai:
                    </p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Explorar funções de alta ordem.</li>
                        <li>
                            Aprender sobre closures e <code>fun</code>.
                        </li>
                        <li>Dominar técnicas de programação funcional avançada.</li>
                    </ol>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 3:</p>
                        <ul className="list-disc pl-6">
                            <li>
                                Crie um módulo <code>string_utils</code> com:
                                <ul className="list-disc pl-6">
                                    <li>
                                        <code>reverso/1</code> para inverter strings.
                                    </li>
                                    <li>
                                        <code>palindromo/1</code> para verificar se uma string é igual ao reverso.
                                    </li>
                                </ul>
                            </li>
                            <li>
                                Dica: Use <code>lists:reverse/1</code> e converta strings para listas.
                            </li>
                        </ul>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Livro: <em>"Erlang Programming"</em> (Francesco Cesarini e Simon Thompson)
                        </li>
                        <li>
                            Documentação:{' '}
                            <a href="https://www.erlang.org/doc/reference_manual" className="text-blue-600 hover:underline">
                                erlang.org/doc/reference_manual
                            </a>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <div className="border border-gray-300 p-4 rounded mb-4">
                        <h4 className="text-lg font-bold mb-2">Box de Destaque (Para Pensadores):</h4>
                        <p className="font-bold">Por que Imutabilidade?</p>
                        <ul className="list-disc pl-6 mb-2">
                            <li>
                                <strong>Thread-Safe:</strong> Dados não mudam → Nenhum lock necessário!
                            </li>
                            <li>
                                <strong>Debugging Fácil:</strong> O estado nunca é alterado → Rastreie valores sem surpresas.
                            </li>
                            <li>
                                <strong>Previsibilidade:</strong> Código funcional puro é como matemática: sempre produz o mesmo resultado.
                            </li>
                        </ul>
                        <p className="italic">[➔ Capítulo 4: Programação Funcional em Erlang – Eleve Seu Código a Outro Nível!]</p>
                    </div>

                    <hr className="my-6" />

                    <SolutionReveal>
                        <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                            {`-module(string_utils).
-export([reverso/1, palindromo/1]).

reverso(Str) ->
    lists:reverse(Str).

palindromo(Str) ->
    Str == reverso(Str).`}
                        </pre>
                        <p className="font-bold">Teste:</p>
                        <pre className="bg-gray-800 text-white p-4 rounded">
                            {`1> string_utils:palindromo("radar").
true
2> string_utils:palindromo("erlang").
false`}
                        </pre>
                    </SolutionReveal>
                </div>

                <div className="flex justify-between">
                    <Link href="/chapter2">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter4">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>
            </div>
        </div>
    );
}
