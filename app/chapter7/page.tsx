import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

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
                    <p className="italic mb-4">Construa sistemas que sobrevivem a falhas de rede, nós travados e até mesmo a apocalipses zumbis!</p>

                    {/* Conteúdo do capítulo... */}

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
                            Documentação:{' '}
                            <a href="https://www.erlang.org/doc/design_principles/distributed_applications" className="text-blue-600 hover:underline">
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
                                <strong>Tolerância a Partições:</strong> CAP theorem? Erlang escolhe <strong>Disponibilidade + Tolerância a Falhas</strong>.
                            </li>
                            <li>
                                <strong>Protocolo de Consenso:</strong> Bibliotecas como <code>pg2</code> (Process Groups) sincronizam estados automaticamente.
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
    );
}
