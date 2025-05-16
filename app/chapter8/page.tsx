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

                    {/* Conteúdo do capítulo... */}

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
