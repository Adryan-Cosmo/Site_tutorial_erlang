import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';

export default function Home() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter1" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Sistemas Distribuídos com Erlang</h1>

                <div className="flex justify-between mb-4">
                    <div></div> {/* Espaço vazio para manter o alinhamento */}
                    <Link href="/chapter2">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 1: Introdução à Linguagem Erlang</h2>
                    <p className="italic mb-4">Por que aprender Erlang? Imagine construir sistemas que nunca param, mesmo que o mundo desabe!</p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Histórico e Contexto</h3>
                    <h4 className="text-lg font-bold mb-2">Anos 1980: Nasce um Herói das Telecomunicações</h4>
                    <p className="mb-4">
                        Nos laboratórios da <strong>Ericsson</strong>, um desafio assombrava os engenheiros: como criar sistemas telefônicos que funcionassem{' '}
                        <strong>24/7</strong>, sem falhas catastróficas? A resposta foi a Erlang, uma linguagem desenhada para ser <strong>concorrente</strong>,{' '}
                        <strong>distribuída</strong> e <strong>tolerante a falhas</strong>. Em 1998, a Ericsson liberou-a como código aberto, e hoje ela
                        alimenta gigantes como o <strong>WhatsApp</strong> (que processa <strong>100 bilhões de mensagens diárias</strong> com apenas 50
                        engenheiros!).
                    </p>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Curiosidade Nerd:</p>
                        <p>
                            O nome "Erlang" é uma homenagem dupla: ao matemático <strong>Agner Krarup Erlang</strong> (pioneiro em teoria de filas) e a um
                            trocadilho interno: <em>"ERicsson LANGuage"</em> 😉.
                        </p>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Por que Erlang? O Superpoder dos Sistemas Imortais</h3>

                    <h4 className="text-lg font-bold mb-2">1. Tolerância a Falhas como Filosofia</h4>
                    <p className="mb-2">
                        Erlang nasceu em ambientes onde <strong>falhas são inevitáveis</strong>, mas <strong>interrupções são proibidas</strong>. Pense em:
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Jogos online:</strong> Se um servidor travar, Erlang reinicia apenas a parte afetada, sem derrubar milhões de jogadores.
                        </li>
                        <li>
                            <strong>Sistemas bancários:</strong> Transações críticas continuam mesmo durante atualizações.
                        </li>
                    </ul>

                    <p className="font-bold mb-2">Como?</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Processos isolados:</strong> Se um processo falhar, outros não são afetados (como compartimentos estanques em um navio).
                        </li>
                        <li>
                            <strong>Supervisores automáticos:</strong> Processos "pais" monitoram e reiniciam filhos automaticamente.
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">2. Concorrência em Escala Industrial</h4>
                    <p className="mb-2">
                        Enquanto Java ou Python lutam com <em>threads</em> pesadas e <em>deadlocks</em>, Erlang usa <strong>processos leves</strong> (geridos
                        pela VM BEAM, não pelo sistema operacional).
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Exemplo:</strong> 2 milhões de processos podem rodar em uma máquina comum, consumindo apenas{' '}
                            <strong>2-3 KB de RAM cada</strong>.
                        </li>
                        <li>
                            <strong>Analogia:</strong> É como ter um exército de formigas trabalhando em paralelo, sem esbarrar umas nas outras.
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">3. Mensagens vs. Memória Compartilhada</h4>
                    <p className="mb-2">
                        Erlang evita os bugs clássicos de concorrência (como condições de corrida) usando <strong>troca assíncrona de mensagens</strong>.
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-2">
                        {`% Processo A envia uma mensagem para o Processo B:
Pid_B ! {mensagem, "Olá, mundo!"}.

% Processo B recebe a mensagem:
receive
    {mensagem, Texto} -> io:format("Recebido: ~s~n", [Texto])
end.`}
                    </pre>
                    <p className="italic mb-4">Nenhum lock, nenhum mutex, apenas comunicação direta!</p>

                    <h4 className="text-lg font-bold mb-2">4. Hot Code Swapping: Atualizações em Voo</h4>
                    <p className="mb-2">Imagine atualizar o motor de um avião enquanto ele voa. Em Erlang, você pode:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Modificar o código de um sistema <strong>em execução</strong> sem downtime.
                        </li>
                        <li>
                            <strong>Caso de uso real:</strong> Operadoras de telecomunicações atualizavam sistemas telefônicos sem desconectar chamadas.
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Principais Características e Paradigmas</h3>

                    <h4 className="text-lg font-bold mb-2">Programação Funcional com Propósito</h4>
                    <p className="mb-2">Erlang é funcional, mas sem radicalismo. Foca em:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Imutabilidade:</strong> Dados nunca mudam (evita efeitos colaterais).
                        </li>
                    </ul>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-2">
                        {`% Funciona:
Lista = [1, 2, 3],
NovaLista = Lista ++ [4].  % Cria uma nova lista, não modifica a original.

% Falha:
X = 5,
X = 10.  % Erro: "no match of right hand side value 10"`}
                    </pre>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Recursão:</strong> Substitui loops por funções que chamam a si mesmas.
                        </li>
                    </ul>

                    <h4 className="text-lg font-bold mb-2">Modelo de Ator: Cada Processo é um Universo</h4>
                    <p className="mb-2">
                        Cada processo em Erlang é um <strong>ator</strong> autônomo:
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>Tem seu próprio estado.</li>
                        <li>Comunica-se apenas por mensagens.</li>
                        <li>Se crashar, não contamina os outros.</li>
                    </ul>
                    <p className="mb-4">
                        <strong>Analogia:</strong> Uma cidade onde todos moram em casas separadas e se comunicam por cartas. Se uma casa pegar fogo, o resto da
                        cidade não queima!
                    </p>

                    <h4 className="text-lg font-bold mb-2">OTP: O Kit de Ferramentas dos Deuses</h4>
                    <p className="mb-2">
                        A <strong>Open Telecom Platform</strong> (OTP) é um conjunto de bibliotecas e padrões que tornam Erlang <strong>indestrutível</strong>.
                        Principais componentes:
                    </p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>GenServer:</strong> Modelo pronto para servidores genéricos.
                        </li>
                        <li>
                            <strong>Supervisor:</strong> Reinicia processos filhos automaticamente.
                        </li>
                        <li>
                            <strong>Application:</strong> Estrutura para sistemas complexos.
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Comparação com Outras Linguagens</h3>
                    <div className="overflow-x-auto mb-4">
                        <table className="min-w-full border border-gray-300">
                            <thead>
                                <tr className="bg-gray-200">
                                    <th className="border border-gray-300 p-2">Característica</th>
                                    <th className="border border-gray-300 p-2">Erlang</th>
                                    <th className="border border-gray-300 p-2">Java</th>
                                    <th className="border border-gray-300 p-2">Python</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Concorrência</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">Processos leves (BEAM)</td>
                                    <td className="border border-gray-300 p-2">Threads do SO</td>
                                    <td className="border border-gray-300 p-2">Threads (limitadas pelo GIL)</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Tolerância a Falhas</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">Recuperação automática</td>
                                    <td className="border border-gray-300 p-2">Exceções manuais</td>
                                    <td className="border border-gray-300 p-2">Frágil</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Uso de Memória</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">~2 KB por processo</td>
                                    <td className="border border-gray-300 p-2">~1 MB por thread</td>
                                    <td className="border border-gray-300 p-2">~1 MB por thread</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <strong>Caso de Uso Ideal</strong>
                                    </td>
                                    <td className="border border-gray-300 p-2">Sistemas distribuídos</td>
                                    <td className="border border-gray-300 p-2">Apps corporativas</td>
                                    <td className="border border-gray-300 p-2">Scripting/Web</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Casos de Sucesso que Provam a Magia</h3>
                    <ol className="list-decimal pl-6 mb-4">
                        <li className="mb-2">
                            <strong>WhatsApp:</strong>
                            <ul className="list-disc pl-6 mb-1">
                                <li>
                                    <strong>1 bilhão de usuários ativos</strong> com 99.9% de uptime.
                                </li>
                                <li>
                                    <strong>2 milhões de conexões por servidor</strong> (em 2016).
                                </li>
                                <li>
                                    <strong>Segredo:</strong> Processos leves + troca de mensagens eficiente.
                                </li>
                            </ul>
                        </li>
                        <li className="mb-2">
                            <strong>RabbitMQ:</strong>
                            <ul className="list-disc pl-6 mb-1">
                                <li>
                                    Sistema de mensagens usado por empresas como <strong>Tesla</strong> e <strong>Atlassian</strong>.
                                </li>
                                <li>Garante entrega de mensagens mesmo em falhas de rede.</li>
                            </ul>
                        </li>
                        <li className="mb-2">
                            <strong>Discord:</strong>
                            <ul className="list-disc pl-6 mb-1">
                                <li>
                                    Combina Erlang com Elixir para suportar <strong>140 milhões de usuários mensais</strong>.
                                </li>
                            </ul>
                        </li>
                    </ol>

                    <div className="bg-blue-100 p-4 mb-4 rounded">
                        <p className="italic font-bold">"Erlang não evita falhas; ela as abraça, aprende com elas e se torna mais forte."</p>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Próximos Passos</h3>
                    <p className="mb-2">
                        No <strong>Capítulo 2</strong>, você vai:
                    </p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Instalar Erlang em seu sistema.</li>
                        <li>
                            Brincar com o <strong>Eshell</strong> (o terminal interativo).
                        </li>
                        <li>Escrever seu primeiro módulo e função.</li>
                    </ol>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio Prévio:</p>
                        <ul className="list-disc pl-6">
                            <li>
                                Acesse{' '}
                                <a href="https://www.erlang.org/" className="text-blue-600 hover:underline">
                                    erlang.org
                                </a>{' '}
                                e instale Erlang.
                            </li>
                            <li>
                                Digite <code className="bg-gray-200 px-1 rounded">erl</code> no terminal e tente calcular{' '}
                                <code className="bg-gray-200 px-1 rounded">2 + 2</code> no Eshell.
                            </li>
                        </ul>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Acadêmicas e Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            <strong>Livro:</strong> <em>"Programming Erlang: Software for a Concurrent World"</em> (Joe Armstrong, 2013) – o "pai" da Erlang
                            explica sua criação.
                        </li>
                        <li>
                            <strong>Paper:</strong> <em>"Making reliable distributed systems in the presence of software errors"</em> (Tese de doutorado de Joe
                            Armstrong, 2003).
                        </li>
                        <li>
                            <strong>Documentação:</strong>{' '}
                            <a href="https://www.erlang.org/docs" className="text-blue-600 hover:underline">
                                erlang.org/doc
                            </a>{' '}
                            – guias oficiais e exemplos.
                        </li>
                    </ul>

                    <p className="italic mb-4">[➔ Capítulo 2: Ambiente de Desenvolvimento – Prepare-se para Codar!]</p>

                    <hr className="my-6" />

                    <div className="border border-gray-300 p-4 rounded mb-4">
                        <h4 className="text-lg font-bold mb-2">Box de Destaque (Para Curiosos):</h4>
                        <p className="font-bold">Por que o WhatsApp escolheu Erlang?</p>
                        <p className="mb-2">Em 2009, o WhatsApp precisava escalar rapidamente com uma equipe pequena. Erlang permitiu:</p>
                        <ul className="list-disc pl-6 mb-2">
                            <li>
                                <strong>Baixo custo:</strong> 1 servidor Erlang substituía 30 servidores Java.
                            </li>
                            <li>
                                <strong>Alta disponibilidade:</strong> Atualizações sem downtime.
                            </li>
                            <li>
                                <strong>Simplicidade:</strong> Código conciso para lógica complexa.
                            </li>
                        </ul>
                        <p className="italic">
                            <strong>Resultado:</strong> Vendido ao Facebook por US$ 19 bilhões em 2014. 🚀
                        </p>
                    </div>
                </div>

                <div className="flex justify-between">
                    <div></div> {/* Espaço vazio para manter o alinhamento */}
                    <Link href="/chapter2">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>
            </div>
        </div>
    );
}
