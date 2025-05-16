import Link from 'next/link';
import Sidebar from '@/components/sidebar';
import { Button } from '@/components/ui/button';
import SolutionReveal from '@/components/solution-reveal';

export default function Chapter2() {
    return (
        <div className="flex">
            <Sidebar currentPage="chapter2" />
            <div className="flex-1 p-4 overflow-y-auto max-h-screen">
                <h1 className="text-3xl font-bold mb-6">Sistemas Distribuídos com Erlang</h1>

                <div className="flex justify-between mb-4">
                    <Link href="/">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter3">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>

                <div className="bg-gray-100 p-6 mb-8">
                    <h2 className="text-2xl font-bold mb-4">Capítulo 2: Ambiente de Desenvolvimento</h2>
                    <p className="italic mb-4">
                        Prepare-se para mergulhar no mundo Erlang! Aqui, você vai transformar seu computador em um laboratório de sistemas distribuídos.
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Instalação e Configuração</h3>
                    <h4 className="text-lg font-bold mb-2">Escolha Sua Arma: Instalando o Erlang</h4>
                    <p className="mb-4">
                        Erlang roda em <strong>Windows</strong>, <strong>Linux</strong> e <strong>macOS</strong>. Seguem os caminhos:
                    </p>

                    <p className="font-bold mb-1">1. Linux (Debian/Ubuntu):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`sudo apt-get update
sudo apt-get install erlang`}
                    </pre>

                    <p className="font-bold mb-1">2. macOS (via Homebrew):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`brew update
brew install erlang`}
                    </pre>

                    <p className="font-bold mb-1">3. Windows:</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Baixe o instalador em{' '}
                            <a href="https://www.erlang.org/downloads" className="text-blue-600 hover:underline">
                                erlang.org/downloads
                            </a>
                            .
                        </li>
                        <li>
                            Siga o assistente (marque <strong>"Add to PATH"</strong> para usar Erlang no terminal).
                        </li>
                    </ul>

                    <p className="font-bold mb-1">4. Docker (Para os Fãs de Containers):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">{`docker run -it erlang:latest`}</pre>

                    <p className="font-bold mb-1">Verificação Mágica:</p>
                    <p className="mb-2">Abra um terminal e digite:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">{`erl -version`}</pre>
                    <p className="mb-4">
                        Se aparecer algo como <code>Erlang/OTP 26 [erts-14.2]</code>, você está pronto! 🎉
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Primeiros Passos com o Eshell</h3>
                    <h4 className="text-lg font-bold mb-2">O Terminal dos Feiticeiros</h4>
                    <p className="mb-4">
                        O <strong>Eshell</strong> (Erlang Shell) é um REPL (<em>Read-Eval-Print Loop</em>) interativo. Experimente:
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> 2 + 2 * 3.
8 % Resultado: 2 + (2*3) = 8
2> io:format("Olá, Mundo!~n").
Olá, Mundo!
ok % 'ok' indica que a função foi executada com sucesso
3> lists:reverse([1,2,3]).
[3,2,1]`}
                    </pre>

                    <p className="font-bold mb-2">Comandos Úteis do Eshell:</p>
                    <div className="overflow-x-auto mb-4">
                        <table className="min-w-full border border-gray-300">
                            <thead>
                                <tr className="bg-gray-200">
                                    <th className="border border-gray-300 p-2">Comando</th>
                                    <th className="border border-gray-300 p-2">O que Faz?</th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>q().</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Sai do Eshell</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>c("arquivo.erl").</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Compila um módulo</td>
                                </tr>
                                <tr>
                                    <td className="border border-gray-300 p-2">
                                        <code>h().</code>
                                    </td>
                                    <td className="border border-gray-300 p-2">Histórico de comandos</td>
                                </tr>
                            </tbody>
                        </table>
                    </div>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio:</p>
                        <p>
                            Calcule o fatorial de 5 usando recursividade diretamente no Eshell!
                            <br />
                            <em>
                                (Dica: Defina uma função temporária com <code>fun Fact(N) {'->'} ... end</code>.)
                            </em>
                        </p>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Instalando o rebar3 em Qualquer SO</h3>
                    <h4 className="text-lg font-bold mb-2">O Mago dos Gerenciadores de Projetos</h4>
                    <p className="mb-4">
                        O <strong>rebar3</strong> é a ferramenta essencial para gerenciar projetos Erlang. Veja como instalá-lo:
                    </p>

                    <p className="font-bold mb-1">1. Linux/macOS (via Script):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`# Baixe e torne o executável:
curl -O https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
# Adicione ao PATH (exemplo para ~/bin):
mkdir -p ~/bin
mv rebar3 ~/bin/
export PATH=$PATH:~/bin # Adicione esta linha ao ~/.bashrc ou ~/.zshrc`}
                    </pre>

                    <p className="font-bold mb-1">2. macOS (via Homebrew):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">{`brew install rebar3`}</pre>

                    <p className="font-bold mb-1">3. Windows (via Chocolatey):</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`# Instale o Chocolatey (se não tiver):
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
# Instale o rebar3:
choco install rebar3`}
                    </pre>

                    <p className="font-bold mb-1">4. Windows (Manual):</p>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Baixe o binário do rebar3 em{' '}
                            <a href="https://www.rebar3.org/" className="text-blue-600 hover:underline">
                                rebar3.org
                            </a>
                            .
                        </li>
                        <li>
                            Extraia o arquivo e adicione o diretório ao <code>PATH</code> do sistema.
                        </li>
                    </ul>

                    <p className="font-bold mb-1">Verifique a Instalação:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">{`rebar3 --version`}</pre>
                    <p className="mb-4">
                        Saída esperada: <code>rebar 3.22.1 on Erlang/OTP 26</code> (versões podem variar).
                    </p>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Estrutura de Projetos em Erlang</h3>
                    <h4 className="text-lg font-bold mb-2">Organização é a Chave da Sanidade</h4>
                    <p className="mb-4">
                        Projetos Erlang seguem a estrutura <strong>OTP</strong> para manter tudo organizado. Um projeto típico com <strong>rebar3</strong> tem:
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`meu_projeto/
├── rebar.config % Configurações de dependências e builds
├── src/
│ ├── meu_projeto.app.src % Metadados da aplicação
│ └── meu_modulo.erl % Código-fonte principal
├── ebin/ % Arquivos compilados (.beam)
└── test/ % Testes automatizados`}
                    </pre>

                    <p className="font-bold mb-2">Passo a Passo para Criar um Projeto:</p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li className="mb-2">
                            Crie um novo projeto:
                            <pre className="bg-gray-800 text-white p-4 rounded mt-2">{`rebar3 new app meu_projeto`}</pre>
                        </li>
                        <li className="mb-2">
                            Navegue até o diretório:
                            <pre className="bg-gray-800 text-white p-4 rounded mt-2">{`cd meu_projeto`}</pre>
                        </li>
                        <li className="mb-2">
                            Compile:
                            <pre className="bg-gray-800 text-white p-4 rounded mt-2">{`rebar3 compile`}</pre>
                        </li>
                    </ol>

                    <p className="font-bold mb-2">
                        Exemplo de Módulo Básico (<code>src/meu_modulo.erl</code>):
                    </p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`-module(meu_modulo).
-export([ola/0]). % Exporta a função 'ola/0' para uso externo

ola() -> io:format("Olá, Mundo Erlang!~n").`}
                    </pre>

                    <p className="font-bold mb-2">Execute no Eshell:</p>
                    <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                        {`1> c("src/meu_modulo.erl"). % Compila o módulo
{ok, meu_modulo}
2> meu_modulo:ola().
Olá, Mundo Erlang!
ok`}
                    </pre>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Erros Comuns e Soluções</h3>
                    <h4 className="text-lg font-bold mb-2">Quando o Feitiço Falha</h4>
                    <ul className="list-disc pl-6 mb-4">
                        <li className="mb-2">
                            <strong>Erro:</strong> <code>Command 'rebar3' not found</code>
                            <br />
                            <strong>Solução:</strong> Verifique se o <code>PATH</code> inclui o diretório do rebar3.
                        </li>
                        <li className="mb-2">
                            <strong>Erro:</strong> <code>undefined function meu_modulo:ola/0</code>
                            <br />
                            <strong>Solução:</strong> Verifique se a função está exportada no módulo.
                        </li>
                        <li className="mb-2">
                            <strong>Erro:</strong> <code>Failed to compile _build/default/lib/meu_projeto/src/meu_modulo.erl</code>
                            <br />
                            <strong>Solução:</strong> Verifique a sintaxe (vírgulas, pontos finais, parênteses).
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Próximos Passos</h3>
                    <p className="mb-2">
                        No <strong>Capítulo 3</strong>, você vai:
                    </p>
                    <ol className="list-decimal pl-6 mb-4">
                        <li>Dominar a sintaxe básica de Erlang.</li>
                        <li>Aprender sobre tipos de dados e imutabilidade.</li>
                        <li>Escrever funções recursivas para substituir loops.</li>
                    </ol>

                    <div className="bg-yellow-100 p-4 mb-4 rounded">
                        <p className="font-bold">Desafio do Capítulo 2:</p>
                        <ul className="list-disc pl-6">
                            <li>
                                Crie um módulo <code>math_fun</code> que exporte uma função <code>cubo/1</code> para calcular o cubo de um número.
                            </li>
                            <li>Compile e teste no Eshell!</li>
                        </ul>
                    </div>

                    <hr className="my-6" />

                    <h3 className="text-xl font-bold mb-2">Referências Técnicas:</h3>
                    <ul className="list-disc pl-6 mb-4">
                        <li>
                            Documentação do rebar3:{' '}
                            <a href="https://www.rebar3.org/" className="text-blue-600 hover:underline">
                                rebar3.org
                            </a>
                        </li>
                        <li>
                            Guia OTP:{' '}
                            <a href="https://www.erlang.org/doc/design_principles/users_guide.html" className="text-blue-600 hover:underline">
                                erlang.org/doc/design_principles/users_guide.html
                            </a>
                        </li>
                    </ul>

                    <hr className="my-6" />

                    <div className="border border-gray-300 p-4 rounded mb-4">
                        <h4 className="text-lg font-bold mb-2">Box de Destaque (Para Hackers):</h4>
                        <p className="font-bold">Por que o rebar3 é Essencial?</p>
                        <ul className="list-disc pl-6 mb-2">
                            <li>
                                <strong>Gerenciamento de Dependências:</strong> Baixe bibliotecas com <code>rebar3 deps</code>.
                            </li>
                            <li>
                                <strong>Builds Automatizados:</strong> Compile, teste e gere releases com um comando.
                            </li>
                            <li>
                                <strong>Integração com OTP:</strong> Crie árvores de supervisão e aplicações complexas sem esforço.
                            </li>
                        </ul>
                        <p className="italic">[➔ Capítulo 3: Sintaxe Básica – Domine os Fundamentos!]</p>
                    </div>

                    <hr className="my-6" />

                    <SolutionReveal>
                        <pre className="bg-gray-800 text-white p-4 rounded mb-4">
                            {`-module(math_fun).
-export([cubo/1]).

cubo(X) -> X * X * X.`}
                        </pre>
                        <p className="font-bold">Teste:</p>
                        <pre className="bg-gray-800 text-white p-4 rounded">
                            {`1> c("src/math_fun.erl").
{ok, math_fun}
2> math_fun:cubo(3).
27`}
                        </pre>
                    </SolutionReveal>
                </div>

                <div className="flex justify-between">
                    <Link href="/">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            ← Previous
                        </Button>
                    </Link>
                    <Link href="/chapter3">
                        <Button variant="outline" className="bg-green-600 text-white hover:bg-green-700">
                            Next →
                        </Button>
                    </Link>
                </div>
            </div>
        </div>
    );
}
