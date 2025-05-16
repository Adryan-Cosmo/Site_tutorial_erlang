/** @type {import('next').NextConfig} */
const nextConfig = {
    //output: "export",
    eslint: {
        ignoreDuringBuilds: true,
    },
    typescript: {
        ignoreBuildErrors: true,
    },
    images: {
        unoptimized: true,
    },
    // Adicione o basePath se você estiver hospedando em um subdiretório
    // basePath: '/erlang-tutorial',
    trailingSlash: true,
};

module.exports = nextConfig;
