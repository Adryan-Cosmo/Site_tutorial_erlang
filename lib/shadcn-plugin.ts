export function shadcnPlugin() {
    return function ({ addUtilities }: any) {
        addUtilities({
            '.example-utility': {
                display: 'flex',
            },
        });
    };
}
