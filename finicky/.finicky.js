export default {
    defaultBrowser: 'Google Chrome',
    rewrite: [
        {
            match: (url) => url.host === 'open.spotify.com',
            url: (url) => {
                const parts = url.pathname
                    .split('/')
                    .filter((p) => p && !p.startsWith('intl-'));
                const [type, id] = parts;
                return `spotify:${type}:${id}`;
            },
        },
    ],
    handlers: [
        {
            match: (url) => url.protocol === 'spotify:',
            browser: 'Spotify',
        },
    ],
};
