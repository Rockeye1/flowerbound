type ElmPagesInit = {
    load: (elmLoaded: Promise<unknown>) => Promise<void>;
    flags: unknown;
};

const config: ElmPagesInit = {
    load: async function (elmLoaded) {
        await elmLoaded;
    },
    flags: function () {
        return "";
    },
};

export default config;
