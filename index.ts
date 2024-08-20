type ElmPagesInit = {
    load: (elmLoaded: Promise<unknown>) => Promise<void>;
    flags: unknown;
};

const config: ElmPagesInit = {
    load: async function (elmLoaded) {
        Object.defineProperty(MouseEvent.prototype, "__svgCoordinates", {
            get(this: MouseEvent) {
                const svg = this.currentTarget as SVGSVGElement;
                if (
                    !svg ||
                    !("createSVGPoint" in svg) ||
                    !("getScreenCTM" in svg)
                )
                    return null;

                const pt = svg.createSVGPoint();
                pt.x = this.clientX;
                pt.y = this.clientY;
                var localPoint = pt.matrixTransform(
                    svg.getScreenCTM()!.inverse()
                );
                return localPoint;
            },
        });
        await elmLoaded;
    },
    flags: function () {
        return "";
    },
};

export default config;
