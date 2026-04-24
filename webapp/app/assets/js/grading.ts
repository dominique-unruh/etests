import {InteractiveElement, JsonObject} from "./interactive-element.js";

type GradingResult = {
    points?: string,
    report?: string,
    error?: string,
    processing?: true,
}

export class Grading extends InteractiveElement<null, GradingResult> {
    private pointsSpan: HTMLSpanElement;
    private reportSpan: HTMLDivElement;

    constructor() {
        super();
        const reachable = this.getAttribute('reachable');
        this.innerHTML = `<h3>Grading (<span id="grading-points"></span>/${reachable} points)</h3><div id="grading-report"></div>`
        this.pointsSpan = this.getElementsByTagName("span")[0]
        this.reportSpan = this.getElementsByTagName("div")[0]
    }

    protected feedbackExternallyChanged(_: GradingResult, result: GradingResult) {
        if (result.points == null)
            this.pointsSpan.innerText = "?"
        else
            this.pointsSpan.innerText = result.points

        // @ts-ignore
        MathJax.typesetClear([this.reportSpan]);
        if (result.error != null)
            this.reportSpan.innerText = "ERROR: " + result.error
        else if (result.report != null)
            this.reportSpan.innerHTML = result.report
        else if (result.processing == true)
            this.reportSpan.innerText = "⌛"
        else {
            this.reportSpan.innerText = "Internal error";
            console.error("Grader returned result without report or error", result);
        }
        // @ts-ignore
        MathJax.typesetPromise([this.math]);
    }
}

customElements.define("etest-grading", Grading);
