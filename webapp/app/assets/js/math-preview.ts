import {InteractiveElement} from "./interactive-element.js";

export class MathPreview extends InteractiveElement<null, string> {
    private math: HTMLSpanElement;

    constructor() {
        super();
        this.math = document.createElement("span")
        this.math.textContent = "[Preview]"
        this.appendChild(this.math);
        this.classList.add("math-preview");
    }

    protected feedbackExternallyChanged(oldValue: string, newValue: string) {
        console.log("UPDATE", newValue);
        if (newValue == '') newValue = '???';
        // @ts-ignore
        MathJax.typesetClear([this.math]);
        this.math.innerHTML = newValue;
        // @ts-ignore
        MathJax.typesetPromise([this.math]);
    }
}

customElements.define("etest-math-preview", MathPreview);
