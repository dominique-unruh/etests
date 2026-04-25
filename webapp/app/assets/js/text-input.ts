import {InteractiveElement} from "./interactive-element.js";

export class TextInput extends InteractiveElement<string, null> {
    private inputField: HTMLInputElement | HTMLTextAreaElement;

    constructor() {
        super("");
        const rows = this.getAttribute("rows")!=null ? parseInt(this.getAttribute("rows")) : 1
        const columns = this.getAttribute("columns")!=null ? parseInt(this.getAttribute("columns")) : 15
        if (rows == 1) {
            this.inputField = document.createElement("input")
            this.inputField.setAttribute("size", columns.toString());
        } else {
            this.inputField = document.createElement("textarea")
            this.inputField.setAttribute("cols", columns.toString());
            this.inputField.setAttribute("rows", rows.toString());
        }
        this.appendChild(this.inputField);
        this.inputField.addEventListener("input", _event => {
            this.updateContent(this.inputField.value)
        })
    }

    protected contentExternallyChanged(oldValue: string, newValue: string) {
        this.inputField.value = newValue;
    }
}


customElements.define("etest-text-input", TextInput);