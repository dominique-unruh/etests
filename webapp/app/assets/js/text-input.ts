import {InteractiveElement} from "./interactive-element.js";

export class TextInput extends InteractiveElement<string, null> {
    private inputField: HTMLInputElement;

    constructor() {
        super("");
        this.inputField = document.createElement("input")
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