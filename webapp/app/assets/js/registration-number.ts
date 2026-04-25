import {InteractiveElement} from "./interactive-element.js";
import {getCookie, setCookie} from "./utils.js";

export class RegistrationNumber extends InteractiveElement<string, null> {
    private inputField: HTMLInputElement;

    constructor() {
        super("");
        this.inputField = document.createElement("input")
        this.appendChild(this.inputField);
        const cookie = getCookie('registrationNumber')
        // console.log("Cookie:", cookie)
        if (cookie != null) {
            this.inputField.value = cookie;
            this.content = cookie;
        }
        this.inputField.addEventListener("input", _event => {
            const regno = this.inputField.value.trim()
            setCookie('registrationNumber', regno);
            this.updateContent(regno)
            // @ts-ignore
            window.stateManager.askForAnswers('student')
        })
    }

    protected contentExternallyChanged(oldValue: string, newValue: string) {
        this.inputField.value = newValue;
    }
}


customElements.define("etest-registration-number", RegistrationNumber);