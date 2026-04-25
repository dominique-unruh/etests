import {InteractiveElement} from "./interactive-element.js";

export class MultipleChoice extends InteractiveElement<string, null> {
    private element: HTMLInputElement | HTMLSelectElement | HTMLFieldSetElement;
    private choiceStyle: string
    private label: string
    private options: string[] = []
    private optionLabels: Map<string, string> = new Map()
    private radios: Map<string, HTMLInputElement> = new Map()

    constructor() {
        super("");
        this.choiceStyle = this.getAttribute("choicestyle");
        const optionsList = JSON.parse(this.getAttribute("options"))
        for (const [name,label] of optionsList) {
            this.options.push(name)
            this.optionLabels.set(name, label)
        }

        this.label = this.getAttribute("label") || "";
        if (this.choiceStyle == "checkbox")
            this.initCheckbox();
        else if (this.choiceStyle == "select")
            this.initSelect();
        else if (this.choiceStyle == "radio")
            this.initRadio();
        else {
            this.innerText = `[MULTIPLE CHOICE NOT IMPLEMENTED FOR STYLE ${this.choiceStyle}]`
            console.error("Unknown style", this.choiceStyle)
        }
        // console.log("MC", this.id, this.choiceStyle, this.label, this.options, this.optionLabels)
    }

    private initRadio() {
        const fieldSet = document.createElement("fieldset")
        const THIS = this

        function addOption(name: string, label: string) {
            const labelElement = document.createElement("label");
            labelElement.innerHTML = " " + label
            const radioButton = document.createElement('input');
            radioButton.type = 'radio';
            radioButton.value = name;
            radioButton.name = "radio-button-" + THIS.id
            labelElement.prepend(radioButton)
            fieldSet.appendChild(labelElement);
            fieldSet.append(" ")
            radioButton.addEventListener("change", event => {
                if (radioButton.checked) THIS.content = name })
            THIS.radios.set(name, radioButton)
        }

        addOption("", "― not selected ―")
        this.radios.get("").checked = true;
        for (const option of this.options)
            addOption(option, this.optionLabels.get(option))
        this.appendChild(fieldSet)
        this.element = fieldSet;
    }

    private initSelect() {
        const element = document.createElement("select")
        element.id = this.id + "@input"
        element.addEventListener("change", event => {
            this.updateContent(element.value)
        })
        this.appendChild(element)
        const notSelected = document.createElement("option");
        notSelected.innerText = "― not selected ―"
        notSelected.value = '';
        element.add(notSelected)
        for (const optionName of this.options) {
            const optionElement = document.createElement("option")
            optionElement.value = optionName
            optionElement.innerHTML = this.optionLabels.get(optionName)
            element.add(optionElement)
        }
        this.element = element;
    }

    private initCheckbox() {
        const element = document.createElement("input")
        element.type = "checkbox"
        element.id = this.id + "@input"
        element.indeterminate = true;
        element.addEventListener("change", event => {
            this.updateContent(element.checked ? this.options[0] : this.options[1])
        })
        this.appendChild(element)
        if (this.label != null && this.label != "") {
            const label = document.createElement("label");
            this.append("\u00A0")
            this.append(label)
            label.setAttribute("for", element.id)
            label.innerHTML = this.label
        }
        this.element = element;
    }

    protected contentExternallyChanged(oldValue: string, newValue: string) {
        if (this.choiceStyle == "checkbox") {
            const element = this.element as HTMLInputElement
            if (newValue == this.options[0]) {
                element.indeterminate = false;
                element.checked = true;
            } else if (newValue == this.options[1]) {
                element.indeterminate = false;
                element.checked = false;
            } else
                element.indeterminate = true;
        } else if (this.choiceStyle == "select")
            (this.element as HTMLSelectElement).value = newValue;
        else if (this.choiceStyle == "radio") {
            const radio = this.radios.get(newValue)
            if (radio == null)
                this.radios.get("").checked = true;
            else
                radio.checked = true;
        } else {
            this.showError("Internal error: Invalid multiple choice style "+this.choiceStyle)
        }
    }
}


customElements.define("etest-multiple-choice", MultipleChoice);