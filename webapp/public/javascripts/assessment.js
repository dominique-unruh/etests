function log_error(...data) {
    console.error(...data)
    errorTextarea = document.getElementById("errors")
    errorTextarea.textContent = data.toString()+"\n"
    document.getElementById('errors-section').style.display = 'block';
}

function updateState(elementName, content) {
    if (elementName == null)
        log_error("In elementEvent, elementName is null (internal eerror).")
    state[elementName] = content
    sendState()
}

function loadReference() {
    function failCallback(obj, statusMessage) {
        log_error("Failed to load reference solution")
        console.log("Failed AJAX call: ", state, obj, statusMessage)
    }
    $.ajax(jsRoutes.controllers.AssessmentController.loadReference(assessmentName).url,
        {method: "GET", dataType: 'json', headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(doActions)
}

function loadAnswers() {
    function failCallback(obj, statusMessage) {
        log_error("Failed to load student answers")
        console.log("Failed AJAX call: ", state, obj, statusMessage)
    }
    let regno = document.getElementById("registration").value
    if (regno == null || regno === "") {
        log_error("Not student registration number specified.")
        document.getElementById("registration").focus()
        return
    }
    $.ajax(jsRoutes.controllers.AssessmentController.loadAnswers(assessmentName, regno).url,
        {method: "GET", dataType: 'json', headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(doActions)
}

function doActions(json) {
    for (let action of json) {
        console.log("Invoking callback " + action.callback + " with:", action.data)
        if (window[action.callback] == null)
            log_error("Server invoked nonexisting callback " + action.callback)
        else
            window[action.callback](action.data)
    }
}

function sendState() {
    clearErrors()
    function failCallback(obj, statusMessage) {
        log_error("Failed to send updated state to server")
        console.log("Failed AJAX call: ", state, obj, statusMessage)
    }
    $.ajax(jsRoutes.controllers.AssessmentController.updateAction(assessmentName).url,
        {method: "POST", dataType: 'json', data: JSON.stringify(state), contentType: 'application/json',
            headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(doActions)
}

function randomStudent() {
    clearErrors()
    function failCallback(obj, statusMessage) {
        log_error("Failed to get random student ID")
        console.log("Failed AJAX call: ", state, obj, statusMessage)
    }
    function successCallback(json) {
        document.getElementById("registration").value = json['registration']
        loadAnswers()
    }
    $.ajax(jsRoutes.controllers.AssessmentController.randomStudent(assessmentName).url,
        {method: "GET", dataType: 'json', headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(successCallback)
}


function clearErrors() {
    document.getElementById('errors').textContent = ''
    document.getElementById('errors-section').style.display = 'none'
}

function showDynexitePdf() {
    let regno = document.getElementById("registration").value
    let url = jsRoutes.controllers.AssessmentController.dynexitePdf(regno).url
    window.open(url, "_blank")
}

function showDynexiteLink() {
    let regno = document.getElementById("registration").value
    let url = jsRoutes.controllers.AssessmentController.dynexiteLink(regno).url
    window.open(url, "_blank")
}

function dynexiteAnswers() {
    clearErrors()
    function failCallback(obj, statusMessage) {
        log_error("Failed to get dynexite answers")
        console.log("Failed AJAX call: ", state, obj, statusMessage)
    }
    function successCallback(text) {
        log_error(text)
    }
    let regno = document.getElementById("registration").value
    if (regno == null || regno === "") {
        log_error("Not student registration number specified.")
        document.getElementById("registration").focus()
        return
    }
    $.ajax(jsRoutes.controllers.AssessmentController.dynexiteAnswers(assessmentName, regno).url,
        {method: "GET", headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(successCallback)
}

