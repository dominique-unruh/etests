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
    $.ajax(jsRoutes.controllers.AssessmentController.loadReference(examName, assessmentName).url,
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
    if (regno !== regno.trim()) {
        log_error("Registration number contains leading/trailing spaces.")
        document.getElementById("registration").focus()
        return
    }
    $.ajax(jsRoutes.controllers.AssessmentController.loadAnswers(examName, assessmentName, regno).url,
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

// Track if an AJAX call for send state is currently in progress
let isAjaxInProgress = false;
// Track if there's a pending state update waiting to be sent
let hasPendingUpdate = false;


function sendState() {
    clearErrors();
    document.getElementById("working").innerText = "(loading)"

    if (isAjaxInProgress) {
        hasPendingUpdate = true;
        return;
    }

    performSendState();
}



function performSendState() {
    document.getElementById("working").innerText = "(loading)"
    isAjaxInProgress = true;
    hasPendingUpdate = false; // Clear pending flag since we're processing now

    function failCallback(obj, statusMessage) {
        log_error("Failed to send updated state to server");
        console.log("Failed AJAX call: ", state, obj, statusMessage);
        document.getElementById("working").innerText = "(loading failed)"
        isAjaxInProgress = false;
        if (hasPendingUpdate)
            performSendState();
        else
            setTimeout(performSendState, 15000);
    }

    function successCallback(json) {
        document.getElementById("working").innerText = ""
        isAjaxInProgress = false;
        if (hasPendingUpdate)
            performSendState();
        doActions(json);
    }

    $.ajax(jsRoutes.controllers.AssessmentController.updateAction(examName, assessmentName).url, {
        method: "POST",
        dataType: 'json',
        data: JSON.stringify(state),
        contentType: 'application/json',
        timeout: 10000,
        headers: {'CSRF-Token': csrfToken}
    })
        .fail(failCallback)
        .done(successCallback);
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
    $.ajax(jsRoutes.controllers.AssessmentController.randomStudent(examName).url,
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
    let url = jsRoutes.controllers.AssessmentController.dynexitePdf(examName, regno).url
    window.open(url, "_blank")
}

function showDynexiteLink() {
    let regno = document.getElementById("registration").value
    let url = jsRoutes.controllers.AssessmentController.dynexiteLink(examName, regno).url
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
    $.ajax(jsRoutes.controllers.AssessmentController.dynexiteAnswers(examName, assessmentName, regno).url,
        {method: "GET", headers: {'CSRF-Token': csrfToken}})
        .fail(failCallback)
        .done(successCallback)
}

function setCookie(name, value) {
    // const expires = new Date();
    // expires.setTime(expires.getTime() + (days * 24 * 60 * 60 * 1000));
    document.cookie = name + '=' + encodeURIComponent(value)/* + ';expires=' + expires.toUTCString()*/ + ';path=/';
}


function getCookie(name) {
    const nameEQ = name + '=';
    const ca = document.cookie.split(';');
    for (let i = 0; i < ca.length; i++) {
        let c = ca[i];
        while (c.charAt(0) === ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) === 0) {
            return decodeURIComponent(c.substring(nameEQ.length, c.length));
        }
    }
    return null;
}

function onLoad() {
    document.getElementById("registration").value = getCookie("registrationNumber");
}