// Generated by purs version 0.11.5
"use strict";
var InputButton = (function () {
    function InputButton() {

    };
    InputButton.value = new InputButton();
    return InputButton;
})();
var InputCheckbox = (function () {
    function InputCheckbox() {

    };
    InputCheckbox.value = new InputCheckbox();
    return InputCheckbox;
})();
var InputColor = (function () {
    function InputColor() {

    };
    InputColor.value = new InputColor();
    return InputColor;
})();
var InputDate = (function () {
    function InputDate() {

    };
    InputDate.value = new InputDate();
    return InputDate;
})();
var InputDatetime = (function () {
    function InputDatetime() {

    };
    InputDatetime.value = new InputDatetime();
    return InputDatetime;
})();
var InputDatetimeLocal = (function () {
    function InputDatetimeLocal() {

    };
    InputDatetimeLocal.value = new InputDatetimeLocal();
    return InputDatetimeLocal;
})();
var InputEmail = (function () {
    function InputEmail() {

    };
    InputEmail.value = new InputEmail();
    return InputEmail;
})();
var InputFile = (function () {
    function InputFile() {

    };
    InputFile.value = new InputFile();
    return InputFile;
})();
var InputHidden = (function () {
    function InputHidden() {

    };
    InputHidden.value = new InputHidden();
    return InputHidden;
})();
var InputImage = (function () {
    function InputImage() {

    };
    InputImage.value = new InputImage();
    return InputImage;
})();
var InputMonth = (function () {
    function InputMonth() {

    };
    InputMonth.value = new InputMonth();
    return InputMonth;
})();
var InputNumber = (function () {
    function InputNumber() {

    };
    InputNumber.value = new InputNumber();
    return InputNumber;
})();
var InputPassword = (function () {
    function InputPassword() {

    };
    InputPassword.value = new InputPassword();
    return InputPassword;
})();
var InputRadio = (function () {
    function InputRadio() {

    };
    InputRadio.value = new InputRadio();
    return InputRadio;
})();
var InputRange = (function () {
    function InputRange() {

    };
    InputRange.value = new InputRange();
    return InputRange;
})();
var InputReset = (function () {
    function InputReset() {

    };
    InputReset.value = new InputReset();
    return InputReset;
})();
var InputSearch = (function () {
    function InputSearch() {

    };
    InputSearch.value = new InputSearch();
    return InputSearch;
})();
var InputSubmit = (function () {
    function InputSubmit() {

    };
    InputSubmit.value = new InputSubmit();
    return InputSubmit;
})();
var InputTel = (function () {
    function InputTel() {

    };
    InputTel.value = new InputTel();
    return InputTel;
})();
var InputText = (function () {
    function InputText() {

    };
    InputText.value = new InputText();
    return InputText;
})();
var InputTime = (function () {
    function InputTime() {

    };
    InputTime.value = new InputTime();
    return InputTime;
})();
var InputUrl = (function () {
    function InputUrl() {

    };
    InputUrl.value = new InputUrl();
    return InputUrl;
})();
var InputWeek = (function () {
    function InputWeek() {

    };
    InputWeek.value = new InputWeek();
    return InputWeek;
})();
var renderInputType = function (v) {
    if (v instanceof InputButton) {
        return "button";
    };
    if (v instanceof InputCheckbox) {
        return "checkbox";
    };
    if (v instanceof InputColor) {
        return "color";
    };
    if (v instanceof InputDate) {
        return "date";
    };
    if (v instanceof InputDatetime) {
        return "datetime";
    };
    if (v instanceof InputDatetimeLocal) {
        return "datetime-local";
    };
    if (v instanceof InputEmail) {
        return "email";
    };
    if (v instanceof InputFile) {
        return "file";
    };
    if (v instanceof InputHidden) {
        return "hidden";
    };
    if (v instanceof InputImage) {
        return "image";
    };
    if (v instanceof InputMonth) {
        return "month";
    };
    if (v instanceof InputNumber) {
        return "number";
    };
    if (v instanceof InputPassword) {
        return "password";
    };
    if (v instanceof InputRadio) {
        return "radio";
    };
    if (v instanceof InputRange) {
        return "range";
    };
    if (v instanceof InputReset) {
        return "reset";
    };
    if (v instanceof InputSearch) {
        return "search";
    };
    if (v instanceof InputSubmit) {
        return "submit";
    };
    if (v instanceof InputTel) {
        return "tel";
    };
    if (v instanceof InputText) {
        return "text";
    };
    if (v instanceof InputTime) {
        return "time";
    };
    if (v instanceof InputUrl) {
        return "url";
    };
    if (v instanceof InputWeek) {
        return "week";
    };
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType line 29, column 19 - line 52, column 16: " + [ v.constructor.name ]);
};
module.exports = {
    InputButton: InputButton, 
    InputCheckbox: InputCheckbox, 
    InputColor: InputColor, 
    InputDate: InputDate, 
    InputDatetime: InputDatetime, 
    InputDatetimeLocal: InputDatetimeLocal, 
    InputEmail: InputEmail, 
    InputFile: InputFile, 
    InputHidden: InputHidden, 
    InputImage: InputImage, 
    InputMonth: InputMonth, 
    InputNumber: InputNumber, 
    InputPassword: InputPassword, 
    InputRadio: InputRadio, 
    InputRange: InputRange, 
    InputReset: InputReset, 
    InputSearch: InputSearch, 
    InputSubmit: InputSubmit, 
    InputTel: InputTel, 
    InputText: InputText, 
    InputTime: InputTime, 
    InputUrl: InputUrl, 
    InputWeek: InputWeek, 
    renderInputType: renderInputType
};
