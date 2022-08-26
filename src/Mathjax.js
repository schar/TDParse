var typeset = function () {
  MathJax.typesetClear();
  MathJax.typeset();
};
var clearPhrase = function () {
  document.getElementById("phraseInput").value = "";
};
var lexFeedback = function (m) {
  document.getElementById("lexFeedback").textContent = m;
};
export{typeset, clearPhrase, lexFeedback};
