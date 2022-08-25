var typeset = function () {
  MathJax.typesetClear();
  MathJax.typeset();
};
var clearPhrase = function () {
  document.getElementById("phraseInput").value = "";
};
export{typeset, clearPhrase};
