
function setDisplay(className, val) {
	var elts = document.getElementsByClassName(className);
	for (var j = 0; j < elts.length; j++) {
		elts[j].style.display = val;
	}
}

function checker(ch, lang) { // force closure capture
	return function () {
		setDisplay(lang, (ch.checked) ? "table-cell" : "none");
	}
}

function makeMorpher(langs) {
	var morpher = document.getElementById("morpher");

	for (var i = 0; i < langs.length; i++) {
		var lang = langs[i];
		var span = document.createElement("span");
		span.className = "cbox";

		var ch = document.createElement("input");
		ch.type = "checkbox";
		ch.onclick = checker(ch, lang);
		if (i < 4) {
			ch.checked = true;
		} else {
			setDisplay(lang, "none");
		}

		span.appendChild(ch);
		span.appendChild(document.createTextNode(lang));

		morpher.appendChild(span);
	}
}
