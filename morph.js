function checker(ch, lang) { // force closure capture
	return function () {
		var elts = document.getElementsByClassName(lang);
		for (var j = 0; j < elts.length; j++) {
			elts[j].style.display = (ch.checked) ? "table-cell" : "none";
		}
	}
}

function makeMorpher(langs) {
	var morpher = document.getElementById("morpher");

	for (var i = 0; i < langs.length; i++) {
		var span = document.createElement("span");
		span.className = "cbox";

		var ch = document.createElement("input");
		ch.type = "checkbox";
		ch.checked = true;
		var lang = langs[i];
		ch.onclick = checker(ch, lang);

		span.appendChild(ch);
		span.appendChild(document.createTextNode(langs[i]));

		morpher.appendChild(span);
	}
}
