
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

		var label = document.createElement("label");
		var ch = document.createElement("input");
		ch.type = "checkbox";
		ch.onclick = checker(ch, lang);
		if (lang === "python" || lang === "d" || lang === "scala" || lang === "haskell") {
			ch.checked = true;
		} else {
			setDisplay(lang, "none");
		}

		label.appendChild(ch);
		label.appendChild(document.createTextNode(lang));

		morpher.appendChild(label);
	}
}

var scrollsel = -1;
function makeToCScroller(ids) {
	var tocLinks = document.getElementById("toc").getElementsByTagName("a");
	window.onscroll = function () {
		var i;
		for (i = 0; i < ids.length; i++) {
			if (document.getElementById(ids[i]).getBoundingClientRect().top > 10)
				break;
		}
		i--;
		if (i != scrollsel) {
			if (i != -1) tocLinks[i].className = "ts";
			if (scrollsel != -1) tocLinks[scrollsel].className = "";
			scrollsel = i;
		}
	}
}
