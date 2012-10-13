function display_info(id) {
    var elem = document.getElementById(id);
    elem.style.display = 'block';
}
function hide(id) {
    var elem = document.getElementById(id);
    elem.style.display = 'none';
}
var suggnum = 0;

function show_elements(classname, ids) {
    var posnum = 0;
    var handled = {};
    for (var i in ids) {
	var id = ids[i];
	if (handled[id])
	    continue;
	else
	    handled[id] = true;
        var loc = document.getElementById(id);
	if(loc) {
            posnum++;
            var n = loc.parentNode;
            var outer = document.createElement('span');
            var num = document.createElement('span');
            var highlight = document.createElement('span');
            var content = document.createTextNode("" + posnum);
            num.className = 'suggestion';
            num.appendChild(content);
            outer.appendChild(num);
            outer.className = 'outerSuggestion';
	    highlight.className = classname;
	    highlight.appendChild(loc.cloneNode(true));
            n.insertBefore(outer, loc);
	    n.replaceChild(highlight, loc);
	}
    }
}

function hide_elements(ids) {
    var handled = {};
    for (var i in ids) {
	var id = ids[i];
	if (handled[id])
	    continue;
	else
	    handled[id] = true; 
        var loc = document.getElementById(id);
	if(loc) {
            var highlight = loc.parentNode;
	    var inner = loc.cloneNode(true);
	    var n = highlight.parentNode;
	    var suggest = highlight.previousSibling;
	    n.removeChild(suggest);
	    n.replaceChild(inner, highlight);
	}
    }
}

function numberSuggestions() {
/*    var spans = document.getElementsByTagName('span');
    var constraints = new Array();
    var c = 0;
    for (var i in spans) {
        var span = spans[i];
        if (span.className == 'missingConstraint' || span.className == 'wrongConstraint') {
            constraints[c++] = span;
        }
    }
    for (var i in constraints) {
        var span = constraints[i];
        suggnum++;
        var n = span.parentNode;
        var outer = document.createElement('span');
        var num = document.createElement('span');
        var content = document.createTextNode("" + suggnum);
        num.className = 'suggestion';
        num.appendChild(content);
        outer.appendChild(num);
        outer.className = 'outerSuggestion';
        n.insertBefore(outer, span);
    }*/
}


