function show_cut(id) {
    var left = document.getElementById("left"+id);
    left.className = 'left';
    var right = document.getElementById("right"+id);
    right.className = 'right';
}

function hide_cut (id) {
    var left = document.getElementById("left"+id);
    left.className = '';
    var right = document.getElementById("right"+id);
    right.className = '';
}
var suggnum = 0;

function show_elements(numbering, ids) {
    var posnum = 0;
    var handled = {};
    var ret = null;
    for (var i in ids) {
	var classname = ids[i][0];
	var id = ids[i][1];
	if (handled[id])
	    continue;
	else
	    handled[id] = true;
        var loc = document.getElementById(id);
	if(loc) {
            posnum++;
            var n = loc.parentNode;
            if (numbering) {
                var outer = document.createElement('span');
                var num = document.createElement('span');
                var content = document.createTextNode("" + posnum);
                num.className = 'suggestion';
                num.appendChild(content);
                outer.appendChild(num);
                outer.className = 'outerSuggestion';
                n.insertBefore(outer, loc);
            }
            var highlight = document.createElement('span');
	    highlight.className = classname;
	    highlight.appendChild(loc.cloneNode(true));
	    n.replaceChild(highlight, loc);
            ret = n;
	}
    }
    return ret;
}

function hide_elements(numbering, ids) {
    var handled = {};
    for (var i in ids) {
	var id = ids[i][1];
	if (handled[id])
	    continue;
	else
	    handled[id] = true; 
        var loc = document.getElementById(id);
	if(loc) {
            var highlight = loc.parentNode;
	    var inner = loc.cloneNode(true);
	    var n = highlight.parentNode;
            if (numbering) {
	        var suggest = highlight.previousSibling;
	        n.removeChild(suggest);
            }
	    n.replaceChild(inner, highlight);
	}
    }
}

var last_shown = [];
var last_numbering = null;
var last_cutid = null;

function show_elements_perm (numbering, ids) {
    last_shown = ids;
    last_numbering = numbering;
    var n = show_elements(numbering, ids);
    if (n != null) n.scrollIntoView();
}

function hide_elements_perm () {
    if (last_numbering != null) {
        hide_elements(last_numbering, last_shown);
        last_shown = [];
        last_numbering = null;
    }
}

function show_cut_perm (id) {
    last_cutid = id;
    show_cut(id);
}

function hide_cut_perm () {
    if (last_cutid != null) {
        hide_cut(last_cutid);
        last_cutid = null;
    }
}

function hide_all () {
    hide_elements_perm();
    hide_cut_perm();
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

function hide_feedback_form() {
    var style = document.getElementById('feedback').style;
    if (style.display == 'none') {
	style.display = 'block';
    } else {
	style.display = 'none';
    }
}

function show_more_expr() {
    var style = document.getElementById('more_expr').style;
    if (style.height == 'auto') {
	style.height = 0;
    } else {
	style.height = 'auto';
    }
}

function show_more_cons() {
    var style = document.getElementById('more_cons').style;
    if (style.height == 'auto') {
	style.height = 0;
    } else {
	style.height = 'auto';
    }
}
