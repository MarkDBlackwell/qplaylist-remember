/* Copyright (C) 2018 Mark D. Blackwell.
    All rights reserved.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
*/

/* See:
http://trentwalton.com/2013/01/16/windows-phone-8-viewport-fix/

The purpose here, quoting from the article, is:

"to get responsive layouts to render properly in IE10[,]
for both Snap Mode and Windows Phone 8."

Keep this as the first script in <head>.
*/

if (navigator.userAgent.match(/IEMobile\/10\.0/)) {
	var msViewportStyle = document.createElement('style');
	msViewportStyle.appendChild(
		document.createTextNode(
		'@-ms-viewport{width:auto!important}'
		)
	);
	document.getElementsByTagName("head")[0].
	appendChild(msViewportStyle);
}
