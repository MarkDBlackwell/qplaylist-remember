<!--
   See http://trentwalton.com/2013/01/16/windows-phone-8-viewport-fix/

   Keep as first script in <head>.
   -->
<script type="text/javascript">
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
</script>
