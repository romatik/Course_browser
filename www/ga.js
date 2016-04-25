(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-76873820-1', 'auto');
ga('send', 'pageview');

$(document).on('change', 'checkGroup', function(e) {
  ga('send', 'event', 'questionchange', 'question change', $(e.currentTarget).val());
});

$(document).on('change', 'course', function(e) {
  ga('send', 'event', 'coursechange', 'course change', $(e.currentTarget).val());
});

$(document).on('click', 'go', function() {
  ga('send', 'event', 'goButton', 'submit press');
});

  