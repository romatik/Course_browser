setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
    setTimeout(function() {
      if ($('html').attr('class')=='shiny-busy') {
        $('div.busy').show()
      }
    })
  } else {
    $('div.busy').hide()
  }
}, 1);


//this will perform hideLink on launch and hide the link
window.onload = function() {
  hideLink();
};

function hideLink()  
{  
   document.getElementById("link_to_faq1").style.visibility="hidden";  

}

//this function is called on "Submit" button and it waits 3s before displaying the link to user
function showLink(x) {
    setTimeout(function(){
      if(x == 1) {
        document.getElementById("link_to_faq1").style.visibility="visible";
      }
    }, 3000);
}

$.fn.dataTable.ext.errMode = 'none';
