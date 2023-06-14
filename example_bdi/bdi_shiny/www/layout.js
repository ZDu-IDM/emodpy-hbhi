
//==========================================
// resize panel base on header's height
//==========================================
function resizePanels() {
  var header = document.getElementsByClassName("navbar")[0];
  var mainPanels = document.getElementsByClassName("mainPanel");
  var sidePanels = document.getElementsByClassName("sidePanel");
  
  if (window.outerWidth > 768)
    for (var i=0; i < mainPanels.length; ++i) {
      mainPanels[i].style.height = "calc(100vh - " + (header.offsetHeight + 110) + "px)";
      mainPanels[i].style.overflowY = "auto";
      mainPanels[i].style.marginBottom = "0px";
      sidePanels[i].style.height = "calc(100vh - " + (header.offsetHeight + 110) + "px)";
      sidePanels[i].style.overflowY = "auto";
      sidePanels[i].style.marginBottom = "0px";
    }
  else
    for (var i=0; i < mainPanels.length; ++i) {
      mainPanels[i].style.height = "auto";
      mainPanels[i].style.overflowY = "auto";
      mainPanels[i].style.marginBottom = "64px";
      sidePanels[i].style.height = "auto";
      sidePanels[i].style.overflowY = "auto";
      sidePanels[i].style.marginBottom = "64px";
      
    }
       
};
    
    
//==========================================
// resize panel when window is resized 
//==========================================
$(window).resize(function(event){
  resizePanels();
})

//==========================================
// resize panels when nav bar is clicked
//==========================================
$(function() {
  $(".navbar").click( function(event){
    resizePanels();
  })
});