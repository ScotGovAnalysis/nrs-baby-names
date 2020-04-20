$(document).keyup(function(event){
    if(event.keyCode == 13){
        $("#goButton").click();
    }
});

$(document).on('click', "#suggestions .shiny-bound-input", function(e) {
   e.stopPropagation();
   if(typeof LINK_CLICK_COUNT == "undefined") {
      LINK_CLICK_COUNT = 1; 
    } else {
      LINK_CLICK_COUNT ++;
    }
    Shiny.onInputChange("js.link_clicked", 
      e.target.id + "_" + LINK_CLICK_COUNT);
});