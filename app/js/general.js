shinyjs.init = function() {
    $(window).resize(shinyjs.getHeight50);

    $('body').ready(function(){
        $('body').on('click', '#TN__WALK_goto', function(){
            $('#SITE_EXPLORE').trigger('click');
        });
    });
}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

shinyjs.getHeight50 = function() {
  Shiny.onInputChange('height50', $(window).height() * .5);
}
