shinyjs.init = function() {
    //$(window).resize(shinyjs.getHeight50);

    $('body').ready(function(){
        $('body').on('click', '[id$="_goto"]', function(){
            $('#SITE_EXPLORE').trigger('click');
        });
    });

    var wheight = $(window).height() - 100 + 'px';
    $('[data-value="participants"]').css('max-height', wheight);
}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

//shinyjs.getHeight50 = function() {
//  Shiny.onInputChange('height50', $(window).height() * .5);
//}

