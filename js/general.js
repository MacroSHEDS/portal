shinyjs.init = function() {
    //$(window).resize(shinyjs.getHeight50);

    //connect map buttons to app tabs
    $('body').ready(function(){
        $('body').on('click', '[id$="_goto"]', function(){
            var goto_id = $(this).attr('id');
            Shiny.setInputValue('MAPDATA', goto_id);
            $('#SITE_EXPLORE').trigger('click');
        });
    });

    //set height of some tab windows
    var wheight = $(window).height() - 100 + 'px';
    $('[data-value="participants"]').css('max-height', wheight);
    $('[data-value="hiring"]').css('max-height', wheight);

    //highlight plots when sites are selected via map
    Shiny.addCustomMessageHandler('flash_plot', function(message) {
        $('#GRAPH_MAIN4a').delay(200).fadeOut(400).fadeIn(400)
        $('#GRAPH_FLOW4').delay(200).fadeOut(400).fadeIn(400)
    });

    //tooltips and some styling
    $('input[name^="CONC_FLUX"][value="Flux"]').parent()
        .attr('title', 'Interpolated flux: mean concentration times mean discharge over the aggregation period (below), linearly interpolated.')
    $('input[name^="CONC_FLUX"][value="VWC"]').parent()
        .attr('title', 'Volume-weighted concentration: summation of instantaneous concentration and discharge measurements divided by total discharge over the aggregation period (below). NOTE: only available when aggregation > daily.')
    //    .css('color', 'blue')
    $('#INCONC3').attr('disabled', 'disabled').parent().parent()
        .attr('title', 'Enabled only when unit is VWC and aggregation > daily.');

    //disable input conc checkbox unless monthly or annual VWC selected
    function govern_inconc3(){
        if( $('input[name=CONC_FLUX3]:checked').val() == 'VWC' &&
                ['Monthly', 'Yearly'].includes($('input[name=AGG3]:checked').val()) ){
            $('#INCONC3').removeAttr('disabled').siblings().css('color', '#333');
            $('input[name=AGG3][value=Daily]').attr('disabled','disabled').siblings().css('color', 'gray');
            $('input[name=AGG3][value=Instantaneous').attr('disabled','disabled').siblings().css('color', 'gray');
        } else {
            $('#INCONC3').attr('disabled','disabled').siblings().css('color', 'gray');
            $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#333');
            $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#333');
        }
    };

    $('body').ready(function(){
        $('#CONC_FLUX3').change(govern_inconc3);
    });

    $('body').ready(function(){
        $('#AGG3').change(govern_inconc3);
    });

    //disable VWC unless monthly or annual aggregation selected
    function govern_VWC3(){
        if( ['Monthly', 'Yearly'].includes($('input[name=AGG3]:checked').val()) ){
            $('input[name=CONC_FLUX3][value=VWC]').removeAttr('disabled').siblings().css('color', '#333');
        } else {
            $('input[name=CONC_FLUX3][value=VWC]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        }
    };

    $('body').ready(function(){
        $('input[name=AGG3]').change(govern_VWC3);
    });

}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

//shinyjs.getHeight50 = function() {
//  Shiny.onInputChange('height50', $(window).height() * .5);
//}

