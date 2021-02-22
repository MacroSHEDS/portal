shinyjs.init = function() {
    //$(window).resize(shinyjs.getHeight50);

    //gotta style landing page this way because css can't uniquely reach it
    var checkExist = setInterval(function() {
        if ($('#landing').length) {
           $('#landing').parent().parent().parent().css({'width': '100%', 'height': '100%', 'margin': '0px'});
           $('#landing').parent().parent().css({'width': '100%', 'height': '100%', 'margin': '0px'});
           $('#landing').parent().css({'width': '100%', 'height': '100%', 'color': '#193d85'});
           clearInterval(checkExist);
        }
    }, 100);

   // var checkExist = setInterval(function() {
   //     if ($('#landing').length) {
   //         $('#main3a').clone().appendTo('#aaa')
   //     }
   // }, 100);

    //connect map buttons to app tabs
    $('body').ready(function(){
        $('body').on('click', '[id$="_goto"]', function(){
            var goto_id = $(this).attr('id') + new Date(); //trigger reactivity
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
        $('#GRAPH_MAIN3a').delay(200).fadeOut(400).fadeIn(400)
        $('#GRAPH_Q3').delay(200).fadeOut(400).fadeIn(400)
    });

    //tooltips and some styling
    $('input[name^="CONC_FLUX"][value="Flux"]').parent()
        .attr('title', 'Interpolated flux: mean concentration times mean discharge over the aggregation period (below), linearly interpolated. NOTE: only available when "Show precip chemistry" is off.')
    $('input[name^="CONC_FLUX"][value="VWC"]').parent()
        .attr('title', 'Volume-weighted concentration: summation of instantaneous concentration and Q (or precipitation) volume divided by total volume over the aggregation period (below). NOTE: only available when aggregation > daily.')
    //    .css('color', 'blue')
    $('#SHOW_PCHEM3').parent().parent().attr('title', 'Enabled only when unit is concentration or VWC and aggregation > daily.');

    //disable input conc checkbox unless monthly or annual conc or VWC selected; manage aggregation options too
    function govern_showpchem3(){

        var datatype = $('input[name=CONC_FLUX3]:checked').val()

        if( ['Concentration', 'VWC'].includes(datatype) &&
                ['Monthly', 'Yearly'].includes($('input[name=AGG3]:checked').val()) ){
            $('#SHOW_PCHEM3').removeAttr('disabled').siblings().css('color', '#FFFFFF');

            if(datatype == 'VWC'){
                $('input[name=AGG3][value=Daily]').attr('disabled','disabled').siblings().css('color', 'gray');
                $('input[name=AGG3][value=Instantaneous]').attr('disabled','disabled').siblings().css('color', 'gray');
            }

        } else {

            $('#SHOW_PCHEM3').attr('disabled','disabled').siblings().css('color', 'gray');

            if(datatype == 'VWC'){
                $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#FFFFFF');
                $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#333');
            }
        }


        if( datatype !== 'VWC' && ! $('#SHOW_PCHEM3').is(':checked') ){
            $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#FFFFFF');
            $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#FFFFFF');
        }

    };

    $('body').ready(function(){
        $('#CONC_FLUX3').change(govern_showpchem3);
    });

    $('body').ready(function(){
        $('#AGG3').change(govern_showpchem3);
    });

    //disable VWC unless monthly or annual aggregation selected
    function govern_VWC3(){
        if( ['Monthly', 'Yearly'].includes($('input[name=AGG3]:checked').val()) ){
            $('input[name=CONC_FLUX3][value=VWC]').removeAttr('disabled').siblings().css('color', '#FFFFFF');
        } else {
            $('input[name=CONC_FLUX3][value=VWC]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        }
    };

    $('body').ready(function(){
        $('input[name=AGG3]').change(govern_VWC3);
    });

    //disable interp flux if precip viz selected
    function govern_flux3(){
        if( $('#SHOW_PCHEM3').is(':checked') ){
            $('input[name=CONC_FLUX3][value=Flux]').attr('disabled', 'disabled').siblings().css('color', 'gray');
            $('input[name=AGG3][value=Instantaneous]').attr('disabled', 'disabled').siblings().css('color', 'gray');
            $('input[name=AGG3][value=Daily]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        } else {
            $('input[name=CONC_FLUX3][value=Flux]').removeAttr('disabled').siblings().css('color', '#FFFFFF');

            if( $('input[name=CONC_FLUX3]:checked').val() !== 'VWC' ){
                $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#333');
                $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#FFFFFF');
            }
        }
    };

    $('body').ready(function(){
        $('#SHOW_PCHEM3').click(govern_flux3);
    });

    ////only show QC plots when their box is checked
    //function govern_qc3(){
    //    if( $('#SHOW_QC3').is(':checked') ){
    //        //$('#inlineQC3a').css('display', '');
    //        $('#inlineQC3a').css('width', '25%');
    //        $('#inlineMAIN3a').css('width', 'auto');
    //    } else {
    //        //$('#inlineQC3a').css('display', 'none');
    //        $('#inlineQC3a').css('width', '0px');
    //        $('#inlineMAIN3a').css('width', '100%');
    //    }
    //};

    //$('body').ready(function(){
    //    $('#SHOW_QC3').click(govern_qc3);
    //});

}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

//shinyjs.getHeight50 = function() {
//  Shiny.onInputChange('height50', $(window).height() * .5);
//}

