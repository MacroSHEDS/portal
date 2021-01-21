shinyjs.init = function() {
    //$(window).resize(shinyjs.getHeight50);
    
    ////gotta style landing page this way because css can't uniquely reach it
    //var checkExist = setInterval(function() {
    //    if ($('#landing').length) {
    //       $('#landing').parent().parent().parent().css({'width': '100%', 'height': '100%', 'margin': '0px'});
    //       $('#landing').parent().parent().css({'width': '100%', 'height': '100%', 'margin': '0px'});
    //       $('#landing').parent().css({'width': '100%', 'height': '100%'});
    //       clearInterval(checkExist);
    //    }
    //}, 100);

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
            $('#SHOW_PCHEM3').removeAttr('disabled').siblings().css('color', '#485580');

            if(datatype == 'VWC'){
                $('input[name=AGG3][value=Daily]').attr('disabled','disabled').siblings().css('color', 'gray');
                $('input[name=AGG3][value=Instantaneous]').attr('disabled','disabled').siblings().css('color', 'gray');
            }

        } else {

            $('#SHOW_PCHEM3').attr('disabled','disabled').siblings().css('color', 'gray');

            if(datatype == 'VWC'){
                $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
                $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
            }
        }


        if( datatype !== 'VWC' && ! $('#SHOW_PCHEM3').is(':checked') ){ 
            $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
            $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
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
            $('input[name=CONC_FLUX3][value=VWC]').removeAttr('disabled').siblings().css('color', '#485580');
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
            $('input[name=CONC_FLUX3][value=Flux]').removeAttr('disabled').siblings().css('color', '#485580');

            if( $('input[name=CONC_FLUX3]:checked').val() !== 'VWC' ){
                $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
                $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
            }
        }
    };

    $('body').ready(function(){
        $('#SHOW_PCHEM3').click(govern_flux3);
    });

    //only show QC plots when their box is checked
    function govern_qc3(){
        if( $('#SHOW_QC3').is(':checked') ){
            //$('#inlineQC3a').css('display', 'inline-block');
            
            //$('[id^="inlineQC3"').attr('style', 'width: 25% !important; display: inline-block; vertical-align: top');
            //$('[id^="inlineMAIN3"').attr('style', 'width: 75% !important; display: inline-block; vertical-align: top');
            $('[id^="inlineQC3"').css('width', '25%');
            $('[id^="inlineMAIN3"').css('width', '75%');
            //$("inlineQC3a").css('width', '25%');
            //$("inlineMAIN3a").css('width', '75%');
        } else {
            //$('#inlineQC3a').css('display', 'none');
            
            //$('[id^="inlineQC3"').attr('style', 'width: 0% !important; display: inline-block; vertical-align: top');
            //$('[id^="inlineMAIN3"').attr('style', 'width: 100% !important; display: inline-block; vertical-align: top');
            $('[id^="inlineQC3"]').css('width', '0%');
            $('[id^="inlineMAIN3"]').css('width', '100%');
            //$('inlineQC3a').css('width', '0%');
            //$('inlineMAIN3a').css('width', '100%');

            //$('#inlineMAIN3a').css('width', 'auto');
        }

        $('#REFRESH').trigger('click');
        //$('#inlineQC3a').offsetHeight
        //$('#inlineMAIN3a').offsetHeight
        //$('#inlineContainerA').offsetHeight
        //$('#inlineQC3a').hide().show(0);
        //$('#inlineQC3a').css('transform', 'translateZ(0)');
    };

    $('body').ready(function(){
        $('#SHOW_QC3').click(govern_qc3);
    });

    //for grab/installed, ensure that at least one box is checked
    function govern_gi3(){

        if( ! $('input[name="INSTALLED_V_GRAB3"][value="G"]').is(':checked') ){
            $('input[name="INSTALLED_V_GRAB3"][value="I"]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        } else {
            $('input[name="INSTALLED_V_GRAB3"][value="I"]').removeAttr('disabled').siblings().css('color', '#333');
        }

        if( ! $('input[name="INSTALLED_V_GRAB3"][value="I"]').is(':checked') ){
            $('input[name="INSTALLED_V_GRAB3"][value="G"]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        } else {
            $('input[name="INSTALLED_V_GRAB3"][value="G"]').removeAttr('disabled').siblings().css('color', '#333');
        }

        $('#REFRESH').trigger('click');
    };

    $('body').ready(function(){
        $('#INSTALLED_V_GRAB3').click(govern_gi3);
    });
   
    //for sensor/nonsensor, ensure that at least one box is checked
    function govern_sn3(){

        if( ! $('input[name="SENSOR_V_NONSENSOR3"][value="S"]').is(':checked') ){
            $('input[name="SENSOR_V_NONSENSOR3"][value="N"]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        } else {
            $('input[name="SENSOR_V_NONSENSOR3"][value="N"]').removeAttr('disabled').siblings().css('color', '#333');
        }

        if( ! $('input[name="SENSOR_V_NONSENSOR3"][value="N"]').is(':checked') ){
            $('input[name="SENSOR_V_NONSENSOR3"][value="S"]').attr('disabled', 'disabled').siblings().css('color', 'gray');
        } else {
            $('input[name="SENSOR_V_NONSENSOR3"][value="S"]').removeAttr('disabled').siblings().css('color', '#333');
        }

        $('#REFRESH').trigger('click');
    };

    $('body').ready(function(){
        $('#SENSOR_V_NONSENSOR3').click(govern_sn3);
    });


    $('body').ready(function(){
        //$('.dataTable tbody tr').on('overflow', 'td', function(index){
        $('#dataTable').on('focus', function(i){
        //$('#SITE_CATALOG_BUTTON').on('click', function(i){
            console.log('gg');
        $('#site_catalog td').each(function(i){
        //$('.dataTable tbody tr td').each(function(i){
            console.log('a');
            $this = $(this);
            var titleVal = $this.text();
            if(typeof titleVal === "string" && titleVal !== ''){
                $this.attr('title', titleVal);
            }
        });
        });
    });
   
    //respond to insertions of shiny modal boxes into document.body
    var dom_observer = new MutationObserver(async function(mutation) {

        var tgt = $(target);
        var shinymodal = tgt.children('#shiny-modal-wrapper');
        //var nmodals = shinymodals.length;
        //
        //if(nmodals > 0){
        if(shinymodal.length > 0){

            //make modal fullscreen
            shinymodal.children('.modal').css({'width': '100%', 'height': '100%', 'margin': '0px'})
                .children('.modal-dialog').css({'width': '100%', 'height': '100%', 'margin': '0px'})
                .children('.modal-content').css({'width': '100%', 'height': '100%'});

            await new Promise(r => setTimeout(r, 1000)); //wait a second for the modal and its table to load

            //for(var i = 0; i < nmodals; i++){ //for each modal present (there can only be one at a time under shiny)
            //
            //var modal = $(shinymodals[i]);
            var modal_id = shinymodal.find('.modal-body').attr('id');
            if(modal_id === 'landing'){
                return;
            }


            //set all td elements' titles to their contents, so the user can mouseover to see hidden text
            shinymodal.find('td').each(function(i){
                var titleVal = $(this).text();
                if(typeof titleVal === "string" && titleVal !== ''){
                    $(this).attr('title', titleVal);
                };
            });

            //set up listeners for all button elements inside table cells
            shinymodal.find('td button').each(function(i){

                var btn = $(this)
                var btn_id = btn.attr('id')

                btn.on('click', function(){

                    if(modal_id == 'variable_catalog'){
                        console.log('VARIABLE SUB-CATALOG LISTENER');
                        Shiny.setInputValue('VARIABLE_SUBCATALOG_BUTTON_LISTENER', btn_id, {priority: 'event'});
//                            Shiny.setInputValue('VARIABLE_SUBCATALOG_BUTTON_LISTENER', null);//, {priority: 'event'});
                    } else if(modal_id == 'site_catalog'){
                        console.log('SITE SUB-CATALOG LISTENER');
                        Shiny.setInputValue('SITE_SUBCATALOG_BUTTON_LISTENER', btn_id, {priority: 'event'});
                    } else {
                        console.log('unhandled button detected inside a modal table');
                    }

                });
            });
            //};
        };
    });

    //configure and register mutation observer for modals
    var config = { attributes: true, childList: true, characterData: true };
    dom_observer.observe(target = document.body, config);

}

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

//shinyjs.getHeight50 = function() {
//  Shiny.onInputChange('height50', $(window).height() * .5);
//}

