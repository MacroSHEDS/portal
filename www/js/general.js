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

    //this lets us trigger an event when a specific element has fully loaded in the DOM
   // function loaded(selector, callback){
   //     $(function(){ callback($(selector)); });
   //     var parentSelector = '* > ' + selector;
   //     $(document).on('DOMNodeInserted', parentSelector, function(e){
   //         callback($(this).find(selector));
   //     });
   // }
   
   function debounce(func, wait, immediate){
       var timeout;
       return function(){
           var context = this, args = arguments;
           var later = function(){
                           timeout = null;
                           if(!immediate) func.apply(context, args);
                       };
           var callNow = immediate && ! timeout;
           clearTimeout(timeout);
           timeout = setTimeout(later, wait);
           if(callNow) func.apply(context, args);
       };
   };

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
    //$('input[name^="CONC_FLUX"][value="Flux"]').parent()
    //    .attr('title', 'Interpolated flux: mean concentration times mean discharge over the aggregation period (below), linearly interpolated. NOTE: only available when "Show precip chemistry" is off.')
    //$('input[name^="CONC_FLUX"][value="VWC"]').parent()
    //    .attr('title', 'Mean flux rate divided by mean discharge rate, over the aggregation period. NOTE: only available when aggregation > daily.');
    //    .attr('title', 'Volume-weighted concentration: summation of instantaneous concentration and Q (or precipitation) volume divided by total volume over the aggregation period (below). NOTE: only available when aggregation > daily.')
    //$('#flags3-tooltip').attr('title', 'Points flagged as erroneous (bad data) have been removed from the dataset. Points flagged as questionable can be toggled here.');
    //$('#interp3-tooltip').attr('title', 'We linearly interpolate data gaps of up to 3 days for discharge and precip, and up to 15 days for chemistry.');
    //$('#SHOW_PCHEM3').parent().parent().attr('title', 'Enabled only when unit is concentration or VWC and aggregation > daily.');
    $('input[name="CONC_FLUX3"][value="VWC"]').parent('label').replaceWith(
        '<div style = "white-space: nowrap">' +
        '<label style = "display: inline-block; vertical-align: middle; white-space: normal">' +
        '<input type="radio" name="CONC_FLUX3" value="VWC">' +
        '<span>Volume-Weighted Concentration</span>' +
        '</label>' +
        '<div class = "ms-tooltip" style = "display: inline-block; vertical-align: middle"' +
        'title = "Total flux divided by total discharge (or precip), over the aggregation period. Only available when aggregation > daily. See Notes/Caveats tab for more.">&#x2753;</div>' +
        '</div>'
    );
    
    //if somebody clicks a question mark, tell them to hover on it instead
    //(this is done separately for tooltips within modals)
    $(document).ready(function(){
        $('.ms-tooltip').click(function(){
            $(this).addClass('instruct-hover').delay(800).queue(function(){
                $(this).removeClass('instruct-hover').dequeue();
            });
        });
    });

    //adjust height on right-side content so that dropdown doesn't go off the page
    $(document).ready(function(){
        $('#ADD_SIZE2').click(function(){
            var current_height = $('.content').height();
            if($('#ADD_SIZE2').is(':checked')){
                $('.content').height(current_height + 150);
            } else {
                $('.content').height(current_height - 150);
            };
        });
    });


   // //disable input conc checkbox unless monthly or annual conc or VWC selected; manage aggregation options too (REPLACED BY govern_agg3)
   // function govern_showpchem3(){

   //     var datatype = $('input[name=CONC_FLUX3]:checked').val()

   //     if( ['Concentration', 'VWC'].includes(datatype) &&
   //             ['Monthly', 'Yearly'].includes($('input[name=AGG3]:checked').val()) ){
   //         $('#SHOW_PCHEM3').removeAttr('disabled').siblings().css('color', '#485580');

   //         if(datatype == 'VWC'){
   //             $('input[name=AGG3][value=Daily]').attr('disabled','disabled').siblings().css('color', 'gray');
   //             $('input[name=AGG3][value=Instantaneous]').attr('disabled','disabled').siblings().css('color', 'gray');
   //         }

   //     } else {

   //         $('#SHOW_PCHEM3').attr('disabled','disabled').siblings().css('color', 'gray');

   //         if(datatype == 'VWC'){
   //             $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
   //             $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
   //         }
   //     }


   //     if( datatype !== 'VWC' && ! $('#SHOW_PCHEM3').is(':checked') ){
   //         $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
   //         $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
   //     }

   // };

   // $('body').ready(function(){
   //     $('#CONC_FLUX3').change(govern_showpchem3);
   // });

   // $('body').ready(function(){
   //     $('#AGG3').change(govern_showpchem3);
   // });

    //disable instantaneous and daily agg if VWC selected
    function govern_agg3(){

        var datatype = $('input[name=CONC_FLUX3]:checked').val();

        if(datatype == 'VWC'){
            $('input[name=AGG3][value=Daily]').attr('disabled','disabled').siblings().css('color', 'gray');
            $('input[name=AGG3][value=Instantaneous]').attr('disabled','disabled').siblings().css('color', 'gray');
        } else {
            $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
            $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
        };
    };

    $('body').ready(function(){
        $('#CONC_FLUX3').change(govern_agg3);
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

   // //disable flux if precip viz selected (why??)
   // function govern_flux3(){
   //     if( $('#SHOW_PCHEM3').is(':checked') ){
   //         $('input[name=CONC_FLUX3][value=Flux]').attr('disabled', 'disabled').siblings().css('color', 'gray');
   //         $('input[name=AGG3][value=Instantaneous]').attr('disabled', 'disabled').siblings().css('color', 'gray');
   //         $('input[name=AGG3][value=Daily]').attr('disabled', 'disabled').siblings().css('color', 'gray');
   //     } else {
   //         $('input[name=CONC_FLUX3][value=Flux]').removeAttr('disabled').siblings().css('color', '#485580');

   //         if( $('input[name=CONC_FLUX3]:checked').val() !== 'VWC' ){
   //             $('input[name=AGG3][value=Instantaneous]').removeAttr('disabled').siblings().css('color', '#485580');
   //             $('input[name=AGG3][value=Daily]').removeAttr('disabled').siblings().css('color', '#485580');
   //         }
   //     }
   // };

   // $('body').ready(function(){
   //     $('#SHOW_PCHEM3').click(govern_flux3);
   // });

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

    //BIPLOT: disable X-axis selection when aggregation == 'Yearly'
    function govern_xvar_section(){
        if( ['MONTHLY2', 'YEARLY2'].includes($('input[name=AGG2]:checked').val()) ){
            $('#X_TYPE2').selectize()[0].selectize.disable();
            //hiding/showing of VAR and UNIT now governed by govern_tier2_dropdown() and govern_tier3_dropdown
            //$('#X_VAR2').parent().hide();
            //$('#X_UNIT2').parent().hide();
            $('#LOG_X2').hide();
        } else {
            $('#X_TYPE2').selectize()[0].selectize.enable();
            //$('#X_VAR2').parent().show();
            //$('#X_UNIT2').parent().show();
            $('#LOG_X2').show();
        }
    };

    $(document).one('shiny:sessioninitialized', function(event){
        govern_xvar_section();
    });

    $('body').ready(function(){
        $('input[name=AGG2]').change(govern_xvar_section);
    });

    //hide selectors with no options; freeze selectors with one option
    function govern_tier3_dropdown(key){

        var unit_obj = $('#' + key + '_UNIT2')
        var unit_val = unit_obj.val()
        var unit_sel = unit_obj.selectize()[0].selectize;

        if( ! unit_val ){
            unit_obj.parent().hide();
        } else {
            unit_obj.parent().show();
        };

        if( Object.keys(unit_sel.options).length == 1 && unit_val !== '' ){
            unit_sel.disable();
        } else {
            unit_sel.enable();
        };
    };

    function govern_tier2_dropdown(key){

        var unit_obj = $('#' + key + '_UNIT2')
        var var_obj = $('#' + key + '_VAR2')
        var var_val = var_obj.val()
        var var_sel = var_obj.selectize()[0].selectize;
        var redundant_options = ['% of record missing', 'Year', 'Month', 'P', 'Q']

        if( ! var_val ){
            var_obj.parent().hide();
            unit_obj.parent().hide();
        } else {
            var_obj.parent().show();
            unit_obj.parent().show();
        };

        if( Object.keys(var_sel.options).length == 1 && redundant_options.includes(var_val) ){
            var_obj.parent().hide();
        } else if( Object.keys(var_sel.options).length == 1 && var_val !== '' ){
            var_obj.parent().show();
            var_sel.disable();
        } else {
            if(var_val){
                var_obj.parent().show();
                var_sel.enable();
            };
        };
    };

    $('body').ready(function(){

        $('#X_TYPE2').change(function(){
            govern_tier2_dropdown('X');
            govern_tier3_dropdown('X');
        });
        $('#X_VAR2').change(function(){
            govern_tier2_dropdown('X');
            govern_tier3_dropdown('X');
        });
        $('#X_UNIT2').change(function(){
            govern_tier3_dropdown('X');
        });

        $('#Y_TYPE2').change(function(){
            govern_tier2_dropdown('Y');
            govern_tier3_dropdown('Y');
        });
        $('#Y_VAR2').change(function(){
            govern_tier2_dropdown('Y');
            govern_tier3_dropdown('Y');
        });
        $('#Y_UNIT2').change(function(){
            govern_tier3_dropdown('Y');
        });

        $('#SIZE_TYPE2').change(function(){
            govern_tier2_dropdown('SIZE');
            govern_tier3_dropdown('SIZE');
        });
        $('#SIZE_VAR2').change(function(){
            govern_tier2_dropdown('SIZE');
            govern_tier3_dropdown('SIZE');
        });
        $('#SIZE_UNIT2').change(function(){
            govern_tier3_dropdown('SIZE');
        });
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


    //control data catalog interactions
    $('body').ready(function(){
        $('#dataTable').on('focus', function(i){
            $('#site_catalog td').each(function(i){
                var $this = $(this);
                var titleVal = $this.text();
                if(typeof titleVal === "string" && titleVal !== ''){
                    $this.attr('title', titleVal);
                }
            });
        });
    });

    //set td elements' titles to their contents, so the user can mouseover to see hidden text
    set_td_titles = function(i){
        var titleVal = $(this).text();
        if(typeof titleVal === "string" && titleVal !== ''){
            $(this).attr('title', titleVal);
        };
    };

    var partial = function(fn, var_args) {
      var args = Array.prototype.slice.call(arguments, 1);
      return function() {
        // Clone the array (with slice()) and append additional arguments
        // to the existing arguments.
        var newArgs = args.slice();
        newArgs.push.apply(newArgs, arguments);
        return fn.apply(this, newArgs);
      };
    };

    //make variable subcatalogs pop up when user clicks "see availability" buttons in modals
    set_tablebutton_listener = function(btn, modal_id_){

        //var btn = $(this);
        var btn = $(btn);
        var btn_id = btn.attr('id');

        btn.on('click', function(){

            if(modal_id_ == 'variable_catalog'){
                console.log('VARIABLE SUB-CATALOG LISTENER');
                Shiny.setInputValue('VARIABLE_SUBCATALOG_BUTTON_LISTENER', btn_id, {priority: 'event'});
//                            Shiny.setInputValue('VARIABLE_SUBCATALOG_BUTTON_LISTENER', null);//, {priority: 'event'});
            } else if(modal_id_ == 'site_catalog'){
                console.log('SITE SUB-CATALOG LISTENER');
                Shiny.setInputValue('SITE_SUBCATALOG_BUTTON_LISTENER', btn_id, {priority: 'event'});
            } else {
                console.log('unhandled button detected inside a modal table');
            }

        });
    };

    //respond to insertions of shiny modal boxes into document.body
    var dom_observer1 = new MutationObserver(async function(mutation) {

        var tgt = $(target);
        var shinymodal = tgt.children('#shiny-modal-wrapper');

        if(shinymodal.length > 0){

            var modal_id = shinymodal.find('.modal-body').attr('id');

            if(modal_id == 'landing'){

                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                }, 1000);
                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                }, 3000);
                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                }, 8000);

                return;
            };

            //make all non-landing modals fullscreen
            shinymodal.children('.modal').css({'width': '100%', 'height': '100%', 'margin': '0px'})
                .children('.modal-dialog').css({'width': '100%', 'height': '100%', 'margin': '0px'})
                .children('.modal-content').css({'width': '100%', 'height': 'auto', 'min-height': '100%'});

            //for catalog modals
            if(/catalog$/.test(modal_id)){

                await new Promise(r => setTimeout(r, 1000)); //wait a second for the modal and its table to load

                shinymodal.find('td').each(set_td_titles);

                //set up listeners for all button elements inside table cells (uses partial application)
                shinymodal.find('td button').each(partial(function(index, value){
                    set_tablebutton_listener(btn = arguments[2], modal_id_ = arguments[0]);
                }, modal_id));
            };
        };
    });

    //configure and register mutation observer for modals
    var mutation_observer_config_1 = { attributes: true, childList: true, characterData: true };
    dom_observer1.observe(target = document.body, mutation_observer_config_1);

    //each time a new catalog page is loaded, remake the cell titles and button events
    $('body').on('click', "a[class^='paginate_button']", function(i, v){

        window.setTimeout(function(){

            var modl = $('.modal-body');
            var modal_id = modl.attr('id');

            modl.find('td').each(set_td_titles);
            
            //set up listeners for all button elements inside table cells (uses partial application)
            modl.find('td button').each(partial(function(index, value){
                set_tablebutton_listener(btn = arguments[2], modal_id_ = arguments[0]);
            }, modal_id));

        }, 200);
    });

    //each time number of shown records changes, remake the cell titles and button events
    $('body').on('change', "select[name^='DataTables_Table_']", function(i, v){

        window.setTimeout(function(){

            var modl = $('.modal-body');
            var modal_id = modl.attr('id');

            modl.find('td').each(set_td_titles);
            
            //set up listeners for all button elements inside table cells (uses partial application)
            modl.find('td button').each(partial(function(index, value){
                set_tablebutton_listener(btn = arguments[2], modal_id_ = arguments[0]);
            }, modal_id));

        }, 200);
    });

    //each time a search is made, remake the cell titles and button events (debounce until last keystroke)
    $('body').on('input', "input[type='search'][aria-controls^='DataTables_Table_']", debounce(function(i, v){

        window.setTimeout(function(){

            var modl = $('.modal-body');
            var modal_id = modl.attr('id');

            modl.find('td').each(set_td_titles);
            
            //set up listeners for all button elements inside table cells (uses partial application)
            modl.find('td button').each(partial(function(index, value){
                set_tablebutton_listener(btn = arguments[2], modal_id_ = arguments[0]);
            }, modal_id));

        }, 400);
    }, 1000));

    //all this crap (and associated CSS) is necessary just to make "macrosheds.org"
    //appear (and STAY appeared) in the bottom of each dygraph as an annotation
    
    window.enable_attribution = false 

   // $('body').on('change', '#DATES3', debounce(function(){
   //     if(enable_attribution){
   //         window.setTimeout(function(){
   //             $('[class^="ms-attribution"]').remove()
   //             $('#GRAPH_PRECIP3').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //             $('[id^="GRAPH_MAIN3"]').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //             $('#GRAPH_Q3').css('position', 'relative').append($('<p class="ms-attribution-high">macrosheds.org</p>'));
   //         }, 2000);
   //     };

   //     //the next listener (for VARS3 and SITES3 changes) executes first! so enable attribution here
   //     enable_attribution = true
   // }, 1000));

   // $('body').on('change', '#VARS3, #SITES3', function(){
   //     if(enable_attribution){
   //         $('[class^="ms-attribution"]').remove()
   //         $('#GRAPH_PRECIP3').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //         $('[id^="GRAPH_MAIN3"]').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //         $('#GRAPH_Q3').css('position', 'relative').append($('<p class="ms-attribution-high">macrosheds.org</p>'));
   //     };
   // });

   // $('body').on('click', 'a[data-value="multisite_exploration"]', function(){
   //     window.setTimeout(function(){
   //         $('[class^="ms-attribution"]').remove()
   //         $('#GRAPH_PRECIP3').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //         $('[id^="GRAPH_MAIN3"]').css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
   //         $('#GRAPH_Q3').css('position', 'relative').append($('<p class="ms-attribution-high">macrosheds.org</p>'));
   //     }, 3000);
   // });
    
    $(document).on('shiny:idle', function(event){
        enable_attribution = true
    });

    $('div[id^="GRAPH_"]').on('shiny:value', function(event){
        if(enable_attribution){
            let $this = $(this)
            window.setTimeout(function(){
                $this.children('[class^="ms-attribution"]').remove()
                if($this.attr('id') === 'GRAPH_Q3'){
                    $this.css('position', 'relative').append($('<p class="ms-attribution-high">macrosheds.org</p>'));
                } else if(/GRAPH_QC3./.test( $this.attr('id') )){
                    console.log($this.attr('id'));
                } else {
                    $this.css('position', 'relative').append($('<p class="ms-attribution-low">macrosheds.org</p>'));
                };
            }, 500);
        };
    });
};

//shinyjs.calcHeight = function(propHeight) {
//  var h = $(window).height() * propHeight;
//  Shiny.onInputChange('plotHeight', Number(h.toFixed(0)));

//shinyjs.getHeight50 = function() {
//  Shiny.onInputChange('height50', $(window).height() * .5);
//}

