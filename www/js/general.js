shinyjs.init = function() {
    //$(window).resize(shinyjs.getHeight50);

    // // // MacroshedsServer Tiling Service Injectors (Scratch Pad)
    // var map = L.map('wesmap', {
    //   // Set latitude and longitude of the map center (required)
    //       center: [42.87111,-97.39728],
    //   // Set the initial zoom level, values 0-18, where 0 is most zoomed-out (required)
    //       zoom: 8
    //     });
    //
    // var mb = L.tileLayer.mbTiles('http://macrosheds.org/map_tiles/epa_ecoregions/eco_l1.mbtiles')
    // map.addLayer(mb);
    // googleSat = L.tileLayer('http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}',{
    //       maxZoom: 20,
    //       subdomains:['mt0','mt1','mt2','mt3']
    //     }).addTo(map)
    // var config = {
    //   // url: "http://macrosheds.org/map_tiles/epa_ecoregions/{z}/{x}/{y}.mbtiles"
    //   url: "http://spatialserver.spatialdev.com/services/vector-tiles/gaul_fsp_india/{z}/{x}/{y}.pbf"
    // };
    // var mapillarySource = new L.TileLayer.MVTSource(config);
    // var url = 'https://d2munx5tg0hw47.cloudfront.net/tiles/{z}/{x}/{y}.mapbox';
    // var mapillaryLayer = L.vectorGrid.protobuf(url).addTo(map);
    //
    // var tms_example = L.tileLayer('', {
    //       tms: true
    //     }).addTo(map);
    //

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

// tab focus control
$("body").on("shown.bs.tab", "a[data-toggle='tab']", function() {
    // console.log("THE SE PST WORKED!")
    var data_focus = $("#right_tabs").children(".active").children('a').attr('data-value')

    Shiny.setInputValue("active_tab", data_focus);
})

// rank list control
    function arrayToList(array) {
        let list = null;
        for (let i = array.length - 1; i >= 0; i--) {
            list = { value: array[i], rest: list };
        }
        return list;
    }

    function rankLister() {
        // set mapdata as top 3 ranks
        var rankList = [];

        $(".rank-list > .rank-list-item").each((index, elem) => {
          rankList.push(elem.id);
        });

        Shiny.setInputValue('MAPDATA', arrayToList(rankList));
    }

    function rankFinder(goto_id) {
        // extract domain and site name, clean
        site_info = goto_id.split('_');
        site_info_end = site_info.length -1

        // var site = site_info[2];
        var domain = site_info[0];
        var site = site_info.slice(1, site_info_end).join(' ');

        // add sites to cart
        let hash = '#';
        let idString = hash.concat(goto_id, "remover")

        if ($('.rank-list').find(idString).length) {

            if ($('.map-site-warning').length == 0) {
                // add warning to user that duplicate site cant be added
                $('.map-site-btn').parent().parent().prepend('<!-- Warning Alert --><div class="alert alert-warning alert map-site-warning" style="padding: 6px; margin-bottom: 8px;">    <strong>Warning!</strong> sites cannot be added to selection list more than once </div>')

                // make it so clicking warning hides it
                $('.map-site-warning').click( function() {
                    $('.map-site-warning').hide();
                });

            }

        } else {
            $('div').find('.rank-list').prepend(
                    '<div class="rank-list-item" id =' + goto_id + 'remover' + ' draggable="false">' + '<strong>' + domain + '</strong>' + ':  ' + site + ' <button type="button" class="btn btn-default btn-sm rankremover" style="float: right"><span class="glyphicon glyphicon-remove"></span> </button></div>'
                );
        }
    }

    //connect map buttons to app tabs
    $('body').ready(function(){
        $('body').on('click', '[id$="_goto"]', function(){

            var goto_id = $(this).attr('id') // + new Date(); //trigger reactivity

            // make "PLOT" button attention styled
            // $("#GEN_PLOTS3").addClass("btn-update");
            $("#GEN_PLOTS3").addClass("btn-warning");

            // make site comparison option highlight as well?
            // $('value="BY_BUCKET2"').addClass("btn-update");

            $('#SITE_EXPLORE').trigger('click');

            rankFinder(goto_id);

            rankLister();

            // imperfect solution to allow draggable to re-order
            // selectizer input
            // $('body').on('mousedown', '[id$="_gotoremover"]', function(){
            //     window.setTimeout(function(){
            //
            //         $("#GEN_PLOTS3").addClass("btn-warning");
            //         $('#SITE_EXPLORE').trigger('click');
            //
            //         rankFinder(goto_id);
            //
            //         rankLister();
            //     }, 1500);
            // });
        });
    });

    $(document).ready( function() {
        $('<span class="glyphicon glyphicon-shopping-cart"></span>  Selected Sites').insertAfter($('span:contains("Selected Sites")'))
    });

    // make it so clicking trash can clears bucket
    $('#map-site-clear').on('click', function() {
        // set mapdata as top 3 ranks
        $('#SITE_EXPLORE').trigger('click');
        // $("#GEN_PLOTS3").addClass("btn-warning");

        var rankList = ['nothing'];

        $(".rank-list > .rank-list-item").each((index, elem) => {
          elem.remove();
        });


        Shiny.setInputValue('MAPDATA', arrayToList(rankList));
    });

    // rank list all-clear trash can
    $('body').ready(function(){
        $('body').on('click', '[id$="_gotoremover"]', function(){
            if ($(this).hasClass('rank-list-item')) {
                if ($(this).find('rankremover')) {
                    $(this).remove()

                    // set mapdata as top 3 ranks
                    var rankList = [];

                    $(".rank-list > .rank-list-item").each((index, elem) => {
                      rankList.push(elem.innerHTML);
                    });

                    Shiny.setInputValue('MAPDATA', arrayToList(rankList));
                }
            }
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

    // adding (?) tooltip to Show Q-C checkbox
    $('#showqc').append(
        '</div>' +
        '<div class = "ms-tooltip" style = "display: inline-block; vertical-align: middle"' +
        'title = "Discharge vs. concentration or flux, (precip and precip chem excluded); the y-axis reflects all selections made in the Unit section below.">&#x2753;</div>' +
        '</div>'
    );

    // adding (?) tooltip to Aggregation block
    $('#aggregation').replaceWith(
        '<div style = "white-space: nowrap" class = "widget-title text-center",>' +
        '<div style = "display: inline-block; vertical-align: middle; white-space: normal">' +
        '<div>  Aggregation  </div>' +
        '</div>' +
        '<div class = "ms-tooltip" style = "display: inline-block; vertical-align: middle"' +
        'title = "Daily aggregation requires that Unit is set to "Concentration" or "Flux". Precipitation aggregates by sum everything else by mean. See Notes/Caveats tab for more.">&#x2753;</div>' +
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

   // $('body').ready(function(){
   //     $('#SHOW_PCHEM3').click(govern_flux3);
   // });

    //only show QC plots when their box is checked
    function govern_qc3(){
        if( $('#SHOW_QC3').is(':checked')){
            $('[id^="inlineQC3"').css('width', '25%');
            $('[id^="inlineMAIN3"').css('width', '75%');
        } else {
            $('[id^="inlineQC3"]').css('width', '0%');
            $('[id^="inlineMAIN3"]').css('width', '100%');
        }

        $('#REFRESH').trigger('click');
    };

    $('body').ready(function(){
        $('#GEN_PLOTS3').click(govern_qc3);
        $('#GEN_PLOTS3').click( function() {
            // make sure plotting reflects sort lsit rank order
            rankLister()
            // $(this).removeClass("btn-update");
            $(this).removeClass("btn-warning");
            // $(this).addClass("btn-primary");
        });
    });

    //BIPLOT: disable X-axis selection when aggregation == 'Yearly'
    function govern_xvar_section(){

        var xtype_ctrl = $('#X_TYPE2').selectize()[0].selectize;
        var agg2_val = $('input[name=AGG2]:checked').val();
        var input_val_mapping = {'MONTHLY2': 'Month', 'YEARLY2': 'Year'};

        if( ['MONTHLY2', 'YEARLY2'].includes(agg2_val) ){

            var xval = input_val_mapping[agg2_val];

            xtype_ctrl.addOption({value: xval, label: xval}, silent=true);
            xtype_ctrl.addItem(xval);
            Shiny.setInputValue('X_TYPE2', xval);

            xtype_ctrl.disable();
            $('#LOG_X2').hide();
        } else {
            xtype_ctrl.removeOption('Year');
            xtype_ctrl.enable();
            $('#LOG_X2').show();
        }
    };

    //$(document).one('shiny:sessioninitialized', function(event){
    $(document).one('shiny:idle', function(event){
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
        console.log('mutation')

        //  info tab stuff
        // map pop up info button activation
          $('body').on('click', "#info_trigger", function() {
              console.log("I have been clicked, and i am the iNFO button!")
              $('a[data-value="Watershed"]').trigger("click");
          });

        var tgt = $(target);
        var shinymodal = tgt.children('#shiny-modal-wrapper');
        //
        // if(shinymodal.length > 0){
        //
        var modal_id = shinymodal.find('.modal-body').attr('id');
        console.log(modal_id)
            if(modal_id == 'undefined'){

                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                    $('#landing #TAKE_TOUR').show();
                    $('#DATA_TOUR').removeAttr('disabled');
                    $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
                }, 1000);
                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                    $('#landing #TAKE_TOUR').show();
                    $('#DATA_TOUR').removeAttr('disabled');
                    $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
                }, 3000);
                window.setTimeout(function(){
                    $('.loading-container').hide();
                    $('#landing #DISMISS_MODAL').show();
                    $('#landing #TAKE_TOUR').show();
                    $('#DATA_TOUR').removeAttr('disabled');
                    $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
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
        // };
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

    //conduct site tour
    $('body').on('click', '#TAKE_TOUR', function(i, v){
        $('#DISMISS_MODAL').trigger('click');
    });

    $('body').on('click', '.cicerone1a .driver-next-btn', function(i, v){
        $('a[data-value="biplot"]').trigger('click');
        Shiny.setInputValue('CONTINUE_TOUR', '' + new Date());
    });

    //conduct data tour

    var set_tour_location = function(dmns, sites, vars){
        //dmns: array of up to 3 string elements
        //sites: array of up to 3 string elements
        //vars: array of up to 3 objects e.g. {value: 'SO4_S', label: 'Sulfate-S (mg/L)'}

        var dmn_ctrl = $('#DOMAINS3').selectize()[0].selectize;
        var sit_ctrl = $('#SITES3').selectize()[0].selectize;
        var var_ctrl = $('#VARS3').selectize()[0].selectize;

        dmn_ctrl.clear(silent=true);
        sit_ctrl.clearOptions(silent=true);
        var_ctrl.clearOptions(silent=true);

        for(e of dmns){
            dmn_ctrl.addItem(e, silent=true);
        };

        Shiny.setInputValue('DOMAINS3', dmns);

        for(var i = 0; i < sites.length; i++){

            var st = sites[i]
            sit_ctrl.addOption({value: st, label: st}, silent=true);

            if(i == sites.length - 1){
                sit_ctrl.addItem(st, silent=true);
            } else {
                sit_ctrl.addItem(st, silent=false);
            };
        };

        Shiny.setInputValue('SITES3', sites);

        for(e of vars){
            var_ctrl.addOption(e, silent=true);
            var_ctrl.addItem(e.value, silent=true);
        };

        Shiny.setInputValue('VARS3', vars.map(x => x.value));
    };

    window.enable_data_tour = false;
    window.next_tour_id = 'a';

    $('body').on('click', '.driver-close-btn', function(){

        //** modify this when adding tour stops
        if(/cicerone[0-9][a]/.test( $(this).parent().parent().attr('class') )){
            Shiny.setInputValue('TRIGGER_LOADING_DOTS', 'loading', {priority: 'event'});
        }
    });

    $('body').on('click', '#DATA_TOUR', function(i, v){

        enable_data_tour = true;
        next_tour_id = 'a';

        $('a[data-value="multisite_exploration"]').trigger('click');

        set_tour_location(['hbef'], ['w6'], [{value: 'SO4_S', label: 'Sulfate-S (mg/L)'}])

        $('#SHOW_PCHEM3').prop('checked', false);
        $('#showqc').prop('checked', false);
        $('input[name="CONC_FLUX3"][value="Concentration"]').prop('checked', true);
        $('input[name="AGG3"][value="Monthly"]').prop('checked', true);
        $('input[name="INSTALLED_V_GRAB3"][value="G"]').prop('checked', true);
        $('input[name="INSTALLED_V_GRAB3"][value="I"]').prop('checked', true);
        $('input[name="SENSOR_V_NONSENSOR3"][value="S"]').prop('checked', true);
        $('input[name="SENSOR_V_NONSENSOR3"][value="N"]').prop('checked', true);
        $('#FLAGS3').prop('checked', true);
        $('#INTERP3').prop('checked', true);

        Shiny.setInputValue('SHOW_PCHEM3:logical', 'false');
        Shiny.setInputValue('showqc:logical', 'false');
        Shiny.setInputValue('CONC_FLUX3', 'Concentration');
        Shiny.setInputValue('AGG3', 'Monthly');
        Shiny.setInputValue('INSTALLED_V_GRAB3', ['G', 'I']);
        Shiny.setInputValue('SENSOR_v_NONSENSOR3', ['S', 'N']);
        Shiny.setInputValue('FLAGS3:logical', 'true');
        Shiny.setInputValue('INTERP3:logical', 'true');

        Shiny.setInputValue('START_DATA_TOUR', '' + new Date());
    });

    $('body').on('click', '.cicerone2a .driver-close-btn', function(i, v){

        Shiny.setInputValue('basedata_change_reloads_plots', 1);
        $('input[name="AGG3"][value="Daily"]').prop('checked', true);
        Shiny.setInputValue('AGG3', 'Daily');
        next_tour_id = 'b';
        set_tour_location(['hjandrews'], ['GSWS09', 'GSWS10'], [{value: 'NO3_N', label: 'Nitrate-N (mg/L)'}]);
        //$('#GEN_PLOTS3').trigger('click');

    });

    $('#VARS_INVISIBLE3').on('shiny:inputchanged', function(event){
        Shiny.setInputValue('basedata_change_reloads_plots', 0);
    });


    //all this crap (and associated CSS) is necessary just to make "macrosheds.org"
    //appear (and STAY appeared) in the bottom of each dygraph as an annotation

    window.enable_attribution = false;

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
        enable_attribution = true;
    });

        //window.setTimeout(function(){
        //    $('#SLIDER_UPDATES_PLOTS').trigger('click');
        //}, 1000); //wait for timeslider debounce
        // var global_trigger = 0;

        $('div[id^="GRAPH_"]').on('shiny:value', function(event){
            let $this = $(this)
            let is_qc_plot = /GRAPH_QC3./.test( $this.attr('id') )
            let is_plot = /GRAPH_MAIN3./.test( $this.attr('id') )

            if(! is_qc_plot){
                $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
                shinyjs.enable("DATES3");

                $('#loading-start').hide();
                // global_trigger++
                // console.log(global_trigger);

                if(enable_data_tour){
                    Shiny.setInputValue('CONTINUE_DATA_TOUR', next_tour_id);
                };
            }

        if(enable_attribution){
            window.setTimeout(function(){
                $this.children('[class^="ms-attribution"]').remove()
                if($this.attr('id') === 'GRAPH_Q3'){
                    $this.css('position', 'relative').append($('<p class="ms-attribution-high">macrosheds.org</p>'));
                } else if(is_qc_plot){
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

// Map State Toggling: Map, Hybdrid, Data (and, map/fullmap toggling)

var vizToggle = 0;

$(document).ready(function(){
    $('#COLLAPSE_DATA').click(function(){
        vizToggle++;

        if (vizToggle%2 != 0) {

            //  map and table stuff
            $(".table").css("min-width", "95%");

            //  button stuff
            $(".map-mode").css("opacity", "0");
            $(".data-mode").toggleClass("glyphicon-circle-arrow-left glyphicon-circle-arrow-right");
            $('#COLLAPSE_DATA').css({'margin': '6px'});
            console.log("expand: data table view");
        } else {
            $(".table").css("min-width", "90%");
            $(".map-mode").css("opacity", "1");
            $(".data-mode").toggleClass("glyphicon-circle-arrow-right glyphicon-circle-arrow-left");
            console.log("collapse: data table view");
        }

    });
});

var mapsToggle = 0;

$(document).ready(function(){
    // on lcicking button to exp/collapse map
    $('#COLLAPSE_SIDEBAR').click(function(){
        //  map counter
        mapsToggle++;

        // if map is not expanded
        if (mapsToggle%2 != 0) {
            // make non-viz button invisible
            $(".data-mode").css("opacity", "0");
            $(".map-mode").toggleClass("glyphicon-circle-arrow-left glyphicon-circle-arrow-right");
            console.log("expand: map mode")
        } else {
            $(".data-mode").css("opacity", "1");
            $(".map-mode").toggleClass("glyphicon-circle-arrow-right glyphicon-circle-arrow-left");
            // $(".map-mode-toggle").toggleClass("map-mode-toggle map-mode-toggle");
            console.log("collapse: map-mode, return to hybrid mode");
        }

    });
});

// attribute toggle
var attrToggle = 0;

$(document).ready(function(){

    var window_height = ($('#sidebarCollapsed').height() * .38);
    $('#MAP').css('height', window_height);

    $('#COLLAPSE_ATTRIBUTES').click(function(){
        attrToggle++;


        if (attrToggle%2 == 0) {

            //  button stuff
            $(".full-map-mode").toggleClass("glyphicon-menu-up glyphicon-menu-down");
            console.log("expand: full map view");
            // dissapear attr table and footer
            $(".rank-list-container").addClass("hidden")
            $("#notes-footer").addClass("hidden")
            // reset map height
            var window_height = ($('#sidebarCollapsed').height() * .8);
            $('#MAP').css('height', window_height);

        } else {
            $(".full-map-mode").toggleClass("glyphicon-menu-down glyphicon-menu-up");
            console.log("collapse: small map, large attribute table view");

            // reappear attr table and footer
            $(".rank-list-container").removeClass("hidden")
            $("#notes-footer").removeClass("hidden")
            $('#MAP').css('height', '350px');
        }

    });
});

// make sure data tab panel is matching map tab panel height, and, remove white space
$(document).ready(function()
    {
       var resizeDelay = 200;
       var doResize = true;
       var resizer = function () {
          if (doResize) {

            //your code that needs to be executed goes here
            $(".data-wrapper-wide").css("height", $(".wrapper").height());
            //  check process
            console.log($(".content-wrapper").height());
            console.log($(".main-sidebar").height());
            $(".content-wrapper").css("height", $(".main-sidebar").height());
            console.log($(".content-wrapper").height());
            console.log('RESIZE')


            doResize = false;
          }
        };
        var resizerInterval = setInterval(resizer, resizeDelay);
        resizer();

        $(window).resize(function() {
          doResize = true;
        });
});
// landing slideshow

function showSlides(n) {
  var i;
  var slides = document.getElementsByClassName("mySlides");
  var dots = document.getElementsByClassName("dot");
  if (n > slides.length) {slideIndex = 1}
    if (n < 1) {slideIndex = slides.length}
    for (i = 0; i < slides.length; i++) {
      slides[i].style.display = "none";
    }
    for (i = 0; i < dots.length; i++) {
      dots[i].className = dots[i].className.replace(" active", "");
    }
  slides[slideIndex-1].style.display = "block";
  dots[slideIndex-1].className += " active";
}

var slideIndex = 1;

$(document).ready(function() {
    showSlides(slideIndex);
});

function plusSlides(n) {
  showSlides(slideIndex += n);
}

function currentSlide(n) {
  showSlides(slideIndex = n);
}

// keeping modal reactivity without splash
$(document).ready(function() {
    if ('#loading-start:visible') {
        console.log("loading in progress")
    }
    if ('#loading-start:hidden') {
        console.log("loading done")
        //
        window.setTimeout(function(){
            $('.loading-container').hide();
            $('#landing #DISMISS_MODAL').show();
            $('#landing #TAKE_TOUR').show();
            $('#DATA_TOUR').removeAttr('disabled');
            $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
        }, 1000);
        window.setTimeout(function(){
            $('.loading-container').hide();
            $('#landing #DISMISS_MODAL').show();
            $('#landing #TAKE_TOUR').show();
            $('#DATA_TOUR').removeAttr('disabled');
            $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
        }, 3000);
        window.setTimeout(function(){
            $('.loading-container').hide();
            $('#landing #DISMISS_MODAL').show();
            $('#landing #TAKE_TOUR').show();
            $('#DATA_TOUR').removeAttr('disabled');
            $('#GEN_PLOTS3').removeClass('disabled').removeAttr('disabled');
        }, 8000);
    };
});

//  orange attention animation funciton
$(document).ready( function() {
    var intervalUpdate = window.setInterval(function(){
        $('#macrosheds-dot').addClass('update');
        setTimeout(
          function()
              {
                  $('#macrosheds-dot').removeClass('update');
              }, 500);
      }, 1000);

      $('#macrosheds-dot').click( function() {
              clearInterval(intervalUpdate);
          }
      );
})

var dotExcited = 0;

$(document).ready( function() {
          if (dotExcited == 0) {
              $('#macrosheds-dot').click( function() {
                if (dotExcited == 0) {
                var intervalUpdate = window.setInterval(function(){
                    $('#macrosheds-dot-two').addClass('update');
                    setTimeout(
                      function()
                          {
                              $('#macrosheds-dot-two').removeClass('update');
                          }, 500);
                  }, 1000);
                dotExcited++;
                }

                  $('#macrosheds-dot-two').click( function() {
                          clearInterval(intervalUpdate);
                          dotExcited++;
                      }
                  );
              }
          );
      }
  })

// var dotExcited = 0;
//
// $(document).ready( function() {
//           if (dotExcited == 0) {
//               $('#macrosheds-dot').click( function() {
//                 if (dotExcited == 0) {
//                 var intervalUpdate = window.setInterval(function(){
//                     $('#macrosheds-dot-two').addClass('update');
//                     setTimeout(
//                       function()
//                           {
//                               $('#macrosheds-dot-two').removeClass('update');
//                           }, 500);
//                   }, 1000);
//                 dotExcited++;
//                 }
//
//                   $('#macrosheds-dot-two').click( function() {
//                           clearInterval(intervalUpdate);
//                           dotExcited++;
//                       }
//                   );
//               }
//           );
//       }
//   })

var legendsActive = 0;

function layerClicks() {
    // console.log('legends info')
    // console.log(legendsActive)

    // Landcover
    $('#Landcover-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id = "Landcover" class="leaflet-control-wms-legend leaflet-control" style="height: 515px; width: 267px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Land_Cover_L48" alt="Legend" style="float: right; margin-top: 35%;"></div></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "Landcover") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id = "Landcover" class="leaflet-control-wms-legend leaflet-control" style="height: 515px; width: 267px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Land_Cover_L48" alt="Legend" style="float: right; margin-top: 35%;"></div></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id = "Landcover" class="leaflet-control-wms-legend leaflet-control" style="height: 515px; width: 267px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Land_Cover_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Land_Cover_L48" alt="Legend" style="float: right; margin-top: 35%;"></div></div>');
                legendsActive ++;
                };
            });

    // Elevation
    $('#3DEP-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id="3DEP" class="leaflet-control-wms-legend leaflet-control" style="height: 66px; width: 110px;"><img class="wms-legend" src="https://elevation.nationalmap.gov:443/arcgis/services/3DEPElevation/ImageServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=3DEPElevation:Hillshade Elevation Tinted" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "3DEP") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id="3DEP" class="leaflet-control-wms-legend leaflet-control" style="height: 66px; width: 110px;"><img class="wms-legend" src="https://elevation.nationalmap.gov:443/arcgis/services/3DEPElevation/ImageServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=3DEPElevation:Hillshade Elevation Tinted" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div class=""> <div id="3DEP" class="leaflet-control-wms-legend leaflet-control" style="height: 66px; width: 110px;"><img class="wms-legend" src="https://elevation.nationalmap.gov:443/arcgis/services/3DEPElevation/ImageServer/WMSServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=3DEPElevation:Hillshade Elevation Tinted" alt="Legend" style="float: right; margin-top: 35%;"></div>');
            legendsActive ++;
                };
            });

    // Impervious
    $('#Impervious-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div id = "Impervious" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Impervious_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Impervious_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "3DEP") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div id = "Impervious" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Impervious_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Impervious_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div id = "Impervious" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2019_Impervious_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2019_Impervious_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
            legendsActive ++;
                };
            });

    // Tree Canopy
    $('#Tree-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div id = "Tree" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Tree_Canopy_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2016_Tree_Canopy_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "3DEP") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div id = "Tree" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Tree_Canopy_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2016_Tree_Canopy_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div id = "Tree" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Tree_Canopy_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2016_Tree_Canopy_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
            legendsActive ++;
                };
            });

    // Tree Canopy Change
    // $('#TreeCanopyChange-Leg').click( function() {
    //     if (legendsActive > 0) {
    //         $('.leaflet-control-wms-legend').replaceWith('<div></div>')
    //         $('.leaflet-top.leaflet-right').append('<div class="leaflet-control-wms-legend leaflet-control"><img class="wms-legend" src="mrlc_display/NLCD_Tree_Canopy_Change_Index_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_Tree_Canopy_Change_Index_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
    //     } else if ($('.leaflet-control-wms-legend').length) {
    //         console.log('legend already present')
    //         $('.leaflet-control-wms-legend').replaceWith('<div></div>')
    //     } else {
    //         $('.leaflet-top.leaflet-right').append('<div class="leaflet-control-wms-legend leaflet-control"><img class="wms-legend" src="mrlc_display/NLCD_Tree_Canopy_Change_Index_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_Tree_Canopy_Change_Index_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
    //         legendsActive ++;
    //             };
    //         });

    // Geology
    $('#Geology-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div id="Geology" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.sciencebase.gov/arcgis/services/Catalog/5888bf4fe4b05ccb964bab9d/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=0" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "Geology") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div id="Geology" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.sciencebase.gov/arcgis/services/Catalog/5888bf4fe4b05ccb964bab9d/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=0" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div id="Geology" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.sciencebase.gov/arcgis/services/Catalog/5888bf4fe4b05ccb964bab9d/MapServer/WmsServer?request=GetLegendGraphic%26version=1.3.0%26format=image/png%26layer=0" alt="Legend" style="float: right; margin-top: 35%;"></div>');
            legendsActive ++;
                };
            });

    // Land Cover Change
    $('#LCChange-Leg').click( function() {
        if ($('.leaflet-control-wms-legend')[0] == undefined) {
            $('.leaflet-top.leaflet-right').append('<div id = "LCChange" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2001_2019_change_index_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2001_2019_change_index_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend')[0].id.toString().trim() == "LCChange") {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else if (legendsActive > 0) {
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
            $('.leaflet-top.leaflet-right').append('<div id = "LCChange" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2001_2019_change_index_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2001_2019_change_index_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
        } else if ($('.leaflet-control-wms-legend').length) {
            console.log('legend already present')
            $('.leaflet-control-wms-legend').replaceWith('<div></div>')
        } else {
            $('.leaflet-top.leaflet-right').append('<div id = "LCChange" class="leaflet-control-wms-legend leaflet-control" style="height: 20px; width: 20px;"><img class="wms-legend" src="https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2001_2019_change_index_L48/ows?service=WMS&amp;request=GetLegendGraphic&amp;format=image%2Fpng&amp;width=20&amp;height=20&amp;layer=NLCD_2001_2019_change_index_L48" alt="Legend" style="float: right; margin-top: 35%;"></div>');
            legendsActive ++;
                };
            });
}

$(document).ready(function() {
    layerClicks();
        });

$(document).ready( function() {
    $(window).on('resize', function() {
        if ($("#attribute-content").width() < 500) {
            // console.log("very small attribute table");
            $('#legend-button').replaceWith(
                    '<div><div class="dropup" id="legend-button"><button class="dropdown-toggle" type="button" id="about-us" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false"><i id= "my-legend" class="well-sm gi-1-x glyphicon glyphicon-th-list" role="button"></i><span class="caret"></span></button><ul class="dropdown-menu" aria-labelledby="legend-drop"><li><a id="3DEP-Leg" href="#">3DEP Elevation</a></li><li><a id="Tree-Leg" href="#">Tree Canopy</a></li><li><a id="Impervious-Leg" href="#">Impervious Surfaces</a></li><li><a id="Landcover-Leg" href="#">Landcover</a></li><li><a id="LCChange-Leg" href="#">Landcover Change</a></li><li><a id="Geology-Leg" href="#">Geology</a></li></ul></div></div>'
                     // <li><a id="Ecoregions" href="#">Ecoregions</a></li> <li><a id="TreeCanopyChange-Leg" href="#">Tree Canopy Change</a></li>
                );
            layerClicks();
            } else {
                $('#legend-button').replaceWith('<div class="dropup" id="legend-button"><button class="btn btn-primary dropdown-toggle" type="button" id="about-us" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">Legend <span class="caret"></span></button><ul class="dropdown-menu" aria-labelledby="legend-drop"><li><a id="3DEP-Leg" href="#">3DEP Elevation</a></li><li><a id="Tree-Leg" href="#">Tree Canopy</a></li><li><a id="Impervious-Leg" href="#">Impervious Surfaces</a></li><li><a id="Landcover-Leg" href="#">Landcover</a></li><li><a id="LCChange-Leg" href="#">Landcover Change</a></li><li><a id="Geology-Leg" href="#">Geology</a></li></ul></div>');
                layerClicks();
                // <li><a id="TreeCanopyChange-Leg" href="#">Tree Canopy Change</a></li>  <li><a id="Ecoregions" href="#">Ecoregions</a></li>
            }
        });
    });

// 2) this variable triggers legend icon to be associated with legend or legend URL for active layer
// 3) legend icon on click/hover displays legend visual
