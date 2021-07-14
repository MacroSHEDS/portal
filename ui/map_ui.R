map_tab = tabPanel('Map',
    leafletOutput("MAP", height=350),
    br(),
    p('* See "Map" section of the Notes/Caveats tab',
      class = 'leftpanel-text'),

    #this button opens the site exploration tab. the button is hidden and unclickable.
    #it's triggered by links in the popups on the map tab.
    actionButton('SITE_EXPLORE', '', style='display: none')
)
