participants_tab <- tabPanel("Participants",
    value = "participants",
    style = "overflow-y: scroll; overflow-x: visible;",
    div(
        style = "padding: 20px;",
        fluidRow(
            class = "text-center people-row",
            h2("Principal Investigators"),
            br(),
            column(
                # class = "people-column",
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/matthew_ross.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b><strong>Matt Ross</strong></b><br>Colorado State</p>'),
                        img(
                            src = "logo/csu.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
            ),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/emily_bernhardt.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Emily Bernhardt</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                )
            )
        ),
        br(),
        fluidRow(
            class = "text-center people-row",
            h2("Steering Committee"),
            br(),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/jill_baron.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Jill Baron</b><br>Colorado State, USGS</p>'),
                        img(
                            src = "logo/usgs.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                        img(
                            src = "logo/csu.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/emma_j_rosi.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Emma J. Rosi</b><br>Cary Institute</p>'),
                        img(
                            src = "logo/cary.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/kaelin_cawley.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Kaelin Cawley</b><br>NEON</p>'),
                        img(
                            src = "logo/neon.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            ),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/nandita_basu.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Nandita Basu</b><br>University of Waterloo</p>'),
                        img(
                            src = "logo/waterloo.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/megan_jones.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Megan Jones</b><br>NEON</p>'),
                        img(
                            src = "logo/neon.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            )
        ),
        br(),
        fluidRow(
            class = "text-center people-row",
            h2("Data Scientists"),
            br(),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/michael_vlah.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Mike Vlah</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/wes_slaughter.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Weston Slaughter</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            ),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/spencer_rhea.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Spencer Rhea</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/cody_flagg.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Cody Flagg</b><br>NEON</p>'),
                        img(
                            src = "logo/neon.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            )
        ),
        br(),
        fluidRow(
            class = "text-center people-row",
            h2("Postdocs"),
            br(),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/amanda_delvecchia.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Amanda DelVecchia</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br(),
                br(),
                div(
                    class = "people-column",
                    img(
                        src = "participants/anna_bergstrom.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Anna Bergstrom</b><br>Boise State</p>'),
                        img(
                            src = "logo/boise_state.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            ),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/nick_marzolf.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Nick Marzolf</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            )
        ),
        br(),
        fluidRow(
            class = "text-center people-row",
            h2("Graduate Students"),
            br(),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/nicholas_gubbins.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Nicholas Gubbins</b><br>Colorado State</p>'),
                        img(
                            src = "logo/csu.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            ),
            column(
                6,
                div(
                    class = "people-column",
                    img(
                        src = "participants/audrey_thellman.jpg",
                        style = "height: 150px;"
                    ),
                    div(
                        class = "people-container",
                        HTML('<p class="leftpanel-text"><b>Audrey Thellman</b><br>Duke</p>'),
                        img(
                            src = "logo/duke.png",
                            style = "height: 50px;",
                            class = "logo"
                        ),
                    ),
                ),
                br()
            )
        )
    )
)
