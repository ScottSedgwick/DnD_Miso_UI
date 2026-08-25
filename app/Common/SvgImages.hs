module Common.SvgImages where

import qualified Miso.Html.Element as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP
import           Miso.Types ( View )

toggleSidebarImage :: View model action
toggleSidebarImage =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "currentcolor"
  , SP.fill_ "none"
  , SP.viewBox_ "0 0 24 24"
  , P.height_ "24"
  , P.width_ "24"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  ]
  [ S.rect_
      [ SP.rx_ "2"
      , SP.y_ "3"
      , SP.x_ "3"
      , P.height_ "18"
      , P.width_ "18"
      ]
  , S.path_ [ SP.d_ "M9 3v18"]
  ]

githubImage :: View model action
githubImage =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "currentcolor"
  , SP.fill_ "none"
  , SP.viewBox_ "0 0 24 24"
  , P.height_ "24"
  , P.width_ "24"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  ]
  [ S.path_
    [ SP.d_
        "M15 22v-4a4.8 4.8 0 0 0-1-3.5c3 0 6-2 6-5.5.08-1.25-.27-2.48-1-3.5.28-1.15.28-2.35 0-3.5 0 0-1 0-3 1.5-2.64-.5-5.36-.5-8 0C6 2 5 2 5 2c-.3 1.15-.3 2.35 0 3.5A5.403 5.403 0 0 0 4 9c0 3.5 3 5.5 6 5.5-.39.49-.68 1.05-.85 1.65-.17.6-.22 1.23-.15 1.85v4"
    ]
  , S.path_ [ SP.d_ "M9 18c-4.51 2-5-2-7-2"]
  ]

toggleDarkModeImage1 :: View model action
toggleDarkModeImage1 =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "currentcolor"
  , SP.fill_ "none"
  , SP.viewBox_ "0 0 24 24"
  , P.height_ "24"
  , P.width_ "24"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  ]
  [ S.circle_ [ SP.r_ "4", SP.cy_ "12", SP.cx_ "12" ]
  , S.path_ [ SP.d_ "M12 2v2" ]
  , S.path_ [ SP.d_ "M12 20v2" ]
  , S.path_ [ SP.d_ "m4.93 4.93 1.41 1.41" ]
  , S.path_ [ SP.d_ "m17.66 17.66 1.41 1.41" ]
  , S.path_ [ SP.d_ "M2 12h2" ]
  , S.path_ [ SP.d_ "M20 12h2" ]
  , S.path_ [ SP.d_ "m6.34 17.66-1.41 1.41" ]
  , S.path_ [ SP.d_ "m19.07 4.93-1.41 1.41" ]
  ]

toggleDarkModeImage2 :: View model action
toggleDarkModeImage2 =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "currentcolor"
  , SP.fill_ "none"
  , SP.viewBox_ "0 0 24 24"
  , P.height_ "24"
  , P.width_ "24"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  ]
  [ S.path_
    [ SP.d_ "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z"]
  ]

upDownChevrons :: View model action
upDownChevrons =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "currentColor"
  , SP.fill_ "none"
  , SP.viewBox_ "0 0 24 24"
  , P.height_ "24"
  , P.width_ "24"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  ]
  [ S.path_ [ SP.d_ "m7 15 5 5 5-5" ]
  , S.path_ [ SP.d_ "m7 9 5-5 5 5" ]
  ]

dragonImage :: View model action
dragonImage =
  H.svg_
  [ SP.strokeLinejoin_ "round"
  , SP.strokeLinecap_ "round"
  , SP.strokeWidth_ "2"
  , SP.stroke_ "red"
  , SP.fill_ "red"
  , SP.viewBox_ "0 0 122.88 106.68"
  , P.xmlns_ "http://www.w3.org/2000/svg"
  , P.height_ "24"
  , P.width_ "24"
  ]
  [ S.path_ [ SP.d_ "M52.4,20.33C60.04,15.89,63.76,8.53,68.19,0c2.26,2.44,3.48,8.44,2.98,16.83 c21.29-1.39,34.78,10.58,51.71-7.42C115.6,37.52,89.32,21.55,78.34,24c16.95,1.92,21.76,11.28,41.28,7.96 c-6.72,8.09-21.59,6.45-34.11,0.75c2.21,6.08,24.83,10.01,20.22,28.16c-4.36-7.49-8.45-12.09-12.29-14.08 c7.91,13.68,7.29,26.15-0.09,39.78c-1.63,3-3.62,5.73-6.05,8.09c3.38-7.47,5.02-14.43,3.58-20.48 c-1.37,5.2-26.98,55.76-13.31,14.34C50.27,115.34,23.7,88.04,0,106.68c7.91-37.07,50.98-22.56,66.84-39.74 c3.26-3.54,4.44-8.33,3.04-11.78c-4.08-10.04-18.37-7.09-23.55-0.96c-3.19,4.91-9.37,10.37-14.44,4.29 c-0.83-0.99-1.3-2.26-1.43-3.78c2.56,1.09,5.12,1.51,7.68,0.26l-2.05-2.3c2.73-0.77,5.78,0.09,8.19-2.42l8.96-5.89 c1.71-1.26-2.13-2.07-11.26-2.7c-0.77,1.99-0.42,3.81,1.79,5.38c-2.62,0.35-4.89-0.35-6.66-2.56c-1.04,0.81-0.89,1.38,1.9,5.81 c-2.51,0.11-5.02-0.89-7.53-2.48c-0.58,2.02,0.27,4.04,0.41,6.06c-1.83-0.53-3.51-1.4-5.02-2.62c-1.52-1.23-2.89-2.8-4.09-4.72 c-0.83-1.88-0.82-3.29-0.18-4.34c0.39-0.65,1.02-1.16,1.85-1.57c3.1-1.53,4.66-1.35,8.29-3.77c3.43-2.28,6.93-5.01,9.91-8.69 C47.47,22.2,47.34,23.27,52.4,20.33L52.4,20.33z M51.69,29.57c-1.19,0.49-1.28,1.07-2.92,1.52c-0.49,0.13-1.35,0.87-1.62,1.3 c-0.84,1.37,2.22,2.1,3.14,0.46c0.23-0.4,1.26-2.3,1.34-2.53c0.08-0.24,0.29-0.43,0.13-0.7C51.74,29.62,51.73,29.56,51.69,29.57 L51.69,29.57z" ]
  ]
