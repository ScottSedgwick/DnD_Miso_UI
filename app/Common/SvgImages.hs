{-# LANGUAGE OverloadedStrings  #-}
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

discordImage :: View model action
discordImage = 
  H.svg_
  [ P.xmlns_ "http://www.w3.org/2000/svg"
  , SP.viewBox_ "0 0 24 24"
  , SP.fill_ "currentColor"
  ]
  [ S.path_
    [ SP.d_ "M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189Z"
    ]
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

homeImage :: View model action
homeImage = 
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
  [ S.rect_
    [ SP.rx_ "1"
    , SP.y_ "3"
    , SP.x_ "3"
    , P.height_ "9"
    , P.width_ "7"
    ]
  , S.rect_
    [ SP.rx_ "1"
    , SP.y_ "3"
    , SP.x_ "14"
    , P.height_ "5"
    , P.width_ "7"
    ]
  , S.rect_
    [ SP.rx_ "1"
    , SP.y_ "12"
    , SP.x_ "14"
    , P.height_ "9"
    , P.width_ "7"
    ]
  , S.rect_
    [ SP.rx_ "1"
    , SP.y_ "16"
    , SP.x_ "3"
    , P.height_ "5"
    , P.width_ "7"
    ]
  ]

githubSideImage :: View model action
githubSideImage = 
  H.svg_
  [ P.xmlns_ "http://www.w3.org/2000/svg"
  , SP.viewBox_ "0 0 24 24"
  , SP.fill_ "currentColor"
  ]
  [ H.title_ [] ["GitHub"]
  , S.path_
      [ SP.d_
          "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12"
      ]
  ]

discordSideImage :: View model action
discordSideImage = 
  H.svg_
  [ P.xmlns_ "http://www.w3.org/2000/svg"
  , SP.viewBox_ "0 0 24 24"
  , SP.fill_ "currentColor"
  ]
  [ H.title_ [] ["Discord"]
  , S.path_
      [ SP.d_
          "M20.317 4.3698a19.7913 19.7913 0 00-4.8851-1.5152.0741.0741 0 00-.0785.0371c-.211.3753-.4447.8648-.6083 1.2495-1.8447-.2762-3.68-.2762-5.4868 0-.1636-.3933-.4058-.8742-.6177-1.2495a.077.077 0 00-.0785-.037 19.7363 19.7363 0 00-4.8852 1.515.0699.0699 0 00-.0321.0277C.5334 9.0458-.319 13.5799.0992 18.0578a.0824.0824 0 00.0312.0561c2.0528 1.5076 4.0413 2.4228 5.9929 3.0294a.0777.0777 0 00.0842-.0276c.4616-.6304.8731-1.2952 1.226-1.9942a.076.076 0 00-.0416-.1057c-.6528-.2476-1.2743-.5495-1.8722-.8923a.077.077 0 01-.0076-.1277c.1258-.0943.2517-.1923.3718-.2914a.0743.0743 0 01.0776-.0105c3.9278 1.7933 8.18 1.7933 12.0614 0a.0739.0739 0 01.0785.0095c.1202.099.246.1981.3728.2924a.077.077 0 01-.0066.1276 12.2986 12.2986 0 01-1.873.8914.0766.0766 0 00-.0407.1067c.3604.698.7719 1.3628 1.225 1.9932a.076.076 0 00.0842.0286c1.961-.6067 3.9495-1.5219 6.0023-3.0294a.077.077 0 00.0313-.0552c.5004-5.177-.8382-9.6739-3.5485-13.6604a.061.061 0 00-.0312-.0286zM8.02 15.3312c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9555-2.4189 2.157-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.9555 2.4189-2.1569 2.4189zm7.9748 0c-1.1825 0-2.1569-1.0857-2.1569-2.419 0-1.3332.9554-2.4189 2.1569-2.4189 1.2108 0 2.1757 1.0952 2.1568 2.419 0 1.3332-.946 2.4189-2.1568 2.4189Z"
      ]
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