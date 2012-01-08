import Graphics.UI.Gtk

main = do
    initGUI
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10, containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
