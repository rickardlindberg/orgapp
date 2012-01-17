import Graphics.UI.Gtk
import Graphics.UI.Gtk.Builder

main = do
    initGUI

    builder <- builderNew
    builderAddFromFile builder "interface.glade"

    mainWindow <- builderGetObject builder castToWindow "main_window"
    onDestroy mainWindow mainQuit

    helloWorldButton <- builderGetObject builder castToButton "hello_world_button"
    onClicked helloWorldButton (putStrLn "Hello World")

    widgetShowAll mainWindow
    mainGUI
