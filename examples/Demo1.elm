
import Layout exposing (Layout)

view = Layout.placeholder "view"

main = Layout.toHtml {x=0,y=0,w=800,h=600} view