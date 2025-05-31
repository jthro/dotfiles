import { App } from "astal/gtk3"
import style from "./style.scss"
import Bar from "./widget/Bar"
import OSD from "./widget/OSD"
import NotificationPopups from "./widget/NotificationPopups"
import Applauncher from "./widget/Applauncher"

let launcher: boolean = false

App.start({
    css: style,
    main() {
        App.get_monitors().map(Bar)
        App.get_monitors().map(OSD)
        App.get_monitors().map(NotificationPopups)
        App.get_monitors().map(Applauncher)
    },
})
