import { App, Astal, Gtk, Gdk } from "astal/gtk3"
import { Variable } from "astal"
import GLib from "gi://GLib?version=2.0"
import Hyprland from "gi://AstalHyprland"
import { bind } from "astal"
import Battery from "gi://AstalBattery"
import { Label } from "astal/gtk3/widget"
import Tray from "gi://AstalTray"
import Cava from "gi://AstalCava"
import Network from "gi://AstalNetwork"

const time = Variable("").poll(1000, "date")

export default function Bar(gdkmonitor: Gdk.Monitor) {
    const { BOTTOM, LEFT, TOP } = Astal.WindowAnchor

    return <window
        className="Bar"
        gdkmonitor={gdkmonitor}
        exclusivity={Astal.Exclusivity.EXCLUSIVE}
        anchor={BOTTOM | LEFT | TOP}
        application={App}>
        <centerbox vertical={true} valign={Gtk.Align.FILL}>
            <centerbox vertical={true} className="top" valign={Gtk.Align.FILL}>
                <Workspaces /> 
                <centerbox vertical={true} className="status" valign={Gtk.Align.FILL} >
                    <Time />
                    <Internet />
                    <BatteryLevel />
                </centerbox>
            </centerbox>
            <AudioVisualiser />
            <box className="trayContainer" halign={Gtk.Align.CENTER}><SysTray /></box>
        </centerbox>
    </window>
}

function SysTray() {
    const tray = Tray.get_default()
    return <box valign={Gtk.Align.END} halign={Gtk.Align.CENTER} className="SysTray" vertical={true}>
        {bind(tray, "items").as(items => items.map(item => (
            <menubutton className="SysTrayButton"
                tooltipMarkup={bind(item, "tooltipMarkup")}
                usePopover={false}
                actionGroup={bind(item, "actionGroup").as(ag => ["dbusmenu", ag])}
                menuModel={bind(item, "menuModel")}>
                <icon gicon={bind(item, "gicon")} iconSize={64}/>
            </menubutton>
        )))}
    </box>
}

function Internet() {
    const network = Network.get_default()

    const wiredSymbol = bind(network, "wired").as(wired => {
        return (wired.get_state() === Network.DeviceState.ACTIVATED) ? "" : "󰌙"
    })

    const wifiSymbol = bind(network, "wifi").as(wifi => {
            return (wifi.get_state() === Network.DeviceState.ACTIVATED) ? "󰤨" : "󰤭"
    })

    return <box className="internet" vertical={true} valign={Gtk.Align.CENTER}>
        <label label={wifiSymbol} />
        <label label={wiredSymbol} />
    </box>
}

function Time({ format = "%H\n%M"}) {
    const time = Variable<string>("").poll(1000, () =>
        GLib.DateTime.new_now_local().format(format)!)

    return <box className="time" halign={Gtk.Align.CENTER}>
        <label
            onDestroy={ () => time.drop() }
            label={time()}
        />
    </box>
}

function Workspaces() {
    const hypr = Hyprland.get_default()

    const icons = new Set<[string, string]>([
        ["Discord", ""],
        ["nvim", ""],
        ["Firefox", "󰈹"],
        ["Emacs", ""],
        ["Steam", ""],
        ["GT: New Horizons", "󰍳"],
    ])


    return <box className="Workspaces" vertical={true}>
        {bind(hypr, "workspaces").as(wss => wss
            .filter(ws => !(ws.id >= -99 && ws.id <= -2))
            .sort((a, b) => a.id - b.id)
            .map(ws => (
                <button
                    className={bind(hypr, "focusedWorkspace").as(fw =>
                        ws === fw ? "focused" : "")}
                    onClicked={() => ws.focus()}>
                    {bind(ws, "last_client").as(cli => {
                        if (cli != null) { 
                            for (let [name, icon] of icons) {
                                if (cli.get_title().includes(name)) {
                                    return icon
                                }
                            }
                        }
                        return ""
                    })}
                </button>
            ))
        )}
    </box>

}

function BatteryLevel() {
    const bat = Battery.get_default()

    return <box className="Battery">
    <box>
        <label className="Charging" label={bind(bat, "charging").as(charging => charging ? "󱐋" : "")} />
        <Label className="BatterySymbol" label={bind(bat, "percentage").as(p => {
            const scaled = Math.floor(p * 100)
            if (scaled > 75) {
                return "󱊣"
            } else if (scaled > 40) {
                return "󱊢"
            } else if (scaled > 10) {
                return "󱊡"
            } else {
                return "󰂎"
            }
        })} />
    </box>
    <label label={bind(bat, "percentage").as(p =>
        `${Math.floor(p * 100)}%`)} />
    </box>
}

function AudioVisualiser() {
    const fg_color = "#4fd6be"
    const cava = Cava.get_default()
    cava.set_bars(9)
    return <box className="audioVisualiser" vertical={true} spacing={4}>
    {bind(cava, "values").as(values => values.reverse().map(value => (
            <box
                css={`all: unset; background-color: ${fg_color}; margin-right: ` + Math.max((20 - value * 10), 2) + "px; margin-left: " + Math.max((20 - value * 10), 2) + "px; border-radius: 5px; padding: 5px;"} 
            />
        )))}
    </box>

}
