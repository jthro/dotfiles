import Quickshell
import Quickshell.Io
import QtQuick

import "../"

Rectangle {
    id: root
    color: Colours.bgTertiary
    implicitHeight: 30
    implicitWidth: clock.implicitWidth + 10
    radius: 5
    border.color: Colours.fgPrimary

    property string time: ""

    Text {
        id: clock
        anchors.centerIn: parent
	font.family: "Monaspace Neon"
        text: root.time
        color: Colours.fgPrimary
    }

    Process {
        id: dateProc
        command: ["date", "+%a %e %b | %H:%M"] 
        running: true
        stdout: StdioCollector {
            onStreamFinished: root.time = this.text.trim() 
        }
    }

    Timer {
        interval: 30000 // 30s is good enough :)
        running: true
        repeat: true
        onTriggered: dateProc.running = true 
    }
}
