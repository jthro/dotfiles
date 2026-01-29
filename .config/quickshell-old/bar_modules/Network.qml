import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell.Io

import "../"

Item {
    id: root
    property var barWindow: null
    
    height: parent.height

    property string connection_name: ""
    property string connection: "Disconnected"
    property bool running: true

    implicitHeight: button.implicitHeight
    implicitWidth: button.implicitWidth

    Process {
	id: nmcliStatus
	command: ["nmcli", "-t", "-f", "NAME,TYPE", "connection", "show", "--active"]
	running: root.running

	stdout: StdioCollector {
	    onStreamFinished: {
		var output = this.text.trim().split('\n')
		var wifiConnected = false
		for (var i = 0; i < output.length; i++) {
		    var parts = output[i].split(':')
		    if (parts[1].includes("ethernet")) {
			root.connection = "Ethernet"
			root.connection_name = parts[0]
			break
		    }
		    if (parts[1].includes("wireless")) {
			root.connection_name = parts[0]
			wifiConnected = true
		    }
		}
		
		connection = wifiConnected ? "Wifi" : "Disconnected"
	    }
	}
    }

    Timer {
	id: timer
	interval: 30000
	running: true
	repeat: true
	onTriggered: {
	    nmcliStatus.running = true
	}
    }

    Rectangle {
	id: button
	implicitWidth: symb.implicitWidth + conntype.implicitWidth
	implicitHeight: 30
	color: Colours.bgTertiary
	border.color: Colours.fgPrimary
	radius: 5
	anchors.centerIn: parent

	MouseArea {
	    anchors.fill: button
	    onClicked: {
		root.running = !root.running
	    }
	}
	
	Label {
	    id: symb
	    color: Colours.fgPrimary
	    font.family: "Symbols Nerd Font Mono"
	    padding: 0
	    topPadding: 9
	    leftPadding: 6
	    text: {
		if (root.connection === "Wifi") {
		    return "󰤨"
		} else if (root.connection === "Ethernet") {
		    return "󰛳"
		} else {
		    return "󰲛"
		}
	    }
	}

	Label {
	    id: conntype
	    color: Colours.fgPrimary
	    font.family: "Monaspace Neon"
	    padding: 0
	    topPadding: 8
	    leftPadding: 30
	    text: {
		root.connection_name
	    }
	}
    }
}
