import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell.Services.Pipewire
import Quickshell.Widgets
import Quickshell

import "../"

Item {
    id: root
    property var barWindow: null

    PwObjectTracker {
	objects: {
	    [Pipewire.defaultAudioSink]
	}
    }

    implicitHeight: rect.implicitHeight
    implicitWidth: rect.implicitWidth


    Rectangle {
	id: rect
	implicitWidth: label.implicitWidth + 10
	implicitHeight: 30
	color: Colours.bgTertiary
	border.color: Colours.fgPrimary
	radius: 5
	anchors.centerIn: parent

	MouseArea {
	    anchors.fill: parent
	    cursorShape: Qt.PointingHandCursor
	    onClicked: {
		menu.isOpen = !menu.isOpen
	    }
	}
	
	Label {
	    property int level: (Pipewire.defaultAudioSink.audio.volume.toFixed(2)) * 100 
	    id: label
	    font.family: "Symbols Nerd Font Mono"
	    color: Colours.fgPrimary
	    topPadding: 7
	    leftPadding: 10
	    anchors.fill: parent
	    text: {
		var icon;
		if (Pipewire.defaultAudioSink.audio.muted) {
		    icon = ""
		} else if (level > 75) {
		    icon = ""
		} else if (level > 25) {
		    icon = ""
		} else {
		    icon = ""
		}
		return icon + " " + label.level
	    }
	}

    }
    
    KeyboardMenu {
	id: menu
	barWindow: root.barWindow
	
	menuModel: {
	    var items = []
	    Pipewire.nodes.values.filter((value) => {
		return !value.isStream && value.isSink
	    }).forEach((elem) => {
		items.push({
		    label: elem.nickname,
		    value: elem,
		    active: Pipewire.defaultAudioSink.id === elem.id
		})
	    })
	    return items
	}
	
	
	onSelected: (value) => {
	    Pipewire.preferredDefaultAudioSink = value;
	}
	
    }

}
