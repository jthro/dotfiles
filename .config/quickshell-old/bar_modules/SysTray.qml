import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell.Services.SystemTray
import Quickshell.Widgets
import Quickshell

import "../"

Item {
    id: root
    property var barWindow: null
    property bool isOpen: false
    implicitWidth: button.implicitWidth
    implicitHeight: button.implicitHeight


    Rectangle {
	id: button
	implicitWidth: 65
	implicitHeight: 30
	color: Colours.bgTertiary
	border.color: Colours.fgPrimary
	radius: 5
	anchors.verticalCenter: parent.verticalCenter

	Label {
	    color: Colours.fgPrimary
	    font.family: "Symbols Nerd Font Mono"
	    padding: 0
	    leftPadding: 5
	    topPadding: 6
	    text: {
		return root.isOpen ? "|" : "|"
	    }
	}

	Label {
	    color: Colours.fgPrimary
	    font.family: "Monaspace Neon"
	    topPadding: 7
	    leftPadding: 27
	    text: "Tray"
	}

	MouseArea {
	    anchors.fill: button
	    onClicked: {
		root.isOpen = !root.isOpen
	    }
	}
	
	PopupWindow {
	    color: "transparent"
	    id: popup
	    visible: isOpen
	    anchor.window: root.barWindow
	    implicitHeight: rect.implicitHeight
	    implicitWidth: rect.implicitWidth
	    anchor.item: button
	    anchor.edges: Edges.Bottom | Edges.Left
	    
	    Rectangle {
		id: rect
		implicitWidth: listView.implicitWidth
		implicitHeight: listView.implicitHeight
		anchors.fill: parent
		radius: 5

		color: Colours.bgTertiary
		border.color: Colours.fgPrimary
		
			    	    
		ListView {
		    implicitHeight: 30 * SystemTray.items["values"].length + 3
		    implicitWidth: 33
		    
		    id: listView

		    anchors.fill: parent
		    anchors.margins: 2


		    
		    model: SystemTray.items["values"]
		    
		    delegate: Rectangle {
			id: delegateRoot
			implicitWidth: 30
			implicitHeight: 30
			radius: 5
			color: "transparent"
			
			IconImage {
			id: icon
			    implicitSize: 20
			    source: modelData.icon
			    anchors.centerIn: parent
			}
			
			MouseArea {
			    anchors.fill: parent
			    
			    onClicked: {
				if (modelData.menu) {
				    menuAnchor.open()
				}
			    }
			}
			
			QsMenuAnchor {
			    id: menuAnchor
			    anchor.window: root.barWindow
			    anchor.item: parent
			    menu: modelData.menu
			}
		    }
		}
	    }
	}
    }
}
