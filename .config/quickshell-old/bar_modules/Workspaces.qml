import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell.Hyprland
import Quickshell.Widgets
import Quickshell

import "../"

Item {
    id: root
    property var barWindow: null
    implicitHeight: rect.implicitHeight
    implicitWidth: rect.implicitWidth


    Rectangle {
	id: rect
	implicitWidth: listView.implicitWidth
	implicitHeight: 30
	color: Colours.bgTertiary
	border.color: Colours.fgPrimary
	radius: 5
	anchors.centerIn: parent

			    	    
	ListView {
	    implicitWidth: 20 * Hyprland.workspaces.values.length + 10
	    implicitHeight: 30
	    orientation: ListView.Horizontal

	    id: listView
	    
	    anchors.fill: parent
	    // anchors.margins: 3
	    
	    model: Hyprland.workspaces
	    
	    delegate: Rectangle {
		id: delegateRoot
		implicitWidth: 20
		implicitHeight: 20
		color: "transparent"

		Label {
		    anchors.fill: parent
		    leftPadding: 7
		    topPadding: 7
		    font.family: "Monaspace Neon"
		    font.bold : {modelData.focused}
		    color: Colours.fgPrimary
		    text: {
			return modelData.focused ? `<u>${modelData.id}</u>`
			    : modelData.id	
		    }
		    
		}
	    }
	}
    }
}
