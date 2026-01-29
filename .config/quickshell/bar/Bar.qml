import Quickshell
import Quickshell.Io
import QtQuick

import "./bar_widgets"

Scope {
    Variants {
	model: Quickshell.screens;
	
	PanelWindow {
	    required property var modelData
	    screen: modelData

	    anchors {
		bottom: true
		left: true
		right: true
	    }
	    
	    implicitHeight: 20

	    ClockWidget {
		anchors.centerIn: parent
	    }
	}
    }
    
}
