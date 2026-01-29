// Bar.qml
import Quickshell
import QtQuick
import QtQuick.Layouts

import "./bar_modules/"

Scope {
    id: root

    Variants {
        model: Quickshell.screens
        delegate: Component {
            PanelWindow {
		id: barWindow
                required property var modelData
                screen: modelData
		focusable: true
		color: "transparent"

		Rectangle {
                    anchors.fill: parent
                    color: "transparent"
		    radius: 5

		    RowLayout {
			anchors.fill: parent

			Item {
			    id: leftGroup
			    Layout.fillHeight: true
			    implicitWidth: leftModules.implicitWidth
			    
			    RowLayout {
				id: leftModules
				anchors.fill: parent

				SysTray {
				    id: sysTray
				    barWindow: barWindow
				    Layout.leftMargin: 15
				    Layout.fillHeight: true
				}
				
				Network {
				    id: network
				    barWindow: barWindow

				    Layout.leftMargin: 20
				    Layout.fillHeight: true
				}
				
				Workspaces {
				    id: workspaces
				    barWindow: barWindow
				    
				    Layout.leftMargin: 20
				    Layout.fillHeight: true
				}
				
			    }
			}
			
			Item { Layout.fillWidth: true }

			Item {
			    id: rightGroup
			    Layout.fillHeight: true
			    implicitWidth: rightModules.implicitWidth
			    
			    RowLayout {
				id: rightModules
				anchors.fill: parent
				
				Audio {
				    id: audio
				    barWindow: barWindow
				    
				    Layout.rightMargin: 10
				    Layout.fillHeight: true
				}
				
				
				Battery {
				    id: battery
				    barWindow: barWindow
				    
				    Layout.rightMargin: 20
				    Layout.fillHeight: true
				}
			    }
			}
                    }
		    Clock {
			id: clock
			anchors.horizontalCenter: parent.horizontalCenter
			anchors.verticalCenter: parent.verticalCenter
		    }

		}

                anchors {
                    top: true
                    left: true
                    right: true
                }

                implicitHeight: 40
		
            }
        }
    }
}
