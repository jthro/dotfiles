import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell.Services.UPower

import "../"

Item {
    id: root
    property var barWindow: null
    
    implicitWidth: rect.implicitWidth
    implicitHeight: parent.implicitHeight

    property int percentage: Math.floor(100 * UPower.displayDevice.percentage)

    Rectangle {
	id: rect
	implicitHeight: parent.implicitHeight
	implicitWidth: row.implicitWidth + 10
	anchors.centerIn: parent
	radius: 5
	border.color: Colours.fgPrimary
	color: Colours.bgTertiary

	MouseArea {
            anchors.fill: parent
            cursorShape: Qt.PointingHandCursor // Shows a hand cursor on hover
            
            onClicked: {
                // root.updateMenuPos()
                menu.isOpen = !menu.isOpen
            }
        }
	
	RowLayout {
            id: row
            anchors.centerIn: parent
            spacing: 8
	    
            Label {
		font.family: "Symbols Nerd Font Mono"
		text: {
		    var icon;
		    if (root.percentage > 85) {
			icon = " ";
		    } else if (root.percentage > 60) {
			icon = " "
		    } else if (root.percentage > 35) {
			icon = " "
		    } else if (root.percentage > 10) {
			icon = " "
		    } else {
			icon = " "
		    }
		    return icon + root.percentage + "%"
		}
		color: Colours.fgPrimary
		font.bold: true
            font.pixelSize: 14
            }
	}
    }

    KeyboardMenu {
        id: menu
        barWindow: root.barWindow
        
        menuModel: {
            var items = [
                { 
                    label: "Power Saver", 
                    value: PowerProfile.PowerSaver, 
                    active: PowerProfiles.profile === PowerProfile.PowerSaver 
                },
                { 
                    label: "Balanced", 
                    value: PowerProfile.Balanced, 
                    active: PowerProfiles.profile === PowerProfile.Balanced 
                }
            ];

            if (PowerProfiles.hasPerformanceProfile) {
                items.push({
                    label: "Performance", 
                    value: PowerProfile.Performance, 
                    active: PowerProfiles.profile === PowerProfile.Performance 
                });
            }
            return items;
        }

        onSelected: (value) => {
            PowerProfiles.profile = value;
        }
    }
}
