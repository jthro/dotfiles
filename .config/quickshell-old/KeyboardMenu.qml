import QtQuick
import QtQuick.Layouts
import QtQuick.Controls
import Quickshell
import Quickshell.Hyprland

// generic keyboard selector list thing
// set custom binds to toggle each one
// same bind to navigate for all of them

PopupWindow {
    id: root

    property var barWindow: null
    property string globalBind: "" // name of global bind, use like bind = $mainMod, B, global, quickshell:<globalBind>
    
    property var menuModel: [] 
    
    signal selected(var value)

    property bool isOpen: false
    property int selectionIndex: 0

    color: "transparent"
    implicitWidth: bg.implicitWidth
    implicitHeight: bg.implicitHeight
    anchor.item: parent
    anchor.edges: Edges.Bottom | Edges.Left
    visible: isOpen

    onIsOpenChanged: {console.log(menuModel)}



    Rectangle {
        id: bg
        implicitWidth: 160
        implicitHeight: Math.max(10, listView.contentHeight + 10)
        color: Colours.bgTertiary
        border.color: Colours.fgPrimary // selection highlighting
        border.width: 1
        radius: 6
	visible: parent.visible

        ListView {
            id: listView
            anchors.fill: parent
            anchors.margins: 5
            clip: true

	    function selectAndClose(value) {
		root.selected(value)
		root.isOpen = false
	    }
	                
            model: root.menuModel
            spacing: 2
	    currentIndex: root.selectionIndex
            
            delegate: Rectangle {
                id: delegateRoot
                implicitWidth: ListView.view.width
                implicitHeight: 30
                radius: 4

		onVisibleChanged: {
		    console.log(modelData.label)
		}
                
                color: (ListView.isCurrentItem) ? Colours.bgSecondary : "transparent"
		
		MouseArea {
                    anchors.fill: parent
                    hoverEnabled: true
                    cursorShape: Qt.PointingHandCursor

                    onEntered: {
                        root.selectionIndex = index
                    }

                    onClicked: {
			listView.selectAndClose(modelData.value)
                    }
                }
		
                RowLayout {
                    anchors.fill: parent
                    anchors.leftMargin: 8
                    anchors.rightMargin: 8
		    
                    Label {
			font.family: 'Monaspace Neon'
                        text: modelData.label
                        color: Colours.fgPrimary
                        Layout.fillWidth: true
                        font.bold: modelData.active 
                    }

		    // active indicator thing
                    Rectangle {
                        implicitWidth: 8; implicitHeight: 8; radius: 4
                        color: modelData.active ? Colours.fgPrimary : "transparent"
                        border.color: Colours.bgTertiary
                        border.width: modelData.active ? 0 : 1
                    }
                }
            }
        }
    }
}
