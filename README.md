# ContextQuickie2
Showing the Windows Explorer Context Menu in Eclipse

# Current State
Under development, basic context menu handling is working.
The explorer context menu items can be added to the Eclipcse context menu in different ways:
- The whole explorer context menu is shown as submenu "Explorer" in the Elipse context menu
- Only specific entries are added to the Eclipse context menu where the following tools are supported:
  - Beyond Compare
  - Notepad++
  - Open With...
  - TortoiseGit
  - TortoiseHg
  - TortoiseSVN

The screenshots below show example context menus based on the described configuration.

# Installation
The release versions are available at the [Eclipse Marketplace](https://marketplace.eclipse.org/content/contextquickie2)

[![Drag to your running Eclipse* workspace. *Requires Eclipse Marketplace Client](https://marketplace.eclipse.org/sites/all/themes/solstice/public/images/marketplace/btn-install.svg)](http://marketplace.eclipse.org/marketplace-client-intro?mpc_install=5305264 "Drag to your running Eclipse* workspace. *Requires Eclipse Marketplace Client")

Alternatively, you can use the following update sites when installing new software in eclipse:
* Releases: http://rolandomagico.github.io/ContextQuickie2/Releases
* Development: http://rolandomagico.github.io/ContextQuickie2/Development

# Configuration

The behavior can be configured in the workspaces preferences:

![ContextQuickie2 Preferences Page](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_Preferences.png)

# Remarks
## Menu Entry "Open"
The menu entry "Open" is implemented to open the selected files/folders with the Windows Explorer.

## Progress Monitoring
Because there are context menu extensions which don't support synchronous invocation, the progress reporting to Eclipse is done by monitoring the child processes of Eclipse during invocation.
If there is a new child process which is known to ContextQuickie2 after invoking a context menu command, the progress of the command execution can be monitored in Eclipse.
If there isn't a new child process after invoking or a command or the process is unknown to ContextQuickie2, progress monitoring cannot be done.

Currently, the following processes are known to ContextQuickie2:
- Bcompare.exe for Beyond Compare
- TortoiseGitProc.exe for TortoiseGit
- TortoiseProc.exe for TortoiseSVN
- thgw.exe for TortoiseHg

The described behavior also affects refreshing the workspace after a command was executed. As an alternative, you can enable "Refresh using native hooks or polling" in the workspace preferendes to let Eclipse update your workspace independent of ContextQuickie2.

# Examples

## TortoiseGit in the Eclipse Context Menu (Main Menu)

![TortoiseGit in the Eclipse Context Menu (Main Menu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseGit_MainMenu.png)

## TortoiseGit in the Eclipse Context Menu (Submenu)

![TortoiseGit in the Eclipse Context Menu (Submenu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseGit.png)

## TortoiseHg in the Eclipse Context Menu (Main Menu)

![TortoiseHg in the Eclipse Context Menu (Main Menu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseHg_MainMenu.png)

## TortoiseHg in the Eclipse Context Menu (Submenu)

![TortoiseHg in the Eclipse Context Menu (Submenu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseHg.png)

## TortoiseSVN in the Eclipse Context Menu (Main Menu)

![TortoiseSVN in the Eclipse Context Menu (Main Menu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseSVN_MainMenu.png)

## TortoiseSVN in the Eclipse Context Menu (Submenu)

![TortoiseSVN in the Eclipse Context Menu (Submenu)](https://github.com/RolandoMagico/ContextQuickie2/raw/main/Images/EclipseExample_TortoiseSVN.png)
