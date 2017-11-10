This package supports reading Xcode projects.

Features:

- Parse Xcode project file (project.pbxproj)
- Extract information about targets, build configurations, build settings and files etc.

Usage:

To obtain the parsed project object (alist):

(`xcode-project-read' PATH-TO-XCODEPROJ)

Helper to locate the project file for any given source file:
(`xcode-project-find-xcodeproj' PATH-TO-FILE)

Then extract information such as targets, build phases, configurations and files.
Most functions return an object (alist), unless otherwise described (e.g. `xcode-project-target-names').

Targets:
(`xcode-project-targets' PROJ)

(`xcode-project-target-names' PROJ)

Build Phases:
(`xcode-project-build-phases' PROJ TARGET-NAME)

Build Configurations:
(`xcode-project-build-config-names' PROJ)

(`xcode-project-build-config' PROJ CONFIG-NAME TARGET-NAME)

Build Settings:
(`xcode-project-build-config-setings' CONFIG)

Build files - as objects:
(`xcode-project-build-files' PROJ TARGET-NAME)

- as paths (relative to project)
(`xcode-project-build-file-paths' PROJ TARGET-NAME)
