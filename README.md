# miniIRChat

Is an open source desktop application for IRC chat

### Target

 * Simple
 * Portable
 * Fast

### Features

 * Portable, one executable file, take it any where.
 * Works in Windows and Linux
 * SSL/TLS 1.3 connection
 * [TODO] Syntax Highlighter for language for message started with ``` (in middle of message too)
 * [TODO] Mutli Server

### Disadvantages

 * There is no SASL
 * No plugins/addon yet

### Contributing

Keep it as simple as possible.

### History

It moved from [minilib](https://github.com/parmaja/minilib/tree/master/socket/demo/lazarus/IRChat) demo folder

### Build

miniIRChat is a FreePascal/Lazarus project, to compile it you need to install all packages it depends on

 * Use source code from github.com in branch "release", or use last tag,  branch "master" is my working upstream, it is not stable.
 * FreePascal FPC 3.2 or later
 * Lazarus last update from subversion repo, it is recommended, or use version 2.1
 * MiniLib https://github.com/parmaja/minilib
 * MiniCtrls https://github.com/parmaja/minictrls

Open each package in Lazarus and compile it in order.

#### Required Packages

    minilib\lib\MiniCommons.lpk
    minilib\xml\source\MiniXML.lpk
    minilib\socket\source\MiniSockets.lpk
    minilib\connection\source\MiniConnections.lpk

    minictrls\lib\MiniLib.lpk
    minictrls\components\native\NativeLib.lpk

### Testing and Use

My prefered IRC server is [ergo](https://github.com/ergochat/ergo/).
