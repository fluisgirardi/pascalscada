PascalSCADA 1.0 - A multiplatform SCADA framework for Lazarus

These components allow stabilish communications with Modbus RTU, Modbus TCP, Siemens PLC and Mitsubish Melsec devices in a easy way (this means: without write code), through window controls (buttons, sliders, animations), tags, protocol drivers and port drivers, without lose advanced features that you can implement using code.

This code is a entire rewrite of PascalSCADA hosted at https://sourceforge.net/projects/pascalscada. The main goal of this initiative is:

  1) Fix some design shortcomings of the actual version of PascalSCADA (0.7.3). A example is the low number of tags that can be inserted in a project form in design time. For more then 1000 tags per form or datamodule, you should create tags in runtime, via source code.
  2) Performance, performance and more performance.
  3) Split some interfaces that has more than one function. By example IHMIInterface, that handles security for HMI controls and serves as Tag interface.
  4) Improve the translation, using .PO files instead of language defines;
  5) Improve the project modularization
  6) Extend the security system to all standard controls of Lazarus
  

This code is under modified LGPL (see COPYING.modifiedLGPL.txt). This means that you can link this library inside your programs for any purpose. Only the included part of the code must remain LGPL.

If you make some improvements to this library, please create a new issue and attach the patch of your changes.

Contact : fabio at pascalscada.com
