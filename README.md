# Delphi components by Kandiral Ruslan

[https://kandiral.ru/delphi/nabor_komponentov_dlya_delphi.html](https://kandiral.ru/delphi/nabor_komponentov_dlya_delphi.html)

## Installation

[https://kandiral.ru/delphi/instrukciya_po_ustanovke_komponentov_na_delphi.html](https://kandiral.ru/delphi/instrukciya_po_ustanovke_komponentov_na_delphi.html)

Components require [JEDI API Library](https://sourceforge.net/projects/jedi-apilib/).
A set of JEDI API libraries is unpacked into a folder (I specify the path for an example, everyone can unpack to wherever it pleases, the main thing is that the path in Environment Variables was spelled correctly). For instance:
```bash
C:\Delphi\JEDI
```
Then unpack the components in the folder:
```bash
C:\Delphi\Kandiral
```
Launch Delphi and go to the menu "Tools"->"Options"->"Environment Variables". Add an environment variable with the name "JEDI" and the value "C:\Delphi\JEDI". Add an environment variable with the name "KANDIRAL" and the value "C:\Delphi\Kandiral". 
 
Go to the menu "Tools"->"Options"->"Library". In the "Library path" add paths:
```bash
$(JEDI)\jwa\branches\2.3\Common
$(JEDI)\jwa\branches\2.3\Includes
$(JEDI)\jwa\branches\2.3\SaCMAPI
$(JEDI)\jwa\branches\2.3\Win32API
$(KANDIRAL)\Release\$(ProductVersion)\$(Platform)
$(KANDIRAL)\Resources
$(KANDIRAL)\dfm
```

In the "Browsing path" add paths:
```bash
$(KANDIRAL)\Common
$(KANDIRAL)\Automation
$(KANDIRAL)\File
$(KANDIRAL)\Media
$(KANDIRAL)\Net
$(KANDIRAL)\Utils
$(KANDIRAL)\Graphics
$(KANDIRAL)\Web
```
Open one group of projects corresponding to your version of Delphi. The name of the project group file is "Kandiral150.groupproj". Where 150 is the [Package Version](http://docwiki.embarcadero.com/RADStudio/Seattle/en/Compiler_Versions)
```bash
        Kandiral150.groupproj        Delphi XE
        Kandiral160.groupproj        Delphi XE2
        Kandiral170.groupproj        Delphi XE3
        Kandiral180.groupproj        Delphi XE4
        Kandiral190.groupproj        Delphi XE5
        Kandiral200.groupproj        Delphi XE6
        Kandiral210.groupproj        Delphi XE7
        Kandiral220.groupproj        Delphi XE8
        Kandiral230.groupproj        Delphi 10 Seattle
        Kandiral240.groupproj        Delphi 10.1 Berlin
        Kandiral250.groupproj        Delphi 10.2 Tokyo
        Kandiral260.groupproj        Delphi 10.3 Rio
```
In Project Manager, right-click on the group name and select "Build All" in the context menu. After compilation, right-click on each package in turn and select "Install" in the context menu.

If everything is done correctly, then the installed components should appear in the component palette.

## License
[GPLv3](https://www.gnu.org/licenses/quick-guide-gplv3.en.html)