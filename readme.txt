��������� ����������� �� ��������� �������
-----------------------------------------------------------


��� ������ ����������� ����������� JEDI API Library & Security Code Library
https://sourceforge.net/projects/jedi-apilib/

����� ��������� JEDI API ������������� � ����� (���� �������� ��� �������, ������ ����� ������������� ���� ���� ���� ����� ������, ������� ���-�� ���� � Environment Variables ��� �������� ���������):
C:\Delphi\JEDI\

����� ���� ��������� ������������� � ����� (��� ������������ ����� ��. ����):
https://kandiral.ru/delphi/nabor_komponentov_dlya_delphi.html
C:\Delphi\Kandiral\

1. ��������� Delphi
2. ������� � ���� "Component"->"Install Packages...". ���� � ������ ���� ��� ������������� ����� ����������(������ ������), ������� ��.
3. ������� � ���� "Tools"->"Options..."->"Environment Variables". ��������� ����������(���� �� ���, � ���� ����, �� ��������� ������������ ����) JEDI=C:\Delphi\JEDI � ���������� KANDIRAL=C:\Delphi\Kandiral
4. ������� � ���� "Tools"->"Options..."->"Library". � "Library path" ��������� ��������� ����(���� �� ��� ���)
        $(JEDI)\jwa\branches\2.3\Common
        $(JEDI)\jwa\branches\2.3\Includes
        $(JEDI)\jwa\branches\2.3\SaCMAPI
        $(JEDI)\jwa\branches\2.3\Win32API
        $(KANDIRAL)\Release\$(ProductVersion)\$(Platform)
        $(KANDIRAL)\Resources
        $(KANDIRAL)\dfm
    � "Browsing path" ��������� ����� ����
	$(KANDIRAL)\Automation
	$(KANDIRAL)\Common
	$(KANDIRAL)\Crypting
	$(KANDIRAL)\File
	$(KANDIRAL)\Graphics
	$(KANDIRAL)\ISO
	$(KANDIRAL)\Media
	$(KANDIRAL)\Net
	$(KANDIRAL)\Utils
	$(KANDIRAL)\Web
	$(KANDIRAL)\Windows
5. ��������� ���� �� ����� ��������, ������� ������������� ����� ������ Delphi. ��� ����� ������ �������� ����� ��������� ��� Kandiral150.groupproj. ��� 150 - ��� Package Version (��������� ��. ��� http://docwiki.embarcadero.com/RADStudio/Seattle/en/Compiler_Versions).
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
    ���� � ������ ��� ������������ ��� �����, �� ����� ������ ����. �� ���� ���� �������� � ��� "Delphi 10 Seattle", � ����� Kandiral230.groupproj ���, �� ��������� ���� Kandiral220.groupproj ��� ���� ������, ������� ����. 
6. � Project Manager �� �������� ������ ���� ������ ������� ���� � � ����������� ���� �������� "Build All"
7. ����� ���������� ���������� �� ������ ������ ���� ������ ������� ���� � � ����������� ���� �������� "Install"

���� �� ������� ���������, �� � ������� ����������� ������ ��������� ������������� ��������.

----------------
Library path:
$(JEDI)\jwa\branches\2.3\Common;$(JEDI)\jwa\branches\2.3\Includes;$(JEDI)\jwa\branches\2.3\SaCMAPI;$(JEDI)\jwa\branches\2.3\Win32API;$(KANDIRAL)\Release\$(ProductVersion)\$(Platform);$(KANDIRAL)\Resources;$(KANDIRAL)\dfm

Browsing path:
$(KANDIRAL)\Automation;$(KANDIRAL)\Common;$(KANDIRAL)\Crypting;$(KANDIRAL)\File;$(KANDIRAL)\Graphics;$(KANDIRAL)\ISO;$(KANDIRAL)\Media;$(KANDIRAL)\Net;$(KANDIRAL)\Utils;$(KANDIRAL)\Web;$(KANDIRAL)\Windows

