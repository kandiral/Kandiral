Установка компонентов от Кандирала Руслана
-----------------------------------------------------------


Для работы компонентов понадобится JEDI API Library & Security Code Library
https://sourceforge.net/projects/jedi-apilib/

Набор библиотек JEDI API распаковываем в папку (путь указываю для примера, каждый может распаковывать туда куда душе будет угодно, главное что-бы путь в Environment Variables был прописан правильно):
C:\Delphi\JEDI\

Набор моих библиотек распаковываем в папку (про расположение папки см. выше):
https://kandiral.ru/delphi/nabor_komponentov_dlya_delphi.html
C:\Delphi\Kandiral\

1. Запускаем Delphi
2. Заходим в меню "Component"->"Install Packages...". Если в списке есть уже установленные ранее компоненты(старая версия), удаляем их.
3. Заходим в меню "Tools"->"Options..."->"Environment Variables". Добавляем переменные(если их нет, а если есть, то проверяем правильность пути) JEDI=C:\Delphi\JEDI и переменную KANDIRAL=C:\Delphi\Kandiral
4. Заходим в меню "Tools"->"Options..."->"Library". В "Library path" добавляем следующие пути(если их там нет)
        $(JEDI)\jwa\branches\2.3\Common
        $(JEDI)\jwa\branches\2.3\Includes
        $(JEDI)\jwa\branches\2.3\SaCMAPI
        $(JEDI)\jwa\branches\2.3\Win32API
        $(KANDIRAL)\Release\$(ProductVersion)\$(Platform)
        $(KANDIRAL)\Resources
        $(KANDIRAL)\dfm
    в "Browsing path" добавляем такие пути
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
5. Открываем одну из групп проектов, которая соответствует вашей версии Delphi. Имя файла группы проектов имеет следующий вид Kandiral150.groupproj. Где 150 - это Package Version (подробнее см. тут http://docwiki.embarcadero.com/RADStudio/Seattle/en/Compiler_Versions).
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
    если в списке нет необходимого вам файла, то берем версии ниже. То есть если например у вас "Delphi 10 Seattle", а файла Kandiral230.groupproj нет, то открываем файл Kandiral220.groupproj или ниже версии, который есть. 
6. В Project Manager на названии группы жмем правой кнопкой мыши и в контекстном меню выбираем "Build All"
7. После компиляции поочередно на каждом пакете жмем правой кнопкой мыши и в контекстном меню выбираем "Install"

Если всё сделано правильно, то в палитре компонентов должны появиться установленные элементы.

----------------
Library path:
$(JEDI)\jwa\branches\2.3\Common;$(JEDI)\jwa\branches\2.3\Includes;$(JEDI)\jwa\branches\2.3\SaCMAPI;$(JEDI)\jwa\branches\2.3\Win32API;$(KANDIRAL)\Release\$(ProductVersion)\$(Platform);$(KANDIRAL)\Resources;$(KANDIRAL)\dfm

Browsing path:
$(KANDIRAL)\Automation;$(KANDIRAL)\Common;$(KANDIRAL)\Crypting;$(KANDIRAL)\File;$(KANDIRAL)\Graphics;$(KANDIRAL)\ISO;$(KANDIRAL)\Media;$(KANDIRAL)\Net;$(KANDIRAL)\Utils;$(KANDIRAL)\Web;$(KANDIRAL)\Windows

