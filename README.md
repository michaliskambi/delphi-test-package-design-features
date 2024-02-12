# Test adding design-time stuff to Delphi IDE

Test of adding design-time stuff to Delphi IDE in a small project.

The end-goal: add a useful design-time menu item to the Delphi IDE for [Castle Game Engine](https://castle-engine.io/).

Documentation:

Embarcadero:

- https://docwiki.embarcadero.com/RADStudio/Athens/en/Extending_the_IDE_Using_the_Tools_API

- https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE

- https://docwiki.embarcadero.com/RADStudio/Athens/en/Creating_or_Extending_a_Package_to_Use_the_Tools_API

- https://docwiki.embarcadero.com/RADStudio/Athens/en/Obtaining_Tools_API_Services

Others:

- https://www.gexperts.org/open-tools-api-faq/#menuitem

    Note: Do not actually search by menu item Caption, because IDE may be localized, instead hardcode the Name. Embarcadero docs say so.

- https://www.davidghoyle.co.uk/WordPress/?p=777

    See sources, e.g. `C:\Program Files (x86)\Embarcadero\Studio\22.0\source\ToolsAPI\ToolsAPI.pas`
