{ Manage menu "Tools -> Test Castle Game Engine" in Delphi IDE. }
unit TestDelphiDesignMenu;

interface

procedure Register;

implementation

uses SysUtils, Classes,
  DesignIntf, ToolsAPI, // design-time only unit
  Vcl.Menus, Vcl.Dialogs;

{ Simplest possible log that will work and be persistent, no matter what state
  application is initialized.

  THIS IS ONLY FOR TESTING,
  this is absolutely too stupid, too inefficient, too hardcoced to be used in
  production code. In real CGE apps use WritelnLog from CastleLog unit. }
procedure WritelnLog(const S: String);
var
  F: TextFile;
begin
  AssignFile(F, 'c:\tmp\package-test-log.txt');
  Append(F);
  Writeln(F, FormatDateTime('yyyy"-"mm"-"dd" "tt', Now) + '> ' + S);
  CloseFile(F);
end;

{ TTestDelphiIdeIntegration ----------------------------------------------- }

type
  TTestDelphiIdeIntegration = class(TComponent)
  strict private
    MenuMyRoot, MenuDebugProjectOptions, MenuDebugGlobalOptions: TMenuItem;
    procedure ClickDebugProjectOptions(Sender: TObject);
    procedure ClickDebugGlobalOptions(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TTestDelphiIdeIntegration.Create(AOwner: TComponent);

  { List menu items for debug purposes. }
  procedure IterateMenuItems(MenuItems: TMenuItem; const LogPrefix: String);
  var
    I: Integer;
    Mi: TMenuItem;
  begin
    for I := 0 To MenuItems.Count - 1 do
    begin
      Mi := MenuItems[I];
      WritelnLog(LogPrefix + 'Menu item ' + IntToStr(I) + ': ' + Mi.Name + ' ' + Mi.Caption);
      IterateMenuItems(MenuItems.Items[I], '- ' + LogPrefix);
    end;
  end;

var
  Services: INTAServices;
begin
  inherited;

  WritelnLog('TTestDelphiIdeIntegration.Create');

  Services := BorlandIDEServices as INTAServices;

  IterateMenuItems(Services.MainMenu.Items, '');

  MenuDebugProjectOptions := TMenuItem.Create(nil);
  MenuDebugProjectOptions.Caption := 'Debug Project Options';
  MenuDebugProjectOptions.OnClick := ClickDebugProjectOptions;

  MenuDebugGlobalOptions := TMenuItem.Create(nil);
  MenuDebugGlobalOptions.Caption := 'Debug Global Options';
  MenuDebugGlobalOptions.OnClick := ClickDebugGlobalOptions;

  MenuMyRoot := TMenuItem.Create(nil);
  MenuMyRoot.Caption := 'Test Package Menu';
  MenuMyRoot.Name := 'TestCastleGameEngineMenu';

  { Use hardcoded menu item name, like
    - ToolsToolsItem ("Configure Tools")
    - ToolsMenu ("Tools" from main menu)
    - ViewTranslationManagerMenu ("Translation Manager")
    This is the proper approach, acoording to
    https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE .

    Note: Do not add menu items after "Configure Tools" (name 'ToolsToolsItem').
    These will be removed when Delphi IDE is started, and replaced with
    menu items that correspond to stuff confiured in "Configure Tools".
    E.g. this will not work reliably, stuff will disappear after Delphi restart:

      Services.AddActionMenu('ToolsToolsItem', nil, MenuMyRoot, true, true);

    So a custom "Tools" submenu should be added before "Configure Tools". }

  Services.AddActionMenu('ViewTranslationManagerMenu', nil, MenuMyRoot, true, false);

  { We can add submenu items using MenuMyRoot.Add or by adding
    using Services.AddActionMenu.
    There doesn't seem to be any difference. }
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, MenuDebugProjectOptions, true, true);
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, MenuDebugGlobalOptions, true, true);
end;

destructor TTestDelphiIdeIntegration.Destroy;
begin
  WritelnLog('TTestDelphiIdeIntegration.Destroy');

  // Incorrect comment: This will automatically free menu items owned by this.

  { Correct comment:

    It seems that MenuMyRoot is not removed from ToolsMenu automatically when
    TTestDelphiIdeIntegration instance is destroyed,
    even when MenuMyRoot is owned by TTestDelphiIdeIntegration (was created as
    "MenuMyRoot := TMenuItem.Create(Self)").
    Freing it explicitly here works.

    Note that we shouldn't free subitems like ChangeEnginePathMenu,
    they will be freed automatically
    (trying to free them would result in invalid pointer exceptions).

    Why? It seems Delphi IDE does something funny and changes ownership
    of items added using Services.AddActionMenu -- so they are owned by their parents
    effectively?

    We create them all now with Owner=nil, to avoid confusion.
  }

  FreeAndNil(MenuMyRoot);

  inherited;
end;

procedure TTestDelphiIdeIntegration.ClickDebugProjectOptions(Sender: TObject);
var
  OptionName: TOTAOptionName;
  ProjectOptions: IOTAProjectOptions;
  Report: TStringList;
  ReportFileName, ValueStr: String;
begin
  Report := TStringList.Create;
  try
    ProjectOptions := GetActiveProject.ProjectOptions;

    for OptionName in ProjectOptions.GetOptionNames do
    begin
      try
        ValueStr := ProjectOptions.Values[OptionName.Name];
      except
        ValueStr := 'Error reading as String';
      end;
      Report.Append(Format('%s: %s', [
        OptionName.Name,
        ValueStr
      ]));
    end;

    ReportFileName := 'c:/tmp/' + IntToStr(Random(100000));

    ShowMessage(Format('Found %d project options, saving to %s', [
      Length(ProjectOptions.GetOptionNames),
      ReportFileName
    ]));

    { Show message before actually writing to ReportFileName,
      so that user knows what's going on in case of error
      at writing (e.g. before directory doesn't exist or is read-only). }

    Report.SaveToFile(ReportFileName);
  finally FreeAndNil(Report) end;
end;

procedure TTestDelphiIdeIntegration.ClickDebugGlobalOptions(Sender: TObject);
var
  Services: IOTAServices;
  EnvironmentOptions: IOTAEnvironmentOptions;
  OptionName: TOTAOptionName;
  Report: TStringList;
  ReportFileName, ValueStr: String;
begin
  Report := TStringList.Create;
  try
    if not Supports(BorlandIDEServices, IOTAServices, Services) then
      raise Exception.Create('Cannot access IOTAServices');
    EnvironmentOptions := Services.GetEnvironmentOptions;
    if EnvironmentOptions = nil then
      raise Exception.Create('Cannot access IOTAEnvironmentOptions');

    for OptionName in EnvironmentOptions.GetOptionNames do
    begin
      try
        ValueStr := EnvironmentOptions.Values[OptionName.Name];
      except
        ValueStr := 'Error reading as String';
      end;
      Report.Append(Format('%s (%d): %s', [
        OptionName.Name,
        Ord(OptionName.Kind),
        ValueStr
      ]));
    end;

    ReportFileName := 'c:/tmp/' + IntToStr(Random(100000));

    ShowMessage(Format('Found %d project options, saving to %s', [
      Length(EnvironmentOptions.GetOptionNames),
      ReportFileName
    ]));

    { Show message before actually writing to ReportFileName,
      so that user knows what's going on in case of error
      at writing (e.g. before directory doesn't exist or is read-only). }

    Report.SaveToFile(ReportFileName);
  finally FreeAndNil(Report) end;
end;

{ initialization / finalization ---------------------------------------------- }

var
  DelphiIdeIntegration: TTestDelphiIdeIntegration;

procedure Register;
begin
  // Seems not necessary in the end.
  { Without this, package is loaded (and so menu items added) only once
    CGE component is accessed. }
  //ForceDemandLoadState(dlDisable);
end;

initialization
  DelphiIdeIntegration := TTestDelphiIdeIntegration.Create(nil);
finalization
  // When unloading the package, make sure to remove menu item
  FreeAndNil(DelphiIdeIntegration);
end.
