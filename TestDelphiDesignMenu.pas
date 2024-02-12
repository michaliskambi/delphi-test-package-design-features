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
    CgeMenu, ChangeEnginePathMenu, OpenEditorMenu, AddPathsMenu, RemovePathsMenu: TMenuItem;
    procedure ClickChangeEnginePath(Sender: TObject);
    procedure ClickOpenEditor(Sender: TObject);
    procedure ClickAddPaths(Sender: TObject);
    procedure ClickRemovePaths(Sender: TObject);
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

  ChangeEnginePathMenu := TMenuItem.Create(nil);
  ChangeEnginePathMenu.Caption := 'Test Change Engine Path...';
  // Configure the path of Castle Game Engine (should have subdirectories like src, examples).
  ChangeEnginePathMenu.OnClick := ClickChangeEnginePath;

  OpenEditorMenu := TMenuItem.Create(nil);
  OpenEditorMenu.Caption := 'Test Open Editor';
  OpenEditorMenu.OnClick := ClickOpenEditor;

  AddPathsMenu := TMenuItem.Create(nil);
  AddPathsMenu.Caption := 'Test Configure Currrent Project to Use Engine';
  AddPathsMenu.OnClick := ClickAddPaths;

  RemovePathsMenu := TMenuItem.Create(nil);
  RemovePathsMenu.Caption := 'Test Remove Engine Configuration from the Currrent Project';
  RemovePathsMenu.OnClick := ClickRemovePaths;

  CgeMenu := TMenuItem.Create(nil);
  CgeMenu.Caption := 'Test Castle Game Engine';
  CgeMenu.Name := 'TestCastleGameEngineMenu';
//  CgeMenu.Add(ChangeEnginePathMenu);
//  CgeMenu.Add(OpenEditorMenu);
//  CgeMenu.Add(AddPathsMenu);
//  CgeMenu.Add(RemovePathsMenu);

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

      Services.AddActionMenu('ToolsToolsItem', nil, CgeMenu, true, true);

    So a custom "Tools" submenu should be added before "Configure Tools". }

  Services.AddActionMenu('ViewTranslationManagerMenu', nil, CgeMenu, true, false);

  { We can add submenu items using CgeMenu.Add (see above) or by adding
    using Services.AddActionMenu.
    There doesn't seem to be any difference. }
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, ChangeEnginePathMenu, true, true);
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, OpenEditorMenu, true, true);
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, AddPathsMenu, true, true);
  Services.AddActionMenu('TestCastleGameEngineMenu', nil, RemovePathsMenu, true, true);
end;

destructor TTestDelphiIdeIntegration.Destroy;
begin
  WritelnLog('TTestDelphiIdeIntegration.Destroy');

  // Incorrect comment: This will automatically free menu items owned by this.

  { Correct comment:

    It seems that CgeMenu is not removed from ToolsMenu automatically when
    TTestDelphiIdeIntegration instance is destroyed,
    even when CgeMenu is owned by TTestDelphiIdeIntegration (was created as
    "CgeMenu := TMenuItem.Create(Self)").
    Freing it explicitly here works.

    Note that we shouldn't free subitems like ChangeEnginePathMenu,
    they will be freed automatically
    (trying to free them would result in invalid pointer exceptions).

    Why? It seems Delphi IDE does something funny and changes ownership
    of items added using Services.AddActionMenu -- so they are owned by their parents
    effectively?

    We create them all now with Owner=nil, to avoid confusion.
  }

  FreeAndNil(CgeMenu);
  // FreeAndNil(ChangeEnginePathMenu);
  // FreeAndNil(OpenEditorMenu);
  // FreeAndNil(AddPathsMenu);
  // FreeAndNil(RemovePathsMenu);

  inherited;
end;

procedure TTestDelphiIdeIntegration.ClickChangeEnginePath(Sender: TObject);
begin
end;

procedure TTestDelphiIdeIntegration.ClickOpenEditor(Sender: TObject);
begin
end;

procedure TTestDelphiIdeIntegration.ClickAddPaths(Sender: TObject);
begin
end;

procedure TTestDelphiIdeIntegration.ClickRemovePaths(Sender: TObject);
begin
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
