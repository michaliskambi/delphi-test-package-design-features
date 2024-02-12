unit TestDelphiDesignMenu;

interface

procedure Register;

implementation

uses SysUtils, Classes,
  DesignIntf, ToolsAPI, // design-time only unit
  Vcl.Menus, Vcl.Dialogs;

{ TTestDelphiIdeIntegration ----------------------------------------------- }

type
  TTestDelphiIdeIntegration = class(TComponent)
  strict private
    procedure ClickChangeEnginePath(Sender: TObject);
    procedure ClickOpenEditor(Sender: TObject);
    procedure ClickAddPaths(Sender: TObject);
    procedure ClickRemovePaths(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TTestDelphiIdeIntegration.Create(AOwner: TComponent);
var
  Services: INTAServices;
  CgeMenu, ChangeEnginePathMenu, OpenEditorMenu, AddPathsMenu, RemovePathsMenu: TMenuItem;
begin
  inherited ;
  Services := BorlandIDEServices as INTAServices;

  ChangeEnginePathMenu := TMenuItem.Create(Self);
  ChangeEnginePathMenu.Caption := 'Test Change Engine Path...';
  // Configure the path of Castle Game Engine (should have subdirectories like src, examples).
  ChangeEnginePathMenu.OnClick := ClickChangeEnginePath;

  OpenEditorMenu := TMenuItem.Create(Self);
  OpenEditorMenu.Caption := 'Test Open Editor';
  OpenEditorMenu.OnClick := ClickOpenEditor;

  AddPathsMenu := TMenuItem.Create(Self);
  AddPathsMenu.Caption := 'Test Configure Currrent Project to Use Engine';
  AddPathsMenu.OnClick := ClickAddPaths;

  RemovePathsMenu := TMenuItem.Create(Self);
  RemovePathsMenu.Caption := 'Test Remove Engine Configuration from the Currrent Project';
  RemovePathsMenu.OnClick := ClickRemovePaths;

  CgeMenu := TMenuItem.Create(Self);
  CgeMenu.Caption := 'Test Castle Game Engine';
  CgeMenu.Name := 'TestCastleGameEngineMenu';
//  CgeMenu.Add(ChangeEnginePathMenu);
//  CgeMenu.Add(OpenEditorMenu);
//  CgeMenu.Add(AddPathsMenu);
//  CgeMenu.Add(RemovePathsMenu);

  // Use hardcoded 'ToolsMenu', this is good acoording to
  // https://docwiki.embarcadero.com/RADStudio/Athens/en/Adding_an_Item_to_the_Main_Menu_of_the_IDE
  Services.AddActionMenu('ToolsMenu', nil, CgeMenu, true, true);

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
  // will automatically free menu items owned by this
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
  //ForceDemandLoadState(dlDisable); // does not fix the issue
end;

initialization
  DelphiIdeIntegration := TTestDelphiIdeIntegration.Create(nil);
finalization
  // When unloading the package, make sure to remove menu item
  FreeAndNil(DelphiIdeIntegration);
end.
