unit UnitFormSettings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitFrameSettings, settings;

type
    TFormSettings = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    private
        { Private declarations }
        FSettings: TSettingsControl;
    public
        { Public declarations }
    end;

var
    FormSettings: TFormSettings;

implementation

{$R *.dfm}

procedure TFormSettings.FormActivate(Sender: TObject);
begin
    FSettings.Validate;
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin
    FSettings := TSettingsControl.Create;
    FSettings.FFrameSettings.Parent := self;
    FSettings.FFrameSettings.Align := alclient;
end;

procedure TFormSettings.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormSettings.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
    CursorPos: TPoint;
const
    AllowDisabled = true;
    AllowWinControls = true;
    AllLevels = true;
begin

    GetCursorPos(CursorPos);
    CursorPos := ScreenToClient(CursorPos);
    ActiveControl := nil;
    if Assigned(ControlAtPos(CursorPos, AllowDisabled,
      AllowWinControls, AllLevels)) then
        FSettings.FFrameSettings.CategoryPanelGroup1.VertScrollBar.Position :=
          FSettings.FFrameSettings.CategoryPanelGroup1.VertScrollBar.Position -
          WheelDelta div 5;
end;

end.
