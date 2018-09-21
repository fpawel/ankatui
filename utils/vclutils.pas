unit vclutils;

interface

uses vcl.stdctrls, vcl.controls;

type
  TControlProc = reference to procedure (const AControl: TControl);

procedure SetButtonMultiline(b: TButton);

procedure ModifyControl(const AControl: TControl;
  const ARef: TControlProc);

implementation

uses Winapi.Windows;



procedure ModifyControl(const AControl: TControl;
  const ARef: TControlProc);
var
  i : Integer;
begin
  if AControl=nil then
    Exit;
  if AControl is TWinControl then begin
    for i := 0 to TWinControl(AControl).ControlCount-1 do
      ModifyControl(TWinControl(AControl).Controls[i], ARef);
  end;
   ARef(AControl);
end;


procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

end.
