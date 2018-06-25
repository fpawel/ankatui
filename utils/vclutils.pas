unit vclutils;

interface

uses vcl.stdctrls;

procedure SetButtonMultiline(b: TButton);

implementation

uses Winapi.Windows;

procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

end.
