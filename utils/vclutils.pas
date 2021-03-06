unit vclutils;

interface

uses vcl.stdctrls, vcl.controls, vcl.ComCtrls, vcl.graphics, system.Types;

type
    TControlProc = reference to procedure(const AControl: TControl);

procedure SetButtonMultiline(b: TButton);

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);

implementation

uses Winapi.Windows;

procedure ModifyControl(const AControl: TControl; const ARef: TControlProc);
var
    i: integer;
begin
    if AControl = nil then
        Exit;
    if AControl is TWinControl then
    begin
        for i := 0 to TWinControl(AControl).ControlCount - 1 do
            ModifyControl(TWinControl(AControl).controls[i], ARef);
    end;
    ARef(AControl);
end;

procedure SetButtonMultiline(b: TButton);
begin
    SetWindowLong(b.Handle, GWL_STYLE, GetWindowLong(b.Handle, GWL_STYLE) or
      BS_MULTILINE);
end;

function PageControl_visible_pages(Control: TCustomTabControl):TArray<TTabSheet>;
var i:integer;
begin
    SetLength(Result, 0);
    with Control as TPageControl do
    for i := 0 to PageCount-1 do
        if Pages[i].TabVisible then
            begin
                SetLength(Result, Length(result)+1);
                Result[Length(result)-1] := Pages[i];
            end;
end;

function page_tab_index(pages:TArray<TTabSheet>; page:TTabSheet):integer;
var pg:TTabSheet;
begin
    Result := 0;
    for pg in pages do
    begin
        if pg = page then
            exit;
        Result := Result + 1;
    end;
    Result := -1;
end;

procedure PageControl_DrawVerticalTab(Control: TCustomTabControl;
  TabIndex: integer; const Rect: system.Types.TRect; Active: boolean);
var
    i: integer;
    PageControl: TPageControl;
    Text: string;
    page_index, x, y: integer;
    txt_width, txt_height: double;
    page:TTabSheet;

begin
    PageControl := Control as TPageControl;
    page := PageControl_visible_pages(PageControl)[TabIndex];
    Text := page.Caption;
    Active := PageControl.ActivePage = page;

    txt_width := PageControl.Canvas.TextWidth(Text);
    txt_height := PageControl.Canvas.TextHeight(Text);

    x := Rect.Left + round((Rect.Width - txt_width) / 2.0);
    y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    if Active then
    begin
        PageControl.Canvas.Brush.Color := clGradientInactiveCaption;
        PageControl.Canvas.Font.Color := clNavy;
    end
    else
    begin
        PageControl.Canvas.Brush.Color := clWindow;
        PageControl.Canvas.Font.Color := clBlack;
    end;

    PageControl.Canvas.TextRect(Rect, x, y, Text);
end;

end.
