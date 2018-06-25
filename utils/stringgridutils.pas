unit stringgridutils;

interface

uses vcl.grids, system.types, system.Classes;

procedure StringGrid_Clear(grd: TStringGrid);
procedure StringGrid_DrawCheckBoxCell(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean);
procedure StringGrid_Redraw(grd: TStringGrid);
procedure StringGrid_RedrawCell(grd: TStringGrid; acol, arow: integer);
procedure StringGrid_RedrawRow(grd: TStringGrid; arow: integer);

implementation

uses winapi.windows, system.math, winapi.uxtheme, vcl.graphics;

procedure StringGrid_Redraw(grd: TStringGrid);
var
    acol, arow: integer;
begin
    with grd do
        for acol := 0 to colcount - 1 do
            for arow := 0 to rowcount - 1 do
                Cells[acol, arow] := Cells[acol, arow];
end;

procedure StringGrid_RedrawCell(grd: TStringGrid; acol, arow: integer);
begin
    with grd do
        if (acol >= 0) AND (acol < colcount) AND (arow >= 0) AND
          (arow < rowcount) then
            Cells[acol, arow] := Cells[acol, arow];
end;

procedure StringGrid_RedrawRow(grd: TStringGrid; arow: integer);
var
    I: integer;
begin
    with grd do
        if (arow >= 0) AND (arow < rowcount) then
            for I := 0 to colcount - 1 do

                Cells[I, arow] := Cells[I, arow];
end;

procedure StringGrid_Clear(grd: TStringGrid);
var
    acol, arow: integer;
begin
    with grd do
        for acol := 1 to colcount - 1 do
            for arow := 1 to rowcount - 1 do
                Cells[acol, arow] := '';
end;

procedure StringGrid_DrawCheckBoxCell(grd: TStringGrid; acol, arow: integer;
  Rect: TRect; State: TGridDrawState; checked: boolean);
const
    PADDING = 4;
var
    h: HTHEME;
    s: TSize;
    r: TRect;
begin
    // FillRect(grd.Canvas.Handle, Rect, GetStockObject(WHITE_BRUSH));
    s.cx := GetSystemMetrics(SM_CXMENUCHECK);
    s.cy := GetSystemMetrics(SM_CYMENUCHECK);
    if UseThemes then
    begin
        h := OpenThemeData(grd.Handle, 'BUTTON');
        if h <> 0 then
            try
                GetThemePartSize(h, grd.Canvas.Handle, BP_CHECKBOX,
                  CBS_CHECKEDNORMAL, nil, TS_DRAW, s);
                r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
                r.Bottom := r.Top + s.cy;
                r.Left := Rect.Left + PADDING;
                r.Right := r.Left + s.cx;
                DrawThemeBackground(h, grd.Canvas.Handle, BP_CHECKBOX,
                  IfThen(checked, CBS_CHECKEDNORMAL,
                  CBS_UNCHECKEDNORMAL), r, nil);
            finally
                CloseThemeData(h);
            end;
    end
    else
    begin
        r.Top := Rect.Top + (Rect.Bottom - Rect.Top - s.cy) div 2;
        r.Bottom := r.Top + s.cy;
        r.Left := Rect.Left + PADDING;
        r.Right := r.Left + s.cx;
        DrawFrameControl(grd.Canvas.Handle, r, DFC_BUTTON,
          IfThen(checked, DFCS_CHECKED, DFCS_BUTTONCHECK));
    end;
    r := system.Classes.Rect(r.Right + PADDING, Rect.Top, Rect.Right,
      Rect.Bottom);

    r.Right := r.Right - 3;

    DrawText(grd.Canvas.Handle, grd.Cells[acol, arow],
      length(grd.Cells[acol, arow]), r, DT_SINGLELINE or DT_VCENTER or
      DT_RIGHT or DT_END_ELLIPSIS);
end;

end.
