unit UnitFrameVar;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UnitData, Vcl.StdCtrls,
    Vcl.Grids, Vcl.ExtCtrls, models;

type
    TFrameVar = class(TFrame)
        StringGrid2: TStringGrid;
        CheckBox2: TCheckBox;
        procedure CheckBox2Click(Sender: TObject);
        procedure StringGrid2SelectCell(Sender: TObject; ACol, ARow: Integer;
          var CanSelect: Boolean);
        procedure StringGrid2KeyPress(Sender: TObject; var Key: Char);
        procedure StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
          Rect: TRect; State: TGridDrawState);
        procedure StringGrid2DblClick(Sender: TObject);
    private
        { Private declarations }
    public
        FCurentProductVar: RProductVar;
        FProductVarValues: TProductVarValues;
        FVars: TArray<TDeviceVar>;
        { Public declarations }
        constructor Create(AOwner: TComponent); override;
        procedure SetCurrentParty(Products: TArray<TProduct>);
        procedure HandleReadVar(x: TReadVar);
        procedure reset;
    end;

implementation

{$R *.dfm}

uses stringgridutils, stringutils, UnitFormPopup;

constructor TFrameVar.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCurentProductVar.FProduct := -1;
    FCurentProductVar.FVar := -1;
    with CheckBox2 do
    begin
        Visible := false;
        Caption := '';
        Width := 15;
        Height := 15;
    end;
    FProductVarValues := TProductVarValues.Create;
end;

procedure TFrameVar.SetCurrentParty(Products: TArray<TProduct>);
var
    i, ARow: Integer;
    v: TDeviceVar;
begin
    for i := 0 to length(FVars) - 1 do
        FVars[i].Free;
    FProductVarValues.Clear;
    FVars := DataModule1.DeviceVars;
    with StringGrid2 do
    begin
        RowCount := length(FVars) + 1;
        ColCount := 2 + length(Products);
        FixedRows := 1;
        Cells[0, 0] := '№';
        Cells[1, 0] := 'Параметр';
        for ARow := 1 to length(FVars) do
        begin
            v := FVars[ARow - 1];
            Cells[0, ARow] := inttostr(v.FVar);
            Cells[1, ARow] := v.FName;
        end;

        for i := 0 to length(Products) - 1 do
        begin
            Cells[i + 2, 0] := inttostr(Products[i].FSerial);
        end;
    end;
end;

procedure TFrameVar.reset;
var
    PrevProductVar: RProductVar;
begin
    PrevProductVar := FCurentProductVar;
    FCurentProductVar.FProduct := -1;
    FCurentProductVar.FVar := -1;
    if (PrevProductVar.FProduct >= 0) and (PrevProductVar.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid2, PrevProductVar.FProduct + 2,
          PrevProductVar.FVar + 1);

    if (FCurentProductVar.FProduct >= 0) and (FCurentProductVar.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid2, FCurentProductVar.FProduct + 2,
          FCurentProductVar.FVar + 1);

end;

procedure TFrameVar.HandleReadVar(x: TReadVar);
var
    PrevProductVar: RProductVar;

begin
    PrevProductVar := FCurentProductVar;
    FCurentProductVar := x.ProductVar;

    FCurentProductVar.FProduct := x.FProductOrder;
    FCurentProductVar.FVar := x.FVarOrder;
    FProductVarValues.AddOrSetValue(x.ProductVar, x.ValueError);
    StringGrid2.Cells[x.ProductVar.FProduct + 2, x.ProductVar.FVar + 1] :=
      x.ValueError.FValue;

    if (PrevProductVar.FProduct >= 0) and (PrevProductVar.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid2, PrevProductVar.FProduct + 2,
          PrevProductVar.FVar + 1);

    if (FCurentProductVar.FProduct >= 0) and (FCurentProductVar.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid2, FCurentProductVar.FProduct + 2,
          FCurentProductVar.FVar + 1);

end;

procedure TFrameVar.StringGrid2DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    a: RProductVar;
begin
    a.FVar := StringGrid2.Row - 1;
    a.FProduct := StringGrid2.Col - 2;
    if FProductVarValues.ContainsKey(a) and FProductVarValues[a].FError then
    begin
        FormPopup.RichEdit1.Font.Color := clRed;
        FormPopup.RichEdit1.Text := FProductVarValues[a].FValue;
        r := StringGrid2.CellRect(StringGrid2.Col, StringGrid2.Row);
        pt := StringGrid2.ClientToScreen(r.BottomRight);
        FormPopup.Left := pt.x + 5;
        FormPopup.Top := pt.Y + 5;
        FormPopup.Show;
    end;

end;

procedure TFrameVar.StringGrid2DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    x, Y: Integer;
    txt_width, txt_height: double;
    s: string;
    Checked: Boolean;
    pv: RProductVar;
const
    lineColor: TColor = $00BCBCBC;
begin

    grd := TStringGrid(Sender);
    s := grd.Cells[ACol, ARow];
    if (ACol = 0) and (ARow > 0) then
        s := '';
    cnv := grd.Canvas;
    cnv.Font := grd.Font;
    if (ARow > 0) and (ACol = 1) then
    begin
        cnv.Font.Size := 10;
        cnv.Font.Color := clNavy;
    end;

    Checked := false;
    if ARow > 0 then
        Checked := FVars[ARow - 1].FChecked;

    if gdFixed in State then
        cnv.Brush.Color := cl3DLight
    else if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if Checked then
        cnv.Brush.Color := grd.Color
    else
        cnv.Brush.Color := clBtnFace;

    pv.FProduct := ACol - 2;
    pv.FVar := ARow - 1;

    if (FCurentProductVar.FProduct >= 0) and (FCurentProductVar.FVar >= 0) and
      (ProductVarEqual(pv, FCurentProductVar)) then
        cnv.Brush.Color := clInfoBk;

    if FProductVarValues.ContainsKey(pv) then
    begin
        if FProductVarValues[pv].FError then
        begin
            cnv.Brush.Color := clBlack;
            cnv.Font.Color := clYellow;
            cnv.Font.Size := 10;
            if gdSelected in State then
                cnv.Brush.Color := clGray;
        end;
    end;

    if cnv.TextWidth(s) + 3 > Rect.Width then
        s := cut_str(s, cnv, Rect.Width);

    txt_width := cnv.TextWidth(s);
    txt_height := cnv.TextHeight(s);

    x := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol <> 1) then
        x := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, x, Y, s);

    if (ACol = 0) and (ARow > 0) then
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked);

    cnv.Pen.Color := lineColor;
    cnv.Pen.Width := -1;

    cnv.MoveTo(Rect.Left, Rect.Bottom);
    cnv.LineTo(Rect.Left, Rect.Top);
    cnv.LineTo(Rect.Right, Rect.Top);

    if ACol = grd.ColCount - 1 then
    begin
        cnv.MoveTo(Rect.Right, Rect.Top);
        cnv.LineTo(Rect.Right, Rect.Bottom);
    end;

    if ARow = grd.RowCount - 1 then
    begin
        cnv.MoveTo(Rect.Left, Rect.Bottom);
        cnv.LineTo(Rect.Right, Rect.Bottom);
    end;

end;

procedure TFrameVar.StringGrid2KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    i: Integer;
    v: Boolean;

begin
    g := Sender as TStringGrid;
    if (g.Row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := FVars[g.Selection.Top - 1].FChecked;
        for i := g.Selection.Top to g.Selection.Bottom do
        begin
            FVars[i - 1].FChecked := not v;
            DataModule1.UpdateDeviceVarChecked(FVars[i - 1].FVar,
              FVars[i - 1].FChecked);
        end;
        StringGrid_Redraw(g);
    end;

    if ord(Key) = 1 then
    begin
        v := DataModule1.InvertVarsChecked;
        for i := 0 to length(FVars) - 1 do
            FVars[i].FChecked := v;
        StringGrid_Redraw(g);
    end;

end;

procedure TFrameVar.StringGrid2SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    grd := Sender as TStringGrid;

    case ACol of
        0:
            begin
                r := grd.CellRect(ACol, ARow);
                r.Left := r.Left + grd.Left + 10;
                r.Right := r.Right + grd.Left;
                r.Top := r.Top + grd.Top + 7;
                r.Bottom := r.Bottom + grd.Top;

                with CheckBox2 do
                begin

                    OnClick := nil;
                    if GetAsyncKeyState(VK_LBUTTON) < 0 then
                    begin
                        FVars[ARow - 1].FChecked := not FVars[ARow - 1]
                          .FChecked;
                        DataModule1.UpdateDeviceVarChecked(FVars[ARow - 1].FVar,
                          FVars[ARow - 1].FChecked);
                        StringGrid_Redraw(grd);
                    end;
                    Checked := FVars[ARow - 1].FChecked;
                    OnClick := CheckBox2Click;

                    Left := r.Left - 6;
                    Top := r.Top - 3;
                    Visible := true;
                end;
            end;

    else
        begin
            CheckBox2.Visible := false;

        end;
    end;

end;

procedure TFrameVar.CheckBox2Click(Sender: TObject);
begin
    with CheckBox2 do
    begin
        FVars[StringGrid2.Row - 1].FChecked := Checked;
        DataModule1.UpdateDeviceVarChecked
          (FVars[StringGrid2.Row - 1].FVar, Checked);
    end;
    StringGrid_Redraw(StringGrid2);
    StringGrid2.SetFocus;
end;

end.
