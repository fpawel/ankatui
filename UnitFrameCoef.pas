unit UnitFrameCoef;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, UnitData,
    stringgridutils, Vcl.StdCtrls, Vcl.ExtCtrls;

type
    TFrameCoef = class(TFrame)
        StringGrid3: TStringGrid;
    CheckBox3: TCheckBox;
    procedure StringGrid3SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure StringGrid3SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure CheckBox3Click(Sender: TObject);
    procedure StringGrid3DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid3KeyPress(Sender: TObject; var Key: Char);
    procedure StringGrid3DblClick(Sender: TObject);
    private
        { Private declarations }
        FCurentProductCoef: RProductVar;

        Last_Edited_Col, Last_Edited_Row: integer;
        FProductCoefValues: TProductVarValues;
    public
        FCoefs: TArray<TDeviceVar>;
        { Public declarations }
        constructor Create(AOwner: TComponent); override;
        procedure SetCurrentParty(Products: TArray<TProduct>);
        procedure HandleReadCoef(x: TReadVar);
        procedure reset;
    end;

implementation

{$R *.dfm}

uses stringutils, UnitFormPopup;

constructor TFrameCoef.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    Last_Edited_Col := -1;
    Last_Edited_Row := -1;
    FCurentProductCoef.FProduct := -2;
    FCurentProductCoef.FVar := -2;
    with CheckBox3 do
    begin
        Visible := false;
        Caption := '';
        Width := 15;
        Height := 15;
    end;
    FProductCoefValues:= TProductVarValues.Create;

end;

procedure TFrameCoef.HandleReadCoef(x: TReadVar);
var
    PrevProductCoef: RProductVar;
begin
    PrevProductCoef := FCurentProductCoef;
    FCurentProductCoef := x.ProductVar;

    FCurentProductCoef.FProduct := x.FProduct;
    FCurentProductCoef.FVar := x.FVar;
    FProductCoefValues.AddOrSetValue(x.ProductVar, x.ValueError);
    StringGrid3.Cells[x.ProductVar.FProduct + 2, x.ProductVar.FVar + 1] :=
      x.ValueError.FValue;

    if (PrevProductCoef.FProduct >= 0) and (PrevProductCoef.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid3, PrevProductCoef.FProduct + 2,
          PrevProductCoef.FVar + 1);

    if (FCurentProductCoef.FProduct >= 0) and (FCurentProductCoef.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid3, FCurentProductCoef.FProduct + 2,
          FCurentProductCoef.FVar + 1);

end;

procedure TFrameCoef.reset;
var
    PrevProductVar: RProductVar;
begin
    PrevProductVar := FCurentProductCoef;
    FCurentProductCoef.FProduct := -1;
    FCurentProductCoef.FVar := -1;
    if (PrevProductVar.FProduct >= 0) and (PrevProductVar.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid3, PrevProductVar.FProduct + 2,
          PrevProductVar.FVar + 1);

    if (FCurentProductCoef.FProduct >= 0) and (FCurentProductCoef.FVar >= 0) then
        StringGrid_RedrawCell(StringGrid3, FCurentProductCoef.FProduct + 2,
          FCurentProductCoef.FVar + 1);

end;


procedure TFrameCoef.CheckBox3Click(Sender: TObject);
begin
    with CheckBox3 do
    begin
        FCoefs[StringGrid3.Row - 1].FChecked := Checked;
        DataModule1.UpdateCoefChecked(FCoefs[StringGrid3.Row - 1].FVar,
          Checked);
    end;
    StringGrid_Redraw(StringGrid3);
    StringGrid3.SetFocus;
end;


procedure TFrameCoef.SetCurrentParty(Products: TArray<TProduct>);
var
    i, arow: integer;
    v: TDeviceVar;
    c: RProductCoefValue;
begin

    for i := 0 to length(FCoefs) - 1 do
        FCoefs[i].Free;
    FCoefs := DataModule1.DeviceCoefs;
    StringGrid_clear(StringGrid3);
    with StringGrid3 do
    begin
        RowCount := length(FCoefs) + 1;
        ColCount := 2 + length(Products);
        FixedRows := 1;
        Cells[0, 0] := '�';
        Cells[1, 0] := '�����������';
        for arow := 1 to length(FCoefs) do
        begin
            v := FCoefs[arow - 1];
            Cells[0, arow] := inttostr(v.FVar);
            Cells[1, arow] := v.FName;
        end;

        for i := 0 to length(Products) - 1 do
        begin
            Cells[i + 2, 0] := inttostr(Products[i].FSerial);
        end;

        OnSetEditText := nil;
        for c in DataModule1.CurrentPartyCoefs do
            Cells[c.Ordinal + 2, c.Coef + 1] := FloatToStr(c.Value);
        OnSetEditText := StringGrid3SetEditText;
    end;
end;

procedure TFrameCoef.StringGrid3DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
    a: RProductVar;
begin
    a.FVar := StringGrid3.Row - 1;
    a.FProduct := StringGrid3.Col - 2;
    if FProductCoefValues.ContainsKey(a) and FProductCoefValues[a].FError then
    begin
        FormPopup.RichEdit1.Font.Color := clRed;
        FormPopup.RichEdit1.Text := FProductCoefValues[a].FValue;
        r := StringGrid3.CellRect(StringGrid3.Col, StringGrid3.Row);
        pt := StringGrid3.ClientToScreen(r.BottomRight);
        FormPopup.Left := pt.x + 5;
        FormPopup.Top := pt.Y + 5;
        FormPopup.Show;
    end;

end;

procedure TFrameCoef.StringGrid3DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    X, Y: integer;
    txt_width, txt_height: double;
    s: string;
    Checked: boolean;
    pv: RProductVar;
const
    lineColor: TColor = $00BCBCBC;
begin
    Checked := false;

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

    if ARow > 0 then
        Checked := FCoefs[ARow - 1].FChecked;

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

    if (FCurentProductCoef.FProduct >= 0) and (FCurentProductCoef.FVar >= 0) and
      (ProductVarEqual(pv, FCurentProductCoef)) then
        cnv.Brush.Color := clInfoBk;

      if FProductCoefValues.ContainsKey(pv) then
    begin
        if FProductCoefValues[pv].FError then
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

    X := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol <> 1) then
        X := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, X, Y, s);

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

procedure TFrameCoef.StringGrid3KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    i: integer;
    v: boolean;

begin
    g := Sender as TStringGrid;
    if (g.Row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := FCoefs[g.Selection.Top - 1].FChecked;
        for i := g.Selection.Top to g.Selection.Bottom do
        begin
            FCoefs[i - 1].FChecked := not v;
            DataModule1.UpdateCoefChecked(FCoefs[i - 1].FVar,
              FCoefs[i - 1].FChecked);
        end;
        StringGrid_Redraw(g);
    end;

    if ord(Key) = 1 then
    begin
        v := DataModule1.InvertCoefsChecked;
        for i := 0 to length(FCoefs) - 1 do
            FCoefs[i].FChecked := v;
        StringGrid_Redraw(g);
    end;

end;

procedure TFrameCoef.StringGrid3SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    grd := Sender as TStringGrid;

    // When selecting a cell
    if grd.EditorMode then
    begin // It was a cell being edited
        grd.EditorMode := false; // Deactivate the editor
        // Do an extra check if the LastEdited_ACol and LastEdited_ARow are not -1 already.
        // This is to be able to use also the arrow-keys up and down in the Grid.
        if (Last_Edited_Col <> -1) and (Last_Edited_Row <> -1) then
            StringGrid3SetEditText(grd, Last_Edited_Col, Last_Edited_Row,
              grd.Cells[Last_Edited_Col, Last_Edited_Row]);
        // Just make the call
    end;
    // Do whatever else wanted


    if (ARow > 0) AND (ACol > 1) then
        grd.Options := grd.Options + [goEditing]
    else
        grd.Options := grd.Options - [goEditing];


    case ACol of
        0:
            begin
                r := grd.CellRect(ACol, ARow);
                r.Left := r.Left + grd.Left + 10;
                r.Right := r.Right + grd.Left;
                r.Top := r.Top + grd.Top + 7;
                r.Bottom := r.Bottom + grd.Top;

                with CheckBox3 do
                begin

                    OnClick := nil;
                    if GetAsyncKeyState(VK_LBUTTON) < 0 then
                    begin
                        FCoefs[ARow - 1].FChecked :=
                          not FCoefs[ARow - 1].FChecked;
                        DataModule1.UpdateCoefChecked(FCoefs[ARow - 1].FVar,
                          FCoefs[ARow - 1].FChecked);
                        StringGrid_Redraw(grd);
                    end;
                    Checked := FCoefs[ARow - 1].FChecked;
                    OnClick := CheckBox3Click;

                    Left := r.Left - 6;
                    Top := r.Top - 3;
                    Visible := true;
                end;
            end;

    else
        begin
            CheckBox3.Visible := false;

        end;
    end;

end;

procedure TFrameCoef.StringGrid3SetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
    s: string;
begin
    With StringGrid3 do
        if (ARow > 0) and (ACol > 1) then
        begin
            // Fired on every change
            if Not EditorMode // goEditing must be 'True' in Options
            then
            begin // Only after user ends editing the cell
                Last_Edited_Col := -1; // Indicate no cell is edited
                Last_Edited_Row := -1; // Indicate no cell is edited
                // Do whatever wanted after user has finish editing a cell
                DataModule1.SetCoefValue(ACol - 2, ARow - 1, Value);
                s := DataModule1.GetCoefValue(ACol - 2, ARow - 1);
                if (Value <> s) then
                begin
                    OnSetEditText := nil;
                    Cells[ACol, ARow] := s;
                    OnSetEditText := StringGrid3SetEditText;
                end;

            end
            else
            begin // The cell is being editted
                Last_Edited_Col := ACol; // Remember column of cell being edited
                Last_Edited_Row := ARow; // Remember row of cell being edited
            end;
        end;

end;

end.
