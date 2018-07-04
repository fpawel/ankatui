unit Unit1;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.Buttons,
    Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
    parties, System.Generics.Collections,
    System.ImageList, UnitData, Vcl.ImgList, Vcl.Menus, VirtualTrees, pipe,
    CurrentWork, msglevel, UnitFormLog, settings, UnitFormPopup, UnitFrameCoef,
    UnitFrameVar,
    UnitFormManualControl;

type

    TEndWorkInfo = class
        FError: string;
        FName: string;
    end;

    TReadProduct = class
        FProduct: integer;
    end;

    TProductConnected = class
        FProduct: integer;
        FOk: boolean;
        FText: string;
    end;

    TForm1 = class(TForm)
        StringGrid1: TStringGrid;
        ComboBox1: TComboBox;
        Panel7: TPanel;
        Panel4: TPanel;
        Panel1: TPanel;
        CheckBox1: TCheckBox;
        Panel3: TPanel;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        CategoryPanelGroup1: TCategoryPanelGroup;
        CategoryPanel1: TCategoryPanel;
        ImageList1: TImageList;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        ToolBar2: TToolBar;
        ToolButtonRun: TToolButton;
        ToolButtonStop: TToolButton;
        Splitter1: TSplitter;
        PageControl1: TPageControl;
        TabSheet1: TTabSheet;
        Panel5: TPanel;
        ImageList2: TImageList;
        PageControl2: TPageControl;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        TabSheet5: TTabSheet;
        TabSheet6: TTabSheet;
        Panel2: TPanel;
        N3: TMenuItem;
        CategoryPanel2: TCategoryPanel;
        VirtualStringTree1: TVirtualStringTree;
        ToolButton2: TToolButton;
        TabSheet7: TTabSheet;
        TabSheet8: TTabSheet;
    N4: TMenuItem;
    N5: TMenuItem;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1CloseUp(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: integer;
          var CanSelect: boolean);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
          Rect: TRect; State: TGridDrawState);
        procedure CheckBox1Click(Sender: TObject);
        procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure ToolButton1Click(Sender: TObject);
        procedure ComboBox1DropDown(Sender: TObject);
        procedure StringGrid1TopLeftChanged(Sender: TObject);
        procedure ComboBox1Exit(Sender: TObject);
        procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
        procedure TFrameRun1ToolButtonRunMouseUp(Sender: TObject;
          Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure N1Click(Sender: TObject);
        procedure ToolButtonStopClick(Sender: TObject);
        procedure CategoryPanelGroup1Resize(Sender: TObject);
        procedure CategoryPanel1Expand(Sender: TObject);
        procedure CategoryPanel1Collapse(Sender: TObject);
        procedure CategoryPanel2Collapse(Sender: TObject);
        procedure CategoryPanel2Expand(Sender: TObject);
        procedure TabSheet5Show(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure CategoryPanel3Collapse(Sender: TObject);
        procedure CategoryPanel3Expand(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure N2Click(Sender: TObject);
        procedure N3Click(Sender: TObject);
        procedure ToolButton2Click(Sender: TObject);
    procedure N4Click(Sender: TObject);
    private
        { Private declarations }
        FProducts: TArray<TProduct>;
        FPipe: TPipe;
        FCurrentWork: TCurrentWork;
        FSettings: TSettingsControl;
        FFrameCoef: TFrameCoef;
        FFrameVar: TFrameVar;
        FReadProduct: integer;
        FFormLog: TFormLog;
        procedure HandleReadVar(content: string);
        procedure HandleReadCoefficient(content: string);
        procedure HandleCurentWorkMessage(content: string);
        procedure HandleProductConnected(content: string);
        procedure HandleReadProduct(content: string);
        procedure HandleEndWork(content: string);

        procedure UpdateGroupHeights;

    public
        { Public declarations }
        procedure SetCurrentParty;
        procedure SetupWorkStarted(work: string; started: boolean);
    end;

var
    Form1: TForm1;

implementation

uses dateutils, rest.json, Winapi.uxtheme, System.Math, UnitFormNewPartyDialog,
    stringgridutils,
    listports, UnitFormParties,
    CurrentWorkTreeData, stringutils, vclutils, UnitFormChart;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
    ARow: integer;
    i: integer;
begin
    FReadProduct := -1;
    FFrameCoef := TFrameCoef.Create(self);
    FFrameCoef.Parent := TabSheet6;
    FFrameCoef.Align := alclient;

    FFrameVar := TFrameVar.Create(self);
    FFrameVar.Parent := TabSheet1;
    FFrameVar.Align := alclient;

    with TFormParties.Create(self) do
    begin
        Splitter1.Parent := TabSheet4;
        Panel1.Parent := TabSheet4;
        VirtualStringTree1.Parent := TabSheet4;
    end;

    with TFormChart.Create(self) do
    begin
        Splitter1.Parent := TabSheet8;
        Panel1.Parent := TabSheet8;
        VirtualStringTree1.Parent := TabSheet8;
    end;

    FFormLog := TFormLog.Create(self);
    with FFormLog do
    begin
        Splitter1.Parent := TabSheet7;
        RichEdit1.Parent := TabSheet7;
        VirtualStringTree1.Parent := TabSheet7;
        FFormLog.FOnRenderMessages := procedure
            begin
                if PageControl2.activePage = TabSheet7 then
                begin
                    FFormLog.RichEdit1.SetFocus;
                    SendMessage(FFormLog.RichEdit1.Handle, EM_SCROLL,
                      SB_LINEDOWN, 0);
                end;
            end;
    end;

    UpdateGroupHeights;

    FSettings := TSettingsControl.Create;
    FSettings.FFrameSettings.Parent := TabSheet5;
    FSettings.FFrameSettings.Align := alclient;

    ToolBar2.Width := 58;
    ComboBox1DropDown(ComboBox1);

    ComboBox1.ItemHeight := 18;
    ComboBox1.Visible := false;

    with CheckBox1 do
    begin
        Visible := false;
        Caption := '';
        Width := 15;
        Height := 15;
    end;

    with StringGrid1 do
    begin
        Cells[0, 0] := '№ п/п';
        Cells[1, 0] := 'COM порт';
        Cells[2, 0] := 'Зав.№';
        Cells[3, 0] := 'Связь';

        ColWidths[0] := 70;
        ColWidths[1] := 90;
        ColWidths[2] := 75;
    end;
    SetCurrentParty;

    FPipe := TPipe.Create;
    FCurrentWork := TCurrentWork.Create(FPipe, Panel5, VirtualStringTree1);

    FPipe.Handle('READ_COEFFICIENT', HandleReadCoefficient);
    FPipe.Handle('READ_VAR', HandleReadVar);

    FPipe.Handle('CURRENT_WORK_MESSAGE', HandleCurentWorkMessage);

    FPipe.Handle('PRODUCT_CONNECTED', HandleProductConnected);

    FPipe.Handle('READ_PRODUCT', HandleReadProduct);

    FPipe.Handle('END_WORK', HandleEndWork);

    FPipe.Connect('ANKAT');

end;

procedure TForm1.HandleEndWork(content: string);
var
    X: TEndWorkInfo;
begin
    FFrameVar.reset;
    FFrameCoef.reset;
    X := TJson.JsonToObject<TEndWorkInfo>(content);
    SetupWorkStarted(X.FName + ': выполнено', false);
    if X.FError <> '' then
    begin
        Panel5.Caption := X.FName + ': ' + X.FError;
        Panel5.Font.Color := clRed;
    end;

    X.Free;

end;

procedure TForm1.HandleProductConnected(content: string);
var
    X: TProductConnected;
begin
    X := TJson.JsonToObject<TProductConnected>(content);
    FProducts[X.FProduct].FConnectionError := not X.FOk;
    FProducts[X.FProduct].FConnection := X.FText;
    StringGrid1.Cells[3, X.FProduct + 1] := X.FText;
    StringGrid_RedrawRow(StringGrid1, X.FProduct + 1);
    X.Free;
end;

procedure TForm1.HandleCurentWorkMessage(content: string);
var
    m: TWorkMsg;
    i: integer;

begin
    m := TJson.JsonToObject<TWorkMsg>(content);

    Panel2.Caption := Format('%s [%d] %s: %s',
      [timetostr(IncHour(m.FCreatedAt, 3)), m.FWorkIndex, m.FWork, m.FText]);
    if m.FLevel >= LError then
    begin
        Panel2.Font.Color := clRed;
        FCurrentWork.SetRunError;
    end
    else
        Panel2.Font.Color := clNavy;

    FFormLog.MoveCursorToLast;
    PrintWorkMessages(FFormLog.RichEdit1, m.FWorkIndex, '', m.FProductSerial,
      m.FCreatedAt, m.FLevel, m.FText);

    if PageControl2.activePage = TabSheet7 then
    begin
        FFormLog.RichEdit1.SetFocus;
        SendMessage(FFormLog.RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
    end;

    m.Free;
end;

procedure TForm1.HandleReadProduct(content: string);
var
    X: TReadProduct;
    PrevProduct: integer;
begin
    X := TJson.JsonToObject<TReadProduct>(content);
    PrevProduct := FReadProduct;
    FReadProduct := X.FProduct;
    if PrevProduct <> FReadProduct then
    begin
        StringGrid_RedrawRow(StringGrid1, PrevProduct + 1);
        StringGrid_RedrawRow(StringGrid1, FReadProduct + 1);
    end;
    X.Free;
end;

procedure TForm1.HandleReadVar(content: string);
var
    i: integer;
    X: TReadVar;
    p: TProduct;
    v: TDeviceVar;
begin
    X := TJson.JsonToObject<TReadVar>(content);
    p := FProducts[X.FProduct];
    v := FFrameVar.FVars[X.FVar];
    Panel2.Font.Color := clNavy;
    Panel2.Caption := Format('Считывание: прибор %d: регистр %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        Panel2.Caption := Panel2.Caption + FloatToStr(X.FValue)
    else
    begin
        Panel2.Caption := Panel2.Caption + X.FError;
        Panel2.Font.Color := clRed;
    end;

    FFrameVar.HandleReadVar(X);
    X.Free;
end;

procedure TForm1.HandleReadCoefficient(content: string);
var
    i: integer;
    X: TReadVar;
    p: TProduct;
    v: TDeviceVar;
begin
    X := TJson.JsonToObject<TReadVar>(content);
    p := FProducts[X.FProduct];
    v := FFrameCoef.FCoefs[X.FVar];
    Panel2.Font.Color := clNavy;
    Panel2.Caption := Format('Считывание: прибор %d: коэффициент %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        Panel2.Caption := Panel2.Caption + FloatToStr(X.FValue)
    else
    begin
        Panel2.Caption := Panel2.Caption + X.FError;
        Panel2.Font.Color := clRed;
    end;

    FFrameCoef.HandleReadCoef(X);
    X.Free;
end;

procedure TForm1.SetupWorkStarted(work: string; started: boolean);
begin
    Panel2.Caption := '';
    Panel5.Caption := work;
    Panel5.Font.Color := clNavy;
    ToolButton1.Visible := not started;
    ToolButtonRun.Visible := not started;
    ToolButtonStop.Visible := started;
    FormManualControl.Button1.Enabled := not started;
    FormManualControl.Button6.Enabled := not started;
    FormManualControl.RadioGroup1.Enabled := not started;
    FormManualControl.GroupBox2.Enabled := not started;
    ToolBar1.Width := 122;
    if started then
        ToolBar1.Width := ToolBar1.Width - ToolButton1.Width;
    // UpdateGroupHeights;
end;

procedure TForm1.UpdateGroupHeights;
var
    i: integer;
    p: TCategoryPanel;
    h: integer;
begin
    if not CategoryPanel2.Collapsed then
    begin
        h := CategoryPanelGroup1.ClientHeight;
        for i := 0 to CategoryPanelGroup1.Panels.Count - 1 do
        begin
            p := TCategoryPanel(CategoryPanelGroup1.Panels.Items[i]);
            if p <> CategoryPanel2 then
                h := h - p.Height;
        end;
        CategoryPanel2.Height := h
    end;

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FPipe.Close;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
    CursorPos: TPoint;
    FormPos: TPoint;

const
    AllowDisabled = true;
    AllowWinControls = true;
    AllLevels = true;
begin
    if PageControl1.activePage <> TabSheet5 then
        exit;

    GetCursorPos(CursorPos);
    CursorPos := TabSheet5.ScreenToClient(CursorPos);
    ActiveControl := nil;
    if Assigned(TabSheet5.ControlAtPos(CursorPos, AllowDisabled,
      AllowWinControls, AllLevels)) then
        FSettings.FFrameSettings.CategoryPanelGroup1.VertScrollBar.Position :=
          FSettings.FFrameSettings.CategoryPanelGroup1.VertScrollBar.Position -
          WheelDelta div 5;
end;

procedure TForm1.SetCurrentParty;
var
    i, ARow, ACol: integer;
    v: TDeviceVar;
    c: RProductCoefValue;
    CurrentPartyID: int64;

begin
    for i := 0 to length(FProducts) - 1 do
        FProducts[i].Free;

    ComboBox1DropDown(ComboBox1);
    FProducts := DataModule1.CurrentPartyProducts;

    CurrentPartyID := DataModule1.CurrentPartyID;
    CategoryPanel1.Caption := Format('Приборы партии № %d', [CurrentPartyID]);
    TabSheet3.Caption := Format('Партия № %d', [CurrentPartyID]);

//    TCategoryPanel(FSettings.FFrameSettings.CategoryPanelGroup1.Panels.First)
//      .Caption := Format('Параметры партии № %d', [CurrentPartyID]);

    with StringGrid1 do
    begin
        RowCount := length(FProducts) + 1;
        FixedRows := 1;
        CategoryPanel1.Height := RowCount * DefaultRowHeight + 50;
        for i := 0 to length(FProducts) - 1 do
        begin
            Cells[0, i + 1] := inttostr(i + 1);
            Cells[1, i + 1] := FProducts[i].FComport;
            Cells[2, i + 1] := inttostr(FProducts[i].FSerial);

        end;
    end;
    FFrameVar.SetCurrentParty(FProducts);
    FFrameCoef.SetCurrentParty(FProducts);
end;

procedure TForm1.N1Click(Sender: TObject);
begin
    FPipe.WriteMsgJSON('READ_VARS', nil);
    SetupWorkStarted('Опрос', true);
    PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
    FPipe.WriteMsgJSON('READ_COEFFICIENTS', nil);
    SetupWorkStarted('Считывание коэффициентов', true);
    PageControl1.ActivePageIndex := 1;
end;

procedure TForm1.N3Click(Sender: TObject);
begin
    FPipe.WriteMsgJSON('WRITE_COEFFICIENTS', nil);
    SetupWorkStarted('Запись коэффициентов', true);
    PageControl1.ActivePageIndex := 1;
end;

procedure TForm1.N4Click(Sender: TObject);
var i:integer;
begin
    if FCurrentWork.SelectedOperation <> nil then
        i := FCurrentWork.SelectedOperation.FOrdinal;
    FPipe.WriteMsgStr('RUN_MAIN_WORK', inttostr(i));
    SetupWorkStarted(FCurrentWork.SelectedOperation.FName, true);
    PageControl1.ActivePageIndex := 0;
end;

procedure TForm1.CategoryPanel1Collapse(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanel1Expand(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanel2Collapse(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanel2Expand(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanel3Collapse(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanel3Expand(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CategoryPanelGroup1Resize(Sender: TObject);
begin
    UpdateGroupHeights;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
    with CheckBox1 do
    begin
        FProducts[StringGrid1.Row - 1].FChecked := Checked;
        DataModule1.UpdateProductChecked(StringGrid1.Row - 1, Checked);
    end;
    StringGrid_Redraw(StringGrid1);
    StringGrid1.SetFocus;
end;

procedure TForm1.ComboBox1CloseUp(Sender: TObject);
begin
    with ComboBox1, StringGrid1 do
    begin
        if ItemIndex <> -1 then
            Cells[Col, Row] := Items[ItemIndex]
        else
            Cells[Col, Row] := '';
        DataModule1.UpdateProductComport(Row - 1, Cells[Col, Row]);
    end;
    StringGrid1.SetFocus;
end;

procedure TForm1.ComboBox1DropDown(Sender: TObject);
begin
    EnumComPorts((Sender as TComboBox).Items);
end;

procedure TForm1.ComboBox1Exit(Sender: TObject);
begin
    ComboBox1.Visible := false;
end;

procedure TForm1.StringGrid1DblClick(Sender: TObject);
var
    r: TRect;
    pt: TPoint;
begin
    with StringGrid1 do
        if (Col = 3) and (Row > 0) and (FProducts[Row - 1].FConnectionError)
        then
        begin
            FormPopup.RichEdit1.Font.Color := clRed;
            FormPopup.RichEdit1.Text := FProducts[Row - 1].FConnection;
            r := CellRect(Col, Row);
            pt := StringGrid1.ClientToScreen(r.BottomRight);
            FormPopup.Left := pt.X + 5;
            FormPopup.Top := pt.Y + 5;
            FormPopup.Show;
        end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
  Rect: TRect; State: TGridDrawState);
var
    grd: TStringGrid;
    cnv: TCanvas;
    X, Y: integer;
    txt_width, txt_height: double;
    s: string;
    Checked: boolean;

const
    lineColor: TColor = $00BCBCBC;
begin

    grd := TStringGrid(Sender);
    s := grd.Cells[ACol, ARow];
    if (ACol = 0) and (ARow > 0) then
        s := '';
    cnv := grd.Canvas;
    cnv.Font := grd.Font;

    if ARow > 0 then
        Checked := FProducts[ARow - 1].FChecked;

    if gdFixed in State then
        cnv.Brush.Color := cl3DLight
    else if gdSelected in State then
        cnv.Brush.Color := clGradientInactiveCaption
    else if Checked then
        cnv.Brush.Color := grd.Color
    else
        cnv.Brush.Color := clBtnFace;

    if (FReadProduct >= 0) and (FReadProduct = ARow - 1) then
        cnv.Brush.Color := clInfoBk;

    if (ACol = 3) AND (ARow > 0) AND (FProducts[ARow - 1].FConnectionError) then
    begin
        cnv.Brush.Color := clBlack;
        cnv.Font.Color := clYellow;
        cnv.Font.Size := 10;

        if gdSelected in State then
            cnv.Brush.Color := clGray;
    end;

    if cnv.TextWidth(s) + 3 > Rect.Width then
        s := cut_str(s, cnv, Rect.Width);

    txt_width := cnv.TextWidth(s);
    txt_height := cnv.TextHeight(s);

    X := Rect.Left + 3;
    // x := Rect.left + round((Rect.Width - txt_width) / 2.0);

    if (ARow > 0) AND (ACol = 2) then
        X := Rect.Right - 3 - round(txt_width);

    Y := Rect.Top + round((Rect.Height - txt_height) / 2.0);

    cnv.TextRect(Rect, X, Y, s);

    if (ACol = 0) and (ARow > 0) then
    begin
        StringGrid_DrawCheckBoxCell(grd, ACol, ARow, Rect, State, Checked);

    end;

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

procedure TForm1.StringGrid1KeyPress(Sender: TObject; var Key: Char);
var
    g: TStringGrid;
    c: TCheckBox;
    i: integer;
    v: boolean;

begin
    g := Sender as TStringGrid;
    c := CheckBox1;

    if (g.Row > 0) AND (ord(Key) in [32, 27]) then
    begin
        v := FProducts[g.Selection.Top - 1].FChecked;

        for i := g.Selection.Top to g.Selection.Bottom do
        begin
            FProducts[i - 1].FChecked := not v;
            DataModule1.UpdateProductChecked(i - 1, FProducts[i - 1].FChecked);

        end;
        StringGrid_Redraw(g);
    end;

    if ord(Key) = 1 then
    begin
        v := DataModule1.InvertProductsChecked;
        for i := 0 to length(FProducts) - 1 do
            FProducts[i].FChecked := v;
        StringGrid_Redraw(g);
    end;

end;

procedure TForm1.StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
    r: TRect;
begin
    if not ComboBox1.Visible then
        exit;

    with Sender as TStringGrid do
    begin
        r := CellRect(Col, Row);
        r.Left := r.Left + Left;
        r.Right := r.Right + Left;
    end;
    ComboBox1.Width := r.Width;
    ComboBox1.Left := r.Left;
    ComboBox1.Height := r.Height;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol, ARow: integer;
  var CanSelect: boolean);
var
    r: TRect;
    grd: TStringGrid;

begin
    grd := Sender as TStringGrid;

    case ACol of
        0:
            begin
                r := grd.CellRect(ACol, ARow);
                r.Left := r.Left + StringGrid1.Left + 10;
                r.Right := r.Right + StringGrid1.Left;
                r.Top := r.Top + StringGrid1.Top + 7;
                r.Bottom := r.Bottom + StringGrid1.Top;

                with CheckBox1 do
                begin

                    OnClick := nil;
                    if GetAsyncKeyState(VK_LBUTTON) < 0 then
                    begin
                        FProducts[ARow - 1].FChecked :=
                          not FProducts[ARow - 1].FChecked;
                        DataModule1.UpdateProductChecked(ARow - 1,
                          FProducts[ARow - 1].FChecked);
                        StringGrid_Redraw(grd);
                    end;
                    Checked := FProducts[ARow - 1].FChecked;
                    OnClick := CheckBox1Click;

                    Left := r.Left - 6;
                    Top := r.Top - 3;
                    Visible := true;
                end;
                ComboBox1.Visible := false;
            end;
        1:
            begin
                r := grd.CellRect(ACol, ARow);
                r.Left := r.Left + grd.Left;
                r.Right := r.Right + grd.Left;
                r.Top := r.Top + grd.Top;
                r.Bottom := r.Bottom + grd.Top;

                with ComboBox1 do
                begin
                    ItemIndex := Items.IndexOf(grd.Cells[ACol, ARow]);
                    if (ItemIndex = -1) then
                    begin
                        Items.Add(grd.Cells[ACol, ARow]);
                        ItemIndex := Items.IndexOf(grd.Cells[ACol, ARow]);
                    end;

                    Width := r.Width;
                    Left := r.Left;
                    Top := r.Top;
                    Visible := true;

                end;
                CheckBox1.Visible := false;
            end;
    else
        begin
            ComboBox1.Visible := false;
            CheckBox1.Visible := false;

        end;
    end;

end;

procedure TForm1.StringGrid1TopLeftChanged(Sender: TObject);
begin
    ComboBox1.Visible := false;
end;

procedure TForm1.TabSheet5Show(Sender: TObject);
begin
    FSettings.Validate;
end;

procedure TForm1.TFrameRun1ToolButtonRunMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
    with ToolButtonRun do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(X, Y);
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
    FormNewPartyDialog.Show;
    FormNewPartyDialog.OnAccept := SetCurrentParty;
end;

procedure TForm1.ToolButton2Click(Sender: TObject);
begin
    FormManualControl.FPipe := FPipe;
    FormManualControl.Show;
end;

procedure TForm1.ToolButtonStopClick(Sender: TObject);
begin
    FPipe.WriteMsgJSON('CURRENT_WORK_STOP', nil);
    ToolButtonStop.Visible := false;

end;

end.
