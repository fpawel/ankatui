unit Unit1;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.Buttons,
    Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
    System.Generics.Collections,
    System.ImageList, UnitData, Vcl.ImgList, Vcl.Menus, VirtualTrees,
    msglevel, UnitFormLog, UnitFormPopup, UnitFrameCoef,
    UnitFrameVar, UnitFormParties,
    UnitFormManualControl, inifiles, System.SyncObjs, models;

type

    TWorkMsg = class
        FLevel: integer;
        FText: string;
        FWorkIndex: integer;
        FWork: string;
        FProductSerial: integer;
        FCreatedAt: TDAteTime;
    end;

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
        ImageList1: TImageList;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        ToolBar2: TToolBar;
        ToolButtonRun: TToolButton;
        ToolButtonStop: TToolButton;
        Splitter1: TSplitter;
        Panel5: TPanel;
        ImageList2: TImageList;
        PageControl2: TPageControl;
        TabSheet3: TTabSheet;
        TabSheet4: TTabSheet;
        N3: TMenuItem;
        TabSheet7: TTabSheet;
        TabSheet8: TTabSheet;
        N4: TMenuItem;
        N5: TMenuItem;
        ToolButton3: TToolButton;
        Panel6: TPanel;
        RichEdit1: TRichEdit;
        PanelConsole: TPanel;
        Panel10: TPanel;
        ToolBar4: TToolBar;
        ImageList3: TImageList;
        ToolButtonConsoleHide: TToolButton;
        ToolButtonMoveConsoleDown: TToolButton;
        ToolButtonMoveConsoleUp: TToolButton;
        PanelConsolePlaceholderRight: TPanel;
        PanelConsolePlaceholderBottom: TPanel;
        SplitterConsoleHoriz: TSplitter;
        SplitterConsoleVert: TSplitter;
        Panel2: TPanel;
        Panel9: TPanel;
        Panel11: TPanel;
        Panel12: TPanel;
        Panel13: TPanel;
        ToolBar5: TToolBar;
        ToolButton7: TToolButton;
        N6: TMenuItem;
        N7: TMenuItem;
        N8: TMenuItem;
        Panel8: TPanel;
        Panel14: TPanel;
        procedure FormCreate(Sender: TObject);
        procedure ComboBox1CloseUp(Sender: TObject);
        procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: integer;
          var CanSelect: boolean);
        procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: integer;
          Rect: TRect; State: TGridDrawState);
        procedure CheckBox1Click(Sender: TObject);
        procedure StringGrid1MouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure ComboBox1DropDown(Sender: TObject);
        procedure StringGrid1TopLeftChanged(Sender: TObject);
        procedure ComboBox1Exit(Sender: TObject);
        procedure StringGrid1KeyPress(Sender: TObject; var Key: Char);
        procedure ToolButtonStopClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var Action: TCloseAction);
        procedure ToolButton3Click(Sender: TObject);
        procedure ToolButtonMoveConsoleUpClick(Sender: TObject);
        procedure ToolButtonMoveConsoleDownClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure PanelConsolePlaceholderBottomResize(Sender: TObject);
        procedure PanelConsolePlaceholderRightResize(Sender: TObject);
        procedure ToolButtonConsoleHideClick(Sender: TObject);
        procedure ToolButton7Click(Sender: TObject);
        procedure N4Click(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure N3Click(Sender: TObject);
        procedure N2Click(Sender: TObject);
        procedure ToolButtonRunMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure N7Click(Sender: TObject);
        procedure N8Click(Sender: TObject);
    private
        { Private declarations }

        function HandleReadVar(content: string): string;
        function HandleReadCoefficient(content: string): string;
        function HandleCurentWorkMessage(content: string): string;
        function HandleProductConnected(content: string): string;
        function HandleReadProduct(content: string): string;
        function HandleEndWork(content: string): string;
        function HandlePromptErrorStopWork(content: string): string;

        procedure OnException(Sender: TObject; E: Exception);

    public

        FProducts: TArray<TProduct>;
        FFrameCoef: TFrameCoef;
        FFrameVar: TFrameVar;
        FReadProduct: integer;
        FFormLog: TFormLog;
        FFormParties: TFormParties;
        FIni: TIniFile;
        FFErrorLogMutex: TCriticalSection;
        { Public declarations }
        procedure SetCurrentParty;
        procedure Init2;
        procedure SetupWorkStarted(work: string; started: boolean);
        procedure SetCoef(product_order, coef_order: integer);
    end;

var
    Form1: TForm1;

implementation

uses DataRichEditOutput, pipe, dateutils, rest.json, Winapi.uxtheme,
    System.Math, UnitFormNewPartyDialog,
    stringgridutils,
    listports, System.IOUtils,
    CurrentWorkTreeData, stringutils, vclutils, UnitFormChart, UnitFormSettings,
    UnitFormCurrentWork, UnitFormDelay, System.Types, System.UITypes, findproc,
    PropertiesFormUnit, UnitHostAppData, UnitFormCurrentChart;

{$R *.dfm}

type
    TProductCoefficient = class
    public
        FProduct, FCoefficient: integer;

    end;

procedure TForm1.FormCreate(Sender: TObject);
begin

    FFErrorLogMutex := TCriticalSection.Create;

    Application.OnException := OnException;

    FFrameCoef := TFrameCoef.Create(self);
    FFrameCoef.Parent := Panel6;
    FFrameCoef.Align := alclient;

    FFrameVar := TFrameVar.Create(self);
    FFrameVar.Align := alclient;

    FIni := TIniFile.Create(ExtractFileDir(paramstr(0)) + '\main.ini');
    FReadProduct := -1;

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

    FFormParties := TFormParties.Create(self);
    with TFormParties.Create(self) do
    begin
        Splitter1.Parent := TabSheet4;
        Panel1.Parent := TabSheet4;
        VirtualStringTree1.Parent := TabSheet4;
    end;

    with TFormChart.Create(self) do
    begin
        Splitter1.Parent := TabSheet8;
        VirtualStringTree1.Parent := TabSheet8;
        Chart1.Parent := TabSheet8;
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

end;

procedure TForm1.OnException(Sender: TObject; E: Exception);
var
    FErrorLog, FStackTrace: TextFile;
    ErrorLogFileName: string;
    StackTraceFileName: string;
    StackTraceFileDir: string;
    StackTraceFileName1: string;
    time_now: TDAteTime;
begin
    FFErrorLogMutex.Acquire;
    time_now := now;
    StackTraceFileDir := ExtractFileDir(paramstr(0)) + '\stack_trace';

    if not ForceDirectories(StackTraceFileDir) then
    begin
        Application.MessageBox('Не удалось создать каталог stack_trace',
          'Анкат: критический сбой', MB_OK or MB_ICONERROR);
        Close;
        Exit;
    end;

    StackTraceFileName1 := formatDateTime('dd_mm_yyyy_hh_nn_ss_zzz', time_now) +
      '.stacktrace';
    StackTraceFileName := StackTraceFileDir + '\' + StackTraceFileName1;
    ErrorLogFileName := ExtractFileDir(paramstr(0)) + '\errors.log';

    AssignFile(FErrorLog, ErrorLogFileName, CP_UTF8);
    if FileExists(ErrorLogFileName) then
        Append(FErrorLog)
    else
        Rewrite(FErrorLog);
    Writeln(FErrorLog, formatDateTime('dd.mm.yyyy.hh:nn:ss.zzz', time_now),
      'MSK ', E.ClassName, ' ', stringreplace(Trim(E.Message), #13, ' ',
      [rfReplaceAll, rfIgnoreCase]));
    Flush(FErrorLog);
    CloseFile(FErrorLog);

    AssignFile(FStackTrace, StackTraceFileName, CP_UTF8);
    Rewrite(FStackTrace);
    Writeln(FStackTrace, E.ClassName);
    Writeln(FStackTrace, E.Message);
    Writeln(FStackTrace, E.StackTrace);
    Flush(FStackTrace);
    CloseFile(FStackTrace);

    FFErrorLogMutex.Release;

    Application.MessageBox(PChar(E.Message), 'Анкат: произошла ошибка',
      MB_OK or MB_ICONERROR);

    if E is EPipeHostError then
        Close;

end;

procedure TForm1.Init2;
begin


    // FCurrentWork := TCurrentWork.Create(FPipe, Panel5, VirtualStringTree1);
    HostAppData.FPipe.Handle('READ_COEFFICIENT', HandleReadCoefficient);
    HostAppData.FPipe.Handle('READ_VAR', HandleReadVar);

    HostAppData.FPipe.Handle('CURRENT_WORK_MESSAGE', HandleCurentWorkMessage);

    HostAppData.FPipe.Handle('PRODUCT_CONNECTED', HandleProductConnected);

    HostAppData.FPipe.Handle('READ_PRODUCT', HandleReadProduct);

    HostAppData.FPipe.Handle('END_WORK', HandleEndWork);

    HostAppData.FPipe.Handle('PROMPT_ERROR_STOP_WORK',
      HandlePromptErrorStopWork);

    FormCurrentWork.Init2;
    FormDelay.Init2;
    PrintLastMessages(RichEdit1, 500);

    if not HostAppData.FPipe.Connect('ANKAT') then
    begin
        Panel5.Caption := 'Нет хост-процеса';
        Panel5.Font.Color := clRed;
    end
    else
    begin
        HostAppData.Init;
        FormManualControl.Init;
    end;
end;

procedure TForm1.SetCoef(product_order, coef_order: integer);
var
    X: TProductCoefficient;
begin
    X := TProductCoefficient.Create;
    X.FProduct := product_order;
    X.FCoefficient := coef_order;
    HostAppData.FPipe.WriteMsgJSON('SET_COEFFICIENT', X);
    SetupWorkStarted('Установка коэффициента ' +
      inttostr(DataModule1.DeviceCoefs[coef_order].FVar) + ' прибора ' +
      inttostr(DataModule1.CurrentPartyProducts[product_order].FSerial), true);
    X.Free;

end;

procedure TForm1.N1Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('READ_VARS', nil);
    SetupWorkStarted('Опрос', true);
    Panel6.Controls[0].Parent := nil;
    FFrameVar.StringGrid2.Parent := Form1.Panel6;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('READ_COEFFICIENTS', nil);
    SetupWorkStarted('Считывание коэффициентов', true);
end;

procedure TForm1.N3Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('WRITE_COEFFICIENTS', nil);
    SetupWorkStarted('Запись коэффициентов', true);
end;

procedure TForm1.N4Click(Sender: TObject);
begin
    FormCurrentWork.Setup;
    FormCurrentWork.Show;
end;

procedure TForm1.N7Click(Sender: TObject);
begin
    FormNewPartyDialog.ShowModal;
    if FormNewPartyDialog.ModalResult = mrOk then
        SetCurrentParty;
end;

procedure TForm1.N8Click(Sender: TObject);
begin
    FormManualControl.Show;
end;

function TForm1.HandleEndWork(content: string): string;
var
    X: TEndWorkInfo;
begin
    Result := '';
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

function TForm1.HandlePromptErrorStopWork(content: string): string;
var
    s: string;
begin
    s := content + #10#13#10#13;
    s := s + 'Нажмите OK чтобы игнорировать ошибку и продолжить автоматическую настройку.'#10#13;
    s := s + 'Нажмите ОТМЕНА чтобы прервать автоматическую настройку.';
    if MessageDlg(s, mtWarning, mbOKCancel, 0) = IDOK then
        Result := 'IGNORE'
    else
        Result := 'ABORT';

end;

function TForm1.HandleProductConnected(content: string): string;
var
    X: TProductConnected;
begin
    Result := '';
    X := TJson.JsonToObject<TProductConnected>(content);
    FProducts[X.FProduct].FConnectionError := not X.FOk;
    FProducts[X.FProduct].FConnection := X.FText;
    StringGrid1.Cells[3, X.FProduct + 1] := X.FText;
    StringGrid_RedrawRow(StringGrid1, X.FProduct + 1);
    X.Free;
end;

function TForm1.HandleCurentWorkMessage(content: string): string;
var
    m: TWorkMsg;
begin
    Result := '';
    m := TJson.JsonToObject<TWorkMsg>(content);

    Panel2.Caption := Format('[%d] %s: %s', [m.FWorkIndex, m.FWork, m.FText]);
    if m.FLevel >= LError then
    begin
        Panel2.Font.Color := clRed;
        FormCurrentWork.SetRunError;
    end
    else
        Panel2.Font.Color := clNavy;

    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
    RichEdit1.SelStart := Length(RichEdit1.Text);
    DataRichEditOutput.PrintWorkMessages(RichEdit1, m.FWorkIndex, m.FWork,
      m.FProductSerial, m.FCreatedAt, m.FLevel, m.FText);
    // FormConsole.RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
    m.Free;
end;

function TForm1.HandleReadProduct(content: string): string;
var
    X: TReadProduct;
    PrevProduct: integer;
begin
    Result := '';
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

function TForm1.HandleReadVar(content: string): string;
var
    X: TReadVar;
    p: TProduct;
    v: TDeviceVar;
begin
    Result := '';
    X := TJson.JsonToObject<TReadVar>(content);
    p := FProducts[X.FProductOrder];
    v := FFrameVar.FVars[X.FVarOrder];
    Panel2.Font.Color := clNavy;
    Panel2.Caption := Format('Считывание: АНКАТ %d: регистр %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        Panel2.Caption := Panel2.Caption + FloatToStr(X.FValue)
    else
    begin
        Panel2.Caption := Panel2.Caption + X.FError;
        Panel2.Font.Color := clRed;
    end;

    FFrameVar.HandleReadVar(X);
    FormCurrentChart.HandleReadVar(x);
    X.Free;
end;

function TForm1.HandleReadCoefficient(content: string): string;
var
    X: TReadVar;
    p: TProduct;
    v: TDeviceVar;
begin
    Result := '';
    X := TJson.JsonToObject<TReadVar>(content);
    p := FProducts[X.FProductOrder];
    v := FFrameCoef.FCoefs[X.FVarOrder];
    Panel2.Font.Color := clNavy;
    Panel2.Caption := Format('Считывание: АНКАТ %d: коэффициент %d: %s: %s: ',
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



    if started then
    begin
        FormCurrentWork.Hide;
        FormCurrentWork.Setup;
        if ToolBar5.Visible then
            ToolButton7.Click;
    end
    else
    begin
        if (Panel6.ControlCount > 0) AND
          (Panel6.Controls[0] = FormCurrentWork.VirtualStringTree1) then
        begin
            ToolBar5.Visible := true;
        end
        else
        begin
            if (Panel6.ControlCount > 0) AND
              (Panel6.Controls[0] <> FFrameCoef.StringGrid3) then
                Panel6.Controls[0].Parent := nil;

            if FFrameCoef.StringGrid3.Parent <> Panel6 then
                FFrameCoef.StringGrid3.Parent := Panel6;
            Panel13.Caption := '   Коэффициенты';
        end;

    end;

    Panel5.Caption := work;
    Panel13.Caption := '   ' + work;

    Panel5.Font.Color := clNavy;

    FormManualControl.Button1.Enabled := not started;
    FormManualControl.Button6.Enabled := not started;
    FormManualControl.RadioGroup1.Enabled := not started;
    FormManualControl.GroupBox2.Enabled := not started;
    ToolButtonRun.Visible := not started;
    ToolButtonStop.Visible := started;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
    FileName: string;
begin
    OnActivate := nil;

    SetCurrentParty;
    Init2;

    FileName := TPath.Combine(ExtractFilePath(paramstr(0)), 'window.position');
    if FileExists(FileName) then
    begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        fs.Read(wp, sizeof(wp));
        fs.Free;
        SetWindowPlacement(Handle, wp);
    end;

    PanelConsolePlaceholderRight.Width := FIni.ReadInteger('positions',
      'console_right_width', 300);
    PanelConsolePlaceholderBottom.Height := FIni.ReadInteger('positions',
      'console_bottom_height', 300);
    if FIni.ReadString('positions', 'console', 'down') = 'right' then
        ToolButtonMoveConsoleUp.Click
    else if FIni.ReadString('positions', 'console', 'down') = 'hiden' then
        ToolButtonConsoleHide.Click;
    RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin
    HostAppData.FPipe.Close;

    fs := TFileStream.Create(TPath.Combine(ExtractFilePath(paramstr(0)),
      'window.position'), fmOpenWrite or fmCreate);
    if not GetWindowPlacement(Handle, wp) then
        raise Exception.Create('GetWindowPlacement: false');
    fs.Write(wp, sizeof(wp));
    fs.Free;
end;

procedure TForm1.SetCurrentParty;
var
    i: integer;
    CurrentPartyID: int64;

begin
    for i := 0 to Length(FProducts) - 1 do
        FProducts[i].Free;

    ComboBox1DropDown(ComboBox1);
    FProducts := DataModule1.CurrentPartyProducts;

    CurrentPartyID := DataModule1.CurrentPartyID;
    // TabSheet3.Caption := Format('Партия № %d %s',
    // [CurrentPartyID,
    // DAtetimetostr(IncHour(DataModule1.CurrentPartyDateTime, 3))]);

    with StringGrid1 do
    begin
        RowCount := Length(FProducts) + 1;
        FixedRows := 1;
        for i := 0 to Length(FProducts) - 1 do
        begin
            Cells[0, i + 1] := inttostr(i + 1);
            Cells[1, i + 1] := FProducts[i].FComport;
            Cells[2, i + 1] := inttostr(FProducts[i].FSerial);

        end;
    end;
    FFrameVar.SetCurrentParty(FProducts);
    FFrameCoef.SetCurrentParty(FProducts);
end;

procedure TForm1.PanelConsolePlaceholderBottomResize(Sender: TObject);
begin
    FIni.WriteInteger('positions', 'console_bottom_height',
      PanelConsolePlaceholderBottom.Height);
end;

procedure TForm1.PanelConsolePlaceholderRightResize(Sender: TObject);
begin
    FIni.WriteInteger('positions', 'console_right_width',
      PanelConsolePlaceholderRight.Width);
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

    Checked := false;
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
    i: integer;
    v: boolean;

begin
    g := Sender as TStringGrid;
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
        for i := 0 to Length(FProducts) - 1 do
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
        Exit;

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

procedure TForm1.ToolButton3Click(Sender: TObject);
begin
    FormSettings.Position := poScreenCenter;
    FormSettings.Show;
    ShowWindow(FormSettings.Handle, SW_RESTORE);
    // PropertiesForm.Show;
end;

procedure TForm1.ToolButton7Click(Sender: TObject);
begin
    ToolBar5.Visible := false;
    if Panel6.ControlCount > 0 then
        Panel6.Controls[0].Parent := nil;
    FFrameCoef.StringGrid3.Parent := Panel6;
    Panel13.Caption := '   Коэффициенты';

end;

procedure TForm1.ToolButtonConsoleHideClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := true;
    ToolButtonMoveConsoleUp.Visible := true;
    ToolButtonConsoleHide.Visible := false;
    SplitterConsoleHoriz.Visible := false;
    SplitterConsoleVert.Visible := false;
    PanelConsolePlaceholderRight.Visible := false;
    PanelConsolePlaceholderBottom.Visible := true;
    PanelConsole.Parent := PanelConsolePlaceholderBottom;
    PanelConsole.Height := Panel2.Height;
    PanelConsolePlaceholderBottom.OnResize := nil;
    PanelConsolePlaceholderBottom.Height := Panel2.Height;
    PanelConsolePlaceholderBottom.OnResize :=
      PanelConsolePlaceholderBottomResize;
    FIni.WriteString('positions', 'console', 'hiden');
end;

procedure TForm1.ToolButtonMoveConsoleDownClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := false;
    ToolButtonMoveConsoleUp.Visible := true;
    ToolButtonConsoleHide.Visible := true;
    SplitterConsoleHoriz.Visible := true;
    SplitterConsoleVert.Visible := false;
    PanelConsolePlaceholderRight.Visible := false;
    PanelConsolePlaceholderBottom.Visible := true;

    PanelConsole.Parent := PanelConsolePlaceholderBottom;
    SplitterConsoleHoriz.Top := 0;
    PanelConsolePlaceholderBottom.Top := 100500;
    FIni.WriteString('positions', 'console', 'down');
    PanelConsolePlaceholderBottom.Height := FIni.ReadInteger('positions',
      'console_bottom_height', 300);
    RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
end;

procedure TForm1.ToolButtonMoveConsoleUpClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := true;
    ToolButtonMoveConsoleUp.Visible := false;
    ToolButtonConsoleHide.Visible := true;
    SplitterConsoleHoriz.Visible := false;
    SplitterConsoleVert.Visible := true;
    PanelConsolePlaceholderRight.Visible := true;
    PanelConsolePlaceholderBottom.Visible := false;

    PanelConsole.Parent := PanelConsolePlaceholderRight;
    SplitterConsoleVert.Left := 0;
    PanelConsolePlaceholderRight.Left := 100500;
    self.Realign;

    FIni.WriteString('positions', 'console', 'right');
    PanelConsolePlaceholderRight.Width := FIni.ReadInteger('positions',
      'console_right_width', 300);
    RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);

end;

procedure TForm1.ToolButtonRunMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
    with ToolButtonRun do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(X, Y);
end;

procedure TForm1.ToolButtonStopClick(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('CURRENT_WORK_STOP', nil);
    ToolButtonStop.Visible := false;

end;

end.
