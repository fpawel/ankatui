unit Unit1;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.Buttons,
    Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
    System.Generics.Collections,
    System.ImageList, UnitData, Vcl.ImgList, Vcl.Menus, VirtualTrees,
    msglevel,

    inifiles, System.SyncObjs, models;

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
        Panel4: TPanel;
        ImageList1: TImageList;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
        ImageList2: TImageList;
        N3: TMenuItem;
        N4: TMenuItem;
        N5: TMenuItem;
        RichEdit1: TRichEdit;
        PanelConsole: TPanel;
        PanelConsoleHeader: TPanel;
        ToolBar4: TToolBar;
        ImageList3: TImageList;
        ToolButtonConsoleHide: TToolButton;
        ToolButtonMoveConsoleDown: TToolButton;
        PanelConsolePlaceholderBottom: TPanel;
        SplitterConsoleHoriz: TSplitter;
        PanelLastMessage: TPanel;
        N6: TMenuItem;
        N7: TMenuItem;
        N8: TMenuItem;
        Panel8: TPanel;
        Panel14: TPanel;
        Panel6: TPanel;
        ImageList4: TImageList;
        PageControlMain: TPageControl;
        TabSheetArchive: TTabSheet;
        TabSheetLogs: TTabSheet;
        TabSheetCharts: TTabSheet;
        TabSheetSettings: TTabSheet;
        TabSheetProducts: TTabSheet;
        PanelTopBar: TPanel;
        ToolBarParty: TToolBar;
        ToolButtonParty: TToolButton;
        ToolButtonStop: TToolButton;
        PanelPartyTopMessage: TPanel;
        TabSheetCoefs: TTabSheet;
        TabSheetVars: TTabSheet;
        TabSheetCurrentChart: TTabSheet;
        CheckBox1: TCheckBox;
        ComboBox1: TComboBox;
        StringGrid1: TStringGrid;
        PageControl1: TPageControl;
        TabSheetParties: TTabSheet;
    Splitter1: TSplitter;
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
        procedure ToolButtonMoveConsoleDownClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure PanelConsolePlaceholderBottomResize(Sender: TObject);
        procedure ToolButtonConsoleHideClick(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure N3Click(Sender: TObject);
        procedure N2Click(Sender: TObject);
        procedure ToolButtonPartyMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure N7Click(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
        procedure PageControlMainChange(Sender: TObject);
        procedure N4Click(Sender: TObject);
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
        procedure UpdatedControlsVisibilityOnStartedChanged(started: boolean);

        procedure addFormToControl(form: TForm; Control: TWinControl);
        procedure InitPipe;
    public

        FProducts: TArray<TProduct>;
        FReadProduct: integer;
        FIni: TIniFile;
        FFErrorLogMutex: TCriticalSection;
        { Public declarations }
        procedure SetCurrentParty;

        procedure SetupWorkStarted(work: string);
        procedure SetCoef(product_order, coef_order: integer);

    end;

var
    Form1: TForm1;

implementation

uses UnitFormManualControl, UnitFormPopup, DataRichEditOutput, pipe, dateutils,
    rest.json, Winapi.uxtheme,
    System.Math, UnitFormNewPartyDialog,
    stringgridutils, UnitFormCharts,
    listports, System.IOUtils,
    CurrentWorkTreeData, stringutils, vclutils,  UnitFormSettings,
    UnitFormCurrentWork, UnitFormDelay, System.Types, System.UITypes, findproc,
    PropertiesFormUnit, UnitHostAppData, UnitFormChartSeries, UnitFormReadVars,
    TlHelp32, UnitFormLog, UnitFormParties, UnitFromCoefs;

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

end;

procedure TForm1.FormActivate(Sender: TObject);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
    FileName: string;
begin
    OnActivate := nil;

    SetCurrentParty;
    InitPipe;

    FileName := TPath.Combine(ExtractFilePath(paramstr(0)), 'window.position');
    if FileExists(FileName) then
    begin
        fs := TFileStream.Create(FileName, fmOpenRead);
        fs.Read(wp, SizeOf(wp));
        fs.Free;
        SetWindowPlacement(Handle, wp);
    end;

    PanelConsolePlaceholderBottom.Height := FIni.ReadInteger('positions',
      'console_bottom_height', 300);
    if FIni.ReadString('positions', 'console', 'down') = 'hiden' then
        ToolButtonConsoleHide.Click;
    RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);

    addFormToControl(FormReadVars, TabSheetVars);
    addFormToControl(FormCoefs, TabSheetCoefs);
    addFormToControl(FormSettings, TabSheetSettings);
    addFormToControl(FormCharts, TabSheetCharts);
    addFormToControl(FormCoefs, TabSheetCoefs);
    addFormToControl(FormChartSeries, TabSheetCurrentChart);

    addFormToControl(FormLog, TabSheetLogs);
    addFormToControl(FormParties, TabSheetParties);

    with FormDelay do
    begin
        Parent := PanelPartyTopMessage;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := false;
    end;

    addFormToControl(FormCurrentWork, TabSheetProducts);

    TabSheetCurrentChart.TabVisible := false;
    TabSheetVars.TabVisible := false;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    wp: WINDOWPLACEMENT;
    fs: TFileStream;
begin
    HostAppData.Pipe.Close;

    fs := TFileStream.Create(TPath.Combine(ExtractFilePath(paramstr(0)),
      'window.position'), fmOpenWrite or fmCreate);
    if not GetWindowPlacement(Handle, wp) then
        raise Exception.Create('GetWindowPlacement: false');
    fs.Write(wp, SizeOf(wp));
    fs.Free;
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

procedure TForm1.InitPipe;
begin
    HostAppData.Pipe.Handle('READ_COEFFICIENT', HandleReadCoefficient);
    HostAppData.Pipe.Handle('READ_VAR', HandleReadVar);

    HostAppData.Pipe.Handle('CURRENT_WORK_MESSAGE', HandleCurentWorkMessage);

    HostAppData.Pipe.Handle('PRODUCT_CONNECTED', HandleProductConnected);

    HostAppData.Pipe.Handle('READ_PRODUCT', HandleReadProduct);

    HostAppData.Pipe.Handle('END_WORK', HandleEndWork);

    HostAppData.Pipe.Handle('PROMPT_ERROR_STOP_WORK',
      HandlePromptErrorStopWork);

    HostAppData.Pipe.Handle('DELAY',
        function(content: string): string
        var
            i: TDelayInfo;
        begin
            Result := '';
            i := TJson.JsonToObject<TDelayInfo>(content);
            if i.FEnabled and not FormDelay.Visible then
            begin
                FormChartSeries.NewChart;
                TabSheetCurrentChart.TabVisible := True;
            end;
            FormDelay.SetupDelay(i);
            i.Free;

        end);

    FormCurrentWork.InitPipe(PanelPartyTopMessage);

    PrintLastMessages(RichEdit1, 500);

    if not HostAppData.Pipe.Connect('ANKAT') then
    begin
        PanelPartyTopMessage.Caption := '   Нет хост-процеса';
        PanelPartyTopMessage.Font.Color := clRed;
        FormCurrentWork.Visible := false;
    end
    else
    begin
        HostAppData.Connect;
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
    HostAppData.Pipe.WriteMsgJSON('SET_COEFFICIENT', X);
    SetupWorkStarted('Установка коэффициента ' +
      inttostr(DataModule1.DeviceCoefs[coef_order].FVar) + ' прибора ' +
      inttostr(DataModule1.CurrentPartyProducts[product_order].FSerial));
    X.Free;

end;

procedure TForm1.N1Click(Sender: TObject);
begin
    HostAppData.Pipe.WriteMsgJSON('READ_VARS', nil);
    SetupWorkStarted('Опрос');
    TabSheetCurrentChart.TabVisible := True;
    TabSheetVars.TabVisible := true;
    FormChartSeries.NewChart;
end;

procedure TForm1.N2Click(Sender: TObject);
begin
    HostAppData.Pipe.WriteMsgJSON('READ_COEFFICIENTS', nil);
    SetupWorkStarted('Считывание коэффициентов');
end;

procedure TForm1.N3Click(Sender: TObject);
begin
    HostAppData.Pipe.WriteMsgJSON('WRITE_COEFFICIENTS', nil);
    SetupWorkStarted('Запись коэффициентов');
end;

procedure TForm1.N4Click(Sender: TObject);
begin
    SetupWorkStarted(FormCurrentWork.WorkName);
    FormCurrentWork.run;
end;

procedure TForm1.N7Click(Sender: TObject);
begin

    FormNewPartyDialog.FAcceptHandler := procedure
        begin
            SetCurrentParty;
            FormNewPartyDialog.ModalResult := mrOk;
        end;
    FormNewPartyDialog.ShowModal;
end;

function TForm1.HandleEndWork(content: string): string;
var
    X: TEndWorkInfo;
begin
    Result := '';
    FormReadVars.reset;
    FormCoefs.reset;
    X := TJson.JsonToObject<TEndWorkInfo>(content);
    if X.FError <> '' then
    begin
        PanelPartyTopMessage.Caption := X.FName + ': ' + X.FError;
        PanelPartyTopMessage.Font.Color := clRed;
    end
    else
    begin
        PanelPartyTopMessage.Caption := X.FName + ': выполнено';
        PanelPartyTopMessage.Font.Color := clNavy;

    end;

    X.Free;

    UpdatedControlsVisibilityOnStartedChanged(false);

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

    PanelLastMessage.Caption := Format('[%d] %s: %s',
      [m.FWorkIndex, m.FWork, m.FText]);
    if m.FLevel >= LError then
    begin
        PanelLastMessage.Font.Color := clRed;
        FormCurrentWork.SetRunError;
    end
    else
        PanelLastMessage.Font.Color := clNavy;

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
    v := DataModule1.DeviceVars[X.FVarOrder];
    PanelLastMessage.Font.Color := clNavy;
    PanelLastMessage.Caption :=
      Format('Считывание: АНКАТ %d: регистр %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        PanelLastMessage.Caption := PanelLastMessage.Caption +
          FloatToStr(X.FValue)
    else
    begin
        PanelLastMessage.Caption := PanelLastMessage.Caption + X.FError;
        PanelLastMessage.Font.Color := clRed;
    end;

    FormReadVars.HandleReadVar(X);
    if X.FError = '' then
        FormChartSeries.AddValue(X.FProductSerial, X.FVar, X.FValue, now);
    X.Free;
end;

function TForm1.HandleReadCoefficient(content: string): string;
var
    X: TReadCoef;
    p: TProduct;
    v: TDeviceVar;
begin
    Result := '';
    X := TJson.JsonToObject<TReadCoef>(content);
    p := FProducts[X.FProductOrder];
    v := FormCoefs.FCoefs[X.FCoefficientOrder];
    PanelLastMessage.Font.Color := clNavy;
    PanelLastMessage.Caption :=
      Format('Считывание: АНКАТ %d: коэффициент %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        PanelLastMessage.Caption := PanelLastMessage.Caption +
          FloatToStr(X.FValue)
    else
    begin
        PanelLastMessage.Caption := PanelLastMessage.Caption + X.FError;
        PanelLastMessage.Font.Color := clRed;
    end;

    FormCoefs.HandleReadCoef(X);
    X.Free;
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
    FormReadVars.SetCurrentParty(FProducts);
    FormCoefs.SetCurrentParty(FProducts);
end;

procedure TForm1.PageControlMainChange(Sender: TObject);
begin
    (Sender as TPageControl).Repaint;
    if PageControlMain.ActivePage = TabSheetSettings then
        FormSettings.SetConfig;
end;

procedure TForm1.PageControlMainDrawTab(Control: TCustomTabControl;
TabIndex: integer; const Rect: TRect; Active: boolean);
begin
    PageControl_DrawVerticalTab(Control, TabIndex, Rect, Active);
end;

procedure TForm1.PanelConsolePlaceholderBottomResize(Sender: TObject);
begin
    FIni.WriteInteger('positions', 'console_bottom_height',
      PanelConsolePlaceholderBottom.Height);
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

    StringGrid_DrawCellBounds(cnv, ACol, ARow, Rect);
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
                    Visible := True;
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
                    Visible := True;

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

procedure TForm1.ToolButtonConsoleHideClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := True;
    ToolButtonConsoleHide.Visible := false;
    SplitterConsoleHoriz.Visible := false;
    PanelConsolePlaceholderBottom.Visible := True;
    PanelConsole.Parent := PanelConsolePlaceholderBottom;
    PanelConsole.Height := PanelLastMessage.Height;
    PanelConsolePlaceholderBottom.OnResize := nil;
    PanelConsolePlaceholderBottom.Height := PanelLastMessage.Height;
    PanelConsolePlaceholderBottom.OnResize :=
      PanelConsolePlaceholderBottomResize;
    FIni.WriteString('positions', 'console', 'hiden');
end;

procedure TForm1.ToolButtonMoveConsoleDownClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := false;
    ToolButtonConsoleHide.Visible := True;
    SplitterConsoleHoriz.Visible := True;
    PanelConsolePlaceholderBottom.Visible := True;

    PanelConsole.Parent := PanelConsolePlaceholderBottom;
    SplitterConsoleHoriz.Top := 0;
    PanelConsolePlaceholderBottom.Top := 100500;
    FIni.WriteString('positions', 'console', 'down');
    PanelConsolePlaceholderBottom.Height := FIni.ReadInteger('positions',
      'console_bottom_height', 300);
    RichEdit1.SetFocus;
    SendMessage(RichEdit1.Handle, EM_SCROLL, SB_LINEDOWN, 0);
end;

procedure TForm1.ToolButtonPartyMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
    with ToolButtonParty do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(X, Y);
end;

procedure TForm1.ToolButtonStopClick(Sender: TObject);
begin
    HostAppData.Pipe.WriteMsgJSON('CURRENT_WORK_STOP', nil);
    ToolButtonStop.Visible := false;

end;

procedure TForm1.SetupWorkStarted(work: string);
begin
    //FormCurrentWork.SetupWorksTree;
    PanelPartyTopMessage.Caption := work;
    PanelPartyTopMessage.Font.Color := clNavy;
    UpdatedControlsVisibilityOnStartedChanged(True);

end;

procedure TForm1.UpdatedControlsVisibilityOnStartedChanged(started: boolean);
begin
    FormManualControl.Enabled := not started;
    ModifyControl(FormManualControl,
        procedure(const AControl: TControl)
        begin
            AControl.Enabled := not started;
        end);
    FormManualControl.Enabled := not started;
    ToolButtonParty.Visible := not started;
    ToolButtonStop.Visible := started;
    if not started then
    begin
        TabSheetVars.TabVisible := false;
    end;
    FormCurrentWork.AllowCheck := not started;
end;

procedure TForm1.addFormToControl(form: TForm; Control: TWinControl);
begin
    with form do
    begin
        Parent := Control;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := True;
        Font.Assign(self.Font);
    end;
end;

end.
