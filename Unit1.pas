unit Unit1;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.Buttons,
    Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,
    System.Generics.Collections,
    System.ImageList, UnitData, Vcl.ImgList, Vcl.Menus, VirtualTrees,
    msglevel, UnitFormPopup, UnitFrameCoef,

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
        ImageList1: TImageList;
        PopupMenu1: TPopupMenu;
        N1: TMenuItem;
        N2: TMenuItem;
    ToolBarParty: TToolBar;
    ToolButtonParty: TToolButton;
        ToolButtonStop: TToolButton;
        Splitter1: TSplitter;
        ImageList2: TImageList;
        N3: TMenuItem;
        N4: TMenuItem;
        N5: TMenuItem;
        PanelCurrentWorkContent: TPanel;
        RichEdit1: TRichEdit;
        PanelConsole: TPanel;
        PanelConsoleHeader: TPanel;
        ToolBar4: TToolBar;
        ImageList3: TImageList;
        ToolButtonConsoleHide: TToolButton;
        ToolButtonMoveConsoleDown: TToolButton;
        ToolButtonMoveConsoleUp: TToolButton;
        PanelConsolePlaceholderRight: TPanel;
        PanelConsolePlaceholderBottom: TPanel;
        SplitterConsoleHoriz: TSplitter;
        SplitterConsoleVert: TSplitter;
    PanelLastMessage: TPanel;
        Panel9: TPanel;
        Panel11: TPanel;
        Panel12: TPanel;
        PanelCurrentWorkTitle: TPanel;
        ToolBarCurrentWorkContent: TToolBar;
        ToolButtonCloseCurrentWork: TToolButton;
        N6: TMenuItem;
        N7: TMenuItem;
        N8: TMenuItem;
        Panel8: TPanel;
        Panel14: TPanel;
        PanelPlaceholderCurrentPartyMain: TPanel;
        Panel6: TPanel;
        ImageList4: TImageList;
        Panel3: TPanel;
    PageControlMain: TPageControl;
        TabSheetParty: TTabSheet;
        TabSheetParties: TTabSheet;
        TabSheetLogs: TTabSheet;
        TabSheetCharts: TTabSheet;
    TabSheetSettings: TTabSheet;
    Panel10: TPanel;
    Panel15: TPanel;
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
        procedure ToolButtonMoveConsoleUpClick(Sender: TObject);
        procedure ToolButtonMoveConsoleDownClick(Sender: TObject);
        procedure FormActivate(Sender: TObject);
        procedure PanelConsolePlaceholderBottomResize(Sender: TObject);
        procedure PanelConsolePlaceholderRightResize(Sender: TObject);
        procedure ToolButtonConsoleHideClick(Sender: TObject);
        procedure ToolButtonCloseCurrentWorkClick(Sender: TObject);
        procedure N4Click(Sender: TObject);
        procedure N1Click(Sender: TObject);
        procedure N3Click(Sender: TObject);
        procedure N2Click(Sender: TObject);
        procedure ToolButtonPartyMouseUp(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X, Y: integer);
        procedure N7Click(Sender: TObject);
        procedure N8Click(Sender: TObject);
        procedure PageControlMainDrawTab(Control: TCustomTabControl;
          TabIndex: integer; const Rect: TRect; Active: boolean);
    procedure PageControlMainChange(Sender: TObject);
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

        procedure UpdateCurrentWorkPanelCaption;

        procedure UpdatedControlsVisibilityOnStartedChanged(started: boolean);

        procedure SetCurrentWorkContent(widget: TControl;
          contetnt_title: string);

    public

        FProducts: TArray<TProduct>;
        FFrameCoef: TFrameCoef;
        FReadProduct: integer;
        FIni: TIniFile;
        FFErrorLogMutex: TCriticalSection;
        { Public declarations }
        procedure SetCurrentParty;
        procedure Init2;
        procedure SetupWorkStarted(widget: TControl; work: string);
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
    PropertiesFormUnit, UnitHostAppData, UnitFormCurrentChart, UnitFormReadVars,
    TlHelp32, UnitFormLog, UnitFormParties;

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
    FFrameCoef.Parent := PanelCurrentWorkContent;
    FFrameCoef.Align := alclient;

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

function processExists(exeFileName: string): boolean;
var
    ContinueLoop: BOOL;
    FSnapshotHandle: THandle;
    FProcessEntry32: TProcessEntry32;
begin
    FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
    FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
    ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
    Result := false;
    while integer(ContinueLoop) <> 0 do
    begin
        if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
          = UpperCase(exeFileName)) or (UpperCase(FProcessEntry32.szExeFile)
          = UpperCase(exeFileName))) then
        begin
            Result := True;
        end;
        ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
    end;
    CloseHandle(FSnapshotHandle);
end;

procedure TForm1.Init2;
begin
    HostAppData.FPipe.Handle('READ_COEFFICIENT', HandleReadCoefficient);
    HostAppData.FPipe.Handle('READ_VAR', HandleReadVar);

    HostAppData.FPipe.Handle('CURRENT_WORK_MESSAGE', HandleCurentWorkMessage);

    HostAppData.FPipe.Handle('PRODUCT_CONNECTED', HandleProductConnected);

    HostAppData.FPipe.Handle('READ_PRODUCT', HandleReadProduct);

    HostAppData.FPipe.Handle('END_WORK', HandleEndWork);

    HostAppData.FPipe.Handle('PROMPT_ERROR_STOP_WORK',
      HandlePromptErrorStopWork);

    HostAppData.FPipe.Handle('DELAY',
        function(content: string): string
        var
            i: TDelayInfo;
        begin
            Result := '';
            i := TJson.JsonToObject<TDelayInfo>(content);
            if i.FEnabled then
            begin
                if not FormDelay.Visible then
                begin
                    FormCurrentChart.NewChart;
                    FormCurrentChart.Setup(PanelCurrentWorkContent);
                end;
            end
            else
            begin
                FormCurrentChart.Hide;
            end;
            FormDelay.SetupDelay(i);
            i.Free;

        end);

    FormCurrentWork.Init2;

    PrintLastMessages(RichEdit1, 500);

    if not HostAppData.FPipe.Connect('ANKAT') then
    begin
        Panel15.Caption := '   Нет хост-процеса';
        Panel15.Font.Color := clRed;
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
    SetupWorkStarted(FFrameCoef, 'Установка коэффициента ' +
      inttostr(DataModule1.DeviceCoefs[coef_order].FVar) + ' прибора ' +
      inttostr(DataModule1.CurrentPartyProducts[product_order].FSerial));
    X.Free;

end;

procedure TForm1.N1Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('READ_VARS', nil);
    SetupWorkStarted(FormReadVars, 'Опрос');
    PanelCurrentWorkContent.Controls[0].Parent := nil;
    with FormReadVars do
    begin
        Parent := self;
        Align := alclient;
        BorderStyle := bsNone;
        Visible := True;
        Parent := self.PanelCurrentWorkContent;
        Font.Assign(self.Font);
    end;
    FormCurrentChart.NewChart;
    FormCurrentChart.Setup(FormReadVars);
end;

procedure TForm1.N2Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('READ_COEFFICIENTS', nil);
    SetupWorkStarted(FFrameCoef, 'Считывание коэффициентов');
end;

procedure TForm1.N3Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('WRITE_COEFFICIENTS', nil);
    SetupWorkStarted(FFrameCoef, 'Запись коэффициентов');
end;

procedure TForm1.N4Click(Sender: TObject);
begin
    FormCurrentWork.SetupDialogMode;
    FormCurrentWork.Show;
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

procedure TForm1.N8Click(Sender: TObject);
begin
    SetCurrentWorkContent(FormManualControl, 'Управление');
    ToolButtonCloseCurrentWork.Visible := True;
end;

function TForm1.HandleEndWork(content: string): string;
var
    X: TEndWorkInfo;
begin
    Result := '';
    FormReadVars.reset;
    FFrameCoef.reset;
    X := TJson.JsonToObject<TEndWorkInfo>(content);
    if X.FError <> '' then
    begin
        PanelLastMessage.Caption := X.FName + ': ' + X.FError;
        PanelLastMessage.Font.Color := clRed;
    end
    else
    begin
        PanelLastMessage.Caption := X.FName + ': выполнено';
        PanelLastMessage.Font.Color := clNavy;

    end;

    if (PanelCurrentWorkContent.ControlCount > 0) AND
      ((PanelCurrentWorkContent.Controls[0] = FormCurrentWork) OR
      (PanelCurrentWorkContent.Controls[0] = FormManualControl)) then
    begin
        ToolButtonCloseCurrentWork.Visible := True;
    end
    else
    begin
        SetCurrentWorkContent(FFrameCoef, 'Коэффициенты');
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

    PanelLastMessage.Caption := Format('[%d] %s: %s', [m.FWorkIndex, m.FWork, m.FText]);
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
    v := FormReadVars.FVars[X.FVarOrder];
    PanelLastMessage.Font.Color := clNavy;
    PanelLastMessage.Caption := Format('Считывание: АНКАТ %d: регистр %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        PanelLastMessage.Caption := PanelLastMessage.Caption + FloatToStr(X.FValue)
    else
    begin
        PanelLastMessage.Caption := PanelLastMessage.Caption + X.FError;
        PanelLastMessage.Font.Color := clRed;
    end;

    FormReadVars.HandleReadVar(X);
    if X.FError = '' then
        FormCurrentChart.AddValue(X.FProductSerial, X.FVar, X.FValue);
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
    v := FFrameCoef.FCoefs[X.FCoefficientOrder];
    PanelLastMessage.Font.Color := clNavy;
    PanelLastMessage.Caption := Format('Считывание: АНКАТ %d: коэффициент %d: %s: %s: ',
      [p.FSerial, v.FVar, v.FName, v.FDescription]);

    if X.FError = '' then
        PanelLastMessage.Caption := PanelLastMessage.Caption + FloatToStr(X.FValue)
    else
    begin
        PanelLastMessage.Caption := PanelLastMessage.Caption + X.FError;
        PanelLastMessage.Font.Color := clRed;
    end;

    FFrameCoef.HandleReadCoef(X);
    X.Free;
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
        fs.Read(wp, SizeOf(wp));
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

    with FormSettings do
    begin
        Parent := TabSheetSettings;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
    end;

    with FormChart do
    begin
        Parent := TabSheetCharts;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
    end;

    with FormLog do
    begin
        Parent := TabSheetLogs;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
    end;

    with FormParties do
    begin
        Parent := TabSheetParties;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
    end;

    with FormDelay do
    begin
        Parent := TabSheetParty;
        Align := alBottom;
        BorderStyle := bsNone;
        Visible := false;
    end;



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
    fs.Write(wp, SizeOf(wp));
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
    FormReadVars.SetCurrentParty(FProducts);
    FFrameCoef.SetCurrentParty(FProducts);
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

procedure TForm1.ToolButtonCloseCurrentWorkClick(Sender: TObject);
begin
    ToolButtonCloseCurrentWork.Visible := false;
    SetCurrentWorkContent(FFrameCoef, 'Коэффициенты');

end;

procedure TForm1.ToolButtonConsoleHideClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := True;
    ToolButtonMoveConsoleUp.Visible := True;
    ToolButtonConsoleHide.Visible := false;
    SplitterConsoleHoriz.Visible := false;
    SplitterConsoleVert.Visible := false;
    PanelConsolePlaceholderRight.Visible := false;
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
    ToolButtonMoveConsoleUp.Visible := True;
    ToolButtonConsoleHide.Visible := True;
    SplitterConsoleHoriz.Visible := True;
    SplitterConsoleVert.Visible := false;
    PanelConsolePlaceholderRight.Visible := false;
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

procedure TForm1.ToolButtonMoveConsoleUpClick(Sender: TObject);
begin
    ToolButtonMoveConsoleDown.Visible := True;
    ToolButtonMoveConsoleUp.Visible := false;
    ToolButtonConsoleHide.Visible := True;
    SplitterConsoleHoriz.Visible := false;
    SplitterConsoleVert.Visible := True;
    PanelConsolePlaceholderRight.Visible := True;
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

procedure TForm1.ToolButtonPartyMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: integer);
begin
    with ToolButtonParty do
        with ClientToScreen(Point(0, Height)) do
            PopupMenu1.Popup(X, Y);
end;

procedure TForm1.ToolButtonStopClick(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('CURRENT_WORK_STOP', nil);
    ToolButtonStop.Visible := false;

end;

procedure TForm1.SetCurrentWorkContent(widget: TControl;
contetnt_title: string);
begin
    if Assigned(widget) then
    begin
        with PanelCurrentWorkContent do
            if (ControlCount > 0) and (Controls[0] <> widget) then
            begin
                Controls[0].Visible := false;
                Controls[0].Parent := self;
            end;
        if widget is TForm then
            with widget as TForm do
            begin
                BorderStyle := bsNone;
                Align := alclient;

            end;
        widget.Parent := PanelCurrentWorkContent;
        widget.Visible := True;
    end;
    PanelCurrentWorkTitle.Caption := '   ' + contetnt_title;
end;

procedure TForm1.SetupWorkStarted(widget: TControl; work: string);
begin
    FormCurrentWork.SetupWorksTree;
    PanelCurrentWorkTitle.Caption := '   ' + work;
    if ToolButtonCloseCurrentWork.Visible then
        ToolButtonCloseCurrentWork.Click;

    PanelLastMessage.Caption := work;
    PanelLastMessage.Font.Color := clNavy;
    UpdatedControlsVisibilityOnStartedChanged(True);
    SetCurrentWorkContent(widget, work);
end;

procedure TForm1.UpdateCurrentWorkPanelCaption;
var
    c: TControl;
begin
    if PanelCurrentWorkContent.ControlCount = 0 then
    begin
        PanelCurrentWorkTitle.Caption := '';
        Exit;
    end;
    c := PanelCurrentWorkContent.Controls[0];
    if c = FormCurrentWork then
        PanelCurrentWorkTitle.Caption := '  ' +
          FormCurrentWork.RootNodeData.FInfo.FName
    else if c = FormReadVars then
        PanelCurrentWorkTitle.Caption := '  Опрос'
    else if c = FFrameCoef then
        PanelCurrentWorkTitle.Caption := '  Коэффициенты'
    else
        PanelCurrentWorkTitle.Caption := '  ???????';
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
end;

end.
