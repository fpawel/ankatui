unit UnitFrameSettings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
    System.Generics.Collections, Vcl.StdCtrls, FireDAC.Stan.Param;

type

    TConfigValue = class
    public
        FSection: string;
        FName: string;
        FValue: string;
        FError: string;
        FVar: string;

        FMin: double;
        FMax: double;
        FMinSet: boolean;
        FMaxSet: boolean;
        FSortOrder: integer;
        FType: string;
        FList: TArray<string>;

        procedure SetStr(str: string);
        procedure SetParam(p: TFDParam);
    end;

    TConfigSection = class
    public
        FName: string;
        FItems: TArray<TConfigValue>;
    end;

    TConfig = class
    public
        FItems: TArray<TConfigSection>;
    end;

    TValidateHandler = reference to procedure(ok: boolean);

    TValueChangedHandler = reference to procedure(p: TConfigValue);

    TFrameSettings = class(TFrame)
        Panel1: TPanel;
        Panel2: TPanel;
        Panel4: TPanel;
        Panel7: TPanel;
    CategoryPanelGroup1: TCategoryPanelGroup;
    private
        { Private declarations }
        FCtrlParam: TDictionary<TWinControl, TConfigValue>;
        FErrorLabel: TDictionary<TWinControl, TLabel>;
        FOnValueChanged: TValueChangedHandler;
        FOnValidate: TValidateHandler;

        procedure on_pipe_msg(msg, content: string);

        procedure ComboBox1Change(Sender: TObject);
        procedure Edit1Change(Sender: TObject);
        procedure ComboBox1DropDown(Sender: TObject);

    public
        { Public declarations }
        procedure Validate;

        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;

        procedure SetConfig(h: TConfig);
        procedure SetParam(p: TConfigValue);

        property OnValueChanged: TValueChangedHandler write FOnValueChanged;
        property OnValidate: TValidateHandler write FOnValidate;

        function Valid: boolean;

        procedure InvalidateControls;

    end;

const
    VtcInt = 'integer';
    VtcFloat = 'real';
    VtcString = 'text';
    VtcComportName = 'comport_name';
    VtcBaud = 'baud';

function GetParam(cfg: TConfig; sect, name: string): TConfigValue;

implementation

{$R *.dfm}

uses listports, RTTI, math, REST.Json, System.Generics.defaults, stringutils;

procedure TConfigValue.SetParam(p: TFDParam);
begin
    if (FType = VtcInt) or (FType = VtcBaud) then
        p.AsInteger := strtoint(FValue)
    else if FType = VtcFloat then
        p.AsFloat := str_to_float(FValue)
    else
        p.AsString := FValue;

end;

procedure TConfigValue.SetStr(str: string);
var
    v: double;
    vInt, i: integer;
    ok: boolean;
begin
    FError := '';
    str := str_validate_decimal_separator(str).Trim;
    if str = '' then
        FError := 'нет значения'
    else
    begin
        ok := true;
        if (FType = VtcInt) or (FType = VtcBaud) then
        begin
            ok := TryStrToInt(str, vInt);
            v := vInt;
        end
        else if FType = VtcFloat then
            ok := TryStrToFloat(str, v);

        if not ok then
            FError := 'не правильный синтаксис'
        else if FMinSet and (v < FMin) then
            FError := 'меньше ' + floattostr(FMin)
        else if FMaxSet and (v > FMax) then
            FError := 'больше ' + floattostr(FMax);
    end;
    if FError = '' then
        FValue := str;
end;

function GetParam(cfg: TConfig; sect, name: string): TConfigValue;
var
    i, j: integer;
begin
    for i := 0 to length(cfg.FItems) - 1 do
        if cfg.FItems[i].FName = sect then
            for j := 0 to length(cfg.FItems[i].FItems) - 1 do
                if cfg.FItems[i].FItems[j].FName = name then
                    exit(cfg.FItems[i].FItems[j]);

end;

constructor TFrameSettings.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FCtrlParam := TDictionary<TWinControl, TConfigValue>.Create;
    FErrorLabel := TDictionary<TWinControl, TLabel>.Create;
end;

destructor TFrameSettings.Destroy;
begin
    FCtrlParam.Free;
    FErrorLabel.Free;
    inherited;

end;

procedure TFrameSettings.InvalidateControls;
var
    pp: TPair<TWinControl, TConfigValue>;
    cb: TComboBox;
begin
    for pp in FCtrlParam do
    begin
        pp.Value.SetStr(pp.Value.FValue);
        FErrorLabel[pp.Key].Caption := pp.Value.FError;
        if pp.Key is TEdit then
            (pp.Key as TEdit).Text := pp.Value.FValue
        else if pp.Key is TComboBox then
        begin
            cb := pp.Key as TComboBox;
            cb.ItemIndex := cb.Items.IndexOf(pp.Value.FValue);
        end;

    end;

end;



function TFrameSettings.Valid: boolean;
var
    p: TLabel;
begin
    for p in FErrorLabel.Values do
        if p.Caption <> '' then
            exit(false);
    exit(true);

end;

procedure TFrameSettings.Validate;
var
    p: TLabel;
begin
    if Assigned(FOnValidate) then
        FOnValidate(Valid);

end;

procedure TFrameSettings.on_pipe_msg(msg, content: string);
begin
    msg := UpperCase(msg);

    if msg = 'CONFIG' then
    begin
        SetConfig(TJson.JsonToObject<TConfig>(content));
        Validate;
        exit;
    end;

    if msg = 'PARAM' then
    begin
        SetParam(TJson.JsonToObject<TConfigValue>(content));
        Validate;
        exit;
    end;

    MessageBox(Handle, Pchar(format('Неожиданное сообщение: %s [%d] %s',
      [msg, length(content), content.Substring(1, min(2000, length(content)))])
      ), 'Ошибка', MB_ICONERROR);

    ExitProcess(1);

    // raise Exception.Create('Wron msg: ' + msg + ', ' + content);

end;

procedure TFrameSettings.SetParam(p: TConfigValue);
var
    i: integer;
    pv: TConfigValue;
    ctrl: TWinControl;

begin
    for i := 0 to FCtrlParam.Count - 1 do
    begin
        pv := FCtrlParam.Values.ToArray[i];
        ctrl := FCtrlParam.Keys.ToArray[i];
        if (pv.FSection = p.FSection) and (pv.FName = p.FName) then
        begin
            FErrorLabel[ctrl].Caption := p.FError;
            pv.FValue := p.FValue;
            if ctrl is TComboBox then
                with ctrl as TComboBox do
                begin
                    OnChange := nil;
                    ItemIndex := Items.IndexOf(p.FValue);
                    Text := p.FValue;
                    OnChange := ComboBox1Change;
                end
            else if ctrl is TEdit then
                with ctrl as TEdit do
                begin
                    OnChange := nil;
                    Text := p.FValue;
                    OnChange := Edit1Change;

                end;
        end;
    end;

end;

procedure TFrameSettings.SetConfig(h: TConfig);
var
    i, j, k: integer;
    panel_param_container, tmp_pn: TPanel;
    panel_section_container : TCategoryPanel;
    ctrl: TWinControl;
    error_label: TLabel;
    Param: TConfigValue;
begin
    Height := 50;

    TArray.Sort<TConfigSection>(h.FItems,
      TDelegatedComparer<TConfigSection>.Construct(
        function(const a, b: TConfigSection): integer
        begin
            Result := TComparer<string>.Default.Compare(a.FName, b.FName);
        end));

    for i := 0 to length(h.FItems) - 1 do
    begin

        TArray.Sort<TConfigValue>(h.FItems[i].FItems,
          TDelegatedComparer<TConfigValue>.Construct(
            function(const a, b: TConfigValue): integer
            begin
                Result := TComparer<integer>.Default.Compare(a.FSortOrder,
                  b.FSortOrder);
            end));

        panel_section_container := TCategoryPanel.Create(self);
        with  panel_section_container do
        begin
            Caption := h.FItems[i].FName;
            PanelGroup := CategoryPanelGroup1;
            Height := 35 * (length(h.FItems[i].FItems) + 1) + 10;
        end;

        with TPanel.Create(self) do
        begin
            Parent := panel_section_container;
            BevelOuter := bvNone;
            Height := 10;
            top := 100500;
            Align := alTop;
        end;

        panel_section_container.Collapse;

        for j := 0 to length(h.FItems[i].FItems) - 1 do
        begin
            Param := h.FItems[i].FItems[j];

            with Param do
            begin
                if FType = VtcBaud then
                begin
                    SetLength(FList, 12);
                    FList[0] := '1200';
                    FList[1] := '2400';
                    FList[2] := '4800';
                    FList[3] := '9600';
                    FList[4] := '14400';
                    FList[5] := '19200';
                    FList[6] := '38400';
                    FList[7] := '56000';
                    FList[8] := '57600';
                    FList[9] := '115200';
                    FList[10] := '128000';
                    FList[11] := '256000';
                end;
            end;

            panel_param_container := TPanel.Create(self);
            with panel_param_container do
            begin
                Parent := panel_section_container;
                Font.Size := 12;
                Align := alTop;
                BevelOuter := bvNone;
                Height := 35;
                top := 100500;
            end;

            Height := Height + panel_param_container.Height;

            tmp_pn := TPanel.Create(self);
            with tmp_pn do
            begin
                Parent := panel_param_container;
                Align := alLeft;
                Constraints.MinWidth := 250;
                Constraints.MaxWidth := 250;
                BevelOuter := bvNone;

                with TPanel.Create(self) do
                begin
                    Align := alBottom;
                    Parent := tmp_pn;
                    Height := 10;
                    BevelOuter := bvNone;
                end;

                with TPanel.Create(self) do
                begin
                    Align := alClient;
                    Parent := tmp_pn;
                    BevelOuter := bvNone;
                    Caption := Param.FName + ':';
                    Alignment := taRightJustify;
                end;
            end;

            if Param.FType = VtcComportName then
            begin
                ctrl := TComboBox.Create(self);
                with ctrl as TComboBox do
                begin
                    Parent := panel_param_container;
                    style := csOwnerDrawFixed;
                    ItemHeight := 19;
                    EnumComPorts(Items);
                    ItemIndex := Items.IndexOf(Param.FValue);
                    OnDropDown := ComboBox1DropDown;
                    OnChange := ComboBox1Change;

                end;
            end
            else if length(Param.FList) > 0 then
            begin
                ctrl := TComboBox.Create(self);
                with ctrl as TComboBox do
                begin
                    Parent := panel_param_container;
                    style := csOwnerDrawFixed;
                    ItemHeight := 19;
                    for k := 0 to length(Param.FList) - 1 do
                    begin
                        Items.Add(Param.FList[k]);
                    end;
                    ItemIndex := Items.IndexOf(Param.FValue);
                    OnChange := ComboBox1Change;
                end;
            end
            else if (Param.FType = VtcInt) or (Param.FType = VtcFloat) or
              (Param.FType = VtcString) then
            begin
                ctrl := TEdit.Create(self);
                with ctrl as TEdit do
                begin
                    Parent := panel_param_container;
                    Text := Param.FValue;
                    OnChange := Edit1Change;
                    Constraints.MaxHeight := 30;
                end;
            end
            else
                raise Exception.Create('Unknown ' + h.FItems[i].FItems
                  [j].FType);

            FCtrlParam.Add(ctrl, Param);
            with ctrl do
            begin
                Align := alLeft;
                width := 150;
                Left := 100500;
            end;

            with TPanel.Create(self) do
            begin
                Align := alLeft;
                width := 5;
                Left := 100500;
                Parent := panel_param_container;
                BevelOuter := bvNone;
            end;

            error_label := TLabel.Create(self);
            FErrorLabel.Add(ctrl, error_label);

            with error_label do
            begin
                Font.Color := clRed;
                Font.Size := 12;
                AutoSize := true;
                WordWrap := true;
                Parent := panel_param_container;
                Align := alClient;
                Caption := Param.FError;
            end;

        end;
    end;

    if length(h.FItems) = 1 then
        panel_section_container.Expand


end;

procedure TFrameSettings.ComboBox1Change(Sender: TObject);
var
    pv: TConfigValue;
    errorLabel: TLabel;

begin
    pv := FCtrlParam[Sender as TWinControl];
    pv.FValue := (Sender as TComboBox).Text;

    errorLabel := FErrorLabel[Sender as TWinControl];
    if (Sender as TComboBox).ItemIndex = -1 then
        errorLabel.Caption := 'не выбрано значение'
    else
        pv.SetStr((Sender as TComboBox).Text);
    errorLabel.Caption := pv.FError;
    Validate;
    if (pv.FError = '') and Assigned(FOnValueChanged) then
        FOnValueChanged(pv);

end;

procedure TFrameSettings.ComboBox1DropDown(Sender: TObject);
begin
    EnumComPorts((Sender as TComboBox).Items);
end;

procedure TFrameSettings.Edit1Change(Sender: TObject);
var
    pv: TConfigValue;
    errorLabel: TLabel;
begin
    pv := FCtrlParam[Sender as TWinControl];
    pv.SetStr((Sender as TEdit).Text);
    errorLabel := FErrorLabel[Sender as TWinControl];
    errorLabel.Caption := pv.FError;
    Validate;
    if (pv.FError = '') and Assigned(FOnValueChanged) then
        FOnValueChanged(pv);
end;

end.
