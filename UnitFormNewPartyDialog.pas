unit UnitFormNewPartyDialog;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
    UnitFrameSettings;

type
    TAcceptHandler = reference to procedure;

    TFormNewPartyDialog = class(TForm)
        Button1: TButton;
        TFrameSettings1: TFrameSettings;
        Panel2: TPanel;
        Button2: TButton;
        Button3: TButton;
        Label1: TLabel;
        procedure FormDeactivate(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
        procedure Button2Click(Sender: TObject);
        procedure Button3Click(Sender: TObject);
    private
        { Private declarations }
        FConfig: TConfig;
        FAcceptHandler: TAcceptHandler;
        function Serials: TArray<Integer>;
        procedure Validate(ok: Boolean);
        procedure SetupFrameSettings;
    public
        property OnAccept: TAcceptHandler write FAcceptHandler;

        { Public declarations }
    end;

var
    FormNewPartyDialog: TFormNewPartyDialog;

implementation

{$R *.dfm}

uses System.Generics.Collections, stringutils, parties, UnitData,
    FireDAC.Stan.Param;

procedure TFormNewPartyDialog.SetupFrameSettings;
begin
    TFrameSettings1.Parent := self;
    TFrameSettings1.Align := alclient;
    TFrameSettings1.SetConfig(FConfig);
    (TCategoryPanel(TFrameSettings1.CategoryPanelGroup1.Panels[0])).Expand;
    (TCategoryPanel(TFrameSettings1.CategoryPanelGroup1.Panels[1])).Expand;
    TFrameSettings1.OnValidate := Validate;
    TFrameSettings1.Validate;

    TFrameSettings1.Visible := true;

end;

procedure TFormNewPartyDialog.Button2Click(Sender: TObject);
var
    xs: TList<TConfigValue>;
begin
    TFrameSettings1.Free;
    TFrameSettings1 := TFrameSettings.Create(self);
    TFrameSettings1.Visible := false;

    xs := TList<TConfigValue>.Create;
    with xs do
    begin
        xs.AddRange(FConfig.FItems[1].FItems);
        add(UnitFrameSettings.TConfigValue.Create);
        with xs[count - 1] do
        begin
            FSortOrder := count - 1;
            FName := inttostr(count);
            FMin := 1;
            FMinSet := true;
            FValue := FName;
            FType := VtcInt;
        end;
    end;
    FConfig.FItems[1].FItems := xs.ToArray;
    xs.Free;
    SetupFrameSettings;

end;

procedure TFormNewPartyDialog.Button3Click(Sender: TObject);
var
    len: Integer;
begin
    if length(FConfig.FItems[1].FItems) = 1 then
        exit;

    TFrameSettings1.Free;
    TFrameSettings1 := TFrameSettings.Create(self);
    TFrameSettings1.Visible := false;

    len := length(FConfig.FItems[1].FItems);

    FConfig.FItems[1].FItems[len - 1].Free;
    setlength(FConfig.FItems[1].FItems, len - 1);

    SetupFrameSettings;

end;

procedure TFormNewPartyDialog.FormCreate(Sender: TObject);
var
    p: UnitFrameSettings.TConfigValue;
    I: Integer;
    vars: TList<TConfigValue>;
    value_list: TList<string>;
begin

    if Assigned(FConfig) then
        FConfig.Free;

    FConfig := TConfig.Create;
    setlength(FConfig.FItems, 2);

    FConfig.FItems[0] := PartyValuesSection(DataModule1.FDQuery1,
      DataModule1.FDQueryConfig);


    FConfig.FItems[1] := TConfigSection.Create;
    with FConfig.FItems[1] do
    begin
        FName := 'Серийные номера приборов';
        setlength(FItems, 5);
        for I := 0 to 4 do
        begin
            FItems[I] := UnitFrameSettings.TConfigValue.Create;
            with FItems[I] do
            begin
                FSortOrder := I;
                FName := inttostr(I + 1);
                FMin := 1;
                FMinSet := true;
                FValue := FName;
                FType := VtcInt;
            end;
        end;
    end;

    DataModule1.FDQuery1.Close;


    FConfig.FItems[0].FSortOrder := 0;
    FConfig.FItems[1].FSortOrder := 1;
    SetupFrameSettings;

end;

procedure TFormNewPartyDialog.Validate(ok: Boolean);
var
    kv: TDictionary<Integer, Integer>;
    ser: Integer;
begin
    Label1.Visible := false;
    if not ok then
    begin
        Button1.Enabled := false;
        exit;
    end;

    kv := TDictionary<Integer, Integer>.Create;
    for ser in Serials do
        if kv.ContainsKey(ser) then
        begin
            Label1.Caption := 'Дублирование серийного номера ' + inttostr(ser);
            Label1.Visible := true;
        end
        else
            kv.add(ser, 0);

    kv.Free;

    Button1.Enabled := not Label1.Visible;
end;

function TFormNewPartyDialog.Serials: TArray<Integer>;
var
    l: TList<Integer>;
    p: UnitFrameSettings.TConfigValue;
begin
    l := TList<Integer>.Create;
    for p in FConfig.FItems[1].FItems do
        if p.FError = '' then
            l.add(strtoint(p.FValue));
    result := l.ToArray;
    l.Free;
end;

procedure TFormNewPartyDialog.Button1Click(Sender: TObject);
var
    I, serial: Integer;
    p: UnitFrameSettings.TConfigValue;
    s: string;
begin

    with DataModule1.FDQuery1 do
    begin
        SQL.Text := 'INSERT INTO party DEFAULT VALUES;';
        Execute;

        for p in FConfig.FItems[0].FItems do
        begin
            SQL.Text :=
              'INSERT OR REPLACE INTO party_value (party_id, var, value)' +
              'VALUES ((SELECT * FROM current_party_id), :key, :val);';
            ParamByName('key').Value := p.FVar;
            p.SetParam(ParamByName('val'));
            Execute;
        end;

        for serial in Serials do
        begin
            SQL.Text := 'INSERT INTO product (party_id, product_serial) VALUES '
              + '((SELECT * FROM current_party_id), :serial)';
            ParamByName('serial').Value := serial;
            Execute;
        end;
        Close;
    end;
    // Form1.SetCurrentParty;
    Hide;
    if Assigned(FAcceptHandler) then
        FAcceptHandler;
end;

procedure TFormNewPartyDialog.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormNewPartyDialog.FormMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
begin
    ActiveControl := nil;
    TFrameSettings1.CategoryPanelGroup1.VertScrollBar.Position :=
      TFrameSettings1.CategoryPanelGroup1.VertScrollBar.Position -
      WheelDelta div 5;
end;

end.
