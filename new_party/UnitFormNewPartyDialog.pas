unit UnitFormNewPartyDialog;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Grids,
    config, PropertiesFormUnit;

type
    TAcceptHandler = reference to procedure;

    TFormNewPartyDialog = class(TForm)
        Button1: TButton;
        Panel2: TPanel;
        ComboBox1: TComboBox;
        Label2: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
        procedure ComboBox1Change(Sender: TObject);
    private
        { Private declarations }
        FParty, FSerials: TConfigSection;
        FPropertiesForm: TPropertiesForm;
        procedure Validate(p: TConfigProperty);

    public

        { Public declarations }
    end;

var
    FormNewPartyDialog: TFormNewPartyDialog;

implementation

{$R *.dfm}

uses System.Generics.Collections, stringutils, UnitData,
    FireDAC.Comp.Client, FireDAC.Stan.Param;

procedure TFormNewPartyDialog.ComboBox1Change(Sender: TObject);
var
    i, n1, n2: integer;
begin
    with FSerials do
    begin
        n1 := Length(FProperties);
        n2 := strtoint(ComboBox1.Text);
        SetLength(FProperties, n2);
        for i := n1 to n2 - 1 do
        begin
            FProperties[i] := TConfigProperty.Create;
            with FProperties[i] do
            begin
                FSortOrder := i;
                FHint := 'є' + inttostr(i + 1);
                FMin := 1;
                FMinSet := true;
                FValue := inttostr(i + 1);
                FType := VtcInt;
            end;
        end;
    end;
    FPropertiesForm.SetConfig([FParty, FSerials]);

end;

procedure TFormNewPartyDialog.FormCreate(Sender: TObject);
var
    i: integer;
begin
    FParty := DataModule1.GetConfig[0];
    FParty.FSortOrder := 0;
    for I := 0 to length(FPArty.FProperties)-1 do
    begin
        FPArty.FProperties[i].SetStr( FPArty.FProperties[i].FDefaultValue );

    end;

    FSerials := TConfigSection.Create;

    with FSerials do
    begin
        FSortOrder := 1;
        FSectionName := 'serials';
        FHint := '—ерийные номера приборов';
        SetLength(FProperties, 5);
        for i := 0 to 4 do
        begin
            FProperties[i] := TConfigProperty.Create;
            with FProperties[i] do
            begin
                FSortOrder := i;
                FHint := 'є' + inttostr(i + 1);
                FMin := 1;
                FMinSet := true;
                FValue := inttostr(i + 1);
                FType := VtcInt;
            end;
        end;
    end;

    FPropertiesForm := TPropertiesForm.Create(self);
    with FPropertiesForm do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        SetConfig([FParty, FSerials]);
        SetPropertyValueChanged(Validate);
    end;
    Validate(nil);
end;

procedure TFormNewPartyDialog.Validate(p: TConfigProperty);
var
    i, J: integer;
begin
    for J := 0 to Length(FSerials.FProperties) - 1 do
    begin
        if (FSerials.FProperties[J].FError = 'повтор€ющеес€ значение') then
            FSerials.FProperties[J].FError := '';
        for i := 0 to Length(FSerials.FProperties) - 1 do
            if (i <> J) and
              (FSerials.FProperties[i].FValue = FSerials.FProperties[J].FValue)
            then
                FSerials.FProperties[J].FError := 'повтор€ющеес€ значение';
    end;

    for i := 0 to Length(FSerials.FProperties) - 1 do
    begin
        if FSerials.FProperties[i].FError <> '' then
        begin
            Button1.Enabled := false;
            Exit;
        end;
    end;

    for i := 0 to Length(FParty.FProperties) - 1 do
    begin
        if FParty.FProperties[i].FError <> '' then
        begin
            Button1.Enabled := false;
            Exit;
        end;
    end;

    Button1.Enabled := true;
end;

procedure TFormNewPartyDialog.Button1Click(Sender: TObject);
var
    l: TList<integer>;
    p: TConfigProperty;
    serials: TArray<integer>;
begin
    l := TList<integer>.Create;
    for p in FSerials.FProperties do
        l.add(strtoint(p.FValue));
    serials := l.ToArray;
    l.Free;

    DataModule1.NewParty(FParty, serials);
    if Application.MainForm = self then
    begin
        DataModule1.FDConnectionProductsDB.Connected := false;
        DataModule1.FDConnectionConfig.Connected := false;
        Close;
    end
    else
        ModalResult := mrOk;
end;

end.
