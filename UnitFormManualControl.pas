unit UnitFormManualControl;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, UnitData,
    pipe;

type
    TSendModbusCmd = class
        FCmd: integer;
        FArg: extended;
    end;

    TFormManualControl = class(TForm)
        GroupBox1: TGroupBox;
        Edit1: TEdit;
        ComboBox2: TComboBox;
        Button1: TButton;
        RadioGroup1: TRadioGroup;
        GroupBox2: TGroupBox;
        Button2: TButton;
        Label1: TLabel;
        Label2: TLabel;
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Edit2: TEdit;
        Label3: TLabel;
        procedure Edit1Change(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    private
        { Private declarations }
        FModbusCommands: TArray<TModbusCommand>;

        procedure InvalidateModbusCommand;

    public
        FPipe: TPipe;
        { Public declarations }
        function GetModbusCommand(var cmd: integer; var arg: extended): boolean;
    end;

var
    FormManualControl: TFormManualControl;

implementation

{$R *.dfm}

uses stringutils, Unit1;

procedure TFormManualControl.Button1Click(Sender: TObject);
var
    X: TSendModbusCmd;
begin
    X := TSendModbusCmd.Create;
    if GetModbusCommand(X.FCmd, X.FArg) then
        FPipe.WriteStrMsg('MODBUS_CMD', X);
    X.Free;
    Form1.SetupWorkStarted('отправка команды', true);
end;

procedure TFormManualControl.ComboBox2Change(Sender: TObject);
begin
    InvalidateModbusCommand;
end;

procedure TFormManualControl.Edit1Change(Sender: TObject);
begin
    InvalidateModbusCommand;
end;

procedure TFormManualControl.InvalidateModbusCommand;
var
    a: integer;
    b: extended;
begin
    Button1.Enabled := GetModbusCommand(a, b);
end;

procedure TFormManualControl.FormCreate(Sender: TObject);
var
    i: integer;
begin
    FModbusCommands := DataModule1.ModbusCommands;
    with ComboBox2 do
    begin
        Items.Clear;
        for i := 0 to length(FModbusCommands) - 1 do
        begin
            Items.Add(FModbusCommands[i].Value);
        end;
        ItemIndex := 0;
    end;
    Label2.Caption := '?';
end;

function TFormManualControl.GetModbusCommand(var cmd: integer;
  var arg: extended): boolean;
begin
    result := true;
    if ComboBox2.ItemIndex = -1 then
        result := TryStrToInt(str_validate_decimal_separator
          (ComboBox2.Text), cmd)
    else
        cmd := FModbusCommands[ComboBox2.ItemIndex].Key;
    result := result AND TryStrToFloat(Edit1.Text, arg);
end;

end.
