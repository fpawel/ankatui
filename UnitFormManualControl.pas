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
    GroupBox3: TGroupBox;
    Edit3: TEdit;
    Button6: TButton;
        procedure Edit1Change(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    private
        { Private declarations }
        procedure InvalidateModbusCommand;

    public
        function GetModbusCommand(var cmd: integer; var arg: extended): boolean;
        procedure Init;

    end;

var
    FormManualControl: TFormManualControl;

implementation

{$R *.dfm}

uses stringutils, Unit1, stringgridutils, UnitHostAppData;

procedure TFormManualControl.Button1Click(Sender: TObject);
var
    X: TSendModbusCmd;
begin
    X := TSendModbusCmd.Create;
    if GetModbusCommand(X.FCmd, X.FArg) then
        HostAppData.FPipe.WriteMsgJSON('MODBUS_CMD', X);
    X.Free;
    Form1.SetupWorkStarted(nil, 'отправка команды');
end;

procedure TFormManualControl.Button6Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgStr('SEND_SET_WORK_MODE',  Edit3.Text);
    Form1.SetupWorkStarted(nil, 'установкарежима работы');
end;

procedure TFormManualControl.ComboBox2Change(Sender: TObject);
begin
    InvalidateModbusCommand;
end;

procedure TFormManualControl.Edit1Change(Sender: TObject);
begin
    InvalidateModbusCommand;
end;

procedure TFormManualControl.Edit3Change(Sender: TObject);
var arg:extended;
begin
    Button6.Enabled := TryStrToFloat( str_validate_decimal_separator(Edit3.Text), arg);
end;

procedure TFormManualControl.InvalidateModbusCommand;
var
    a: integer;
    b: extended;
begin
    Button1.Enabled := GetModbusCommand(a, b);
end;

procedure TFormManualControl.Init;
var
    i: integer;
begin
    with ComboBox2 do
    begin
        Items.Clear;
        for i := 0 to length(HostAppData.FCmds.FItems) - 1 do
        begin
            Items.Add(HostAppData.FCmds.FItems[i].FStr);
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
        cmd := HostAppData.FCmds.FItems[ComboBox2.ItemIndex].FCmd;
    result := result AND TryStrToFloat(Edit1.Text, arg);
end;

end.
