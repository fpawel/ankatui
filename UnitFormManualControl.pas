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
        Button3: TButton;
        Button4: TButton;
        Button5: TButton;
        Edit2: TEdit;
    Label2: TLabel;
    Button6: TButton;
        procedure Edit1Change(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure Button6Click(Sender: TObject);
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
        HostAppData.Pipe.WriteMsgJSON('MODBUS_CMD', X);
    X.Free;
    Form1.SetupWorkStarted( 'отправка команды');
end;

procedure TFormManualControl.Button6Click(Sender: TObject);
begin
    HostAppData.Pipe.WriteMsgStr('SEND_SET_WORK_MODE',  Edit1.Text);
    Form1.SetupWorkStarted( 'установка режима работы');
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

procedure TFormManualControl.Init;
var
    i: integer;
begin
    with ComboBox2 do
    begin
        Items.Clear;
        for i := 0 to length(HostAppData.Cmds.FItems) - 1 do
        begin
            Items.Add(HostAppData.Cmds.FItems[i].FStr);
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
        cmd := HostAppData.Cmds.FItems[ComboBox2.ItemIndex].FCmd;
    result := result AND TryStrToFloat(Edit1.Text, arg);
end;

end.
