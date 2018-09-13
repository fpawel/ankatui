unit UnitFormDelay;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
    Vcl.StdCtrls, models;

type

    TFormDelay = class(TForm)
        PanelPlaceHolder: TPanel;
        ProgressBar1: TProgressBar;
        Panel1: TPanel;
        Timer1: TTimer;
        Panel2: TPanel;
        Panel3: TPanel;
        Button1: TButton;
        Panel4: TPanel;
        Panel5: TPanel;
        procedure Timer1Timer(Sender: TObject);
        procedure Button1Click(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        procedure Stop;
        procedure SetupDelay(i: TDelayInfo);
    end;

var
    FormDelay: TFormDelay;

implementation

{$R *.dfm}

uses rest.json, System.DateUtils, Unit1, UnitHostAppData;

procedure TFormDelay.Stop;
begin

end;

procedure TFormDelay.Button1Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('SKIP_DELAY', nil);
end;

procedure TFormDelay.SetupDelay(i: TDelayInfo);
begin
    Panel3.Caption := '00:00:00';
    Panel2.Caption := i.FName;
    ProgressBar1.Position := 0;
    ProgressBar1.Max := i.FDurationMS;
    Timer1.Enabled := i.FEnabled;
    Panel5.Caption := TimeToStr(IncMilliSecond(0, i.FDurationMS));
    if i.FEnabled then
    begin
        PanelPlaceHolder.Parent := Form1.Panel5;
    end
    else
    begin
        PanelPlaceHolder.Parent := self;
    end;
end;

procedure TFormDelay.Timer1Timer(Sender: TObject);
var
    s: string;
    v: TDateTime;
begin
    s := Panel3.Caption;
    if TryStrToTime(s, v) then
        Panel3.Caption := FormatDateTime('HH:mm:ss', IncSecond(v));
    ProgressBar1.Position := ProgressBar1.Position + integer(Timer1.Interval);

end;

end.
