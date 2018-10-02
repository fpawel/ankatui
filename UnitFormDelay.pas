unit UnitFormDelay;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.ComCtrls,
    Vcl.StdCtrls, models, Vcl.ToolWin;

type

    TFormDelay = class(TForm)
        PanelPlaceHolder: TPanel;
        ProgressBar1: TProgressBar;
        Panel1: TPanel;
        Timer1: TTimer;
    ToolBar2: TToolBar;
    ToolButton1: TToolButton;
    LabelCurrentTime: TLabel;
    LabelTotalTime: TLabel;
    LabelWhat: TLabel;
    Panel14: TPanel;
    LabelProgress: TLabel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
        procedure Timer1Timer(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
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

uses rest.json, System.DateUtils, Unit1, UnitHostAppData, math;

procedure TFormDelay.Stop;
begin

end;

procedure TFormDelay.SetupDelay(i: TDelayInfo);
begin
    LabelCurrentTime.Caption := '00:00:00';
    LabelWhat.Caption := i.FName;
    LabelProgress.Caption := '';
    ProgressBar1.Position := 0;
    ProgressBar1.Max := i.FDurationMS;
    Timer1.Enabled := i.FEnabled;
    LabelTotalTime.Caption := TimeToStr(IncMilliSecond(0, i.FDurationMS));
    Visible := i.FEnabled;
end;

procedure TFormDelay.Timer1Timer(Sender: TObject);
var
    s: string;
    v: TDateTime;
begin
    s := LabelCurrentTime.Caption;
    if TryStrToTime(s, v) then
        LabelCurrentTime.Caption := FormatDateTime('HH:mm:ss', IncSecond(v));
    ProgressBar1.Position := ProgressBar1.Position + integer(Timer1.Interval);

    LabelProgress.Caption :=
        Inttostr( ceil(ProgressBar1.Position * 100 / ProgressBar1.Max) ) + '%';

end;

procedure TFormDelay.ToolButton1Click(Sender: TObject);
begin
    HostAppData.FPipe.WriteMsgJSON('SKIP_DELAY', nil);
end;

end.
