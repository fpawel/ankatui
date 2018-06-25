unit UnitFrameDelay;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList,
  Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TFrameDelay = class(TFrame)
    Panel3: TPanel;
    Panel9: TPanel;
    LabelDelay: TLabel;
    ProgressBarDelay: TProgressBar;
    Panel1: TPanel;
    ToolBar1: TToolBar;
    ToolButtonSkip: TToolButton;
    ImageList2: TImageList;
    TimerDelay: TTimer;
    procedure TimerDelayTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(aowner: TComponent); override;
  end;

implementation

{$R *.dfm}

constructor TFrameDelay.Create(aowner: TComponent);
begin
    inherited Create(aowner);
end;


procedure TFrameDelay.TimerDelayTimer(Sender: TObject);
begin
    ProgressBarDelay.Position := ProgressBarDelay.Position + 1000;
end;

end.
