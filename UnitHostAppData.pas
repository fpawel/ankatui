unit UnitHostAppData;

interface

uses
    System.SysUtils, pipe, System.Classes;

type
    THostAppData = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        FPipe: TPipe;
    end;

var
    HostAppData: THostAppData;

implementation

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure THostAppData.DataModuleCreate(Sender: TObject);
begin
    FPipe := TPipe.Create;

end;

end.
