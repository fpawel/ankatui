unit UnitHostAppData;

interface

uses
    System.SysUtils, pipe, System.Classes;

type
    TCmd = class
    public
        FCmd: integer;
        FStr: string;
    end;

    TCmds = class
    public
       FItems : array of  TCmd ;
    end;

    THostAppData = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }


    public
        { Public declarations }
        FPipe: TPipe;
        FCmds : TCmds;

        procedure Init;



    end;

var
    HostAppData: THostAppData;

implementation

uses rest.json;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure THostAppData.DataModuleCreate(Sender: TObject);
begin
    FPipe := TPipe.Create;
end;

procedure THostAppData.Init;
var str:string;
begin
    FCmds := TJson.JsonToObject<TCmds>
      (HostAppData.FPipe.Fetch1('MODBUS_COMMANDS', nil))
end;



end.
