unit UnitHostAppData;

interface

uses
    System.SysUtils, pipe, System.Classes, HostAppModels;

type

    THostAppData = class(TDataModule)
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
        FCmds: TCmds;
        FRootWork: TOperationInfo;
        FPipe: TPipe;
    public
        { Public declarations }

        function Connect: boolean;

        property Cmds: TCmds read FCmds;
        property RootWork: TOperationInfo read FRootWork;
        property pipe: TPipe read FPipe;

    end;

var
    HostAppData: THostAppData;

implementation

uses rest.json, shellapi, Winapi.Windows, Vcl.Forms, findproc;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure THostAppData.DataModuleCreate(Sender: TObject);
begin
    FPipe := TPipe.Create;
end;

procedure Run_host_app;
var
    exe_dir, exe_file_name: string;
begin
    // $(HOMEDRIVE)$(HOMEPATH)\.ankat
    exe_dir := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable
      ('HOMEPATH') + '\.ankat\';
    exe_file_name := exe_dir + 'ankathost.exe';
    ShellExecute(0, 'open', PChar(exe_file_name), '-waitpeer=true',
      PChar(exe_dir), SW_SHOWNORMAL);
    while not ProcessExists('ankathost.exe') do
        Application.ProcessMessages;

end;

function THostAppData.Connect: boolean;

begin

    if not FPipe.Connect('ANKAT') then
    begin
        Run_host_app;

        while not FPipe.Connect('ANKAT') do
            Application.ProcessMessages;

    end;
    if FPipe.Connected then
    begin
        FCmds := TJson.JsonToObject<TCmds>
          (FPipe.Fetch1('MODBUS_COMMANDS', nil));
        FRootWork := TJson.JsonToObject<TOperationInfo>
          (FPipe.Fetch1('CURRENT_WORKS', nil));
    end;
    Result := FPipe.Connected;

end;

end.
