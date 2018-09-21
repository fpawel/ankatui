unit pipe;

interface

uses System.Classes, Winapi.Windows, REST.Json, System.SyncObjs,
    System.Generics.Collections,
    sysutils;

type

    EPipeHostError = class(Exception);

    TStrMsg = record
        Msg: string;
        Str: string;
    end;

type
    RaiseExceptionHandler = procedure(s: string) of object;

    TPipeConnection = class
    private
        { Private declarations }
        hPipe: THANDLE; // дескриптор
        FName: string;
        FRaiseException: RaiseExceptionHandler;
        FConnected: boolean;
        procedure raise_exception(error: string);
        procedure thisReadFile(var Buffer; numberOfbytesToRead: DWORD);
        procedure thisWriteFile(var Buffer; numberOfbytesToWrite: DWORD);
    public
        function FormatError(error: string): string;

        { Public declarations }
        Constructor Create(ARaiseException: RaiseExceptionHandler);

        function Connect(pipe_server: string): boolean;

        procedure WriteInt32(v: LONGWORD);
        function ReadInt32: LONGWORD;

        procedure WriteString(s: string);
        function ReadString: string;

        procedure WriteInt64(value: int64);
        function ReadInt64: int64;

        procedure WriteFloat64(value: double);
        function ReadFloat64: double;

        function ReadDateTime: TDateTime;

        function ReadStrMsg: TStrMsg;
        procedure WriteStrMsg(m: TStrMsg);
    end;

    TReadPipeHandler = reference to function(content: string): string;

    TPipe = class(TThread)
    private
        FPipeMasterToPeerConn: TPipeConnection;
        FPipePeerToMasterConn: TPipeConnection;
        FHandlers: TDictionary<string, TReadPipeHandler>;
        FConnected: boolean;

        procedure raise_exception(error: string);
    public
        { Public declarations }
        Constructor Create;
        procedure Execute; override;
        function Connect(pipe_server: string): boolean;
        procedure Handle(Msg: string; h: TReadPipeHandler);

        procedure WriteMsgJSON(Msg: string; obj: TObject);
        procedure WriteMsgStr(Msg: string; Str: string);
        procedure Close;

        function Fetch1(Msg: string; obj: TObject): string;
        function Fetch2(Msg: string; Str: string): string;
        property Connected: boolean read FConnected;

    end;

implementation

uses System.WideStrUtils, System.dateutils, vcl.forms,
    math, inifiles;

type
    _LONGWORD_BYTES = record

        case Integer of
            0:
                (bytes: array [0 .. 3] of byte);
            1:
                (value: LONGWORD);
    end;

type
    _INT64_BYTES = record

        case Integer of
            0:
                (bytes: array [0 .. 7] of byte);
            1:
                (value_int64: int64);

            2:
                (value_double: double);
    end;

function millis_to_datetime(x_millis: int64): TDateTime;
begin
    result := IncHour(IncMilliSecond(EncodeDateTime(1970, 1, 1, 0, 0, 0, 0),
      x_millis), 3);
end;

procedure TPipe.raise_exception(error: string);
begin

    Synchronize(
        procedure
        begin
            if FConnected then
            begin
                FConnected := false;
                raise EPipeHostError.Create(error);
            end;
        end);
end;

Constructor TPipeConnection.Create(ARaiseException: RaiseExceptionHandler);
begin
    FRaiseException := ARaiseException;
end;

function TPipeConnection.Connect(pipe_server: string): boolean;
var
    s: string;
begin
    FName := pipe_server;
    s := '\\.\pipe\$' + pipe_server + '$';
    hPipe := CreateFileW(PWideChar(s), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    result := hPipe <> INVALID_HANDLE_VALUE;
    if result then
    begin
        FConnected := true;
    end;

end;

procedure TPipeConnection.thisReadFile(var Buffer; numberOfbytesToRead: DWORD);
var
    readed_count: DWORD;
begin
    if not ReadFile(hPipe, Buffer, numberOfbytesToRead, readed_count, nil) then
    begin
        raise_exception('ReadFile');
    end;

    if readed_count <> numberOfbytesToRead then
    begin
        raise_exception(Format('ReadFile: readed_count=%d, must be %d',
          [readed_count, numberOfbytesToRead]));
    end;
end;

procedure TPipeConnection.thisWriteFile(var Buffer;
numberOfbytesToWrite: DWORD);
var
    writen_count: DWORD;
begin
    if not FConnected then
    begin
        raise_exception('not connected');
    end;

    if not(WriteFile(hPipe, Buffer, numberOfbytesToWrite, writen_count, nil))
    then
    begin
        raise_exception('WriteFile');
    end;
    if writen_count <> numberOfbytesToWrite then
    begin
        raise_exception(Format('WriteFile: writen_count=%d, must be %d',
          [writen_count, numberOfbytesToWrite]));
    end;
end;

procedure TPipeConnection.raise_exception(error: string);
begin
    FRaiseException(self.FormatError(error));
end;

function TPipeConnection.FormatError(error: string): string;
var
    Buffer: array [0 .. 2047] of Char;
    strWinError: string;
begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, $400, @Buffer,
      SizeOf(Buffer), nil);
    strWinError := Trim(string(Buffer));
    if error <> '' then
        if strWinError <> '' then
            result := error + ': ' + strWinError
        else if strWinError <> '' then
            result := strWinError;
    if result <> '' then
        result := FName + ': ' + result
    else
        result := FName + ': failed';
end;

function TPipeConnection.ReadFloat64: double;
var
    x: _INT64_BYTES;
begin
    thisReadFile(x.bytes[0], 8);
    result := x.value_double;
end;

procedure TPipeConnection.WriteFloat64(value: double);
var
    x: _INT64_BYTES;
begin
    x.value_double := value;
    thisWriteFile(x.bytes, 8);
end;

function TPipeConnection.ReadInt64: int64;
var
    x: _INT64_BYTES;
begin
    thisReadFile(x.bytes, 8);
    result := x.value_int64;
end;

procedure TPipeConnection.WriteInt64(value: int64);
var
    x: _INT64_BYTES;
begin
    x.value_int64 := value;
    thisWriteFile(x.bytes, 8);
end;

function TPipeConnection.ReadInt32: LONGWORD;
var
    x: _LONGWORD_BYTES;
begin
    thisReadFile(x.bytes, 4);
    result := x.value;
end;

procedure TPipeConnection.WriteInt32(v: LONGWORD);
var
    x: _LONGWORD_BYTES;
begin
    x.value := v;
    thisWriteFile(x.bytes, 4);
end;

procedure TPipeConnection.WriteString(s: string);
var
    len: Integer;
    ptr_bytes: TBytes;

begin
    ptr_bytes := WideBytesOf(s);
    len := Length(ptr_bytes);
    WriteInt32(len);
    thisWriteFile(ptr_bytes[0], len);
end;

function TPipeConnection.ReadString: string;
var
    readed_count: DWORD;
    len: LONGWORD;
    i: Integer;
    Buffer: TBytes;
    Str: string;
begin
    len := ReadInt32;
    if len = 0 then
        exit('');
    SetLength(Buffer, len);
    thisReadFile(Buffer[0], len);
    result := WideStringOf(Buffer);
end;

function TPipeConnection.ReadDateTime: TDateTime;
begin
    result := millis_to_datetime(ReadInt64);
end;

function TPipeConnection.ReadStrMsg: TStrMsg;
begin
    if ReadInt32 <> $5555 then
    begin
        raise_exception('error while reading 0x5555');
    end;
    result.Msg := ReadString;
    result.Str := ReadString;
end;

procedure TPipeConnection.WriteStrMsg(m: TStrMsg);
begin
    WriteInt32($5555);
    WriteString(m.Msg);
    WriteString(m.Str);
end;

Constructor TPipe.Create;
begin
    inherited Create(true);
    FHandlers := TDictionary<string, TReadPipeHandler>.Create;
    FPipePeerToMasterConn := TPipeConnection.Create(self.raise_exception);
    FPipeMasterToPeerConn := TPipeConnection.Create(self.raise_exception);
end;

function TPipe.Connect(pipe_server: string): boolean;
begin
    if not FPipePeerToMasterConn.Connect(pipe_server + '_PEER_TO_MASTER') then
        exit(false);
    if not FPipeMasterToPeerConn.Connect(pipe_server + '_MASTER_TO_PEER') then
        exit(false);
    FConnected := true;
    self.Suspended := false;
    result := true;
end;

procedure TPipe.Handle(Msg: string; h: TReadPipeHandler);
begin
    if FConnected then
        self.raise_exception('connected');
    if FHandlers.ContainsKey(Msg) then
        self.raise_exception(Msg + ': handler elready exits');
    FHandlers.Add(Msg, h);
end;

procedure termintate_error(error: string);
begin
    Application.MessageBox(PWideChar(error), 'Анкат: критический сбой',
      MB_OK or MB_ICONERROR);
    Application.Terminate;
end;

procedure TPipe.Execute;
var
    m: TStrMsg;
    s: string;
begin
    while (not terminated) and FConnected do
    begin
        m := FPipeMasterToPeerConn.ReadStrMsg;
        s := '';
        Synchronize(
            procedure
            begin
                if (not terminated) and FConnected then
                begin
                    if not FHandlers.ContainsKey(m.Msg) then
                        termintate_error(m.Msg + ': not found handler');
                    try
                        s := FHandlers[m.Msg](m.Str);
                    except
                        on e: Exception do
                            termintate_error(m.Msg + ': ' + e.Message);
                    end;

                end;
            end);
        if s <> '' then
            FPipeMasterToPeerConn.WriteString(s);
    end;
end;

procedure TPipe.WriteMsgJSON(Msg: string; obj: TObject);
begin
    if Assigned(obj) then
        WriteMsgStr(Msg, TJson.ObjectToJsonString(obj))
    else
        WriteMsgStr(Msg, '');
end;

procedure TPipe.WriteMsgStr(Msg: string; Str: string);
var
    m: TStrMsg;
begin
    if not FConnected then
        self.raise_exception('not connected');
    m.Msg := Msg;
    m.Str := Str;
    FPipePeerToMasterConn.WriteStrMsg(m);
end;

function TPipe.Fetch1(Msg: string; obj: TObject): string;
begin
    WriteMsgJSON(Msg, obj);
    result := FPipePeerToMasterConn.ReadString;
end;

function TPipe.Fetch2(Msg: string; Str: string): string;
begin
    WriteMsgStr(Msg, Str);
    result := FPipePeerToMasterConn.ReadString;
end;

procedure TPipe.Close;
begin
    if not FConnected then
        exit;

    WriteMsgJSON('CLOSE', nil);

    FConnected := false;
    CloseHandle(FPipeMasterToPeerConn.hPipe);
    CloseHandle(FPipePeerToMasterConn.hPipe);
    Terminate;
end;

end.
