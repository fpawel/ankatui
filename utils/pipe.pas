unit pipe;

interface

uses System.Classes, REST.Json, System.SyncObjs, System.Generics.Collections;

const
    BUF_SIZE = 200000;

type
    TPipeErrorHandler = reference to procedure(err: string);

    TStrMsg = record
        Msg: string;
        Str: string;
    end;

    TPipeConnection = class
    private
        { Private declarations }
        hPipe: THANDLE; // дескриптор

        FErrorHandler: TPipeErrorHandler;

    public
        procedure RaiseError(error: string);

        { Public declarations }
        Constructor Create(pipe_server: string;
          ErrorHandler: TPipeErrorHandler);
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

    TReadPipeHandler = reference to function(content: string):string;

    TPipe = class(TThread)
    private
        FPipeMasterToPeerConn: TPipeConnection;
        FPipePeerToMasterConn: TPipeConnection;
        FHandlers: TDictionary<string, TReadPipeHandler>;
        FConnected: boolean;
        procedure Execute; override;
        procedure RaiseError(error: string);

    public
        { Public declarations }
        Constructor Create;
        procedure Connect(pipe_server: string);
        procedure Handle(msg: string; h: TReadPipeHandler);

        procedure WriteMsgJSON(Msg: string; obj: TObject);
        procedure WriteMsgStr(Msg: string; str: string);
        procedure Close;

    end;

procedure ClosePipe(pipe: TPipe);

implementation

uses sysutils, Winapi.Windows, System.WideStrUtils, System.dateutils, vcl.forms,
    math;


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

procedure ClosePipe(pipe: TPipe);
begin
    pipe.Close;

end;

Constructor TPipeConnection.Create(pipe_server: string;
  ErrorHandler: TPipeErrorHandler);
var
    s: string;
begin
    FErrorHandler := ErrorHandler;
    s := '\\.\pipe\$' + pipe_server + '$';
    hPipe := CreateFileW(PWideChar(s), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if hPipe = INVALID_HANDLE_VALUE then
        RaiseError(s);

end;

procedure TPipeConnection.RaiseError(error: string);
var
    Buffer: array [0 .. 2047] of Char;
begin
    FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, GetLastError, $400, @Buffer,
      SizeOf(Buffer), nil);
    if error <> '' then
        error := error + ': ' + string(Buffer)
    else
        error := string(Buffer);
    FErrorHandler(LowerCase(error));
end;

function TPipeConnection.ReadFloat64: double;
var
    x: _INT64_BYTES;
    readed_count: DWORD;
begin
    if not ReadFile(hPipe, x.bytes, 8, readed_count, nil) then
    begin
        RaiseError('ReadFloat64: ReadFile error');
        exit;
    end;
    if readed_count <> 8 then
    begin
        RaiseError('ReadFloat64: readed_count <> 8');
        exit;
    end;
    result := x.value_double;

end;

procedure TPipeConnection.WriteFloat64(value: double);
var
    writen_count: DWORD;
    x: _INT64_BYTES;

begin
    x.value_double := value;
    if not(WriteFile(hPipe, x.bytes, 8, writen_count, nil)) then
    begin
        RaiseError('WriteFloat64: WriteFile error');
        exit;
    end;
    if writen_count <> 8 then
    begin
        RaiseError('WriteFloat64: writen_count <>8');
        exit;
    end;
end;

function TPipeConnection.ReadInt64: int64;
var
    x: _INT64_BYTES;
    readed_count: DWORD;
begin
    if not ReadFile(hPipe, x.bytes, 8, readed_count, nil) then
    begin
        RaiseError('ReadInt64: ReadFile error');
        exit;
    end;
    if readed_count <> 8 then
    begin
        RaiseError('ReadInt64: readed_count <> 8: ' + inttostr(readed_count));
        exit;
    end;
    result := x.value_int64;

end;

procedure TPipeConnection.WriteInt64(value: int64);
var
    writen_count: DWORD;
    x: _INT64_BYTES;

begin
    x.value_int64 := value;
    if not(WriteFile(hPipe, x.bytes, 8, writen_count, nil)) then
    begin
        RaiseError('WriteInt64: WriteFile error');
        exit;
    end;
    if writen_count <> 8 then
    begin
        RaiseError('WriteInt64: writen_count <>8');
        exit;
    end;
end;

function TPipeConnection.ReadInt32: LONGWORD;
var
    x: _LONGWORD_BYTES;
    readed_count: DWORD;
begin
    if not ReadFile(hPipe, x.bytes, 4, readed_count, nil) then
    begin
        RaiseError('ReadInt32: ReadFile error');
        exit;
    end;
    if readed_count <> 4 then
    begin
        RaiseError('ReadInt32: readed_count <> 4:' + inttostr(readed_count));
        exit;
    end;
    result := x.value;
end;

procedure TPipeConnection.WriteInt32(v: LONGWORD);
var
    writen_count: DWORD;
    x: _LONGWORD_BYTES;

begin
    x.value := v;
    if not(WriteFile(hPipe, x.bytes, 4, writen_count, nil)) then
    begin
        RaiseError('WriteInt: WriteFile error');
        exit;
    end;
    if writen_count <> 4 then
    begin
        RaiseError('WriteInt: writen_count <> 4');
        exit;
    end;
end;

procedure TPipeConnection.WriteString(s: string);
var
    tmp: DWORD;
    pos, len, n: Cardinal;
    Buffer: array [0 .. BUF_SIZE - 1] of byte;
    ptr_bytes: TBytes;
    i: Integer;

begin
    ptr_bytes := WideBytesOf(s);
    len := Length(ptr_bytes);
    WriteInt32(len);

    if len = 0 then
        exit;

    pos := 0;
    while pos < len do
    begin
        n := min(len - pos, BUF_SIZE);

        for i := 0 to n - 1 do
            Buffer[i] := ptr_bytes[pos + i];

        if not(WriteFile(hPipe, Buffer, n, tmp, nil)) then
        begin
            RaiseError('WriteString: WriteFile error');
            exit;
        end;
        pos := pos + n;
    end;
end;

function TPipeConnection.ReadString: string;
var
    readed_count: DWORD;
    pos, len: LONGWORD;
    Buffer: array [0 .. BUF_SIZE - 1] of byte;
    Str: string;

begin
    len := ReadInt32;
    pos := 0;
    while pos < len do
    begin
        if not ReadFile(hPipe, Buffer, min(BUF_SIZE, len - pos),
          readed_count, nil) then
        begin
            RaiseError('ReadString: ReadFile error');
            exit;
        end;
        pos := pos + readed_count;
        SetString(Str, PAnsiChar(@Buffer[0]), readed_count);
        result := result + Str;
    end;
end;

function TPipeConnection.ReadDateTime: TDateTime;
var
    s: string;
begin
    result := millis_to_datetime(ReadInt64);
end;

function TPipeConnection.ReadStrMsg: TStrMsg;
begin
    if ReadInt32 <> $5555 then
    begin
        RaiseError('error while reading 0x5555');
        exit;
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
    FHandlers := TDictionary<string, TReadPipeHandler>.create;
end;

procedure TPipe.connect(pipe_server: string);
begin
    try
    FConnected := true;
    FPipePeerToMasterConn := TPipeConnection.Create
      (pipe_server + '_PEER_TO_MASTER', RaiseError);
    FPipeMasterToPeerConn := TPipeConnection.Create
      (pipe_server + '_MASTER_TO_PEER', RaiseError);
      except
        FConnected := false;
        raise;
      end;
    Self.Suspended := false;
end;

procedure TPipe.Handle(msg: string; h: TReadPipeHandler);
begin
    if  FConnected then
        RaiseError('connected');
    if FHandlers.ContainsKey(msg) then
        RaiseError(msg + ': handler elready exits');
    FHandlers.Add(msg, h);
end;

procedure TPipe.RaiseError(error: string);
begin
    Synchronize(
        procedure
        begin
            if FConnected then
                raise Exception.Create(error);
        end);
end;

procedure TPipe.Execute;
var
    m: TStrMsg;
begin
    while (not terminated) and FConnected do
    begin
        m := FPipeMasterToPeerConn.ReadStrMsg;
        Synchronize(
            procedure
            var s:string;
            begin
                if (not terminated) and FConnected then
                begin
                    if not FHandlers.ContainsKey(m.Msg) then
                        RaiseError(m.Msg + ': not found handler');
                    s := FHandlers[m.Msg](m.Str);
                    if s <> '' then
                        FPipeMasterToPeerConn.WriteString(s);


                end;
            end);
    end;
end;

procedure TPipe.WriteMsgJSON(Msg: string; obj: TObject);

begin
    if Assigned(obj) then
        WriteMsgStr(Msg, TJson.ObjectToJsonString(obj))
    else
        WriteMsgStr(Msg, '');
end;

procedure TPipe.WriteMsgStr(Msg: string; str: string);
var
    m: TStrMsg;
begin
    if not FConnected then
        RaiseError('not connected');
    m.Msg := Msg;
    m.Str := str;
    FPipePeerToMasterConn.WriteStrMsg(m);
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
