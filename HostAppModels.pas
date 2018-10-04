unit HostAppModels;

interface

type
    TCmd = class
    public
        FCmd: integer;
        FStr: string;
    end;

    TCmds = class
    public
        FItems: array of TCmd;
    end;

    TNotifyOperation = class
        FOrdinal: integer;
        FName: string;
        FRun: boolean;
    end;

    TOperationInfo = class
    public
        FName: string;
        FOrdinal: integer;
        FChildren: array of TOperationInfo;
        FHasError: boolean;
        FHasMessage: boolean;
    end;

implementation

end.
