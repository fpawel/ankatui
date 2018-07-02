unit CurrentWorkTreeData;

interface

uses VirtualTrees, System.Generics.Collections;

type

    

    TNotifyInteger = class
        FValue: int64;
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

    

    TNodeData = class
        FInfo: TOperationInfo;
        FRun: boolean;
        FParent: TNodeData;
        FChildren: TList<TNodeData>;
        FDescendants: TList<TNodeData>;

        FNode: PVirtualNode;

        function PathEqualsTo(path: array of integer): boolean;
        function Index: integer;
        function Current: boolean;
        function Text: string;
        function NotifyOperation: TNotifyOperation;
        function Root: TNodeData;
        function IniKey: string;
        constructor Create(aNode: PVirtualNode; ainfo: TOperationInfo);

        procedure EnumDescendants;
        function Ordinal: integer;
    end;

    PTreeData = ^rTreeData;

    rTreeData = record
        X: TNodeData;
    end;

    

implementation

uses  System.sysutils;



constructor TNodeData.Create(aNode: PVirtualNode; ainfo: TOperationInfo);
begin
    FChildren := TList<TNodeData>.Create;
    FRun := false;
    FInfo := ainfo;
    FNode := aNode;
end;

procedure TNodeData.EnumDescendants;
var
    i: integer;
begin
    if not Assigned(Root.FDescendants) then
        Root.FDescendants := TList<TNodeData>.Create;
    Root.FDescendants.Add(self);
    for i := 0 to FChildren.Count - 1 do
    begin
        FChildren[i].EnumDescendants;
    end;
end;

function TNodeData.Ordinal: integer;
var
    i: integer;
begin
    for i := 0 to Root.FDescendants.Count - 1 do
    begin
        if Root.FDescendants[i] = self then
            result := i;
    end;
end;

function TNodeData.Root: TNodeData;
begin
    result := self;
    while Assigned(result.FParent) do
        result := result.FParent;

end;

function TNodeData.IniKey: string;
begin
    result := inttostr(Ordinal);
end;

function TNodeData.NotifyOperation: TNotifyOperation;
var
    i: integer;
begin
    result := TNotifyOperation.Create;
    result.FRun := True;
    result.FOrdinal := Ordinal;
end;

function TNodeData.Current: boolean;
begin
    result := (FChildren.Count = 0) and FRun;
end;

function TNodeData.Index: integer;
var
    i: integer;
begin
    if not Assigned(FParent) then
    begin
        result := -1;
        exit;
    end;
    for i := 0 to FParent.FChildren.Count - 1 do
    begin
        if self = FParent.FChildren[i] then
        begin
            result := i;
            exit;
        end;
    end;
    raise Exception.Create('node not found');
end;

function TNodeData.PathEqualsTo(path: array of integer): boolean;
var
    i: integer;
    p: TNodeData;
begin
    p := self;
    i := length(path) - 1;
    while Assigned(p) and (i >= 0) do
    begin
        if (p.Index <> path[i]) then
        begin
            result := false;
            exit;
        end;
        p := p.FParent;
        i := i - 1;
    end;
    result := (p = nil) and (i = -1);
end;

function TNodeData.Text: string;
var
    p: TNodeData;
begin
    result := FInfo.FName;
    p := self;
    while Assigned(p.FParent) and Assigned(p.FParent.FParent) do
    begin
        result := p.FParent.FInfo.FName + '. ' + result;
        p := p.FParent;

    end;

end;



end.
