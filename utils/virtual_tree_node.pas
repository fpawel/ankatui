unit virtual_tree_node;

interface

uses
    System.SysUtils, System.Variants,
    System.Classes,
    FireDAC.Comp.Client, UnitData,
    VirtualTrees;

type

    PTreeData = ^RTreeData;

    RColumn = record
        Text: string;
        FontSize: integer;
        ImageIndex: integer;
    end;

    TNodeData = class
    public
        FNode: PVirtualNode;
        FPopulated: boolean;
        FTreeView: TVirtualStringTree;
        FColumn: array [0 .. 2] of RColumn;

        procedure Populate; virtual; abstract;
        constructor Create(ATreeView: TVirtualStringTree;
          ANode: PVirtualNode); virtual;
    end;

    RTreeData = record
        X: TNodeData;
    end;


procedure FreeNodeData(t: TVirtualStringTree; n: PVirtualNode);

implementation

procedure FreeNodeData(t: TVirtualStringTree; n: PVirtualNode);
var
    p: PTreeData;
begin
    if not Assigned(n) then
        exit;
    p := t.GetNodeData(n);
    p.X.Free;
    freeNodeData(t, n.FirstChild);
    freeNodeData(t, n.NextSibling);
end;

constructor TNodeData.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode);
var
    d: RTreeData;
    c: RColumn;
    i: integer;
begin
    inherited Create;
    d.X := self;
    FTreeView := ATreeView;
    FNode := ANode;
    ATreeView.SetNodeData(FNode, PTreeData(d));
    ATreeView.HasChildren[FNode] := true;
    for i := 0 to 2 do
    begin
        FColumn[i].ImageIndex := -1;
    end;

end;

end.
