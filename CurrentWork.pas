unit CurrentWork;

interface

uses CurrentWorkTreeData, System.Generics.Collections, Vcl.Graphics,
    VirtualTrees, Vcl.comctrls, Vcl.Controls, Vcl.ExtCtrls,
    System.Types, pipe, UnitData;

type
    TOperationCheckState = class
    public
        FOrdinal: integer;
        FCheckState: string;
    end;

    TWorkMsg = class
        FLevel: integer;
        FText: string;
        FWork: integer;
        FProductSerial: integer;
        FCreatedAt: TDAteTime;
    end;

    TCurrentWork = class
        FTreeView: TVirtualStringTree;
        FRichEdit: TRichEdit;
        FPanelTopText: TPanel;
        FPipe: TPipe;

        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);

        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);

        procedure TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);

        procedure TreeViewPaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);

        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);

        procedure TreeViewChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

        procedure AddNode(par: PVirtualNode; op_info: TOperationInfo);
        function RootNodeData: TNodeData;

        procedure RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
          var Handled: boolean);

        procedure Run;

        procedure AddWorkMessage(x: TWorkMsg);

        constructor Create(APipe: TPipe; APanelTopText: TPanel;
          ATreeView: TVirtualStringTree; ARichEdit: TRichEdit);
    end;

implementation

uses rest.json, System.sysutils, richeditutils, dateutils, stringutils,
    msglevel, variants,
    Winapi.Windows, Winapi.Messages, variantutils;

function checkStateToStr(x: TCheckState): string;
begin
    case x of
        csUncheckedNormal:
            exit('csUncheckedNormal');
        csUncheckedPressed:
            exit('csUncheckedPressed');
        csCheckedNormal:
            exit('csCheckedNormal');
        csCheckedPressed:
            exit('csCheckedPressed');
        csMixedNormal:
            exit('csMixedNormal');
        csMixedPressed:
            exit('csMixedPressed');
        csUncheckedDisabled:
            exit('csUncheckedDisabled');
        csCheckedDisabled:
            exit('csCheckedDisabled');
        csMixedDisabled:
            exit('csMixedDisabled');
    else
        exit('csCheckedNormal')
    end;
end;

function parseCheckState(x: string): TCheckState;
begin
    if x = 'csUncheckedNormal' then
        exit(csUncheckedNormal);

    if x = 'csUncheckedPressed' then
        exit(csUncheckedPressed);

    if x = 'csCheckedNormal' then
        exit(csCheckedNormal);

    if x = 'csCheckedPressed' then
        exit(csCheckedPressed);

    if x = 'csMixedNormal' then
        exit(csMixedNormal);

    if x = 'csMixedPressed' then
        exit(csMixedPressed);

    if x = 'csUncheckedDisabled' then
        exit(csUncheckedDisabled);

    if x = 'csCheckedDisabled' then
        exit(csCheckedDisabled);

    if x = 'csMixedDisabled' then
        exit(csMixedDisabled);

    exit(csCheckedNormal);
end;

constructor TCurrentWork.Create(APipe: TPipe; APanelTopText: TPanel;
  ATreeView: TVirtualStringTree; ARichEdit: TRichEdit);
begin
    inherited Create;
    FPipe := APipe;
    FTreeView := ATreeView;
    FRichEdit := ARichEdit;
    FPanelTopText := APanelTopText;

    FTreeView.OnBeforeCellPaint := TreeViewBeforeCellPaint;
    FTreeView.OnPaintText := TreeViewPaintText;
    FTreeView.OnChecked := TreeViewChecked;
    FTreeView.OnGetText := VirtualStringTree1GetText;
    FTreeView.OnGetImageIndex := VirtualStringTree1GetImageIndex;
    FTreeView.OnChange := VirtualStringTree1Change;

    FRichEdit.OnContextPopup := RichEdit1ContextPopup;

    FPipe.Handle('SETUP_CURRENT_WORKS',
        procedure(content: string)
        var
            Node: PVirtualNode;
            p: PTreeData;
        begin
            Node := FTreeView.GetFirst;
            while Assigned(Node) do
            begin
                p := FTreeView.GetNodeData(Node);
                p.x.Free;
                Node := FTreeView.GetNext(Node);
            end;
            FTreeView.Clear;
            AddNode(nil, TJson.JsonToObject<TOperationInfo>(content));

            Node := FTreeView.GetFirst;
            Node := FTreeView.GetNext(Node);
            if Assigned(Node) then
                FTreeView.TreeOptions.MiscOptions :=
                  FTreeView.TreeOptions.MiscOptions + [toCheckSupport]
            else
                FTreeView.TreeOptions.MiscOptions :=
                  FTreeView.TreeOptions.MiscOptions - [toCheckSupport];
        end);

    FPipe.Handle('CURRENT_WORK',
        procedure(content: string)
        var
            d: TNodeData;
            op: TNotifyOperation;
        begin
            op := TJson.JsonToObject<TNotifyOperation>(content);
            d := RootNodeData.FDescendants[op.FOrdinal];
            if op.FRun then
            begin
                // VirtualStringTree1.Selected[d.FNode] := true;
                FTreeView.Expanded[d.FNode] := True;
                d.FInfo.FCreatedAt := now;
                d.FInfo.FHasMessage := True;
                d.FInfo.FHasError := false;

            end;
            if (d.FChildren.Count = 0) and op.FRun then
            begin
                FPanelTopText.Caption := d.text;
                FPanelTopText.Font.Color := clNavy;
            end;
            d.FRun := op.FRun;
            FTreeView.RepaintNode(d.FNode);
        end);

end;

procedure TCurrentWork.AddWorkMessage(x: TWorkMsg);
var
    n: TNodeData;
    s: string;
begin
    if x.FProductSerial <> 0 then
        s := format('%s: прибор %d: %s', [inttostr2(x.FWork),
          x.FProductSerial, x.FText])
    else
        s := format('%s: %s', [inttostr2(x.FWork), x.FText]);

    RichEdit_AddText(FRichEdit, IncHour(x.FCreatedAt, 3), x.FLevel, s);
    if x.FLevel >= LError then
        for n in RootNodeData.FDescendants do
            if n.FRun then
            begin
                n.FInfo.FHasError := True;
                FTreeView.RepaintNode(n.FNode);
            end;
    RichEdit_SrollDown(FRichEdit);
end;

function TCurrentWork.RootNodeData: TNodeData;
begin
    result := PTreeData(FTreeView.GetNodeData(FTreeView.GetFirst)).x;
end;

procedure TCurrentWork.AddNode(par: PVirtualNode; op_info: TOperationInfo);
var
    Node: PVirtualNode;
    parent_tree_data: PTreeData;
    d: rTreeData;
    i: integer;
begin

    Node := FTreeView.AddChild(par);
    Node.CheckState := parseCheckState
      (DataModule1.GetCurrentWorkCheckState(op_info.FOrdinal));

    FTreeView.Expanded[Node] := True;
    FTreeView.CheckType[Node] := ctTriStateCheckBox;

    d.x := TNodeData.Create(Node, op_info);

    if par <> nil then
    begin
        parent_tree_data := FTreeView.GetNodeData(par);
        d.x.FParent := parent_tree_data.x;
        parent_tree_data.x.FChildren.Add(d.x);
    end;
    FTreeView.SetNodeData(Node, PTreeData(d));

    for i := 0 to length(op_info.FChildren) - 1 do
    begin
        AddNode(Node, op_info.FChildren[i]);
    end;

    if par = nil then
        d.x.EnumDescendants;

    Node := FTreeView.GetFirst;
    while Assigned(Node) do
    begin
        FTreeView.Expanded[Node] := True;
        Node := FTreeView.GetNext(Node);
    end;

end;

procedure TCurrentWork.TreeViewChecked(Sender: TBaseVirtualTree;
Node: PVirtualNode);
var
    p: PTreeData;
    d: TNodeData;
    x: TOperationCheckState;
begin
    p := Sender.GetNodeData(Node);
    d := p.x;
    x := TOperationCheckState.Create;
    x.FOrdinal := d.FInfo.FOrdinal;
    x.FCheckState := checkStateToStr(FTreeView.CheckState[Node]);
    FPipe.WriteStrMsg('CURRENT_WORK_CHECKED_CHANGED', x);
end;

procedure TCurrentWork.TreeViewBeforeCellPaint(Sender: TBaseVirtualTree;
TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
CellPaintMode: TVTCellPaintMode; CellRect: TRect;

var ContentRect: TRect);
var
    p: PTreeData;
    d: TNodeData;
begin

    p := Sender.GetNodeData(Node);
    d := p.x;

    if d.FRun then
    begin
        TargetCanvas.Brush.Color := clMoneyGreen;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if d.FInfo.FHasError then
    begin
        TargetCanvas.Brush.Color := cl3DLight;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

    if d.FInfo.FHasMessage then
    begin
        TargetCanvas.Brush.Color := clInfoBk;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;

end;

procedure TCurrentWork.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := True;
end;

procedure TCurrentWork.Run;
var
    Node: PVirtualNode;
    p: PTreeData;
    d: TNodeData;
    i: integer;
begin
    Node := FTreeView.GetFirst;
    d := RootNodeData;
    while Assigned(Node) do
    begin
        if FTreeView.Selected[Node] then
        begin
            p := FTreeView.GetNodeData(Node);
            d := p.x;
            break;
        end;
        Node := FTreeView.GetNext(Node);
    end;
    FRichEdit.Lines.Clear;
    for i := 0 to d.Root.FDescendants.Count - 1 do
    begin
        d.Root.FDescendants[i].FInfo.FHasError := false;
        d.Root.FDescendants[i].FInfo.FCreatedAt := 0;
        d.Root.FDescendants[i].FInfo.FHasMessage := false;
        FTreeView.RepaintNode(d.Root.FDescendants[i].FNode);
    end;
    FPipe.WriteStrMsg('CURRENT_WORK_START', d.NotifyOperation);
end;

procedure TCurrentWork.TreeViewPaintText(Sender: TBaseVirtualTree;

const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
TextType: TVSTTextType);
var
    p: PTreeData;
    d: TNodeData;
begin

    p := Sender.GetNodeData(Node);
    d := p.x;
    if d.FInfo.FHasError and ((not Sender.Selected[Node]) or ((Column > 0)))
    then
    begin
        TargetCanvas.Font.Color := clRed;
    end;

    if Column = 2 then
        TargetCanvas.Font.Color := clGreen
    else if Column = 1 then
        TargetCanvas.Font.Color := clNavy;

end;

procedure TCurrentWork.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
var CellText: string);
var
    p: PTreeData;
    d: TNodeData;

begin

    p := Sender.GetNodeData(Node);
    d := p.x;
    case Column of
        0:
            CellText := d.FInfo.FName;
        1:
            CellText := inttostr2(d.FInfo.FOrdinal);
        2:
            if d.FInfo.FHasMessage then
                CellText := TimeToStr(d.FInfo.FCreatedAt);
    end;
end;

procedure TCurrentWork.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
var Ghosted: boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
    d: TNodeData;

begin
    p := Sender.GetNodeData(Node);
    d := p.x;
    if (Column = 0) and (Kind in [ikNormal, ikSelected]) then
    begin

        if d.FRun then
            ImageIndex := 2
        else if d.FInfo.FHasError then
            ImageIndex := 0
        else if d.FInfo.FHasMessage then
            ImageIndex := 1;

    end;

end;

procedure TCurrentWork.VirtualStringTree1Change(Sender: TBaseVirtualTree;
Node: PVirtualNode);
var
    p: PTreeData;
    i: integer;
    s: string;
begin
    if Assigned(Node) then
    begin
        p := Sender.GetNodeData(Node);
        FRichEdit.Lines.Clear;
        DataModule1.PrintCurrentWorkMessages(FRichEdit, p.x.FInfo.FOrdinal);
        FTreeView.RepaintNode(Node);
    end;
end;

end.
