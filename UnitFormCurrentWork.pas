unit UnitFormCurrentWork;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, VirtualTrees,
    System.ImageList, Vcl.ImgList, CurrentWorkTreeData;

type
    TFormCurrentWork = class(TForm)
        VirtualStringTree1: TVirtualStringTree;
        Button1: TButton;
        ImageList2: TImageList;
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1Checked(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure Button1Click(Sender: TObject);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
    private
        { Private declarations }
        procedure AddNode(par: PVirtualNode; op_info: TOperationInfo);
        function RootNodeData: TNodeData;

        procedure Reset;
        procedure ResetError;
    public
        { Public declarations }
        procedure SetRunError;
        function SelectedOperation: TOperationInfo;
    end;

var
    FormCurrentWork: TFormCurrentWork;

const
    BS_LEFT = $100;
    BS_RIGHT = $200;
    BS_CENTER = 768;
    BS_TOP = $400;
    BS_BOTTOM = $800;
    BS_VCENTER = 3072;

implementation

{$R *.dfm}

uses rest.json, stringutils,  Unit1, unitdata;

type
    TOperationCheckState = class
    public
        FOrdinal: integer;
        FCheckState: string;
    end;

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

procedure TFormCurrentWork.Button1Click(Sender: TObject);
var
    o: TOperationInfo;
begin
    o := FormCurrentWork.SelectedOperation;
    if o = nil then
        o := RootNodeData.FInfo;
    Reset;
    Form1.FPipe.WriteMsgStr('RUN_MAIN_WORK', inttostr(o.FOrdinal));
    Form1.SetupWorkStarted(o.FName, true);
    Form1.Panel6.Controls[0].Parent := nil;
    VirtualStringTree1.Parent := Form1.Panel6;
    ModalResult := mrOk;
end;

procedure TFormCurrentWork.FormCreate(Sender: TObject);
var
    defstyle: dWord;
begin
    defstyle := GetWindowLong(Button1.Handle, GWL_STYLE);
    SetWindowLong(Button1.Handle, GWL_STYLE, defstyle or BS_LEFT );

    Form1.FPipe.Handle('SETUP_CURRENT_WORKS',
        procedure(content: string)
        var
            Node: PVirtualNode;
            p: PTreeData;
        begin
            Node := VirtualStringTree1.GetFirst;
            while Assigned(Node) do
            begin
                p := VirtualStringTree1.GetNodeData(Node);
                p.x.Free;
                Node := VirtualStringTree1.GetNext(Node);
            end;
            VirtualStringTree1.Clear;
            AddNode(nil, TJson.JsonToObject<TOperationInfo>(content));

            Node := VirtualStringTree1.GetFirst;
            Node := VirtualStringTree1.GetNext(Node);
            if Assigned(Node) then
                VirtualStringTree1.TreeOptions.MiscOptions :=
                  VirtualStringTree1.TreeOptions.MiscOptions + [toCheckSupport]
            else
                VirtualStringTree1.TreeOptions.MiscOptions :=
                  VirtualStringTree1.TreeOptions.MiscOptions - [toCheckSupport];
        end);

    Form1.FPipe.Handle('CURRENT_WORK',
        procedure(content: string)
        var
            d: TNodeData;
            op: TNotifyOperation;
        begin
            op := TJson.JsonToObject<TNotifyOperation>(content);
            d := RootNodeData.FDescendants[op.FOrdinal];
            if op.FName <> d.FInfo.FName then
                exit;

            if op.FRun then
            begin
                // VirtualStringTree1.Selected[d.FNode] := true;
                VirtualStringTree1.Expanded[d.FNode] := True;
                d.FInfo.FHasMessage := True;
                d.FInfo.FHasError := false;
            end;
            if (d.FChildren.Count = 0) and op.FRun then
            begin
                form1.Panel5.Caption := d.text;
                form1.Panel5.Font.Color := clNavy;
            end;
            d.FRun := op.FRun;
            VirtualStringTree1.RepaintNode(d.FNode);
        end);

    Form1.FPipe.Connect('ANKAT');

end;

procedure TFormCurrentWork.VirtualStringTree1BeforeCellPaint
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
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

procedure TFormCurrentWork.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
    Button1.Caption := '   Запустить: [0] Настройка Анкат';
    if SelectedOperation <> nil then
        Button1.Caption := Format('   Запустить: [%d] %s',
         [ SelectedOperation.FOrdinal, AnsiLowerCase(SelectedOperation.FName)]);

end;

procedure TFormCurrentWork.VirtualStringTree1Checked(Sender: TBaseVirtualTree;
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
    x.FCheckState := checkStateToStr(VirtualStringTree1.CheckState[Node]);
    Form1.FPipe.WriteMsgJSON('CURRENT_WORK_CHECKED_CHANGED', x);
end;

procedure TFormCurrentWork.VirtualStringTree1GetImageIndex
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: TImageIndex);
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

procedure TFormCurrentWork.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
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
    end;
end;

procedure TFormCurrentWork.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
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

    if Column = 1 then
        TargetCanvas.Font.Color := clNavy;

end;

function TFormCurrentWork.SelectedOperation: TOperationInfo;
begin
    if Assigned(VirtualStringTree1.FocusedNode) then
        Result := PTreeData(VirtualStringTree1.GetNodeData
          (VirtualStringTree1.FocusedNode)).x.FInfo
    else
        Result := nil;
end;

procedure TFormCurrentWork.ResetError;
var
    n: TNodeData;
begin
    for n in RootNodeData.FDescendants do
        if n.FInfo.FHasError then
        begin
            n.FInfo.FHasError := false;
            VirtualStringTree1.RepaintNode(n.FNode);
        end;
end;

procedure TFormCurrentWork.SetRunError;
var
    n: TNodeData;
begin
    for n in RootNodeData.FDescendants do
        if n.FRun then
        begin
            n.FInfo.FHasError := true;
            VirtualStringTree1.RepaintNode(n.FNode);
        end;
end;

function TFormCurrentWork.RootNodeData: TNodeData;
begin
    Result := PTreeData(VirtualStringTree1.GetNodeData
      (VirtualStringTree1.GetFirst)).x;
end;

procedure TFormCurrentWork.AddNode(par: PVirtualNode; op_info: TOperationInfo);
var
    Node: PVirtualNode;
    parent_tree_data: PTreeData;
    d: rTreeData;
    i: integer;
begin

    Node := VirtualStringTree1.AddChild(par);
    d.x := TNodeData.Create(Node, op_info);

    if par <> nil then
    begin
        parent_tree_data := VirtualStringTree1.GetNodeData(par);
        d.x.FParent := parent_tree_data.x;
        parent_tree_data.x.FChildren.Add(d.x);
    end;
    VirtualStringTree1.SetNodeData(Node, PTreeData(d));

    for i := 0 to length(op_info.FChildren) - 1 do
    begin
        AddNode(Node, op_info.FChildren[i]);
    end;

    if par = nil then
        d.x.EnumDescendants;

    VirtualStringTree1.Expanded[Node] := true;
    VirtualStringTree1.CheckType[Node] := ctTriStateCheckBox;
    Node.CheckState := parseCheckState
      (DataModule1.GetCurrentWorkCheckState(op_info.FOrdinal));

    Node := VirtualStringTree1.GetFirst;
    while Assigned(Node) do
    begin
        VirtualStringTree1.Expanded[Node] := true;
        Node := VirtualStringTree1.GetNext(Node);
    end;

end;

procedure TFormCurrentWork.Reset;
var
    Node: PVirtualNode;
    p: PTreeData;
    d: TNodeData;
    i: integer;
begin
    Node := VirtualStringTree1.GetFirst;
    d := RootNodeData;
    while Assigned(Node) do
    begin
        if VirtualStringTree1.Selected[Node] then
        begin
            p := VirtualStringTree1.GetNodeData(Node);
            d := p.x;
            break;
        end;
        Node := VirtualStringTree1.GetNext(Node);
    end;
    for i := 0 to d.Root.FDescendants.Count - 1 do
    begin
        d.Root.FDescendants[i].FInfo.FHasError := false;
        d.Root.FDescendants[i].FInfo.FHasMessage := false;
        VirtualStringTree1.RepaintNode(d.Root.FDescendants[i].FNode);
    end;

end;

end.
