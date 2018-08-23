unit PropertiesFormUnit;

// Virtual Treeview sample form demonstrating following features:
// - Property page like string tree with individual node editors.
// - Incremental search.
// Written by Mike Lischke.
{$WARN UNSAFE_CODE OFF} // Prevent warnins that are not applicable

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ImgList, ExtCtrls, UITypes, System.ImageList,
    PropertyValueEditors;

const
    // Helper message to decouple node change handling from edit handling.
    WM_STARTEDITING = WM_USER + 778;

type

    TSection = class
        FProperties: array of TPropertyData;
        FName: string;
        FHint: string;
    end;

    TSections = class
        FSections: array of TSection;

    end;

    TPropertiesForm = class(TForm)
        VST3: TVirtualStringTree;
        TreeImages: TImageList;
        procedure FormCreate(Sender: TObject);
        procedure VST3Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3CreateEditor(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; out EditLink: IVTEditLink);
        procedure VST3Editing(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var Allowed: Boolean);
        procedure VST3GetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
          var HintText: string);
        procedure VST3GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var Index: TImageIndex);
        procedure VST3GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure VST3InitChildren(Sender: TBaseVirtualTree; Node: PVirtualNode;
          var ChildCount: Cardinal);
        procedure VST3InitNode(Sender: TBaseVirtualTree;
          ParentNode, Node: PVirtualNode;
          var InitialStates: TVirtualNodeInitStates);
        procedure VST3PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure VST3IncrementalSearch(Sender: TBaseVirtualTree;
          Node: PVirtualNode; const SearchText: string; var Result: Integer);
        procedure RadioGroup1Click(Sender: TObject);
        procedure VST3StateChange(Sender: TBaseVirtualTree;
          Enter, Leave: TVirtualTreeStates);
        procedure VST3FreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure VST3DrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
          Node: PVirtualNode; Column: TColumnIndex; const Text: string;
          const CellRect: TRect; var DefaultDraw: Boolean);
        procedure VST3BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
    private
        FSections: TSections;
        procedure WMStartEditing(var Message: TMessage);
          message WM_STARTEDITING;
    end;

var
    PropertiesForm: TPropertiesForm;

    // ----------------------------------------------------------------------------------------------------------------------

implementation

uses
    Math, PropertyUtils, stringgridutils;

{$R *.DFM}
// ----------------- TPropertiesForm ------------------------------------------------------------------------------------

procedure TPropertiesForm.FormCreate(Sender: TObject);
begin
    // We assign these handlers manually to keep the demo source code compatible
    // with older Delphi versions after using UnicodeString instead of WideString.
    // VST3.OnGetText := VST3GetText;
    // VST3.OnGetHint := VST3GetHint;
    // VST3.OnIncrementalSearch := VST3IncrementalSearch;

    // Always tell the tree how much data space per node it must allocated for us. We can do this here, in the
    // object inspector or in the OnGetNodeDataSize event.
    VST3.NodeDataSize := SizeOf(TPropertyData);
    // The VCL (D7 and lower) still uses 16 color image lists. We create a high color version explicitely because it
    // looks so much nicer.
    ConvertToHighColor(TreeImages);

    FSections := TSections.Create;
    setlength(FSections.FSections, 3);

    FSections.FSections[0] := TSection.Create;
    FSections.FSections[1] := TSection.Create;
    FSections.FSections[2] := TSection.Create;
    setlength(FSections.FSections[0].FProperties, 3);
    setlength(FSections.FSections[1].FProperties, 3);
    setlength(FSections.FSections[2].FProperties, 3);

    FSections.FSections[0].FName := 'Sect1';
    FSections.FSections[1].FName := 'Sect2';
    FSections.FSections[2].FName := 'Sect3';

    FSections.FSections[0].FProperties[0] := TPropertyData.Create;
    with FSections.FSections[0].FProperties[0] do
    begin
        FType := VtcString;
        FName := 'PARAM1';
        FHint := 'Hint Parama 1';
        FValue := 'value1';
        FChanged := false;
    end;

    FSections.FSections[0].FProperties[1] := TPropertyData.Create;
    with FSections.FSections[0].FProperties[1] do
    begin
        FType := VtcInt;
        FName := 'Param 2 int';
        FHint := 'Hint Parama 2';
        FValue := '2';
        FChanged := false;
    end;

    FSections.FSections[0].FProperties[2] := TPropertyData.Create;
    with FSections.FSections[0].FProperties[2] do
    begin
        FType := VtcFloat;
        FName := 'Param 3 float';
        FHint := 'Hint Parama 3';
        FValue := '3';
        FChanged := false;
    end;

    FSections.FSections[1].FProperties[0] := TPropertyData.Create;
    with FSections.FSections[1].FProperties[0] do
    begin
        FType := VtcString;
        FName := 'PARAM4';
        FHint := 'Hint Parama ';
        FValue := 'value4';
        setlength(FPickList, 5);
        FPickList[0] := 'value4';
        FPickList[1] := 'value5';
        FPickList[2] := 'value6';
        FPickList[3] := 'value7';
        FPickList[4] := 'value8';
        FChanged := false;
    end;

    FSections.FSections[1].FProperties[1] := TPropertyData.Create;
    with FSections.FSections[1].FProperties[1] do
    begin
        FType := VtcComportName;
        FName := 'Com Port';
        FHint := 'Hint Parama 2';
        FValue := 'COM1';
        FChanged := false;
    end;

    FSections.FSections[1].FProperties[2] := TPropertyData.Create;
    with FSections.FSections[1].FProperties[2] do
    begin
        FType := VtcBaud;
        FName := 'Param baud';
        FHint := 'Hint Parama 2';
        FValue := '9600';
        FChanged := false;
    end;

    FSections.FSections[2].FProperties[0] := TPropertyData.Create;
    with FSections.FSections[2].FProperties[0] do
    begin
        FType := VtcBool;
        FName := 'PARAM1';
        FHint := 'Hint Parama 1';
        FValue := '0';
        FChanged := false;
    end;

    FSections.FSections[2].FProperties[1] := TPropertyData.Create;
    with FSections.FSections[2].FProperties[1] do
    begin
        FType := VtcBool;
        FName := 'PARAM1';
        FHint := 'Hint Parama 1';
        FValue := '1';
        FChanged := false;
    end;

    FSections.FSections[2].FProperties[2] := TPropertyData.Create;
    with FSections.FSections[2].FProperties[2] do
    begin
        FType := VtcFloat;
        FName := 'Param 3 float';
        FHint := 'Hint Parama 3';
        FValue := '3';
        FChanged := false;
    end;

    VST3.RootNodeCount := length(FSections.FSections);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
    Data: PPropertyData;
begin
    Data := Sender.GetNodeData(Node);
    if Node.Parent = Sender.RootNode then
        ChildCount := length(FSections.FSections[Node.Index].FProperties)
    else
        ChildCount := 0;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3InitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);

var
    Data: PPropertyData;
    section_index: Integer;

begin

    if ParentNode = nil then
    begin
        InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
        Sender.SetNodeData(Node, TPropertyData.Create);
        Data := Sender.GetNodeData(Node);
        Data.FValue := FSections.FSections[Node.Index].FName;
    end
    else
    begin

        Sender.SetNodeData(Node, FSections.FSections[Node.Parent.Index]
          .FProperties[Node.Index]);

    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);

var
    Data: PPropertyData;

begin
    if TextType = ttNormal then
        case Column of
            0:
                if Node.Parent = Sender.RootNode then
                begin
                    // root nodes
                    CellText := FSections.FSections[Node.Index].FName;
                end
                else
                    CellText := FSections.FSections[Node.Parent.Index]
                      .FProperties[Node.Index].FName;
            1:
                begin
                    if Node.Parent <> Sender.RootNode then
                    begin
                        Data := Sender.GetNodeData(Node);
                        CellText := Data.FValue;
                    end;
                end;
        end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);

var
    Data: PPropertyData;

begin
    // Add a dummy hint to the normal hint to demonstrate multiline hints.
    if (Column = 0) and (Node.Parent <> Sender.RootNode) then
    begin
        Data := Sender.GetNodeData(Node);
        HintText := Data.FHint;
        if (Sender as TVirtualStringTree).Hintmode <> hmTooltip then
            HintText := HintText + #13 + '(Multiline hints are supported too).';
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var Index: TImageIndex);

var
    Data: PPropertyData;

begin
    if (Kind in [ikNormal, ikSelected]) and (Column = 0) then
    begin
        if Node.Parent = Sender.RootNode then
            Index := 1 // root nodes, this is an open folder
        else
        begin
            Data := Sender.GetNodeData(Node);
            if Data.FType <> '' then
                Index := 3
            else
                Index := 2;
        end;
    end;
end;


// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3Editing(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);

var
    Data: PPropertyData;

begin
    with Sender do
    begin
        Data := GetNodeData(Node);
        Allowed := (Node.Parent <> RootNode) and (Column = 1) and
          (Data.FType <> '');
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
    with TargetCanvas do
    begin
        if not(vsSelected in Node.States) then
        begin
            if Odd(Node.Index) then
                Brush.Color := cl3DLight
            else
                Brush.Color := clWhite;
            FillRect(CellRect);
        end;

    end;
end;

procedure TPropertiesForm.VST3Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

begin
    with Sender do
    begin
        // Start immediate editing as soon as another node gets focused.
        if Assigned(Node) and (Node.Parent <> RootNode) and
          not(tsIncrementalSearching in TreeStates) then
        begin
            // We want to start editing the currently selected node. However it might well happen that this change event
            // here is caused by the node editor if another node is currently being edited. It causes trouble
            // to start a new edit operation if the last one is still in progress. So we post us a special message and
            // in the message handler we then can start editing the new node. This works because the posted message
            // is first executed *after* this event and the message, which triggered it is finished.
            PostMessage(Self.Handle, WM_STARTEDITING, WPARAM(Node), 0);
        end;
    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3CreateEditor(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; out EditLink: IVTEditLink);

// This is the callback of the tree control to ask for an application defined edit link. Providing one here allows
// us to control the editing process up to which actual control will be created.
// TPropertyEditLink implements an interface and hence benefits from reference counting. We don't need to keep a
// reference to free it. As soon as the tree finished editing the class will be destroyed automatically.

begin
    EditLink := TPropertyEditLink.Create;
end;

procedure TPropertiesForm.VST3DrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
    Data: PPropertyData;
    R: TRect;
begin
    Data := Sender.GetNodeData(Node);
    if (Column = 1) and (Data.FType = VtcBool) then
    begin
        R := CellRect;
        R.Left := R.Left - 9;
        R.Right := R.Right - 9;
        DrawCheckbox(Sender, TargetCanvas, R, Data.FValue <> '0', '');
        DefaultDraw := false;
    end

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);

var
    Data: PPropertyData;

begin
    // Make the root nodes underlined and draw changed nodes in bold style.
    if Node.Parent = Sender.RootNode then
        TargetCanvas.Font.Style := [fsUnderline]
    else
    begin
        Data := Sender.GetNodeData(Node);
        if Data.FChanged then
            TargetCanvas.Font.Style := [fsBold]
        else
            TargetCanvas.Font.Style := [];

    end;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3IncrementalSearch(Sender: TBaseVirtualTree;
  Node: PVirtualNode; const SearchText: string; var Result: Integer);

var
    S, PropText: string;

begin
    S := SearchText;

    if Node.Parent = Sender.RootNode then
    begin
        // root nodes
        PropText := FSections.FSections[Node.Index].FName;
    end
    else
        PropText := FSections.FSections[Node.Parent.Index].FProperties
          [Node.Index].FName;

    // By using StrLIComp we can specify a maximum length to compare. This allows us to find also nodes
    // which match only partially. Don't forget to specify the shorter string length as search length.
    Result := StrLIComp(PChar(S), PChar(PropText),
      Min(length(S), length(PropText)))
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.RadioGroup1Click(Sender: TObject);

begin

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3StateChange(Sender: TBaseVirtualTree;
  Enter, Leave: TVirtualTreeStates);
begin

end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.WMStartEditing(var Message: TMessage);

// This message was posted by ourselves from the node change handler above to decouple that change event and our
// intention to start editing a node. This is necessary to avoid interferences between nodes editors potentially created
// for an old edit action and the new one we start here.

var
    Node: PVirtualNode;

begin
    Node := Pointer(Message.WPARAM);
    // Note: the test whether a node can really be edited is done in the OnEditing event.
    VST3.EditNode(Node, 1);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertiesForm.VST3FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    Data: PPropertyData;

begin
    Data := Sender.GetNodeData(Node);
    Finalize(Data^);
end;

end.
