unit UnitFormParties;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormPartiesNodes, Vcl.ImgList;

type

    TFormParties = class(TForm)
        ImageList1: TImageList;
        Panel1: TPanel;
        HtmlViewer1: THtmlViewer;
        RichEdit1: TRichEdit;
        Splitter1: TSplitter;
        VirtualStringTree1: TVirtualStringTree;
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var Allowed: boolean);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
          var Handled: boolean);
    private
        { Private declarations }
        procedure Refresh;
    public
        { Public declarations }
    end;

var
    FormParties: TFormParties;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils,
    variantutils, stringgridutils, virtual_tree_node, UnitHostAppData,
  DataRichEditOutput;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure TFormParties.FormCreate(Sender: TObject);
begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;
    Refresh;
end;

procedure TFormParties.Refresh;
var
  year: Integer;
begin
    VirtualStringTree1.Clear;
    for year in DataModule1.PartiesYears do
        TNodeYear.Create(VirtualStringTree1, year);

end;


procedure TFormParties.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := True;
end;

procedure TFormParties.VirtualStringTree1BeforeCellPaint
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
begin
    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;
end;

procedure TFormParties.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeParty then
    begin
        HtmlViewer1.LoadFromString(
            HostAppData.FPipe.Fetch2('PARTY_INFO',
                IntToStr((p.X as TNodeParty).FPartyID)) );
        HtmlViewer1.Align := alClient;
        HtmlViewer1.Visible := True;
        exit;
    end;

    if p.X is TNodePartyDayLog then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := True;
        with p.X as TNodePartyDayLog do
            PrintDayLog(RichEdit1, Fday, Fmonth, Fyear);
        exit;
    end;

    if p.X is TNodeWorkLog then
    begin
        RichEdit1.Align := alClient;
        RichEdit1.Visible := True;
        PrintWorkLog(RichEdit1, (p.X as TNodeWorkLog).FRecordID);
        exit;
    end;

end;

procedure TFormParties.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormParties.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := True;
end;

procedure TFormParties.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        case Kind of
            ikNormal, ikSelected:
                ImageIndex := p.X.FColumn[Column].ImageIndex;

        end;

end;

procedure TFormParties.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormParties.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
    p: PTreeData;

begin
    p := Sender.GetNodeData(Node);
    if not Column in [0, 1, 2] then
        exit;

    case Column of
        0:
            begin

                if p.X is TNodeParty then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end
                else if p.X is TNodeProduct then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clMaroon;
                end

                else if (p.X is TNodePartyDayLog) or (p.X is TNodePartyLogsRoot) then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clTeal;
                    TargetCanvas.Font.Style := [fsItalic];

                end

                else if p.X is TNodeWorkLog then
                begin
                    if not Sender.Selected[Node] then
                    begin
                        TargetCanvas.Font.Color := clTeal;
                        if (p.X as TNodeWorkLog).FHasError then
                            TargetCanvas.Font.Color := clRed;
                    end;

                    TargetCanvas.Font.Style := [fsItalic];

                end;

            end;
    end;

end;

end.
