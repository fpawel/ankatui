unit UnitFormChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormChartNodes, Vcl.ImgList, VclTee.TeeGDIPlus,
    VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart, VclTee.Series;

type

    TFormChart = class(TForm)
        ImageList1: TImageList;
        Splitter1: TSplitter;
        VirtualStringTree1: TVirtualStringTree;
        Chart2: TChart;
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
        procedure Chart2AfterDraw(Sender: TObject);

    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormChart: TFormChart;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils,
    variantutils, stringgridutils;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure freeNodeData(t: TVirtualStringTree; n: PVirtualNode);
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

procedure TFormChart.Chart2AfterDraw(Sender: TObject);
var
    ser: TChartSeries;
    node_var_product: TNodeVarProduct;
begin
    for ser in Chart2.SeriesList do
    begin
        node_var_product := TNodeVarProduct(ser.TagObject);
        node_var_product.FColor := ser.Color;
        node_var_product.FColorSet := true;
        VirtualStringTree1.RepaintNode(node_var_product.FNode);

    end;

end;

procedure TFormChart.FormCreate(Sender: TObject);
var
    d: RTreeData;
    year: integer;

begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    for year in DataModule1.SeriesYears do
        TNodeYear.Create(VirtualStringTree1, year);
    Chart2.Visible := false;
end;

procedure TFormChart.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := true;
end;

procedure TFormChart.VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
    p: PTreeData;
    d: TNodeData;
    node_var_product: TNodeVarProduct;
begin
    p := Sender.GetNodeData(Node);
    d := p.X;

    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
    end;
    
    if (Column = 4) AND (p.X is TNodeVarProduct) then
    begin
        node_var_product := p.X as TNodeVarProduct;
        if node_var_product.FColorSet then
        begin
            TargetCanvas.Brush.Color := node_var_product.FColor;
            CellRect.Top := CellRect.Top + 10;
            CellRect.Bottom := CellRect.Bottom - 10;
            TargetCanvas.FillRect(CellRect);
        end;

    end;
end;

procedure TFormChart.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
    node_var: TNodeVar;
    node_var_product: TNodeVarProduct;
    ser: TFastLineSeries;
    n: PVirtualNode;
    X: string;
    y: double;
    FmtStngs: TFormatSettings;
begin

    Chart2.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeVar then
    begin
        node_var := p.X as TNodeVar;
        Chart2.Visible := true;
        Chart2.SeriesList.Clear;
        Chart2.Title.Caption := node_var.FVarName;

        GetLocaleFormatSettings(GetThreadLocale, FmtStngs);
        FmtStngs.DateSeparator := '.';
        FmtStngs.ShortDateFormat := 'dd/MM/yyyy';
        FmtStngs.TimeSeparator := ':';
        FmtStngs.LongTimeFormat := 'h:mm:ss';

        if node.ChildCount = 0 then
            VirtualStringTree1.Expanded[Node] := true;    

        n := Node.FirstChild;
        while Assigned(n) do
        begin
            p := Sender.GetNodeData(n);

            node_var_product := p.X as TNodeVarProduct;
            ser := TFastLineSeries.Create(nil);
            ser.XValues.DateTime := true;
            ser.Title := inttostr(node_var_product.FSerial);
            ser.Active := n.CheckState = csCheckedNormal;
            ser.TagObject := node_var_product;

            with TFDQuery.Create(nil) do
            begin
                Connection := DataModule1.FDConnectionProductsDB;
                SQL.Text :=
                  'SELECT x,y FROM chart_value_info WHERE product_serial = :product_serial '
                  + 'AND read_var_id = :read_var_id AND series_id = :series_id;';
                ParamByName('product_serial').Value := node_var_product.FSerial;
                ParamByName('read_var_id').Value := node_var_product.FVar;
                ParamByName('series_id').Value :=
                  node_var_product.FSeriesInfo.SeriesID;
                open;
                First;
                while not Eof do
                begin
                    X := FieldValues['x'];
                    y := FieldValues['y'];
                    ser.AddNullXY(StrToDateTime(X, FmtStngs), y);
                    Next;
                end;
                Close;
                Free;
            end;
            Chart2.AddSeries(ser);
            n := n.NextSibling;
        end;

        exit;
    end;

end;

procedure TFormChart.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormChart.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
    d: RTreeData;
    v: variant;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := true;
end;

procedure TFormChart.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
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

procedure TFormChart.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2, 3] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormChart.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
    p: PTreeData;
    node_var_product: TNodeVarProduct;

begin
    p := Sender.GetNodeData(Node);
    if not Column in [0, 1, 2] then
        exit;

    case Column of
        0:
            begin

                if p.X is TNodeSeries then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end
                else if p.X is TNodeVar then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clMaroon;
                end
                else if p.X is TNodeVarProduct then
                begin
                    node_var_product := p.X as TNodeVarProduct;
                    if node_var_product.FColorSet then
                        TargetCanvas.Font.Color := node_var_product.FColor;

                end;

            end;

    end;

end;

end.
