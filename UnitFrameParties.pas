unit UnitFrameParties;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, System.Generics.collections,
    Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList,
    Vcl.ImgList, VirtualTrees, Vcl.ExtCtrls, HTMLUn2, HtmlView,
    FireDAC.Comp.Client, Vcl.StdCtrls, Vcl.ComCtrls, UnitData;

type
    PTreeData = ^RTreeData;

    RColumn = record
        Text: string;
        FontSize: integer;
        ImageIndex: integer;
    end;

    TNodeData = class
    protected
        FNode: PVirtualNode;
        FPopulated: boolean;
        FTreeView: TVirtualStringTree;
        FColumn: array [0 .. 2] of RColumn;
    public
        procedure Populate; virtual; abstract;
        constructor Create(ATreeView: TVirtualStringTree;
          ANode: PVirtualNode); virtual;
    end;

    TNodeYear = class(TNodeData)
        FYear: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; AYear: integer);
    end;

    TNodeMonth = class(TNodeData)
        FNodeYear: TNodeYear;
        FMonth: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANodeYear: TNodeYear;
          AMonth: integer);
    end;

    TNodeDay = class(TNodeData)
        FNodeMonth: TNodeMonth;
        FDay: integer;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree;
          ANodeMonth: TNodeMonth; ADay: integer);
    end;

    TNodeParty = class(TNodeData)

        FNodeDay: TNodeDay;
        FPartyID: int64;
        FWaht: string;
        FValues: TKeysValues;
        procedure Populate; override;
        constructor Create(ATreeView: TVirtualStringTree; ANodeDay: TNodeDay;
          APartyID: int64; AWaht: string);


    end;

    TNodeProduct = class(TNodeData)
        FNodeParty: TNodeParty;
        FSerial: integer;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          ANodeParty: TNodeParty; ASerial: integer);
    end;

    TNodePartyLogs = class(TNodeData)
        FNodeParty: TNodeParty;
        constructor Create(ATreeView: TVirtualStringTree;
          ANodeParty: TNodeParty);
        procedure Populate; override;
    end;

    TNodePartyProducts = class(TNodeData)
        FNodeParty: TNodeParty;
        constructor Create(ATreeView: TVirtualStringTree;
          ANodeParty: TNodeParty);
        procedure Populate; override;
    end;

    TNodeWork = class(TNodeData)
        FCreatedAt: TDateTime;
        FWork: string;
        FRecordID: int64;
        FHasError: boolean;
        FHasChildren: boolean;
        FWorkIndex: integer;
        constructor Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
          FDQuery: TFDQuery);
        procedure Populate; override;
    end;

    RTreeData = record
        X: TNodeData;
    end;

    TFrameParties = class(TFrame)
        VirtualStringTree1: TVirtualStringTree;
        ImageList1: TImageList;
        HtmlViewer1: THtmlViewer;
        Splitter1: TSplitter;
        Panel1: TPanel;
        RichEdit1: TRichEdit;
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var Allowed: boolean);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
    private
        { Private declarations }

    public
        { Public declarations }
        constructor Create(aowner: TComponent); override;
    end;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils;

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

constructor TNodeData.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode);
var
    d: RTreeData;
    c: RColumn;
    i : integer;
begin
    inherited Create;
    d.X := self;
    FTreeView := ATreeView;
    FNode := ANode;
    ATreeView.SetNodeData(FNode, PTreeData(d));
    ATreeView.HasChildren[FNode] := true;
    for i :=0 to 2 do
    begin
        FColumn[i].ImageIndex := -1;
    end;

end;

constructor TNodeYear.Create(ATreeView: TVirtualStringTree; AYear: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(nil));
    FYear := AYear;
    FColumn[0].ImageIndex := 0;
    FColumn[0].Text := inttostr(FYear);

end;

constructor TNodeMonth.Create(ATreeView: TVirtualStringTree;
  ANodeYear: TNodeYear; AMonth: integer);
var
    dt: TDateTime;
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANodeYear.FNode));
    FNodeYear := ANodeYear;
    FMonth := AMonth;
    FColumn[0].ImageIndex := 1;
    dt := EncodeDateTime(2000, FMonth, 1, 0, 0, 0, 0);
    FColumn[0].Text := FormatDateTime('mm', dt) + ' ' +
      FormatDateTime('mmmm', dt);
end;

constructor TNodeDay.Create(ATreeView: TVirtualStringTree;
  ANodeMonth: TNodeMonth; ADay: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANodeMonth.FNode));
    FNodeMonth := ANodeMonth;
    FDay := ADay;
    FColumn[0].ImageIndex := 2;
    FColumn[0].Text := inttostr(FDay);
    if FDay < 10 then
        FColumn[0].Text := '0' + FColumn[0].Text;
end;

constructor TNodeParty.Create(ATreeView: TVirtualStringTree; ANodeDay: TNodeDay;
  APartyID: int64; AWaht: string);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANodeDay.FNode));
    FPartyID := APartyID;
    FValues := DataModule1.PartyValues(FPartyID);
    FWaht := AWaht;
    FColumn[0].Text := AWaht;
    FColumn[0].ImageIndex := 3;
end;



constructor TNodeProduct.Create(ATreeView: TVirtualStringTree;
  ANode: PVirtualNode; ANodeParty: TNodeParty; ASerial: integer);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    FNodeParty := ANodeParty;
    FColumn[0].ImageIndex := 4;
    FSerial := ASerial;
    FColumn[0].Text := '№ ' + inttostr(ASerial);

end;

constructor TNodePartyProducts.Create(ATreeView: TVirtualStringTree;
  ANodeParty: TNodeParty);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANodeParty.FNode));
    FColumn[0].Text := 'приборы партии';
    FNodeParty := ANodeParty;

end;

constructor TNodePartyLogs.Create(ATreeView: TVirtualStringTree;
  ANodeParty: TNodeParty);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANodeParty.FNode));
    FColumn[0].Text := 'журнал партии';
    FNodeParty := ANodeParty;
end;

constructor TNodeWork.Create(ATreeView: TVirtualStringTree; ANode: PVirtualNode;
  FDQuery: TFDQuery);
begin
    inherited Create(ATreeView, ATreeView.AddChild(ANode));
    with FDQuery do
    begin
        FWork := FieldValues['work'];
        FRecordID := FieldValues['record_id'];
        FCreatedAt := FieldValues['created_at'];
        FHasError := FieldValues['has_error'];
        FWorkIndex := FieldValues['work_index'];
        FHasChildren := FieldValues['has_children'];
    end;

    FColumn[0].Text := FWork;
    FColumn[1].Text := inttostr2(FWorkIndex);
    FColumn[2].Text := FormatDateTime('HH:nn', IncHour(FCreatedAt, 3));
    if FHasError then
        FColumn[0].ImageIndex := 5;

    FPopulated := not FHasChildren;
    FTreeView.HasChildren[FNode] := FHasChildren;

end;

procedure TNodePartyLogs.Populate;
begin
    with DataModule1.FDQueryPartyWorks do
    begin
        ParamByName('party_id').Value := FNodeParty.FPartyID;
        open;
        First;
        while not Eof do
        begin
            TNodeWork.Create(FTreeView, FNode, DataModule1.FDQueryPartyWorks);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeWork.Populate;
begin
    with DataModule1.FDQueryWorksByParentRecordID do
    begin
        ParamByName('parent_record_id').Value := FRecordID;
        open;
        First;
        FTreeView.HasChildren[FNode] := false;
        while not Eof do
        begin
            TNodeWork.Create(FTreeView, FNode,
              DataModule1.FDQueryWorksByParentRecordID);
            Next;
            FTreeView.HasChildren[FNode] := true;
        end;
        Close;
    end;

end;

procedure TNodeYear.Populate;
begin
    with DataModule1.FDQuery1 do
    begin
        SQL.Text := 'SELECT * FROM party_year_month WHERE year = :year;';
        ParamByName('year').Value := FYear;
        open;
        First;
        while not Eof do
        begin
            TNodeMonth.Create(FTreeView, self, FieldValues['month']);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeMonth.Populate;
begin
    with DataModule1.FDQuery1 do
    begin
        SQL.Text := 'SELECT * FROM party_year_month_day ' +
          'WHERE year = :year AND month = :month;';
        ParamByName('year').Value := FNodeYear.FYear;
        ParamByName('month').Value := FMonth;
        open;
        First;
        while not Eof do
        begin
            TNodeDay.Create(FTreeView, self, FieldValues['day']);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeDay.Populate;
begin
    with DataModule1.FDQuery1 do
    begin
        SQL.Text := 'SELECT * FROM party_info ' +
          'WHERE year = :year AND month = :month AND day = :day;';
        ParamByName('year').Value := FNodeMonth.FNodeYear.FYear;
        ParamByName('month').Value := FNodeMonth.FMonth;
        ParamByName('day').Value := FDay;
        open;
        First;
        while not Eof do
        begin
            TNodeParty.Create(FTreeView, self, FieldValues['party_id'],
              FieldValues['what']);
            Next;
        end;
        Close;
    end;
end;

procedure TNodeParty.Populate;
begin
    TNodePartyProducts.Create(FTreeView, self);
    TNodePartyLogs.Create(FTreeView, self);
end;

procedure TNodePartyProducts.Populate;
begin
    with DataModule1.FDQuery1 do
    begin
        SQL.Text :=
          'SELECT product_serial FROM product where party_id = :party_id;';
        ParamByName('party_id').Value := FNodeParty.FPartyID;
        open;
        First;
        while not Eof do
        begin
            TNodeProduct.Create(FTreeView, FNode, FNodeParty,
              FieldValues['product_serial']);
            Next;
        end;
        close;
    end;

end;

constructor TFrameParties.Create(aowner: TComponent);
var
    d: RTreeData;
  year: Integer;

begin
    inherited Create(aowner);
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    for year in DataModule1.PartiesYears do
        TNodeYear.Create(VirtualStringTree1, year);
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;

end;

procedure TFrameParties.VirtualStringTree1BeforeCellPaint
  (Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
  var ContentRect: TRect);
var
    p: PTreeData;
    d: TNodeData;
begin
    p := Sender.GetNodeData(Node);
    d := p.X;

    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
        exit;
    end;
end;

procedure TFrameParties.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
    str, s, str_message: string;
    v: TKeyValue;
    created_at: TDateTime;
    level, work_index: integer;
begin
    RichEdit1.Visible := false;
    HtmlViewer1.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeParty then
        with p.X as TNodeParty do
        begin

            str := '<html> <head> <title> Партия</title> </head> <body> <table border="0"> '
              + '<style TYPE="text/css"> td { font-size: 20px; padding-left:10px; padding: 1px 10px;} '
              + '.col2 { color: #000080; font-weight: bold; } </style>';
            for v in FValues do
            begin
                str := str +
                  Format('<tr><td align="right">%s:</td><td class="col2">%s</td></tr>',
                  [v.Key, v.Value]);
            end;
            str := str + '</table></body></html>';
            HtmlViewer1.LoadFromString(str);
            HtmlViewer1.Align := alClient;
            HtmlViewer1.Visible := true;
            exit;
        end;

    if p.X is TNodeWork then
        with p.X as TNodeWork, DataModule1.FDQueryWorkMessages do
        begin
            ParamByName('record_id').Value := FRecordID;
            RichEdit1.Lines.Clear;
            open;
            First;
            while not Eof do
            begin
                created_at := FieldValues['created_at'];
                level := FieldValues['level'];
                str_message := FieldValues['message'];
                work_index := FieldValues['work_index'];
                RichEdit_AddText(RichEdit1, IncHour(created_at, 3), level,
                  '[' + inttostr2(work_index) + '] ' + str_message);

                Next;
            end;
            Close;
            RichEdit1.Align := alClient;
            RichEdit1.Visible := true;
            exit;
        end;

end;

procedure TFrameParties.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFrameParties.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
    d: RTreeData;
    v: Variant;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := true;
end;

procedure TFrameParties.VirtualStringTree1GetImageIndex
  (Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: boolean; var ImageIndex: TImageIndex);
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

procedure TFrameParties.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFrameParties.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
    p: PTreeData;

begin
    p := Sender.GetNodeData(Node);
    if not Column in [0,1,2] then
        exit;



    case Column of
        0:
            begin
                if p.X is TNodePartyProducts then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clGreen;
                    TargetCanvas.Font.Style := [fsItalic];

                end

                else if p.X is TNodeParty then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end
                else if p.X is TNodeProduct then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clMaroon;
                end

                else if p.X is TNodePartyLogs then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clTeal;
                    TargetCanvas.Font.Style := [fsItalic];

                end

                else if p.X is TNodeWork then
                begin
                    if not Sender.Selected[Node] then
                    begin
                        TargetCanvas.Font.Color := clTeal;
                        if (p.X as TNodeWork).FHasError then
                            TargetCanvas.Font.Color := clRed;
                    end;

                    TargetCanvas.Font.Style := [fsItalic];

                end;

            end;
    end;

end;

end.
