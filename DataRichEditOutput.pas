unit DataRichEditOutput;

interface

uses RichEdit, vcl.comctrls;

procedure PrintCurrentWorkMessages(ARichEdit: TRichEdit; work_index: integer);
procedure PrintDayLog(ARichEdit: TRichEdit; day, month, year: integer);
procedure PrintWorkLog(ARichEdit: TRichEdit; record_id: longint);

procedure PrintWorkMessages(ARichEdit: TRichEdit; work_index: integer;
  work: string; product_serial: variant; created_at: TDatetime; level: integer;
  Text: string);

procedure PrintLastMessages(ARichEdit: TRichEdit; count: integer);

implementation

uses FireDAC.Comp.Client, System.SysUtils, dateutils, UnitData, richeditutils,
    variantutils, stringutils;

procedure PrintWorkMessages(ARichEdit: TRichEdit; work_index: integer;
  work: string; product_serial: variant; created_at: TDatetime; level: integer;
  Text: string);
var
    s: string;
begin
    s := Text;
    if (not VariantIsEmptyOrNull(product_serial)) AND (product_serial <> 0) then
        s := '¿Õ ¿“ ' + inttostr(product_serial) + ': ' + s;
    if (work <> '') AND (work_index = 0) then
        s := work + ': ' + s;
    if work_index <> 0 then
        s := '[' + inttostr2(work_index) + ']: ' + s;
    RichEdit_AddText(ARichEdit, IncHour(created_at, 3), level, s);
end;

procedure PrintCurrentWorkMessages(ARichEdit: TRichEdit; work_index: integer);
begin
    ARichEdit.Lines.Clear;
    with DataModule1.FDQueryCurrentWorkMessages do
    begin
        ParamByName('work_index').Value := work_index;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, work_index, '',
              FieldValues['product_serial'], FieldValues['created_at'],
              FieldValues['level'], FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

procedure PrintLastMessages(ARichEdit: TRichEdit; count: integer);
begin
    ARichEdit.Lines.Clear;
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM work_log2 LIMIT :count;';
        ParamByName('count').Value := count;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, FieldValues['work_index'],
              FieldValues['work_name'], FieldValues['product_serial'],
              FieldValues['created_at'], FieldValues['level'],
              FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

procedure PrintDayLog(ARichEdit: TRichEdit; day, month, year: integer);
begin
    ARichEdit.Lines.Clear;
    with DataModule1.FDQueryDayLog do
    begin
        ParamByName('day').Value := day;
        ParamByName('month').Value := month;
        ParamByName('year').Value := year;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, FieldValues['work_index'],
              FieldValues['work_name'], FieldValues['product_serial'],
              FieldValues['created_at'], FieldValues['level'],
              FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

procedure PrintWorkLog(ARichEdit: TRichEdit; record_id: longint);
begin
    ARichEdit.Lines.Clear;
    with DataModule1.FDQueryWorkMessages do
    begin
        ParamByName('work_id').Value := record_id;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, FieldValues['work_index'], '',
              FieldValues['product_serial'], FieldValues['created_at'],
              FieldValues['level'], FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

end.
